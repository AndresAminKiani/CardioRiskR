##############################################################################
##  causal_tree_plus 
##  * fits a Causal Tree **or** a Transformed-Outcome Tree  
##  * grid-searches cp + maxdepth with k-fold CV (can fork out on N cores)  
##  * optional convex smoothing layer on ONE numeric covariate (B-splines)  
##  * quick-n-dirty split-count variable importance  
##  * S3 helpers: print(), varimp(), predict()  
## 
##############################################################################

causal_tree_plus <- function(
  # --------- data bits ----------
  formula,
  data,
  treat,

  # --------- row weights / subset ----------
  weights        = NULL,
  subset         = NULL,

  # --------- tree knobs ----------
  split_rule     = c("CT", "TOT"),
  cv_rule        = c("TOT", "matching"),
  leaf_min       = 2L,
  mtry           = NULL,
  max_depth      = NULL,

  # --------- pscore ----------
  prop_score     = NULL,

  # --------- grid search ----------
  grid           = list(cp = c(0, .005, .01),
                        maxdepth = c(5L, 10L, 20L)),
  nfolds         = 5L,
  eval_metric    = c("ATE", "MSE", "AUUC"),

  # --------- convex add-on ----------
  convex         = FALSE,
  convex_var     = NULL,

  # --------- misc ----------
  importance     = TRUE,
  parallel       = FALSE,
  ncores         = max(1L, parallel::detectCores() - 1L),
  seed           = 2025,
  verbose        = TRUE,

  # --------- dots to rpart / CT ----------
  ...) {

  # ==== STEP 0  sanity ====
  split_rule  <- match.arg(split_rule)
  cv_rule     <- match.arg(cv_rule)
  eval_metric <- match.arg(eval_metric)

  if (!all(treat %in% c(0,1))) stop("treat must be 0/1")
  if (nrow(data) != length(treat)) stop("treat len != rows")

  if (!is.null(subset)) {
    data    <- data[subset, , drop = FALSE]
    treat   <- treat[subset]
    if (!is.null(weights)) weights <- weights[subset]
  }

  mf <- model.frame(formula, data = data, na.action = na.causalTree)
  y  <- model.response(mf)
  x  <- causalTree::causalTree.matrix(mf)
  if (is.null(weights)) weights <- rep(1, nrow(mf))

  # ==== STEP 1  p-score ====
  if (is.null(prop_score)) {
    ps_fit     <- stats::glm(treat ~ ., data = as.data.frame(x),
                             family = binomial())
    prop_score <- stats::predict(ps_fit, type = "response")
  }
  if (any(prop_score <= 0 | prop_score >= 1))
    stop("prop_score outside (0,1)")

  # ==== STEP 2  builder ====
  build_tree <- function(cp_val, depth_val) {

    ctrl_lst <- list(cp        = cp_val,
                     minbucket = leaf_min,
                     maxdepth  = if (is.null(depth_val)) 30L else depth_val,
                     xval      = 0)
    if (!is.null(mtry)) ctrl_lst$maxsurrogate <- mtry
    ct_ctrl  <- do.call(causalTree::causalTree.control, ctrl_lst)

    if (split_rule == "TOT") {
      w_inv  <- 1 / (prop_score - 1 + treat)
      pieces <- strsplit(deparse(formula), "~", fixed = TRUE)[[1]]
      if (length(pieces) != 2) stop("need y ~ x formula")
      f_int  <- as.formula(paste(pieces[1], "* w_inv ~", pieces[2]))

      model <- rpart::rpart(f_int,
                            data    = data.frame(data, w_inv = w_inv),
                            weights = weights,
                            method  = "anova",
                            control = rpart::rpart.control(cp        = cp_val,
                                                           minbucket = leaf_min,
                                                           maxdepth  = if (is.null(depth_val)) 30L else depth_val,
                                                           xval      = 0),
                            ...)
      class(model) <- "causalTree"
      return(model)
    }

    causalTree::causalTree(formula      = formula,
                           data         = data,
                           weights      = weights,
                           treatment    = treat,
                           split.option = "CT",
                           cv.option    = cv_rule,
                           minsize      = leaf_min,
                           p            = if (cv_rule == "TOT") prop_score else -1,
                           control      = ct_ctrl,
                           ...)
  }

  # ==== STEP 3  scorer ====
  score_tree <- function(tr) {
    tau_hat <- predict(tr, newdata = data, type = "vector")

    if (eval_metric == "MSE") {
      pseudo <- treat * y / prop_score - (1 - treat) * y / (1 - prop_score)
      return(mean((tau_hat - pseudo)^2))
    }
    if (eval_metric == "ATE") return(abs(mean(tau_hat)))

    ord <- order(-tau_hat)                    # AUUC rough
    sum(cumsum(y[ord] * treat[ord])) / nrow(data)
  }

  # ==== STEP 4  grid materialise ====
  if (is.null(grid$cp))       grid$cp       <- 0
  if (is.null(grid$maxdepth)) grid$maxdepth <- if (is.null(max_depth)) 30L else max_depth
  grid_df <- expand.grid(cp = grid$cp,
                         maxdepth = grid$maxdepth,
                         KEEP.OUT.ATTRS = FALSE)

  set.seed(seed)
  folds <- sample(rep(1:nfolds, length = nrow(data)))

  # helper for parLapply
  helper_run_fold <- function(fid, cpv, dv) {
    idx <- folds != fid
    score_tree(build_tree(cpv, dv))
  }

  if (parallel && nfolds > 1) {
    cl <- parallel::makeCluster(min(ncores, nfolds))
    on.exit(parallel::stopCluster(cl), add = TRUE)
  }

  perf <- numeric(nrow(grid_df))
  for (g in seq_len(nrow(grid_df))) {
    cpv <- grid_df$cp[g]; dv <- grid_df$maxdepth[g]

    if (nfolds == 1) {
      perf[g] <- score_tree(build_tree(cpv, dv))
    } else {
      res <- if (parallel)
        parallel::parLapply(cl, 1:nfolds, helper_run_fold, cpv, dv)
      else
        lapply(1:nfolds, helper_run_fold, cpv, dv)
      perf[g] <- mean(unlist(res))
    }

    if (verbose)
      cat("grid", g, "cp", cpv, "depth", dv, "metric", round(perf[g],4), "\n")
  }

  grid_df$metric <- perf
  best <- which.min(perf)
  final_tree <- build_tree(grid_df$cp[best], grid_df$maxdepth[best])

  # ==== STEP 5  convex smoother ====
  cvx_fit <- NULL
  if (convex) {
    if (is.null(convex_var) || !convex_var %in% names(data))
      stop("convex_var missing / bad")
    if (!is.numeric(data[[convex_var]])) stop("convex_var must be numeric")

    # leaf-wise avg
    leaf_tau  <- predict(final_tree, type = "vector")
    leaf_id   <- attr(predict(final_tree, type = "matrix"), "node")
    knots     <- aggregate(cbind(te = leaf_tau,
                                 x  = data[[convex_var]]),
                           by = list(node = leaf_id),
                           FUN = mean)

    check_and_install("cobs")
    cvx_fit <- cobs::cobs(knots$x, knots$te,
                          constraint = "convex",
                          lambda = 0)
  }

  # ==== STEP 6  var-imp ====
  vimp <- NULL
  if (importance && !is.null(final_tree$splits)) {
    tmp <- rowsum(final_tree$splits[, "improve"],
                  rownames(final_tree$splits))
    vimp <- sort(as.numeric(tmp), decreasing = TRUE)
    names(vimp) <- rownames(tmp)[order(-as.numeric(tmp))]
  }

  # ==== STEP 7  predict closure ====
  pred_fun <- function(newdata) {
    raw <- predict(final_tree, newdata = newdata, type = "vector")
    if (!is.null(cvx_fit)) {
      xnew <- newdata[[convex_var]]
      ok   <- is.finite(xnew)
      if (any(ok))
        raw[ok] <- stats::predict(cvx_fit, z = xnew[ok])[ ,2]
    }
    node <- attr(predict(final_tree, newdata = newdata, type = "matrix"), "node")
    list(te_hat = raw, node_id = node)
  }

  out <- list(fit        = final_tree,
              grid_perf  = grid_df,
              vimp       = vimp,
              call       = match.call(),
              predict    = pred_fun,
              convex_on  = convex,
              convex_var = convex_var,
              convex_fit = cvx_fit)
  class(out) <- "causal_tree_plus"
  out
}

check_and_install <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    suppressMessages(utils::install.packages(pkg, quiet = TRUE))
    if (!requireNamespace(pkg, quietly = TRUE))
      stop("could not install package ", pkg)
  }
}

varimp.causal_tree_plus <- function(object, top = 20, ...) {
  if (is.null(object$vimp))
    stop("importance was FALSE at fit")
  head(object$vimp, n = top)
}
