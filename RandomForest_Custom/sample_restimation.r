# Fit a causal forest with optional sample-splitting
#
# X           data.frame or matrix of covariates (n x p)
# Y           numeric outcome vector (length n)
# W           binary treatment indicator (0 / 1 or FALSE / TRUE)
# n_trees     integer, number of causal trees
# sample_frac fraction of observations drawn per tree (0–1)
# min_leaf    minimum leaf size (passed to causalTree)
# honest      TRUE = use a separate re-estimation sample,
#             FALSE = grow each tree on one sample only
#
# Returns a list with:
#   $trees  – list of fitted causalTree objects
#   $inbag  – n x n_trees logical matrix showing which rows
#             were sampled for each tree
#
causal_forest_basic <- function(X, Y, W,
                                n_trees     = 500,
                                sample_frac = 0.1,
                                min_leaf    = 1,
                                honest      = TRUE) {

  # Sanity checks
  if (any(is.na(X)) || anyNA(Y) || anyNA(W))
    stop("Inputs contain missing values; please handle them first.")

  if (!is.numeric(Y))
    stop("`Y` must be numeric.")

  if (!all(W %in% c(0, 1, FALSE, TRUE)))
    stop("`W` must be binary (0/1 or FALSE/TRUE).")

  X <- as.data.frame(X)
  n <- nrow(X)
  sample_size <- max(1L, floor(sample_frac * n))

  # Helper to grow one tree (with optional re-estimation)
  build_tree <- function(idx_train, idx_reestimate = NULL) {
    tree <- causalTree::causalTree(
      Y ~ .,
      data        = data.frame(X, Y = Y)[idx_train, ],
      treatment   = W[idx_train],
      method      = "anova",
      cp          = 0,
      minbucket   = min_leaf,
      cv.option   = "matching",
      split.option = "CT",
      xval        = 0
    )

    if (!is.null(idx_reestimate)) {
      tree <- causalTree::refit.causalTree(
        tree,
        newx      = X[idx_reestimate, ],
        newy      = Y[idx_reestimate],
        treatment = W[idx_reestimate]
      )
    }
    tree
  }

  # Grow the forest
  forest <- list(
    trees = vector("list", n_trees),
    inbag = matrix(FALSE, nrow = n, ncol = n_trees)
  )

  message("Building ", n_trees, " causal trees")

  for (t in seq_len(n_trees)) {

    if (honest) {
      idx <- sample.int(n, 2 * sample_size, replace = FALSE)
      idx_train <- idx[1:sample_size]
      idx_reest <- idx[(sample_size + 1):(2 * sample_size)]
      forest$trees[[t]] <- build_tree(idx_train, idx_reest)
      forest$inbag[c(idx_train, idx_reest), t] <- TRUE
    } else {
      idx_train <- sample.int(n, sample_size, replace = FALSE)
      forest$trees[[t]] <- build_tree(idx_train)
      forest$inbag[idx_train, t] <- TRUE
    }

    if (t %% 10 == 0 || t == n_trees)
      message("  finished tree ", t)
  }

  class(forest) <- "causal_forest_basic"
  forest
}
