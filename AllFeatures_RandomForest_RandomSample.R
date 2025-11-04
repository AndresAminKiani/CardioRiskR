## Installing necessary packages 
install.packages(setdiff(c("randomForest", "ggplot2", "caret","cowplot", "rfUtilities", 
                           "writexl"), rownames(installed.packages())))
#install.packages("writexl")
library(randomForest)
library(caret)
library(cowplot)
library(rpart)
library("dplyr")
library("writexl")

## Loading the dataset
Heart_data <- read.csv("CVD_cleaned.csv")
head(Heart_data)

## We will select only 20000 participants in our analysis. 
Number_ofparticipants <- 10000
## This is a very large dataset, let's only select 10000 samples from both groups
set.seed(150)
Yes_case <- sample(which(Heart_data$Heart_Disease == "Yes"), Number_ofparticipants)
set.seed(150)
No_case <- sample(which(Heart_data$Heart_Disease == "No"), Number_ofparticipants)
Data_yes <- Heart_data[Yes_case, ]
Data_no <-  Heart_data[No_case, ]
Data_total <- rbind(Data_yes, Data_no)

## We will convert the Age variable that is in range form in the dataset to a numerical form.
My_Age_Heart_Data <- data.frame(Data_total$Age_Category, Data_total$Heart_Disease)
names(My_Age_Heart_Data) <- c("Age_Category", "Heart_Disease") 

#  This code gets the data in the right format
My_Age_Heart_Data$Age_min <- 
  as.numeric(substr(My_Age_Heart_Data$Age_Category, start = 1, stop = 2))
My_Age_Heart_Data$Age_max <- 
  as.numeric(substr(My_Age_Heart_Data$Age_Category, start = 4, stop = 5))
for (i in 1:Number_ofparticipants){
  # if (is.na(My_Age_Heart_Data$Age_min[i])){
  #   My_Age_Heart_Data$Age_min[i] = My_Age_Heart_Data$Age_max[i]}
  if (is.na(My_Age_Heart_Data$Age_max[i])){
    My_Age_Heart_Data$Age_max[i] = My_Age_Heart_Data$Age_min[i]}
}
## We check if there were other missing age value included in the data.
more_missing_agevalue <- Data_total$Age_Category[(is.na(My_Age_Heart_Data$Age_max))]

## Since all the missing values were participants 80 and older, we replace those missing values with 80.
My_Age_Heart_Data$Age_max[(is.na(My_Age_Heart_Data$Age_max))] = as.numeric(80)
# This code gives me the final age value to be used in the analysis.
My_Age_Heart_Data$FinalAge <- 
  (My_Age_Heart_Data$Age_min + My_Age_Heart_Data$Age_max)/2
head(My_Age_Heart_Data)

## Adding cleaned Age data in the data
Data_total$FinalAge <- My_Age_Heart_Data$FinalAge
is.na(Data_total$FinalAge)

## Converting the variables that are in character form to factors
Data_total$Diabetes <- factor(Data_total$Diabetes)
Data_total$Checkup <- factor(Data_total$Checkup)
Data_total$General_Health <- factor(Data_total$General_Health)
Data_total$Arthritis <- factor(Data_total$Arthritis)
Data_total$Sex <- factor(Data_total$Sex)
Data_total$Smoking_History <- factor(Data_total$Smoking_History)
Data_total$Height_.cm. <- as.numeric(Data_total$Height_.cm.)
Data_total$Skin_Cancer <- factor(Data_total$Skin_Cancer)
Data_total$Other_Cancer <- factor(Data_total$Other_Cancer)
Data_total$Exercise <- factor(Data_total$Exercise)
Data_total$Depression <- factor(Data_total$Depression)

## Here we will remove the Diabetes column that do not make sense to us. 
Extra_DiabetesandnullAge_Column <- 
  which((Data_total$Diabetes == "Yes, but female told only during pregnancy") | 
          (Data_total$Diabetes == "No, pre-diabetes or borderline diabetes"))

## This is our final data with clean variables
Data_total <- Data_total[-Extra_DiabetesandnullAge_Column, ]
Data_total$Diabetes <- factor(Data_total$Diabetes)
str(Data_total)

## Dependent variable vs independent variable of the model 
InputtoHeartDisease_Model2 <- 
  data.frame(Data_total$Fruit_Consumption, Data_total$Alcohol_Consumption,
  Data_total$Green_Vegetables_Consumption, Data_total$FriedPotato_Consumption,
  Data_total$FinalAge, Data_total$BMI, Data_total$Diabetes,
  Data_total$Checkup, Data_total$General_Health, Data_total$Arthritis,
  Data_total$Sex, Data_total$Smoking_History, Data_total$Height_.cm.,
  Data_total$Weight_.kg.,Data_total$Skin_Cancer, Data_total$Other_Cancer, 
  Data_total$Exercise, Data_total$Depression)
names(InputtoHeartDisease_Model2) <- 
  c("Fruit", "Alcohol", "GreenVeggie", "FriedPotato",
    "FinalAge", "BMI", "Diabetes", "Checkup", "GeneralHealth", 
    "Arthritis", "Sex", "SmokingHistory", "Height_.cm", 
    "Weight_.kg.","SkinCancer", "OtherCancer", "Exercise", "Depression")

## Dependent Variable
Heart_label <- as.factor(Data_total$Heart_Disease)


## Partitioning of the data into training/testing for random forest model
set.seed(222)
sample_data <- sample(nrow(InputtoHeartDisease_Model2), .7* nrow(InputtoHeartDisease_Model2))
train_data <- InputtoHeartDisease_Model2[sample_data, ]
test_data <- InputtoHeartDisease_Model2[-sample_data, ]
HeartLabel_Train <- Heart_label[sample_data]
HeartLabel_Test <- Heart_label[-sample_data]

## This is our random forest model with all features
our_model_500 <- randomForest(HeartLabel_Train ~ ., data = train_data, ntree = 500, 
                              proximity = T, mtry = 3)
our_model_500

## Parameter Tuning A: here we find the optimal number of decision trees
plot(our_model_500, log = "y", main = "All-features: Error rate as a function 
     of  ntrees (= 500)", lwd = 3, bty = "n", ylim = c(0.2,0.8))
legend("top", colnames(our_model_500$err.rate), col = 1:3, cex = 0.8, fill = 1:3)

## This is our random forest model with all features
our_model_1000 <- randomForest(HeartLabel_Train ~ ., data = train_data, ntree = 1000, 
                              proximity = T, mtry = 3)
our_model_1000
plot(our_model_1000, log = "y", main = "All-features: Error rate as a function 
     of  ntrees (= 1000)", lwd = 3, bty = "n", ylim = c(0.2,0.8))
legend("top", colnames(our_model_1000$err.rate), col = 1:3, cex = 0.8, fill = 1:3)

## Parameter Tuning B: now we will find the optimal number of split of the tree
oob_values <- vector(length = 10)
for (i in 1:10) {
  Num_model <- randomForest(HeartLabel_Train ~ ., data = train_data, mtry = i, ntree = 500)
  oob_values[i] <- Num_model$err.rate[nrow(Num_model$err.rate), 1]
}
plot(oob_values, main = "Error rate as a function of Num of Nodes", lwd = 3, bty = "n",
     ylim = c(0.2,0.8), col = 2, xlab = "Nodes", ylab = "OOB Rate")

## We will record the out of bag error estimate of the optimal model 
our_model_500 <- randomForest(HeartLabel_Train ~ ., data = train_data, ntree = 500, 
                              proximity = T, mtry = 2)
our_model_500

## Testing the model accuracy on the training data
train_rf <- predict(our_model_500, train_data)
Train_Summary <- confusionMatrix(train_rf, HeartLabel_Train)
Train_Summary

## Testing the model accuracy on the testing data
test_rf <- predict(our_model_500, test_data)
Test_Summary <- confusionMatrix(test_rf, HeartLabel_Test)
Test_Summary ## All features

## Which variables are most important according to our model
varImpPlot(our_model, sort = T, n.var = 10, main = "Variable Importance Plot") 

#install.packages("ggalt")
suppressPackageStartupMessages({
  library(ggalt)
  library(randomForest)
  library(data.table)
})

## Here we get a nice looking plot for the feature important index
imp <- data.table(importance(our_model_500), keep.rownames = TRUE)
imp = arrange(imp, MeanDecreaseGini)
imp[, rn := factor(rn, unique(rn))]
ggplot(melt(imp, id.vars="rn")[grep("Mean", variable)], 
       aes(x=rn, y=value, label = round(value, 1))) + 
  geom_lollipop(point.size = 4, point.colour = "orange3", pch = 19, bg = 2) +
  geom_text(aes(nudge_y = 2), hjust = -.3) +
  coord_flip() +
  #facet_wrap(~variable) +
  theme_minimal() +
  labs(y="MeanDecreaseGini", x=NULL, 
  title = "Feature Importance Plot from Cardio Vascular Dataset", cex = 0.9) +
  expand_limits(y = 1300) + theme(plot.title = element_text(face = "bold", hjust = 0.5))