## Installing necessary packages 
install.packages(setdiff(c("randomForest", "ggplot2", "caret"), rownames(installed.packages())))
library(randomForest)
library(caret)

## Loading the dataset
Heart_data <- read.csv("CVD_cleaned.csv")
head(Heart_data)

## This is a very large dataset, let's only select 10000 samples from both groups
Yes_case <- which(Heart_data$Heart_Disease == "Yes")[1:10000]
No_case <- which(Heart_data$Heart_Disease == "No")[1:10000]
Data_yes <- Heart_data[Yes_case, ]
Data_no <-  Heart_data[No_case, ]
Data_total <- rbind(Data_yes, Data_no)

## Age Data 
My_Age_Heart_Data <- data.frame(Data_total$Age_Category, Data_total$Heart_Disease)
names(My_Age_Heart_Data) <- c("Age_Category", "Heart_Disease")

#  This code gets the data in the right format
My_Age_Heart_Data$Age_min <- as.numeric(substr(My_Age_Heart_Data$Age_Category, start = 1, stop = 2))
My_Age_Heart_Data$Age_max <- as.numeric(substr(My_Age_Heart_Data$Age_Category, start = 4, stop = 5))
for (i in 1:20000){
  if (is.na(My_Age_Heart_Data$Age_min[i])){
    My_Age_Heart_Data$Age_min[i] = My_Age_Heart_Data$Age_max[i]}
  if (is.na(My_Age_Heart_Data$Age_max[i])){
    My_Age_Heart_Data$Age_max[i] = My_Age_Heart_Data$Age_min[i]}
}

# This code gives me the average value # look at the age data to see what I am talking about
My_Age_Heart_Data$Average <- (My_Age_Heart_Data$Age_min + My_Age_Heart_Data$Age_max)/2

## Adding cleaned Age data in the data
Data_total$Average <- My_Age_Heart_Data$Average

## Dependent variable vs independent variable
Heart_withFood <- data.frame(Data_total$Fruit_Consumption, Data_total$Alcohol_Consumption,
                             Data_total$Green_Vegetables_Consumption, Data_total$FriedPotato_Consumption,
                             Data_total$Average)
Heart_label <- as.factor(Data_total$Heart_Disease)

## Partitioning of the data for k-means clustering method
set.seed(222)
sample_data <- sample(nrow(Data_total), .7* nrow(Data_total))
train_data <- Heart_withFood[sample_data,]
test_data <- Heart_withFood[-sample_data,]
HeartLabel_Train <- Heart_label[sample_data]
HeartLabel_Test <- Heart_label[-sample_data]


## Does the algorithm get better when it used 4 predictors vs 2 predictors vs more?
## Check when the algorithm equal to 
randomforest_model <- rfcv(train_data, HeartLabel_Train, cv.fold = 5, scale = "log")
sum(randomforest_model$predicted$"4" == HeartLabel_Train)/ length(HeartLabel_Train)
sum(randomforest_model$predicted$"2" == HeartLabel_Train)/ length(HeartLabel_Train)
sum(randomforest_model$predicted$"1" == HeartLabel_Train)/ length(HeartLabel_Train)

##
trControl <- trainControl(method = "cv", number = 10, search ="grid")
rf_model <- train(HeartLabel_Train ~., data = train_data,
                  method = "rf", metric= "Accuracy", 
                  trControl = trControl)
# print(rf_model)]



