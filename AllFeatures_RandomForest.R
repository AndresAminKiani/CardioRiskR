## Installing necessary packages 
install.packages(setdiff(c("randomForest", "ggplot2", "caret","cowplot", "rfUtilities"), 
                         rownames(installed.packages())))
install.packages("writexl")
library(randomForest)
library(caret)
library(cowplot)
library(rpart)
library("dplyr")
library("writexl")
## Loading the dataset
Heart_data <- read.csv("CVD_cleaned.csv")
head(Heart_data)

Number_ofparticipants <- 10000
## This is a very large dataset, let's only select 10000 samples from both groups
Yes_case <- which(Heart_data$Heart_Disease == "Yes")[(1:Number_ofparticipants + 1)/2]
No_case <- which(Heart_data$Heart_Disease == "No")[(1:Number_ofparticipants + 2)/2]
Data_yes <- Heart_data[Yes_case, ]
Data_no <-  Heart_data[No_case, ]
Data_total <- rbind(Data_yes, Data_no)

## Age Data 
My_Age_Heart_Data <- data.frame(Data_total$Age_Category, Data_total$Heart_Disease)
names(My_Age_Heart_Data) <- c("Age_Category", "Heart_Disease") ## 200 
#  This code gets the data in the right format
My_Age_Heart_Data$Age_min <- as.numeric(substr(My_Age_Heart_Data$Age_Category, start = 1, stop = 2))
My_Age_Heart_Data$Age_max <- as.numeric(substr(My_Age_Heart_Data$Age_Category, start = 4, stop = 5))
for (i in 1:Number_ofparticipants){
  if (is.na(My_Age_Heart_Data$Age_min[i])){
    My_Age_Heart_Data$Age_min[i] = My_Age_Heart_Data$Age_max[i]}
  if (is.na(My_Age_Heart_Data$Age_max[i])){
    My_Age_Heart_Data$Age_max[i] = My_Age_Heart_Data$Age_min[i]}
}
# This code gives me the average value # look at the age data to see what I am talking about
My_Age_Heart_Data$FinalAge <- (My_Age_Heart_Data$Age_min + My_Age_Heart_Data$Age_max)/2
## Adding cleaned Age data in the data
Data_total$FinalAge <- My_Age_Heart_Data$FinalAge

## Converting the variables in character to factors
Data_total$Diabetes <- factor(Data_total$Diabetes)
Data_total$Checkup <- factor(Data_total$Checkup)
Data_total$General_Health <- factor(Data_total$General_Health)
Data_total$Arthritis <- factor(Data_total$Arthritis)
Data_total$Sex <- factor(Data_total$Sex)
Data_total$Smoking_History <- factor(Data_total$Smoking_History)
Data_total$Height_.cm. <- factor(Data_total$Height_.cm.)
Data_total$Skin_Cancer <- factor(Data_total$Skin_Cancer)
Data_total$Other_Cancer <- factor(Data_total$Other_Cancer)
Data_total$Exercise <- factor(Data_total$Exercise)
Data_total$Depression <- factor(Data_total$Depression)

Extra_DiabetesandnullAge_Column <- 
  which((Data_total$Diabetes == "Yes, but female told only during pregnancy") | 
          (Data_total$Diabetes == "No, pre-diabetes or borderline diabetes") | 
          is.na(Data_total$FinalAge))

## Dependent variable vs independent variable of the model 
InputtoHeartDisease <- data.frame(Data_total$Fruit_Consumption, Data_total$Alcohol_Consumption,
                                  Data_total$Green_Vegetables_Consumption, Data_total$FriedPotato_Consumption,
                                  Data_total$FinalAge, Data_total$BMI, Data_total$Diabetes,
                                  Data_total$Checkup, Data_total$General_Health, Data_total$Arthritis,
                                  Data_total$Sex, Data_total$Smoking_History, Data_total$Height_.cm.,
                                  Data_total$Weight_.kg.,Data_total$Skin_Cancer, Data_total$Other_Cancer, 
                                  Data_total$Exercise, Data_total$Depression)
InputtoHeartDisease_Model <- InputtoHeartDisease[-Extra_DiabetesandnullAge_Column, ]
names(InputtoHeartDisease_Model) <- c("Fruit", "Alcohol", "GreenVeggie", "FriedPotato",
                                      "FinalAge", "BMI", "Diabetes", "Checkup", "GeneralHealth", 
                                      "Arthritis", "Sex", "SmokingHistory", "Height_.cm", 
                                      "Weight_.kg.","SkinCancer", "OtherCancer", "Exercise", "Depression")
Heart_label <- as.factor(Data_total$Heart_Disease)[-Extra_DiabetesandnullAge_Column]
#Heart_label <- ifelse(HeartLabel_Train == "Yes", 1, 0)

## Partitioning of the data for random forest
set.seed(222)
sample_data <- sample(nrow(InputtoHeartDisease_Model), .7* nrow(InputtoHeartDisease_Model))
train_data <- InputtoHeartDisease_Model[sample_data, ]
#write_xlsx(train_data, "RawInput.xlsx")
test_data <- InputtoHeartDisease_Model[-sample_data, ]
HeartLabel_Train <- Heart_label[sample_data]
HeartLabel_Test <- Heart_label[-sample_data]

## This is our random forest model with all features
our_model_500 <- randomForest(HeartLabel_Train ~ ., data = train_data, ntree = 500, 
                              proximity = T, mtry = 3)
#save(our_model_500, file = "RandomforestModel_3nodes500ntree_EntireFeatures.RData")
our_model_500
#our_model_500$err.rate
plot(our_model_500, log = "y", main = "Error rate as a function of  ntrees (= 500)", lwd = 2, bty = "n")
legend("top", colnames(our_model$err.rate), col = 1:3, cex = 0.8, fill = 1:3)

## Histogram of trees node
hist(treesize(our_model_500),
     main = "No. of Nodes for the Trees",
     col = "steelblue")

## Testing the accuracy on the train data
train_rf <- predict(our_model_500, train_data)
Train_Summary <- confusionMatrix(train_rf, HeartLabel_Train)
Train_Summary

## Testing the accuracy on the train data
test_rf <- predict(our_model_500, test_data)
Test_Summary <- confusionMatrix(test_rf, HeartLabel_Test)
Test_Summary

## Which variables are important and how much
varImpPlot(our_model, sort = T, n.var = 10, main = "Variable Importance Plot") 

#install.packages("ggalt")
suppressPackageStartupMessages({
  library(ggalt)
  library(randomForest)
  library(data.table)
})

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
  labs(y="MeanDecreaseGini", x=NULL, title = "Feature Importance Plot from Cardio Vascular Dataset", cex = 0.9) +
  expand_limits(y = 1300) + theme(plot.title = element_text(face = "bold", hjust = 0.5))

# the value with the largest meandecreasegini is the most important.



