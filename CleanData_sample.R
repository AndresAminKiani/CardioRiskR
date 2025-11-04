## Installing necessary packages 
install.packages(setdiff(c("randomForest", "ggplot2", "caret","cowplot", "rfUtilities"), 
                         rownames(installed.packages())))
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
names(My_Age_Heart_Data) <- c("Age_Category", "Heart_Disease") ## 200 
#  This code gets the data in the right format
My_Age_Heart_Data$Age_min <- as.numeric(substr(My_Age_Heart_Data$Age_Category, start = 1, stop = 2))
My_Age_Heart_Data$Age_max <- as.numeric(substr(My_Age_Heart_Data$Age_Category, start = 4, stop = 5))
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
My_Age_Heart_Data$FinalAge <- (My_Age_Heart_Data$Age_min + My_Age_Heart_Data$Age_max)/2
head(My_Age_Heart_Data)

## Adding cleaned Age data in the data
Data_total$FinalAge <- My_Age_Heart_Data$FinalAge

## Converting the variables that are in character form to factors
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

## Here we will remove the Diabetes column that do not make sense to us. 
Extra_DiabetesandnullAge_Column <- 
  which((Data_total$Diabetes == "Yes, but female told only during pregnancy") | 
          (Data_total$Diabetes == "No, pre-diabetes or borderline diabetes") | 
          is.na(Data_total$FinalAge))

## This is our final data with clean variables
Data_total <- Data_total[-Extra_DiabetesandnullAge_Column, ]
#sum(CVD_cleaned_Data$Heart_Disease == "No")
