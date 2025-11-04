## Installing necessary packages 
install.packages(setdiff(c("randomForest", "ggplot2", "caret","cowplot", "rfUtilities"), 
                         rownames(installed.packages())))
library(randomForest)
library(caret)
library(cowplot)
library(rpart)

## Loading the dataset
Heart_data <- read.csv("CVD_cleaned.csv")
head(Heart_data)

Number_ofparticipants <- 100

## This is a very large dataset, let's only select 10000 samples from both groups
Yes_case <- which(Heart_data$Heart_Disease == "Yes")[(1:Number_ofparticipants + 1)/2]
No_case <- which(Heart_data$Heart_Disease == "No")[(1:Number_ofparticipants + 2)/2]
Data_yes <- Heart_data[Yes_case, ]
Data_no <-  Heart_data[No_case, ]
Data_total <- rbind(Data_yes, Data_no)

## Depression Data 
My_Depression_Heart_Data <- data.frame(Data_total$Depression, Data_total$Heart_Disease)
names(My_Depression_Heart_Data) <- c("Depression", "Heart_Disease") ## 200 
My_Depression_Heart_Data$Depression <- factor(My_Depression_Heart_Data$Depression)
My_Depression_Heart_Data$Depression <- 
  ifelse(My_Depression_Heart_Data$Depression == "Yes", "Depressed", "Undepressed")

## Depression ratio in heart dieased or heart-healthy
ggplot(My_Depression_Heart_Data, aes(Heart_Disease, fill = Depression, col = Depression)) + 
  geom_bar(position=position_dodge()) + labs(y = "Number of People", 
 title = "Depression in Heart_Dieseased vs. Heart Healthy Population") + theme_minimal() +
  scale_fill_manual(values=c("grey86","lightgoldenrod3"))  + 
  scale_colour_manual(values = c('black','darkgoldenrod4'))

## Diabetes Data 
My_Diabetes_Heart_Data <- data.frame(Data_total$Diabetes, Data_total$Heart_Disease)
names(My_Diabetes_Heart_Data) <- c("Diabetes", "Heart_Disease") ## 200 
My_Diabetes_Heart_Data$Depression <- factor(My_Diabetes_Heart_Data$Diabetes)
Extra_Diabetes_Column <- 
  which((My_Diabetes_Heart_Data$Diabetes == "Yes, but female told only during pregnancy") | 
          (My_Diabetes_Heart_Data$Diabetes == "No, pre-diabetes or borderline diabetes"))
My_Diabetes_Heart_Data <- My_Diabetes_Heart_Data[-Extra_Diabetes_Column, ]
My_Diabetes_Heart_Data$Diabetes <- 
  ifelse(My_Diabetes_Heart_Data$Diabetes == "Yes", "With-Diabetes", "No-Diabetes")

## Diabetes ratio in heart dieased or heart-healthy
ggplot(My_Diabetes_Heart_Data, aes(Heart_Disease, fill = Diabetes, col = Diabetes)) + 
  geom_bar(position=position_dodge()) + labs(y = "People", 
title = "Diabetes in Heart-Dieseased vs. Heart-Healthy population") + theme_minimal() +
scale_fill_manual(values=c("palegreen3","seashell2"))  + 
scale_colour_manual(values = c('green4','seashell4')) 

## All Variables of the data
names(Data_total)
unique(Data_total$Checkup)
unique(Data_total$General_Health)
unique(Data_total$Arthritis)
unique(Data_total$Sex)
unique(Data_total$Smoking_History)
unique(Data_total$Height_.cm.)
unique(Data_total$Weight_.kg.)
unique(Data_total$Skin_Cancer)
unique(Data_total$Other_Cancer)
unique(Data_total$Exercise)

## Do they include missing values?
sum(is.na(Data_total$Checkup))
sum(is.na(Data_total$General_Health))
sum(is.na(Data_total$Arthritis))
sum(is.na(Data_total$Sex))
sum(is.na(Data_total$Smoking_History))
sum(is.na(Data_total$Height_.cm.))
sum(is.na(Data_total$Skin_Cancer))
sum(is.na(Data_total$Other_Cancer))
sum(is.na(Data_total$Exercise))
