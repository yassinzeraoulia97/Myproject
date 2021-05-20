#Loading the data set and libraries

library(tidyverse)
library(dslabs)
library(ggplot2)
library(caret)
library(readr)
library(dplyr)
healthcare_dataset_stroke_data <- read_csv("healthcare-dataset-stroke-data.csv")


#exploration of the data set and rename the data set


mydata <- healthcare_dataset_stroke_data

nrow(mydata)
ncol(mydata)
head(mydata)

#remove unnecessary columns 

newdata <- mydata[, c("gender", "age", "hypertension", "heart_disease", "work_type", "Residence_type", "avg_glucose_level", "bmi", "smoking_status", "stroke")]

head(newdata)

#Check N.A inside different variables

anyNA.data.frame(newdata)

#Summary statistics for each variable inside the data set 

summary(newdata)

# converting the character vector to numeric and computing the mean

newdata$bmi <- (as.numeric(newdata$bmi))

mean(newdata$bmi, na.rm = TRUE)


