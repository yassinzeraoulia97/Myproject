#Loading the data set


library(readr)
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

#Summary statistics for each variable inside the dataset 

summary(newdata)


