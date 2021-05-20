#Loading the data set and libraries
library(naniar) # handling missing data
library(skimr) # quick overview over the datase # ML 
library(MLmetrics) # F1 Score
library(imbalance) # algorithms to deal with imbalanced datasets
library(gridExtra) # display plots in grids
library(patchwork) # arrange plots side by side
library(tidyverse)
library(dslabs)
library(ggplot2)
library(caret)
library(readr)
library(dplyr)
healthcare_dataset_stroke_data <- read_csv("healthcare-dataset-stroke-data.csv")

# set a seed for reproducible results
set.seed(88)

# custom plot size function
fig <- function(width, heigth){
  options(repr.plot.width = width, repr.plot.height = heigth)
}

## ggplot custom theme
theme_bigfont <- theme(plot.title = element_text(size=22),
                       axis.text.x= element_text(size=15),
                       axis.text.y= element_text(size=15), 
                       axis.title=element_text(size=18),
                       legend.text = element_text(size = 14))

# read data into R
stroke_data <- healthcare_dataset_stroke_data

# check the first few rows
head(stroke_data)

# summary of the data

summary(stroke_data)



#########Data cleaning / handling missing data



# check unique values of categorical values
cat("Gender:")
unique(stroke_data$gender)
cat("Married:")
unique(stroke_data$ever_married)
cat("Work type:")
unique(stroke_data$work_type)
cat("Residence type:")
unique(stroke_data$Residence_type)
cat("Smoking:")
unique(stroke_data$smoking_status)

# how many "N/A" values are in my dataset per column?
miss_scan_count(data = stroke_data, search = list("N/A", "Unknown"))

#There are 201 "N/A" values in the bmi column that likely caused this column to be parsed as character, although it should be numerical.


###there are a lot of "Unknown" values in smoking_status

fig(15, 8)

stroke_data %>%
  group_by(smoking_status) %>%
  summarise(count = length(smoking_status)) %>%
  mutate(smoking_status = factor(smoking_status)) %>%
  ggplot(aes(x = fct_reorder(smoking_status, count), y = count, fill = factor(ifelse(smoking_status=="Unknown","Unknown","Known")))) +
  geom_col() +
  geom_text(aes(label = count, x = smoking_status, y = count), size = 6, hjust = 1.5) +
  coord_flip() +
  scale_fill_manual(values = c("Unknown" = "green", "Known" = "darkgrey")) +
  labs(x = "smoking status") +
  theme(legend.position = "none") +
  theme_bigfont



# replace the "N/A" in bmi
stroke_data_clean <- replace_with_na(data = stroke_data, replace = list(bmi = c("N/A"), smoking_status = c("Unknown"))) %>%
  # change bmi to numeric 
  mutate(bmi = as.numeric(bmi))

# check
summary(stroke_data_clean)
unique(stroke_data_clean$smoking_status)




# check distribution of bmi
ggplot(stroke_data_clean, aes(x = bmi)) +
  geom_histogram() +
  labs(title = "Distribution of BMI") +
  theme_bigfont

#Only 5% of the people inside the dataset had a stroke 

fig(10, 8)

# plot prop of people who had a stroke
stroke_data_imp2 %>%
  select(stroke) %>%
  ggplot(aes(x = stroke)) +
  geom_bar() +
  theme_bigfont

# count how many people had a stroke and the prop
stroke_data_imp2 %>%
  group_by(stroke) %>%
  summarize(n = n()) %>%
  mutate(prop = round(n / sum(n), 2))

#We see that only 5% of all the people in the data set had a stroke at some point.
#This means that our baseline dummy model has an accuracy of 95%. 
#That is if we would predict a person to not have a stroke all the time.

