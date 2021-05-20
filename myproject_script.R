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



# impute median and bind shadow to evaluate imputation
stroke_data_imp <- bind_shadow(stroke_data_clean) %>% 
  impute_median_at(.vars = c("bmi")) %>%
  add_label_shadow()

# Explore the median values in bmi in the imputed dataset
ggplot(stroke_data_imp, 
       aes(x = bmi_NA, y = bmi)) + 
  geom_boxplot() +
  labs(title = "Comparison, no-missing vs. imputed values for BMI") +
  theme_bigfont


stroke_data_imp <- impute_median_at(stroke_data_clean, .vars = c("bmi"))


fig(16,8)

p1 <- ggplot(stroke_data_imp, 
             aes(x = smoking_status, fill = smoking_status)) + 
  geom_bar() +
  labs(title = "Before filling in NA values in smoking_status") +
  theme(legend.position = "none") +
  theme_bigfont

# fill imputation based on previous unique value in "smoking_status" column
after <- stroke_data_imp %>% 
  fill(smoking_status)
# mode imputation which leads to worse performance of models:
#mutate(across(c(smoking_status)), replace(., is.na(.), "never smoked"))

# Explore the median values in bmi in the imputed dataset
p2 <- ggplot(after, 
             aes(x = smoking_status, fill = smoking_status)) + 
  geom_bar() +
  labs(title = "After filling in NA values in smoking_status") +
  theme(legend.position = "none") +
  theme_bigfont

p1 + p2

###########################################################
stroke_data_imp2 <- stroke_data_imp %>%
  fill(smoking_status) %>%
  #mutate(across(c(smoking_status)), replace(., is.na(.), "never smoked")) %>%
  mutate(across(c(hypertension, heart_disease), factor),
         across(where(is.character), as.factor),
         across(where(is.factor), as.numeric),
         stroke = factor(ifelse(stroke == 0, "no", "yes")))

stroke_data_imp2 <- stroke_data_imp2 %>%
  mutate(bmi = case_when(bmi < 18.5 ~ "underweight",
                         bmi >= 18.5 & bmi < 25 ~ "normal weight",
                         bmi >= 25 & bmi < 30 ~ "overweight",
                         bmi >= 30 ~ "obese"),
         bmi = factor(bmi, levels = c("underweight", "normal weight", "overweight", "obese"), order = TRUE))



#Only 5% of the people inside the data set had a stroke 

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


#Balancing the imbalance


# check imbalance ratio
imbalanceRatio(as.data.frame(stroke_data_imp2), classAttr = "stroke")

stroke_test <- stroke_data_imp2 %>%
  mutate(
    stroke = as.character(stroke),
    across(where(is.factor), as.numeric),
    stroke = factor(stroke)
  )

stroke_oversampled <- oversample(as.data.frame(stroke_test), classAttr = "stroke", ratio = 1, method = "MWMOTE")

head(stroke_oversampled)

stroke_oversampled %>%
  group_by(stroke) %>%
  summarize(n = n()) %>%
  mutate(prop = round(n / sum(n), 2))

stroke_data_final <- stroke_oversampled %>% select(-id)

# total number of observations
n_obs <- nrow(stroke_data_final)

# shuffle the dataset randomly
permuted_rows <- sample(n_obs)

# Randomly order data
stroke_shuffled <- stroke_data_final[permuted_rows,]

# Identify row to split on
split <- round(n_obs * 0.7)

# Create train
train <- stroke_shuffled[1:split,]

# Create test
test <- stroke_shuffled[(split + 1):nrow(stroke_shuffled),]

# check if train is really 70% of the original 
nrow(train) / nrow(stroke_data_final)

###model building 

# custom train control
myControl <- trainControl(
  method = "cv", 
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = TRUE
)

myGrid <- expand.grid(
  alpha = c(0,1),
  lambda = seq(0.00001, 1, length = 20)
)

set.seed(42)
glmnet_model <- train(
  stroke ~ .,
  train,
  method = "glmnet",
  tuneGrid = myGrid,
  trControl = myControl
  
)

plot(glmnet_model)

max(glmnet_model[["results"]]$ROC)

mm_test <- test %>% select(-stroke)

glmnet_pred <- predict(glmnet_model, newdata = mm_test) 

confusionMatrix(glmnet_pred, factor(test[["stroke"]]), positive = "yes")


######random forest

rfGrid <- data.frame(
  .mtry = c(2,3,5,6),
  .splitrule = "gini",
  .min.node.size = 5
)

rfControl <- trainControl(
  method = "oob",
  number = 5,
  verboseIter = TRUE
)

rf_model <- train(
  stroke ~ .,
  train,
  method = "ranger",
  tuneLength = 3,
  tuneGrid = rfGrid,
  trControl = rfControl
)

rf_model
#Out-of-bag error is low for the random forest model, it's accuracy is higher than baseline for all of the mtry parameters.
##Let's evaluate it on the test dataset.

rf_pred <- predict(rf_model, newdata = mm_test) 

confusionMatrix(rf_pred, factor(test[["stroke"]]), positive = "yes")
