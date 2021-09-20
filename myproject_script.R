#Loading the data set and libraries

library(tidyverse)
library(dslabs)
library(ggplot2)
library(caret)
library(readr)
library(dplyr)
library(corrplot)
library(reshape2)
library(ggplot2)
library(rsample)
library(caret)
library(data.table)
library(FactoMineR)
library(viridis)
library(rattle)
library(mice)
library(VIM)
library(VGAM)
library(pROC)

#data exploration

healthcare_dataset_stroke_data <- read_csv("healthcare-dataset-stroke-data.csv")

data <- as.data.frame(healthcare_dataset_stroke_data)

str(data)

data <- unique(data)
data <- na.omit(data)

#Remove unnecessary columns from the dataset 

data <- data %>% select( "gender", "age", "hypertension", "heart_disease", "Residence_type","avg_glucose_level", "bmi", "smoking_status","stroke")


data$gender <- as.factor(data$gender)
data$smoking_status <- as.factor(data$smoking_status)
data$Residence_type <- as.factor(data$Residence_type)


#Data exploration 

data %>% ggplot(aes(age,fill = gender)) +
  geom_histogram(alpha =0.5, aes(y = ..density..),col="black", bins = 10) +
  theme(legend.title=element_text(family="Times",size=20),
        legend.text=element_text(family="Times",face ="italic",size=15),
        plot.title=element_text(family="Times", face="bold", size=20),
        axis.title.x=element_text(family="Times", face="bold", size=12),
        axis.title.y=element_text(family="Times", face="bold", size=12)) +
  xlab("age") +
  ylab("cases") +
  ggtitle("Strokes distribution per age and gender")


#as we can see there is some  positive correlation between age, hypertention and heart disease
#From this plot the correletion dosen't seems to bee as high as expected 

corrplot(cor(data[,2:4],method ="pearson"),diag =FALSE,
         title ="Correlation Plot", method ="ellipse",
         tl.cex =0.7, tl.col ="black", cl.ratio =0.2
)

data$bmi <- as.character(data$bmi)
data$bmi <- as.numeric(data$bmi)
is.na(data$bmi) <- NULL

#as expected even between bmi and glucose levels there is a slightly positive correlation

corrplot(cor(data[,6:7],method ="pearson", use = "na.or.complete"),diag =FALSE,
         title ="Correlation Plot", method ="ellipse",
         tl.cex =0.7, tl.col ="black", cl.ratio =0.2
)

data %>% ggplot(aes(stroke, fill = smoking_status)) +
  geom_bar(alpha =2, col="black") +
  theme(legend.title=element_text(family="Times",size=20),
        legend.text=element_text(family="Times",face ="italic",size=15),
        plot.title=element_text(family="Times", face="bold", size=20),
        axis.title.x=element_text(family="Times", face="bold", size=12),
        axis.title.y=element_text(family="Times", face="bold", size=12)) +
  xlab("stroke") +
  ylab("cases") +
  ggtitle("Strokes and smoking status")

#distribution age and smoking status 

data %>% ggplot(aes(age, fill = smoking_status)) +
  geom_density(alpha =0.5, stat = "count", na.rm = TRUE, width = 5, position = "stack") +
  theme(legend.title=element_text(family="Times",size=20),
        legend.text=element_text(family="Times",face ="italic",size=15),
        plot.title=element_text(family="Times", face="bold", size=20),
        axis.title.x=element_text(family="Times", face="bold", size=12),
        axis.title.y=element_text(family="Times", face="bold", size=12)) +
  xlab("age") +
  ylab("cases") +
  ggtitle("Age and smoking status")



################################################################################
################################################################################

data <- as.data.table(data)

str(data)

#The histogram of glucose leveles and bmi is not normally distributed in a 
#traditional bell-shape and the Q-Q plot poorly resembles a straight y = x line.

par(mfrow = c(1,2))
hist(data$avg_glucose_level, 100)
qqnorm(data$avg_glucose_level)

par(mfrow = c(1,2))
hist(data$bmi, 100)
qqnorm(data$bmi)

#Shapiro-Wilk normality test, we see the p-value is significant, and thus we reject the null hypothesis of normal data

shapiro.test(data$bmi)
shapiro.test(data$avg_glucose_level[1:5000])

#Missing data in the data set

aggr(data, prop = TRUE,
    numbers = TRUE)


#Split data into train and test set

data1 <- initial_split(data = data, prop = 0.8)


data_train <- training(data1)
data_test <- testing(data1)


#########################################################################


#the following plots give us some insight: in general stroke happens starting from the age of 40,


p1 <- ggplot(data = data_train, aes(x = stroke, y= age))
p1 + geom_point(aes(colour = data_train$hypertension)) +
  scale_colour_viridis(discrete = FALSE)

p1 <- ggplot(data = data_train, aes(x = stroke, y= age))
p1 + geom_point(aes(colour = data_train$heart_disease)) +
  scale_colour_viridis(discrete = FALSE)



####################################################################################
#CART

range(data_train$avg_glucose_level)

data_train$bmi <- as.numeric(data_train$bmi)

data_train$glucose_cat_tr <- cut(data_train$avg_glucose_level, breaks = c(55,80,110,150,271), labels = c("low","normal","high","very high"))
data_train$bmi_cat_tr <- cut(data_train$bmi, breaks = c(0, 18, 24,29, 100), labels = c("underweight", "normal","overweight","obese") )

data_test$glucose_cat_ts <- cut(data_test$avg_glucose_level, breaks = c(55,80,110,150,271), labels = c("low","normal","high","very high"))
data_test$bmi_cat_ts <- cut(data_test$bmi, breaks = c(0, 18, 24,29, 100), labels = c("underweight", "normal","overweight","obese") )

set.seed(12345)
cartModel <- train(x = data_train[, c("stroke",  "hypertension", "heart_disease","bmi_cat_tr")],
                   y = factor(data_train$glucose_cat_tr),
                   method = "rpart",
                   preProcess = NULL,
                   tuneLength = 10,
                   trControl = trainControl(method = "cv",
                                            number = 6
                   )
)

cartModel

plot(cartModel$finalModel)
text(cartModel$finalModel, cex = 0.5)
fancyRpartPlot(cartModel$finalModel, cex = 0.4, main = "")
##############################################################################
#Logistic regression model 


m.lr <- glm(stroke ~ avg_glucose_level+age+heart_disease+hypertension,
             family = binomial(link = "logit"),
             data = data_train, model = TRUE)
summary(m.lr)

cut_off <-roc(response= data_train$stroke, predictor= m.lr$fitted.values)


e <-cbind(cut_off$thresholds,cut_off$sensitivities+cut_off$specificities)
best_t <-subset(e,e[,2]==max(e[,2]))[,1]
#Plot ROC Curve
plot(1-cut_off$specificities,cut_off$sensitivities,type="l",
     ylab="Sensitivity",xlab="1-Specificity",col="red",lwd=3,
     main ="ROC Curve for Train")
abline(a=0,b=1)
abline(v = best_t) #add optimal t to ROC curve

cat(" The best value of cut-off for classifier is ", best_t)

# Predict the probabilities for test and apply the cut-off
predict_prob <-predict(m.lr, newdata=data_test, type="response")
#Apply the cutoff to get the class
class_pred <-ifelse(predict_prob >0.045,1,0)
#Classification table
table(data_test$stroke,class_pred)

#Classification rate
sum(diag(table(data_test$stroke,class_pred))/nrow(data_test))

#with a logistic regression model we reached 67% good classification on the test data

anova(m.lr, test="Chisq")

#The chi-square test on four of the variables is significant as the p-value is less than 0.05. 
#4 out of five contributions to the model are significant.
