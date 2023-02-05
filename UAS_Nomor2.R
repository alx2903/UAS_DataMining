library(readxl)
library(MASS)
library(dplyr)
library(tidyr)
library(tidyverse)
library(caTools)
library(ROCR) 
library(caret)
library(WVPlots)
library(lmtest)
library(randomForest)

df <- read_excel("C:/Users/USER/Desktop/BINUS Data Mining & Visualization/STAT6157016 - Data Mining and Visualization - Copy/Breast Cancer Wisconsin (Diagnostic).xlsx")

df$diagnosis <- as.factor(df$diagnosis)
summary(df)

sum(is.na(df$id))
sum(is.na(df$diagnosis))
sum(is.na(df$radius_mean))
sum(is.na(df$texture_mean))
sum(is.na(df$perimeter_mean))
sum(is.na(df$area_mean))
sum(is.na(df$smoothness_mean))
sum(is.na(df$compactness_mean))
sum(is.na(df$concavity_mean))
sum(is.na(df$`concave points_mean`))
sum(is.na(df$symmetry_mean))
sum(is.na(df$fractal_dimension_mean))

df$diagnosis <- ifelse(df$diagnosis == "M", 1, 0)
df$diagnosis <- factor(df$diagnosis, levels = c(0, 1))

table(df$diagnosis)

set.seed(357)
inTrain <- createDataPartition(y = df$diagnosis, p = .70, list = FALSE)
training <- df[inTrain,]
testing <- df[-inTrain,]

logitmod <- glm(diagnosis ~ ., family = "binomial", data=training)
summary(logitmod)

pred <- predict(logitmod, newdata = testing, type = "response")

y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testing$diagnosis

mean(y_pred == y_act) 

exp(coefficients(logitmod))
coeftest(logitmod)
varImp(logitmod)

testing$prediction <- predict(logitmod, newdata = testing)

accuracy <- sum(testing$diagnosis == y_pred_num)/length(testing$diagnosis)
precision <- sum(testing$diagnosis == 1 & y_pred_num == 1)/(sum(y_pred_num == 1))
recall <- sum(testing$diagnosis == 1 & y_pred_num == 1)/(sum(training == 1))

accuracy
precision
recall

# ROC-AUC Curve
ROCPred <- prediction(pred, testing$diagnosis) 
ROCPer <- performance(ROCPred, measure = "tpr", 
                      x.measure = "fpr")

auc <- performance(ROCPred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Plotting curve
plot(ROCPer, colorize = TRUE, 
     print.cutoffs.at = seq(0.1, by = 0.1), 
     main = "ROC CURVE")
abline(a = 0, b = 1)

auc <- round(auc, 4)
legend(.6, .4, auc, title = "AUC", cex = 1)