library(ggplot2)
library(dplyr)
library(Hmisc)
library(cowplot)
library(WVPlots)
library(readxl)

df <- read_excel("C:/Users/USER/Desktop/BINUS Data Mining & Visualization/STAT6157016 - Data Mining and Visualization - Copy/Insurance.xlsx")

sum(is.na(df$age))
sum(is.na(df$sex))
sum(is.na(df$bmi))
sum(is.na(df$children))
sum(is.na(df$smoker))
sum(is.na(df$region))
sum(is.na(df$expenses))

x <- ggplot(df, aes(age, expenses)) +
  geom_jitter(color = "blue", alpha = 0.5) +
  theme_light()

y <- ggplot(df, aes(bmi, expenses)) +
  geom_jitter(color = "green", alpha = 0.5) +
  theme_light()

p <- plot_grid(x, y) 
title <- ggdraw() + draw_label("1. Correlation between Charges and Age / BMI", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

x <- ggplot(df, aes(sex, expenses)) +
  geom_jitter(aes(color = sex), alpha = 0.7) +
  theme_light()

y <- ggplot(df, aes(children, expenses)) +
  geom_jitter(aes(color = children), alpha = 0.7) +
  theme_light()

p <- plot_grid(x, y) 
title <- ggdraw() + draw_label("2. Correlation between Charges and Sex / Children covered by insurance", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

x <- ggplot(df, aes(smoker, expenses)) +
  geom_jitter(aes(color = smoker), alpha = 0.7) +
  theme_light()

y <- ggplot(df, aes(region, expenses)) +
  geom_jitter(aes(color = region), alpha = 0.7) +
  theme_light()

p <- plot_grid(x, y) 
title <- ggdraw() + draw_label("3. Correlation between Charges and Smoker / Region", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

n_train <- round(0.8 * nrow(df))
train_indices <- sample(1:nrow(df), n_train)
Data_train <- df[train_indices, ]
Data_test <- df[-train_indices, ]

formula_0 <- as.formula(expenses ~ .)

model_0 <- lm(formula_0, data = Data_train)
summary(model_0)

#Saving R-squared
r_sq_0 <- summary(model_0)$r.squared

#predict data on test set
prediction_0 <- predict(model_0, newdata = Data_test)
#calculating the residuals
residuals_0 <- Data_test$expenses - prediction_0
#calculating Root Mean Squared Error
rmse_0 <- sqrt(mean(residuals_0^2))

formula_1 <- as.formula(expenses ~ age + bmi + children + smoker + region)

model_1 <- lm(formula_1, data = Data_train)
summary(model_1)

r_sq_1 <- summary(model_1)$r.squared

prediction_1 <- predict(model_1, newdata = Data_test)

residuals_1 <- Data_test$expenses - prediction_1
rmse_1 <- sqrt(mean(residuals_1^2))

print(paste0("R-squared for first model:", round(r_sq_0, 4)))
print(paste0("R-squared for new model: ", round(r_sq_1, 4)))
print(paste0("RMSE for first model: ", round(rmse_0, 2)))
print(paste0("RMSE for new model: ", round(rmse_1, 2)))

Data_test$prediction <- predict(model_1, newdata = Data_test)
ggplot(Data_test, aes(x = prediction, y = expenses)) + geom_point(color = "blue", alpha = 0.7) + geom_abline(color = "red") + ggtitle("Prediction vs. Real values")

Data_test$residuals <- Data_test$expenses - Data_test$prediction

ggplot(data = Data_test, aes(x = prediction, y = residuals)) + geom_pointrange(aes(ymin = 0, ymax = residuals), color = "blue", alpha = 0.7) + geom_hline(yintercept = 0, linetype = 3, color = "red") + ggtitle("Residuals vs. Linear model prediction")

GainCurvePlot(Data_test, "prediction", "expenses", "Model")
