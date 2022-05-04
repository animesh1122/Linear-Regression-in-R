library(ggplot2)
library(dplyr)
library(Hmisc) 
library(cowplot) 
library(psych)
library(WVPlots) 
  
#load dataset 

Data <- read.csv("D:/R/insurance.csv")
sample_n(Data, 5)

#Description of data.
describe(Data)

# Descriptive statistics by group(MALE/FEMALE)
describe(Data~sex)
# Descriptive statistics by group(REGION)
describe(Data~region)
# Descriptive statistics by group(SMOKER)
describe(Data~smoker)


glimpse(Data)
summary(Data)

##########################Plot#####################
#1.Correlation between Charges and Age 
#2.Correlation between Charges and BMI
 
x <- ggplot(Data, aes(age, charges)) +
  geom_jitter(color = "blue", alpha = 0.5) +
  theme_light()
 
y <- ggplot(Data, aes(bmi, charges)) +
  geom_jitter(color = "green", alpha = 0.5) +
  theme_light()

#Grid a plot together(1+2)

p <- plot_grid(x, y) 
title <- ggdraw() + draw_label("Correlation between Charges and Age / BMI", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

#3.Correlation between Charges and Smoker
#4.Correlation between Charges and Region

x <- ggplot(Data, aes(smoker, charges)) +
  geom_jitter(aes(color = smoker), alpha = 0.7) +
  theme_light()

y <- ggplot(Data, aes(region, charges)) +
  geom_jitter(aes(color = region), alpha = 0.7) +
  theme_light()

#Grid a plot together(3+4)

p <- plot_grid(x, y) 
title <- ggdraw() + draw_label("Correlation between Charges and Smoker / Region", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

####################Linear Regression Model############################

train <- round(0.8 * nrow(Data))
train_indices <- sample(1:nrow(Data), train)
Data_train <- Data[train_indices, ]
Data_test <- Data[-train_indices, ]

formula_0 <- as.formula("charges ~ age + sex + bmi + children + smoker + region")

#Train and Test Model
model_0 <- lm(formula_0, data = Data_train)
summary(model_0) 

#Save the model
r_sq_0 <- summary(model_0)$r.squared

#predict test dataset
prediction_0 <- predict(model_0, newdata = Data_test)
#calculating the residuals
residuals_0 <- Data_test$charges - prediction_0
#calculating Root Mean Squared Error
rmse_0 <- sqrt(mean(residuals_0^2))


# variable SEX is not significant, while smoking seems to have a huge influence on charges.
#training new model without SEX variable.

formula_1 <- as.formula("charges ~ age + bmi + children + smoker + region")

model_1 <- lm(formula_1, data = Data_train)
summary(model_1)

#save new model
r_sq_1 <- summary(model_1)$r.squared

prediction_1 <- predict(model_1, newdata = Data_test)
residuals_1 <- Data_test$charges - prediction_1
rmse_1 <- sqrt(mean(residuals_1^2))

#compare the two models

print(paste0("R-squared for first model:", round(r_sq_0, 4)))
print(paste0("R-squared for new model: ", round(r_sq_1, 4)))


print(paste0("RMSE for first model: ", round(rmse_0, 2)))
print(paste0("RMSE for new model: ", round(rmse_1, 2)))

#here both models performance are similar, second model has a good result compare to first one.

#plot a graph Prediction vs real value
Data_test$prediction <- predict(model_1, newdata = Data_test)
ggplot(Data_test, aes(x = prediction, y = charges)) + 
  geom_point(color = "blue", alpha = 0.7) + 
  geom_abline(color = "red") +
  ggtitle("Prediction vs. Real values")

#TESTING WITH NEW DATA 
#1.
Animesh <- data.frame(age = 30,
                  bmi = 27.9,
                  children = 0,
                  smoker = "yes",
                  region = "northwest")
print(paste0("Health care charges for Animesh: ", round(predict(model_1, Animesh), 2)))
#2.
Animesh_Patel <- data.frame(age = 30, 
                      bmi = 27.9,
                      children = 0,
                      smoker = "no",
                      region = "northwest")
print(paste0("Health care charges for Animesh_Patel: ", round(predict(model_1, Animesh_Patel), 2)))

#3.
Amisha <- data.frame(age = 40,
                   bmi = 50,
                   children = 2,
                   smoker = "no",
                   region = "southeast")
print(paste0("Health care charges for Amisha: ", round(predict(model_1, Amisha), 2)))

#4.

Jayesh <- data.frame(age = 30,
                   bmi = 31.2,
                   children = 0,
                   smoker = "no",
                   region = "northeast")
print(paste0("Health care charges for Jayesh: ", round(predict(model_1, Jayesh), 2)))

