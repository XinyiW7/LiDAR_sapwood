setwd("~/Desktop")

library(tidyverse)
library(ggplot2)

## Data collation
df <- read.csv("Oak_data.csv")
df <- df %>% 
  mutate(sapwood_depth = sapwood_depth/1000,
         bark = bark/1000,
         dbh_measured = perimeter/100/pi,
         area_measured = pi*(dbh_measured/2)^2,
         bark_area = area_measured - pi*(dbh_measured/2-bark)^2,
         hardwood_area = area_measured - pi*(dbh_measured/2-(bark+sapwood_depth))^2,
         sapwood_area = area_measured - bark_area - hardwood_area
         )

df <- df %>% 
  mutate(d_05 = radius_05*2,
         d_1 = radius_1*2,
         d_15 = radius_15*2,
         d_2 = radius_2*2,
         d_25 = radius_25*2,
         d_3 = radius_3*2,
         dbh_scan = radius_13*2
  )

# Save as CSV
write.csv(df, "Oak_data.csv", row.names = FALSE)

df<- read.csv("~/Desktop/Oak_data.csv")

#Sapwood area explained by multiple diameters
# Check outliers
boxplot(df[,18:23], col = c("grey", "grey", "grey", "grey"),
        xlab = "Groups", ylab = "Diameter (m)")

# Prove that the scan data is valid
scan_measured_mod <- lm(dbh_measured ~ dbh_scan, data = df)
r_squared <- summary(scan_measured_mod)$r.squared
coefficients_lm<- coef(scan_measured_mod)
print(coefficients_lm)
plot <- df %>%
  ggplot(aes(dbh_scan,dbh_measured))+
  geom_point()

plot_with_line <- plot +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  
  annotate("text", x = 0.75 * max(df$dbh_scan), y = 0.95 * max(df$dbh_measured),
           label = paste("R^2 =", round(r_squared, 3)))
print(plot_with_line)

summary(scan_measured_mod)
RMSE_MS <- sqrt(mean((df$dbh_measured - df$dbh_scan)^2))

## Sapwood area explained by tape measured DBH
mod1 <- lm(sapwood_area~dbh_measured,data=df)
summary(mod1)
coefficients <- coef(mod1)
print(coefficients)

df %>% 
  ggplot(aes(dbh_measured,sapwood_area))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_text(aes(x = 0.8 * max(dbh_measured), y = 0.9 * max(sapwood_area),label = paste("R^2 =", round(summary(mod1)$r.squared, 3))))

# Create QQ plot for the residuals of the linear model
residuals <- resid(mod1)
qqnorm(residuals, main = "QQ Plot of Residuals")
qqline(residuals)

## Spawood area explained by LiDAR scanning
set.seed(1234)
library(Matrix)
library(glmnet)

# independent variable，dbh_height
x <- df[, 18:23]

# dependent variable
y <- df$sapwood_area

# ridge model
ridge_model <- glmnet(x, y, alpha = 0)
print(ridge_model)

# Choose best lambdaa
x <- as.matrix(df[, 18:23])

# Cross-validation
cv_ridge <- cv.glmnet(x, y, alpha = 0)
print(cv_ridge)
plot(cv_ridge)

# best lambda
best_lambda <- cv_ridge$lambda.min
print(best_lambda)
lambda <- best_lambda

# Ridge regression mod2 
mod2 <- glmnet(x, y, alpha = 0, lambda = lambda)
mod2
summary(mod2)

# Adjusted R2 calculate
y_predicted <- predict(mod2, s = lambda, newx = x)

# Sum of Squares Total and Error
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

# R2
rsq <- 1 - sse / sst
rsq

# Adjusted R2
n <- nrow(x)
p <- ncol(x)
adj_r_squared <- 1 - (1 - rsq) * ((n - 1) / (n - p - 1))
print(adj_r_squared)

## Cross- validation compare two model
install.packages("caret")
library(caret)
install.packages("lattice")
ctrl <- trainControl(method = "cv", number = 10)  
results_lm <- train(sapwood_area ~ dbh_measured, data = df, method = "lm", trControl = ctrl)
results_ridge <- train(x = x, y = y, data = df, method = "glmnet", trControl = ctrl, tuneGrid = expand.grid(alpha = 0, lambda = lambda))

# result
print(results_lm)
print(results_ridge)

## Sapwood area predicted by tape measurement (Repeat to predict different sample size)
df <- read.csv("~/Desktop/Test_data.csv")
modlm10 <- lm(sapwood_area_10~dbh_measured_10,data=df)
summary(modlm10)

# Test model
save(modlm10, file = "modlm10.RData")
load("modlm10.RData")

df <- read.csv("~/Desktop/Predict_data.csv")
new_data <- data.frame(dbh_measured_10 = df$dbh_measured_10)

# Predict
lmpredict_sapwood_area_10 <- data.frame(predict(modlm10, newdata = new_data))
print(lmpredict_sapwood_area_10)

combined_data <- data.frame(
  lmpredict_sapwood_area_10 = lmpredict_sapwood_area_10,
  lmpredict_sapwood_area_20 = lmpredict_sapwood_area_20,
  lmpredict_sapwood_area_30 = lmpredict_sapwood_area_30
)
write.csv(combined_data, "Result_data.csv", row.names = FALSE)

# Compare predict and measure
df <- read.csv("~/Desktop/Result_data.csv")
lm_mod10<- lm(measured_sapwood_area_10~lmpredict_10,data=df)
summary(lm_mod10)

# Repeat above to get 3 samples results

## Sapwood area measurement by LiDAR scanning
df <- read.csv("~/Desktop/rrtest10.csv")

x <- df[, 18:23]
y <- df$sapwood_area

# ridge_model
ridge_model <- glmnet(x, y, alpha = 0)
print(ridge_model)

x <- as.matrix(df[, 18:23])

cv_ridge <- cv.glmnet(x, y, alpha = 0)
print(cv_ridge)

# best lambda
best_lambda <- cv_ridge$lambda.min
print(best_lambda)

lambda <- best_lambda

ridge_model10 <- glmnet(x, y, alpha = 0, lambda = lambda)
ridge_model10

# Adjusted R2 calculate
y_10 <- predict(ridge_model10, s = lambda, newx = x)

# Sum of Squares Total and Error
sst <- sum((y - mean(y))^2)
sse <- sum((y_10 - y)^2)

# R2 calculate
rsq10 <- 1 - sse / sst
rsq10

# Adjusted R2
n <- nrow(x)
p <- ncol(x)
adj_rsq10 <- 1 - (1 - rsq10) * ((n - 1) / (n - p - 1))
print(adj_rsq10)

# Save the pre-trained Ridge Regression model to ridge_model.RData file
save(ridge_model10, file = "ridge_model10.RData")

# Load the pre-trained Ridge Regression model from ridge_model.RData file
load("ridge_model10.RData")

new_data <- read.csv("Predict_data.csv")
new_x <- as.matrix(new_data[, 2:7])

# Predict new y values using the pre-trained Ridge Regression model
rrpredicted_sapwood_area_10 <- predict(ridge_model10, s = best_lambda, newx = new_x)

# Save the predicted values 
write.csv(rrpredicted_sapwood_area_10, "rrResult.csv", row.names = FALSE)


df <- read.csv("~/Desktop/rrResult.csv")

# Compare predict and measure
rrmod10 <- lm(measured_sapwood_area_10~rrpredict_sapwood_area_10,data=df)
summary(rrmod10)

df %>% 
  ggplot(aes(rrpredict_sapwood_area_10,measured_sapwood_area_10 ))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_text(aes(x = 0.7 * max(rrpredict_sapwood_area_10), y = 0.9 * max(measured_sapwood_area_10), label = paste("R^2 =", round(summary(rrmod10)$r.squared, 3))))

# Calculate Mean Absolute Error (MAE)
MAE10 <- mean(abs(df4$predict_sapwood_area_10 - df$measured_sapwood_area_10))

# Calculate Root Mean Squared Error (RMSE)
RMSE10 <- sqrt(mean((df4$predict_sapwood_area_10 - df$measured_sapwood_area_10)^2))

# Repeat to get 3 samples results
