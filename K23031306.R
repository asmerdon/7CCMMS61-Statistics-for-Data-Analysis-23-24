# a lot of the code in this r script is not used the final  report, as I changed around what kind of data i wanted in my linear model(s)
setwd("C:/Users/alexs/Documents/Masters/Statistics for Data Science/coursework/")
californiaHousing <- read.csv("California_housing.csv")
library(faraway)
help(californiaHousing)
head(californiaHousing)

#summary
numeric_vars <- californiaHousing[, sapply(californiaHousing, is.numeric)]

summary_stats <- describe(numeric_vars)
table_data_df <- data.frame(
  Mean = round(summary_stats$mean, 3),
  Median = round(summary_stats$median, 3),
  SD = round(summary_stats$sd, 3),
  Skewness = round(summary_stats$skew, 3),
  Kurtosis = round(summary_stats$kurt, 3)
)
rownames(table_data_df) <- names(numeric_vars)
print(table_data_df)

y_labels <- prettyNum(pretty(californiaHousing$median_house_value), big.mark = ",")
axis(2, at = pretty(californiaHousing$median_house_value), labels = y_labels)

pairs(median_house_value ~ median_income + house_median_age + total_rooms + total_bedrooms + population + households, 
      data = californiaHousing, col = 'blue', pch = 20, cex = 0.5)

summary_info <- summary(californiaHousing)
summary_info

#box plot code
variables_of_interest <- c("house_median_age", "total_rooms", "total_bedrooms", 
                           "households", "median_income", "median_house_value")

labels <- c("House Median Age", "Total Rooms", "Total Bedrooms", 
            "Households", "Median Income", "Median House Value")

par(mfrow = c(3, 2), cex = 0.75) 

for (i in seq_along(variables_of_interest)) {
  boxplot(californiaHousing[[variables_of_interest[i]]], 
          main = labels[i], col = "skyblue")
}

par(mfrow = c(1, 1))

options(scipen = 999, digits = 4) # so scienctific numbrs not used

#Linear regression:
housinglm <- lm(median_house_value ~ median_income + house_median_age + total_rooms + population + households, data = californiaHousing)
summary(housinglm)
#use adjusted r-value (multiple regression).
#0,5629 means about 50% of the median_house_value can be explained by the model

#equation:
#-17506.340 41271.707median_income + 1981.358median_house_age + 9.314total_rooms -11.703population

residuals_housing <- housinglm$residuals
head(residuals_housing)
fits_housing <- housinglm$fitted.values
head(fits_housing)
head(californiaHousing$median_house_value)

#residuals plot vs estimated house value (not good!)
plot(fits_housing,residuals_housing,xlab='Estimated house value', ylab='residuals', pch=20,col='blue')
abline(h=0)

#population, households and total rooms are both count data, so should not have used. 

#without any count variables
housinglm3 <- lm(median_house_value ~ median_income + house_median_age, data = californiaHousing)
summary(housinglm3)

residuals_housing3 <- housinglm3$residuals
head(residuals_housing3)
fits_housing3 <- housinglm3$fitted.values
head(fits_housing3)
head(californiaHousing$median_house_value)

#residuals plot vs estimated house value (lm3)
plot(fits_housing3,residuals_housing3,xlab='Estimated house value', ylab='residuals', pch=20,col='blue')
abline(h=0)

housinglm5 <- lm(median_house_value ~ median_income + house_median_age + total_rooms + population + households + longitude + latitude, data = californiaHousing)
summary(housinglm5)

#Equation:
#-17506.340 41271.707median_income + 1981.358median_house_age + 9.314total_rooms -11.703population

residuals_housing5 <- housinglm5$residuals
head(residuals_housing5)
fits_housing5 <- housinglm5$fitted.values
head(fits_housing5)
head(californiaHousing$median_house_value)

#residuals plot vs estimated house value (not good!)
plot(fits_housing5,residuals_housing5,xlab='Estimated house value', ylab='residuals', pch=20,col='blue')
abline(h=0)

# residuals with individual predictors
par(mfrow = c(2, 4)) 
plot(californiaHousing$median_income, residuals_housing, xlab = 'Median income', ylab = 'Residuals', pch = 20, col = 'chartreuse4')
abline(h = 0)
plot(californiaHousing$house_median_age, residuals_housing, xlab = 'House median age', ylab = 'Residuals', pch = 20, col = 'chartreuse4')
abline(h = 0)
plot(californiaHousing$total_rooms, residuals_housing, xlab = 'Total rooms', ylab = 'Residuals', pch = 20, col = 'chartreuse4')
abline(h = 0)
plot(californiaHousing$population, residuals_housing, xlab = 'Population', ylab = 'Residuals', pch = 20, col = 'chartreuse4')
abline(h = 0)
plot(californiaHousing$households, residuals_housing, xlab = 'Households', ylab = 'Residuals', pch = 20, col = 'chartreuse4')
abline(h = 0)
plot(californiaHousing$longitude, residuals_housing, xlab = 'Longitude', ylab = 'Residuals', pch = 20, col = 'chartreuse4')
abline(h = 0)
plot(californiaHousing$latitude, residuals_housing, xlab = 'Latitude', ylab = 'Residuals', pch = 20, col = 'chartreuse4')
abline(h = 0)

##All this is second linear model:
#removing the 500,000 values:
filtered_data <- subset(californiaHousing, median_house_value < 500000)
housinglm4 <- lm(median_house_value ~ median_income + house_median_age, data = filtered_data)
summary(housinglm4)

residuals_housing4 <- housinglm4$residuals
head(residuals_housing4)
fits_housing4 <- housinglm4$fitted.values
head(fits_housing4)
head(californiaHousing$median_house_value)

#residuals plot vs estimated house value (not good!)
par(mfrow = c(1,1))

plot(fits_housing4,residuals_housing4,xlab='Estimated house value', ylab='residuals', pch=20,col='blue')
abline(h=0)

par(mfrow = c(1, 2))
plot(filtered_data$median_income, residuals_housing4, xlab='Median income', ylab='residuals', pch=20, col='chartreuse4')
abline(h=0)
plot(filtered_data$house_median_age, residuals_housing4, xlab='House median age', ylab='residuals', pch=20, col='chartreuse4')
abline(h=0)

#qq plots
qqnorm(filtered_data$median_income)
qqline(filtered_data$median_income)

qqnorm(filtered_data$house_median_age)
qqline(filtered_data$house_median_age)


library(MASS)
stud_res_housing<-studres(housinglm4)
plot(fitted(housinglm4),sqrt(abs(stud_res_housing)),xlab='Estimated house prices', ylab='sqrt of absolute value studentized residuals', pch=20, col='red' )
std_res_housing<-rstandard(housinglm4)
plot(fitted(housinglm4),sqrt(abs(std_res_housing)),xlab='Estimated house prices', ylab='sqrt of absolute value standardized residuals', pch=20, col='cyan' )

par(mfrow=c(1,1))
boxcox(housinglm4, plotit=T) # unused

#transform response variable (3rd linear model)
trans_median_house_value<-sqrt(filtered_data$median_house_value)
housinglm6 <- lm(trans_median_house_value ~ median_income + house_median_age, data = filtered_data)
summary(housinglm6)
plot(fitted(housinglm6),residuals(housinglm6), xlab="Fitted ", ylab="residuals", pch=20, col='blue')