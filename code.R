# to import and read the excel file to the R console readxl library has been used
library("readxl")
# the excel file has been read to a data frame named main_data.df, which will be used 
main_data.df <- read_excel("3122831BodyFatData.xlsx",range="A1:B253")
attach(main_data.df)
View(main_data.df)
dim(main_data.df)
# the data has 252 items in 2 columns
# is.na(main_data.df)
sum(is.na(main_data.df))
# CHECKED MISSING VALUE IN THE DATASET, and the dataset seems clean with no missing values as the output is zero.
str(main_data.df)
typeof(Abdomen)
typeof(BodyFat)
quantile(main_data.df$BodyFat, type=6)
quantile(main_data.df$Abdomen, type=6)
# SUMMARY STATISTICS OF THE DATA
summary(main_data.df)
#STANDARD DEVIATION
sd(main_data.df$BodyFat)
sd(main_data.df$Abdomen)
# RANGE
max(main_data.df$BodyFat) - min(main_data.df$BodyFat)
max(main_data.df$Abdomen) - min(main_data.df$Abdomen)
# VARIANCE
var(main_data.df$BodyFat)
var(main_data.df$Abdomen)
# SKEWNESS CHECK
library(e1071)
skewness(main_data.df$Abdomen)
skewness(main_data.df$BodyFat)

# BOX PLOT OF THE DATA
library(ggplot2)
ggplot(main_data.df, aes(x=Abdomen)) + geom_boxplot() + ggtitle("Abdomen Measure Summary")
ggplot(main_data.df, aes(x=BodyFat)) + geom_boxplot() + ggtitle("BodyFat Summary")
# HISTOGRAM OF THE DATA
ggplot(main_data.df, aes(x=BodyFat)) + geom_histogram(color="white", fill="black") + ggtitle("BodyFat Summary")
ggplot(main_data.df, aes(x=Abdomen)) + geom_histogram(color="white", fill="black") + ggtitle("Abdomen Measure Summary")
# the boxplot and histogram have been attached to the report.
# referred both hist and box plot from: http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
# PLOTTING GRAPH OF VARIABLES TO CHECK THE RELATIONSHIP
plot(BodyFat~Abdomen, data=main_data.df, main="Body Fat Vs Abdomen Measurement", xlab="Abdomen Measurement", ylab="Body Fat")

# LINEAR MODEL OF THE WHOLE DATASET – This was created just as a test and to compare the results with the trained data model. This model has not been used to interpret the results.
# FINDING CORRELATION
cor(main_data.df$Abdomen, main_data.df$BodyFat)
cor(main_data.df$Abdomen, main_data.df$BodyFat)^2
# DEVELOPING A LINEAR MODEL
lin_model<-lm(BodyFat~Abdomen, data=main_data.df)
summary(lin_model)

# HYPOTHESIS TEST TO PROVE LINEAR RELATIONSHIP OF VARIABLES
# n has been defined as the total number of observations in one variable
n = length(bodyfat_data.df$BodyFat)
df = n-2
# as we are doing two-sided t-test, alpha has been considered as 0.025
alpha = 0.025
sum_x <- sum(bodyfat_data.df$Abdomen)
sum_x
sum_y <- sum(bodyfat_data.df$BodyFat)
sum_y
sum_xy <- sum((bodyfat_data.df$Abdomen)*(bodyfat_data.df$BodyFat))
sum_xy
sum_x_sqr <- sum((bodyfat_data.df$Abdomen)^2)
sum_x_sqr
sum_y_sqr <- sum((bodyfat_data.df$BodyFat)^2)
sum_y_sqr
# SXX, SYY, and SXY is calculated below to find the beta hat and sigma squared
s_xy<- sum_xy-((sum_x*sum_y)/n)
s_xy
s_xx <- sum_x_sqr-((sum_x^2)/n)
s_xx
s_yy<-sum_y_sqr-((sum_y^2)/n)
s_yy
beta_hat<-s_xy/s_xx
beta_hat
sigma_sqr<-(s_yy-((s_xy^2)/s_xx))/(n-2)
sigma_sqr
#the observed t-value has been calculated below
t_obs <- beta_hat/(sqrt(sigma_sqr/s_xx))
t_obs
# for the two-sided t-test, we are calculating p-values on both sides of the distribution and adding it up to get the P_VALUE
p_right <- pt(abs(t_obs), df = n-2, lower.tail = FALSE)
p_left <- pt(-abs(t_obs), df = n-2, lower.tail = TRUE)
p_value <- p_right+p_left
p_value
# the command below will provide output if p_value ≤ alpha = TRUE and prove there is insufficient evidence to support the null hypothesis
p_value <= alpha

# REMOVING THE OUTLIERS FROM THE ABDOMEN AND STORING THE DATA IN A NEW DATAFRAME
# Interquartile range has been calculated and tried to remove the outliers from the upper and lower fences.
dim(main_data.df)
quartiles <- quantile(main_data.df$Abdomen, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(main_data.df$Abdomen)
lower_quart <- quartiles[1] - 1.5*iqr
upper_quart <- quartiles[2] + 1.5*iqr
newdata_no_outlier <- subset(main_data.df, main_data.df$Abdomen > lower_quart & main_data.df$Abdomen < upper_quart)
boxplot(newdata_no_outlier)$stats
# FINDING CORRELATION OF THE DATA WITH NO OUTLIERS
cor(newdata_no_outlier$Abdomen, newdata_no_outlier$BodyFat)

# SPLITTING DATA INTO TRAIN(0.7) AND TEST(0.3)
library(dplyr)
set.seed(123)
newdata_no_outlier$id <- 1:nrow(newdata_no_outlier)
traindata_nooutlier <- newdata_no_outlier %>% dplyr::sample_frac(0.70)
testdata_nooutlier  <- dplyr::anti_join(newdata_no_outlier, traindata_nooutlier, by = 'id')
model_nooutlier<-lm(BodyFat~Abdomen, data=traindata_nooutlier)
summary(model_nooutlier)

# FINDING CORRELATION OF THE TRAIN DATA
cor(traindata_nooutlier$Abdomen, traindata_nooutlier$BodyFat)
# CORRELATION GRAPH
result = cor(traindata_nooutlier, method = "pearson", use = "complete.obs")
round(result,3)
library("PerformanceAnalytics")
chart.Correlation(traindata_nooutlier, histogram=TRUE, pch=19)
# to plot the correlation graph, referred: http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software

# Q-Q PLOT AND Q-Q LINE
library(nortest)
qqnorm(model_nooutlier$residuals)
qqline(model_nooutlier$residuals)
# ANDERSON DARLING TEST TO FIND NORMALITY OF RESIDUALS
ad.test(model_nooutlier$residuals)

# PLOTTING THE MODEL'S FITTED VALUE AGAINST THE RESIDUALS
plot (model_nooutlier$fitted.values, model_nooutlier$residuals, main="Fitted Value vs Residuals",  xlab="Fitted Value of Model",  ylab="Residuals of Model")
abline(h=0)

# CONFIDENCE INTERVAL OF THE MODEL
confint(model_nooutlier)

# PLOTTING THE FITTED LINE
plot (traindata_nooutlier$BodyFat, model_nooutlier$fitted.values, main="Observed BodyFat vs Fitted Value of the Model",  xlab="Observed Body Fat",  ylab="Fitted Value of Linear Model")
abline(a=0,b=1)

# PLOTTING HORIZONTAL LINE, WHICH IS A MEAN OF Y-VALUE
rline <- lm(BodyFat~Abdomen, data=traindata_nooutlier)
with(traindata_nooutlier, segments(Abdomen, fitted(rline), Abdomen, BodyFat, col="red"))
with(traindata_nooutlier, plot(Abdomen,BodyFat, main="BodyFat vs Abdomen Measure", xlab="Abdomen", ylab="BodyFat"))
abline(h = 18.8647, lwd=2, col="blue")
with(traindata_nooutlier, segments(Abdomen,18.8647, Abdomen,BodyFat, col="red"))

# PLOTTING THE REGRESSION LINE
with(traindata_nooutlier, plot(Abdomen,BodyFat, main="Regression Line", xlab="Abdomen Measure", ylab="Body Fat"))
abline(rline,lwd=2,col="blue")
with(traindata_nooutlier, segments(Abdomen,fitted(rline),Abdomen,BodyFat,col="red"))

# PREDICTING BODYFAT USING THE LINEAR MODEL
# to predict the body fat while the abdomen measure is 100CM using the model 
# referred: https://www.digitalocean.com/community/tutorials/predict-function-in-r
predict_100 <- as.data.frame(100)
colnames(predict_100) <- "Abdomen"
predicted_value_nooutlier <- predict(model_nooutlier, newdata = predict_100)
predicted_value_nooutlier

# PREDICTING BODY FAT FOR 90CM ABDOMEN MEASURE
predict_90 <- as.data.frame(90)
colnames(predict_90) <- "Abdomen"
predicted_bodyfat_90 <- predict(lin_model, newdata = predict_90)
print(predicted_bodyfat_90)

library(caret)
library(tidyverse)

# PREDICTIONS ON TEST DATA
predictions <- predict(model_nooutlier, newdata = testdata_nooutlier)
# predictions - commented out as the ouput is too large
predictions_fulldata <- predict(lin_model, newdata = main_data.df)

# MEAN SQUARED ERROR
mse <- mean((testdata_nooutlier$BodyFat - predictions) ^ 2)
mse
mse_fulldata <- mean((main_data.df$BodyFat - predictions_fulldata) ^ 2)
mse_fulldata
# ROOT MEAN SQUARED ERROR
rmse <- sqrt(mean((testdata_nooutlier$BodyFat - predictions) ^ 2))
rmse
rmse_fulldata <- sqrt(mean((main_data.df$BodyFat - predictions_fulldata) ^ 2))
rmse_fulldata
# R-SQUARED
r_squared <- summary(model_nooutlier)$r.squared
r_squared
r_squared_fulldata <- summary(lin_model)$r.squared
r_squared_fulldata

# TRAIN ERROR AND TEST ERROR
train_predictions <- predict(model_nooutlier, newdata = traindata_nooutlier)
test_predictions <- predict(model_nooutlier, newdata = testdata_nooutlier)
test_error <- mean((testdata_nooutlier$BodyFat - test_predictions) ^ 2)
train_error <- mean((traindata_nooutlier$BodyFat - train_predictions) ^ 2)
fulldata_error <- mean((main_data.df$BodyFat - predictions_fulldata) ^ 2)

fulldata_error
train_error
test_error

#PLOTTING TRAIN AND TEST ERROR
plot(c(train_error, test_error), type = "l", xlab = "Prediction Data", ylab = "Error")

# PLOTTING PREDICTIONS OF TRAIN DATA MODEL ON TEST DATA
plot(testdata_nooutlier$BodyFat, predictions, xlab = "Observed BodyFat", ylab = "Predictions of fitted model on test data")
abline(lm(predictions ~ testdata_nooutlier$BodyFat))
       

# PLOTTING PREDICTIONS ON WHOLE DATASET
plot(main_data.df$BodyFat, predictions_fulldata)
# abline(lm(predictions ~ main_data.df$BodyFat))

# PRECISION OF THE MODEL
sse <- sum((testdata_nooutlier$BodyFat - predictions) ^ 2)
tss <- sum((testdata_nooutlier$BodyFat - mean(testdata_nooutlier$BodyFat)) ^ 2)
precision <- 1 - (sse / tss)
precision

