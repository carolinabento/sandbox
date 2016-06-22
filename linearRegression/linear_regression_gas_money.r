#To run in the command line
# RScript <filename>.r

#loading dataset
gas_money <- read.csv("gas_money.csv", header = TRUE)

#Plot: Money Spent on Gas vs Milage
png('gas_money.png')
plot(gas_money$total_miles,gas_money$total_paid,
 xlab="Miles",ylab="Dollars", pch=20, col="darkblue", main="Money Spent on Gas vs Milage"
,xlim=range(300,450), ylim=range(20,45))
dev.off()


### Build Linear Regression Model
# format of the relationship to model (outcome_variable ~ dependent_variable)
linear_model <- lm(total_paid ~ total_miles, data=gas_money)

#Coefficients: Slope and Intercept
coefficients <- coefficients(linear_model)
#Residuals
residuals <- residuals(linear_model)

#approx. miles from San Francisco,CA to Las Vegas, NV
miles_sf_vegas <- 570*2
sf_vegas_df <- data.frame(total_miles=miles_sf_vegas)

#Predicting the amount of money necessary for the SF->Vegas and back
gas_money_vegas <- predict(linear_model, sf_vegas_df, interval="predict")
print("Predictions")
print(gas_money_vegas)


#Summary of the Model's characteristics
print("Linear Model Summary")
summary(linear_model)

## Standard Error of The Model
se <- sqrt(diag(vcov(linear_model)))
print("Standard Error")
print(se)

#Plot: Money Spent on Gas vs Milage + Fitted Line
png('gas_money_fitted.png')
plot(gas_money$total_miles,gas_money$total_paid,
 xlab="Miles",ylab="Dollars", pch=20, col="darkblue",
 main="Money Spent on Gas vs Milage",
 xlim=range(300,450), ylim=range(20,45))
abline(linear_model, lwd=2, col="red")
dev.off()

png('gas_money_residuals_dataset.png')
plot(gas_money$total_paid, col="darkblue", axes=FALSE, xlab="", ylab="",pch=1)
par(new=T)
plot(fitted(linear_model), col="red",ylab="Dollars", xlab="", xaxt='n',pch=6)
par(new=T)
legend (x = "bottomleft", y = 2, legend = c("Dataset","Residuals"),
	col=c("darkblue","red"),pch=c(1,6))
par(new=F)
dev.off()

png('gas_money_residuals_plot.png')
plot(residuals, col="red",pch=20, xlab="", ylab="Residuals")
abline(0,0)
dev.off()


