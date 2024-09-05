install.packages('ggplot2')
library(googlesheets4)
library(ggplot2)
library(tseries)
library(lmtest)

list.files()

quarterly_data1 = read.csv("/Users/tulgakagan/Desktop/PROJECT/Econometrics Project - CLEAN_DATA.csv")

time = as.yearqtr((quarterly_data1$TIME))
short_term_i = quarterly_data1$dff
inf = quarterly_data1$inf
target = quarterly_data1$target
gdp = quarterly_data1$adj_gdpc1
gdppot = quarterly_data1$adj_gdppot
#outputgap = quarterly_data1$outputgap (this is the old version of our calculation, it is not relevant anymore)
total_debt = quarterly_data1$Debt
unemployment_rate = quarterly_data1$UN.rate
crisis_dummy = quarterly_data1$Crisis_Dummy
recession_dummy = quarterly_data1$Recession_Dummy
general_dummy = quarterly_data1$General_Dummy

inf_diff = inf - target
outputgap = (gdp-gdppot)/gdppot*100



model = lm(short_term_i~ inf_diff + outputgap) #TAYLOR'S RULE
summary(model)
y1hat = fitted(model)

model2 = lm(short_term_i ~ inf_diff + outputgap + unemployment_rate) #with unemployment rate, unemployment rate is insignificant
summary(model2)
cor(unemployment_rate, outputgap) #very strong correlation with output gap


summary(lm(short_term_i ~unemployment_rate))

alt_model = lm(short_term_i~ inf_diff + outputgap + total_debt) #TAYLOR'S RULE + DEBT
summary(alt_model)

cor(inf_diff, total_debt) #strong correlation

model_inf = lm(short_term_i ~ inf_diff)
summary(model_inf)

model_gap = lm(short_term_i ~ outputgap)
summary(model_gap)

#short term interest over time
ggplot(mapping=aes(x = time, y = short_term_i)) + geom_line()

#interest and inflation joint graph over time
plot(time,short_term_i,type="l",col="red")
lines(time,inf,col="green")


#Fitted values and observed values graph
ggplot(mapping=aes(y=short_term_i, x=y1hat)) + geom_point() + geom_abline(intercept=0, slope = 1)


plot(x=time, y=short_term_i, col = "red", type="l")
lines(x = time, y = y1hat, col = "green")
legend(2000,9.5,legend=c("observed values", "fitted values"),col = c("red", "green"), lty = 1:1)


#just observing the OLS estimates (these two are equivalent)
ggplot(mapping=aes(x = (3.56621 + 1.6156*inf_diff + 0.6206*outputgap), y=short_term_i)) + geom_point() + stat_smooth(method="lm")
ggplot(mapping=aes(x = y1hat, y=short_term_i)) + geom_point() + stat_smooth(method="lm")

#inf_diff regression line
ggplot(mapping=aes(x = inf_diff, y=short_term_i)) + geom_point() + stat_smooth(method="lm")

#outputgap regression line
ggplot(mapping=aes(x = outputgap, y = short_term_i)) + geom_point() + stat_smooth(method="lm")


#DEBT regression line
ggplot(mapping=aes(x = total_debt, y = short_term_i)) + geom_point() + stat_smooth(method="lm")

#Unemployment rate regression line (not good)
ggplot(mapping=aes(x = unemployment_rate, y = short_term_i))

#Diagnostic checks
par(mfrow=c(2,2))
plot(model, 1)
plot(model, 2)
plot(model, 3)
plot(model, 4)
plot(model, 5)

plot(alt_model, 1)
plot(alt_model, 2)
plot(alt_model, 3)
plot(alt_model, 5)

plot(model$fitted.values, model$model$BMI)
hist(model$residuals)
hist(alt_model$residuals)

# Breusch-Pagan test
lmtest::bptest(model)
lmtest::bptest(alt_model)


#Jarque-Bera test
jarque.bera.test(model$residuals)
jarque.bera.test(alt_model$residuals)

#RESET test:
reset(model)
reset(alt_model)

#Durbin-Watson test:
library(car)
durbinWatsonTest(model)
durbinWatsonTest(alt_model)

#Regression plot alternative
ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}
ggplotRegression(model_inf)
ggplotRegression(model_gap)



