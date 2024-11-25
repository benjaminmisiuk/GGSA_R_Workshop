#install.packages("earth")
#install.packages("randomForest")

#load the data
df <- read.csv("canada_beer.csv")

head(df)

plot(df$temp, df$breweries_per_100k); cor(df$temp, df$breweries_per_100k)
plot(df$precip, df$breweries_per_100k); cor(df$precip, df$breweries_per_100k)

#fit a linear model between precipitation and breweries per 100k
lm_mod <- lm(formula = breweries_per_100k ~ precip, data = df)
#predict the x values on the plot using the model
df$lm <- predict(lm_mod, df)
i <- order(df$precip)
lines(df$precip[i], df$lm[i])

lm_mod

#fit a random forest
library(randomForest)
rf_mod <- randomForest(formula = breweries_per_100k ~ precip, data = df)
#predict the x values on the plot using the model
df$rf <- predict(rf_mod, df)
lines(df$precip[i], df$rf[i], col = 'red')

#fit a MARS model
library(earth)
mars_mod <- earth(formula = breweries_per_100k ~ precip, data = df)
#predict the x values on the plot using the model
df$mars <- predict(mars_mod, df)
lines(df$precip[i], df$mars[i], col = 'blue')

#predict how many breweries there would be in a city with 1000mm of precipitation
predict(lm_mod, data.frame(precip = 1000))
predict(rf_mod, data.frame(precip = 1000))
predict(mars_mod, data.frame(precip = 1000))

#fit models with both temperature and precipitation
lm_mod <- lm(formula = breweries_per_100k ~ temp + precip, data = df)
rf_mod <- randomForest(formula = breweries_per_100k ~ temp + precip, data = df)
mars_mod <- earth(formula = breweries_per_100k ~ temp + precip, data = df)

#predict how many breweries there would be in a city with 1000mm of precipitation and 5 degrees
predict(lm_mod, data.frame(temp = 5, precip = 1000))
predict(rf_mod, data.frame(temp = 5, precip = 1000))
predict(mars_mod, data.frame(temp = 5, precip = 1000))
