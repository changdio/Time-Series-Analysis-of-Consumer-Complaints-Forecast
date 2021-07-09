library(tidyverse)
library(ggplot2)
library(psych)

df <- read.csv("complaints.csv")

df <- df %>% 
  mutate(Date.received = as.Date(Date.received)) %>% 
  mutate(year.month = format(Date.received, '%Y-%m')) %>% 
  group_by(year.month) %>% 
  summarize(complaints.count = n())

# Remove outliers
df <- df %>%
  filter(row_number() <= n()-12)

library(fpp)

# Increasing trend, seasonal trend (dip near end of year) 
ts.df = ts(df[,-1],start=c(2011,12),frequency=12)

autoplot(ts.df) +
  xlab("Year") +
  ylab("Complaints count")

# decomposition
decompose.df = decompose(ts.df, "multiplicative")
decompose.df
plot(decompose.df)

# Dickey-fuller test
adf.test(ts.df, k = 12)

# Stationary
stationary.df <- diff(log(ts.df))
autoplot(stationary.df) +
  xlab("Year") +
  ylab("Complaints count")

adf.test(stationary.df, k = 12)

# ACF and PACF
acf(ts.df)
acf(stationary.df)

ggAcf(ts.df)
ggAcf(stationary.df)

pacf(ts.df)
pacf(stationary.df)

ggPacf(ts.df)
ggPacf(stationary.df)

#fit
fit <- arima(log(ts.df),c(0,1,1), seasonal = list(order = c(0,1,1), period = 12))

predicted_values <- predict(fit, n.ahead = 10*12)
predicted_values_converted <- 2.718^predicted_values$pred

ts.plot(ts.df,predicted_values_converted, log = "y", lty = c(1,3))
title(main="Original and forecasted values",
      xlab="Time", ylab="Number of Consumer Complaints")

# 2022 complaints total
sum(predicted_values_converted[22:33])

part1 <- ts(ts.df,frequency=12,start=c(2011,12),end=c(2018,12))
fit_test <- arima(log(part1), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))
predicted_values_test <- predict(fit_test, n.ahead = 1*12)
predicted_values_test_converted <- 2.718^predicted_values_test$pred

#Predicted values
pred <- round(predicted_values_test_converted,digits=0)
pred

orig <- tail(ts.df,15)
orig

table1 <- data.frame(Date = c("Jan 2019","Feb 2019","Mar 2019","Apr 2019","May 2019","Jun 2019","Jul 2019","Aug 2019", "Sep 2019","Oct 2019","Nov 2019","Dec 2019"),
                     Predicted = c(pred[1:12]),
                     Original = c(orig[1:12])
                     )

table2 <- data.frame(Date = c("Jan 2022","Feb 2022","Mar 2022","Apr 2022","May 2022","Jun 2022","Jul 2022","Aug 2022", "Sep 2022","Oct 2022","Nov 2022","Dec 2022"),
                     Predicted = c(predicted_values_converted[22:33]))