#loading packages
library(tidyverse)
library(tsibble)
library(ggplot2)
library(fpp3)
library(dplyr)

#australian population
mydata <- filter(global_economy, Country == "Australia")
view(mydata)
mydata |> autoplot(Population)

#naive
mydata |>
  model(RW(Population ~ drift()))|>
  forecast(h = "5 years") |>
  autoplot(mydata, level = NULL) +
  geom_line(data = slice(mydata, range(cumsum(!is.na(Population)))), 
            aes(y = Population,  linetype = "dotted", colour = "#0072B2"))+
                labs(title = "Australian Population")


#nsw lambs
mydata1 <- filter(aus_livestock, State == "New South Wales")
mydata2 <- filter(mydata1, Animal == "Lambs")
mydata2 |> autoplot(Count)

#seasonal naive
mydata2 |>
  model(SNAIVE(Count ~ lag("year"))) |>
  forecast(h = "5 years") |>
  autoplot(mydata2, level = NULL) +
  geom_point(data = slice(mydata2,  (n() - 3):n()), 
            aes(y = Count, colour = "#0072B2"))+
  labs(title = "NSW lambs")


#Australian takeaway food turnover
dt <- filter(aus_retail, Industry == "Cafes, restaurants and takeaway food services")
dt1 <- filter(dt, State == "Australian Capital Territory")
dt1 |> autoplot(Turnover)

#drift method
dt1|> model(RW(Turnover ~ drift()))|>
  forecast(h = "5 years")|>
  autoplot(dt1, level = NULL)+
  geom_line(
    data = slice(dt1, range(cumsum(!is.na(Turnover)))),
    aes(y = Turnover), linetype = "dotted", colour = "#0072B2"
  )


#q3
# Extract data of interest
recent_production <- aus_production |>
  filter(year(Quarter) >= 1992)
# Define and estimate a model
fit <- recent_production |> model(SNAIVE(Beer))
# Look at the residuals
fit |> gg_tsresiduals()
# Look a some forecasts
fit |> forecast() |> autoplot(recent_production)


#conclusion
#Quarter graph looks very random so it is probably white noise
#the acf doesnt have equal variance because some of the points are very negatively corelated and some are postive amd some are slightly postive and negative
#the residual histogram is not normal

