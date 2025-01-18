#loading libraries
library(tidyverse)
install.packages("tsibble")
library(tsibble)
library(ggplot2)

#3a loading the data set 
tute1 <- readr::read_csv("tute1.csv")
View(tute1)

#3b converting data to time series 
mytimeseries <- tute1 |>
  mutate(Quarter = yearquarter(Quarter)) |>
  as_tsibble(index = Quarter)
view(mytimeseries)

#3c time series plots of each of the three series 
mytimeseries |>
  pivot_longer(-Quarter) |>
  ggplot(aes(x = Quarter, y = value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y")
  -------------------------------------------------
  
  #4a installing the package
#install.packages("USgas")
  install.packages("USgas")
library(USgas)

#4b creating tsibble from us total
mydata <- as_tsibble(us_total, index = year, key = state)
View(mydata)

#4c plot the natural gas

mydata1 <- filter(mydata, state == "Maine" | state == "Vermont" | state == "New Hampshire" | state == "Massachusetts" 
                     | state == "Connecticut"  | state == "Rhode Island" )

View(mydata1)
ggplot(mydata1, aes(x=year, y= y, color = state)) + geom_line()

#10
view(aus_livestock)
mydata2 <- filter(aus_livestock, State == "Victoria")
mydata3 <- filter(mydata2, Animal == "Pigs")
mydata3 <- mydata3 |> mutate(Year = year(Month))
mydata4 <- filter(mydata3, Year <= 1995 & Year >= 1990)
mydata4 |> ACF(Count) |> autoplot()
