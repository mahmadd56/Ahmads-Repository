#loadinf libraries
library(tsibble)
library(fpp3)
-------------------
  
#filtering data
View(aus_livestock)
dt<- filter(aus_livestock, State == "New South Wales")
dt<- filter(dt, Animal == "Pigs")
view(dt)

-------------------------
  
#question8 part a
autoplot(dt)
--------------------
  
#question 8 part b
dt1<- dt %>% filter(year(Month) < 2013)
dt2<- slice(dt,(n()-71):n())
-------------------------
  
#question 8 part c
dt3 <- dt1 |>
  model(
    Mean = MEAN(Count),
    Naive = NAIVE(Count),
    Seasonal_naive = SNAIVE(Count),
    Drift = RW(Count ~ drift()))
dt4 <- dt3 %>% forecast(h=72)
accuracy(dt4,dt2)
-------------------
  
#question 8 part d
dt5 <- dt1 %>%
  model(RW(Count ~ drift()))  
dt5%>%
  gg_tsresiduals()

#Yes, it does resemble white noise
---------------------------------

#question 11 part a
View(aus_production)
mydata <- aus_production %>% filter(year(Quarter) < 2005)
mydata %>%  
  model(STL(Bricks ~ trend(window = 7) +
              season(window = "periodic"),
            robust = TRUE)) |>
  components() |>
  autoplot()

---------------------------------

#question 11part b and part c
mydata1 <- mydata |>
  model(STL(Bricks ~ trend(window = 7), robust = TRUE)) |>
  components() |>
  select(-.model)
mydata1 %>% autoplot(season_adjust)
mydata1 |>
  model(NAIVE(season_adjust)) |>
  forecast() |>
  autoplot(mydata1)

------------------------
#question 11 part d
mydata2 <- mydata|>
  model(stlf = decomposition_model(
    STL(Bricks ~ trend(window = 7), robust = TRUE),
    NAIVE(season_adjust)
  ))
mydata2 |>
  forecast() |>
  autoplot(mydata)

---------------------

#question 11 part e
mydata2 |> gg_tsresiduals()

#plots doesnt look correlated since the lines are out of the blue dotted lines and there isnt a clear trend

----------------------
  
#question 11 part f
mydata2 <- mydata|>
  model(stlf = decomposition_model(
    STL(Bricks ~ trend(window = 7), robust = FALSE),
    NAIVE(season_adjust)
  ))
mydata2 |> gg_tsresiduals()
#it does make a slight difference but still there isnt a clear trend and the lags are still out of blue dotted lines.

-----------------------

#question 11 part g
df <- aus_production %>%
  filter(year(Quarter)>2003) %>% select(Quarter,Bricks) %>% drop_na()
df1 <- df|>
  model(stlf = decomposition_model(
    STL(Bricks ~ trend(window = 7), robust = TRUE),
    SNAIVE(season_adjust)
  ))
df1 |>
  forecast() |>
  autoplot(df)






bricks <- aus_production %>% filter(year(Quarter) < 2005)
bricks %>%  
  model(STL(Bricks ~ trend(window = 7) +
              season(window = "periodic"),
            robust = TRUE)) |>
  components() |>
  autoplot()
#11b and c
dcmp <- bricks |>
  model(STL(Bricks ~ trend(window = 7), robust = TRUE)) |>
  components() |>
  select(-.model)
dcmp %>% autoplot(season_adjust)
dcmp |>
  model(NAIVE(season_adjust)) |>
  forecast() |>
  autoplot(dcmp)
#11d
fit_dcmp <- bricks|>
  model(stlf = decomposition_model(
    STL(Bricks ~ trend(window = 7), robust = TRUE),
    NAIVE(season_adjust)
  ))
fit_dcmp |>
  forecast() |>
  autoplot(bricks)