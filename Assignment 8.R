#loading libraries
library(fpp3)
library(ggplot2)

#running the code given
jan14_vic_elec <- vic_elec |>
  filter(yearmonth(Time) == yearmonth("2014 Jan")) |>
  index_by(Date = as_date(Time)) |>
  summarise(
    Demand = sum(Demand),
    Temperature = max(Temperature)
  )
#1a
ggplot( data = jan14_vic_elec, aes(x = Temperature,y = Demand)) +geom_point() +
  geom_smooth(method = "lm", se = FALSE)
jan14_vic_elec |>
  model(TSLM(Demand ~ Temperature)) |>
  report()
#when the temperature increases, people turn on their AC's for the entire day and fans are also running the entire day, hence increasing the electricity demand


#1b
dt <- jan14_vic_elec |> model(TSLM(Demand ~ Temperature)) 
dt |> gg_tsresiduals()

#in the start there is positive correlation but after that there wasn’t a strong correlation 
#the outlier was in line graph, there were 2 and both in jan 27.
#the model i think isnt adequate


#1c

myforecast <- jan14_vic_elec |>
  model(TSLM(Demand ~ Temperature)) |>
  forecast(
    new_data(jan14_vic_elec, 4) |>
      mutate(Temperature = 15)
  ) |>
  autoplot(jan14_vic_elec)

jan14_vic_elec |>
  model(TSLM(Demand ~ Temperature)) |>
  forecast(
    new_data(jan14_vic_elec, 1) |>
      mutate(Temperature = 35)
  ) |>
  autoplot(jan14_vic_elec)

#when the temperature is 15, demand is less and when the temperature is 35, demand increases
#i believe this forecast because demand will increase when the temperature increases


#1d

# 95% interval from a standard normal distribution
interval <- hilo(dist_normal(0, 1), 95)
interval

jan14_vic_elec |>
  model(TSLM(Demand ~ Temperature)) |>
  forecast(
    new_data(jan14_vic_elec, 1) |>
      mutate(Temperature = 35)
  ) |> hilo(95)

#ask prof leahy

jan14_vic_elec |>
  model(TSLM(Demand ~ Temperature)) |>
  forecast(
    new_data(jan14_vic_elec, 1) |>
      mutate(Temperature = 35)
  ) |>  hilo(95)

#1e
vic_elec <- vic_elec |>
  index_by(Date = as_date(Time)) |>
  summarise(
    Demand = sum(Demand),
    Temperature = max(Temperature)
  )

ggplot( data = vic_elec, aes(x = Temperature,y = Demand)) +geom_point()+
  geom_smooth(method = "lm", se = FALSE)
 
#There is no correlation but there is a trend that we can see


#4a
autoplot(souvenirs)
#whenever christmas comes, the sales increase then it decreases 


#4b
#we would use logarithms for this because currently there is no equal variance, what log function will do that it will create equal variance hence helping us to predict our forecast

#4c

mysouvernirs <- souvenirs |> mutate(fest = case_when(year(Month) > 1987 & month(Month) == 3 ~ 1, TRUE ~ 0))

mysouvernirs <- souvenirs |> mutate(new_sales = log(Sales)) |> select(-Sales) 

mysouvernirs |> autoplot()
mysouvernirs <- mysouvernirs |> mutate(surf = case_when(year(Month) >= 1988 & month(Month) == 3 ~ 1,
                                         TRUE ~ 0))

fit <- mysouvernirs |> 
  model(TSLM(new_sales ~ trend() + season() + surf)
  )
report(fit)

#4d

augment(fit) |>
  ggplot(aes(x = new_sales, y = .fitted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  labs(y = "Fitted values", x = "normal values")


#4e

boxplot(.resid~month(Month), augment(fit))

#4f

#there are high coefficients for the month of november and december which shows that there is a high correlation in those 2 months
#for the other 10 months, there isnt much correlation

#4g

augment(fit) |> features(.innov, ljung_box, lag = 24)

#since we have residuals that are closer to white noise and the p value also is very low, this shows our model is good

#4h


fit <- mysouvernirs |>
  model(TSLM(new_sales ~ trend() + season()))
mysouvernirs2 <- fit |> forecast(h = "3 years")
mysouvernirs2 |> hilo(95)
mysouvernirs2 |> autoplot(mysouvernirs)

#4i

# since we are using dummy variables i dont think these are that accurate

#section 8
#1

dt <- aus_livestock |> filter(Animal == "Pigs", State == "Victoria")
autoplot(dt)
# a.
fit1 <- dt |> model(additive = ETS(Count ~ error("A") + trend("A") + season("A")))
report(fit1)
fc <- fit1 |> forecast(h = "4 months")
fc |> autoplot(dt)

# b.
sd(augment(fit1)$.resid)
84776 + 1.96 * 7759.141
84776 - 1.96 * 7759.141

hilo(fc, 95)

#we have a narrow interval as compared to the other interval