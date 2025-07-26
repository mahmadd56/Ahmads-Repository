#loading libraries
library(fpp3)
view(global_economy)

#q2 part 1
mydata <- filter(global_economy, Country == "United States")
view(mydata)
mydata |> autoplot(GDP)

#q2 part 2
mydata1 <- filter(aus_livestock, Animal == "Bulls, bullocks and steers")
mydata2 <- filter(mydata1, State == "Victoria")

mydata2 |> autoplot(log(Count))
# logs makes it smoother and the variance is smaller

#q2 part 3
vic_elec |> autoplot(log(Demand))
# logs makes it smoother and the variance is smaller

#q2 part 4
aus_production |> autoplot(log(Gas))
# logs make it more linear 


#q5
#tobacco from aus production
lambda <- aus_production |>
  features(Tobacco, features = guerrero) |>
  pull(lambda_guerrero)
aus_production |>
  autoplot(box_cox(Tobacco, lambda)) 
  

#ansett economy
#filtering
dt <- filter(ansett, Class == "Economy")
view(dt)
dt1 <- filter(dt, Airports == "MEL-ADL")
view(dt1)

#plotting
lambda <- dt1 |>
  features(Passengers, features = guerrero) |>
  pull(lambda_guerrero)
dt1 |>
  autoplot(box_cox(Passengers, lambda)) 

#pedestrian
#filtering
mt <- filter(pedestrian, Sensor == "Southern Cross Station")
view(mt)

#plotting
lambda <- mt |>
  features(Count, features = guerrero) |>
  pull(lambda_guerrero)
mt |>
  autoplot(box_cox(Count, lambda)) 

