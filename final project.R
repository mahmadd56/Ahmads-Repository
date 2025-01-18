library(tidycensus)
library(tidyverse)
library(plotly)
library(tigris)
options(tigris_use_cache = TRUE)

#Total Population 2022
totalpop2022 = get_acs(geography = "county",variables = "B01001_001",year = 2022)
totalpop2022 = totalpop2022 |> rename(totalpop22 = "estimate")

#hispanic population in America 2022
datahisp2022 <- get_acs(geography = "county", variables = "B03001_003", year = 2022)
datahisp2022 = datahisp2022 |> rename(hisppop22 = "estimate")

#total African American population in America 2022
datablack2022 = get_acs(geography = "county",variables = "B01001B_001",year = 2022)
datablack2022 = datablack2022 |> rename(blackpop22 = "estimate")

# Indian Asian 2022 population 
dataind2022 = get_acs(geography = "county",variables = "B02015_021",year = 2022)
dataind2022 = dataind2022 |> rename(indpop22 = "estimate")

#Nepali 2022 population
datanepal2022 = get_acs(geography = "county",variables = "B02015_024",year = 2022)
datanepal2022 = datanepal2022 |> rename(nepalpop22 = "estimate")

#Pakistan 2022 population
datapak2022 = get_acs(geography = "county",variables = "B02015_025",year = 2022)
datapak2022 = datapak2022 |> rename(pakpop22 = "estimate")

#Bangladesh 2022 population
databangladesh2022 = get_acs(geography = "county",variables = "B02015_022",year = 2022)
databangladesh2022 = databangladesh2022 |> rename(bangladeshpop22 = "estimate")

#Srilankan 2022 population
dataSri2022 = get_acs(geography = "county",variables = "B02015_027",year = 2022)
dataSri2022 = dataSri2022 |> rename(Sripop22 = "estimate")

#Total Population 2017
totalpop2017 = get_acs(geography = "county",variables = "B01001_001",year = 2017)
totalpop2017 = totalpop2017 |> rename(totalpop17 = "estimate")

#hispanic population in America 2017
datahisp2017 <- get_acs(geography = "county", variables = "B03001_003", year = 2017)
datahisp2017 = datahisp2017 |> rename(hisppop17 = "estimate")

#total black population in America 2017
datablack2017 = get_acs(geography = "county",variables = "B01001B_001",year = 2017)
datablack2017 = datablack2017 |> rename(blackpop17 = "estimate")

# Indian Asian 2017 population
dataind2017 = get_acs(geography = "county",variables = "B02018_002",year = 2017, geometry = TRUE )
dataind2017 = dataind2017 |> rename(indpop17 = "estimate")

#Nepali 2017 population
datanepal2017 = get_acs(geography = "county",variables = "B02018_016",year = 2017)
datanepal2017 = datanepal2017 |> rename(nepalpop17 = "estimate")

#Pakistan 2017 population
datapak2017 = get_acs(geography = "county",variables = "B02018_018",year = 2017)
datapak2017 = datapak2017 |> rename(pakpop17 = "estimate")

#Bangladesh 2017 population
databangladesh2017 = get_acs(geography = "county",variables = "B02018_003",year = 2017)
databangladesh2017 = databangladesh2017 |> rename(bangladeshpop17 = "estimate")

#Srilankan 2017 population
dataSri2017 = get_acs(geography = "county",variables = "B02018_019",year = 2017)
dataSri2017 = dataSri2017 |> rename(Sripop17 = "estimate")

#GDP 2017 - 2022
gdp = read.csv("cleanedgdp.csv")
gdp = gdp |> rename(gdp2017 = "X2017")
gdp = gdp |> rename(gdp2022 = "X2022")

#classification data
classifydata = read.csv("classification.csv")
#------------------------

#merge 
jointotal = full_join(totalpop2017,totalpop2022, by = "GEOID") 
jointotal = jointotal |> mutate(totalpopchange17_22 = totalpop22-totalpop17)

joinhisp = full_join(datahisp2017,datahisp2022, by = "GEOID")
joinhisp = joinhisp |> mutate(hisppopchange17_22 = hisppop22-hisppop17)

joinblackpop = full_join(datablack2017,datablack2022,by= "GEOID")
joinblackpop = joinblackpop |> mutate(blackpopchange17_22 = blackpop22 - blackpop17)

joinind = full_join(dataind2017,dataind2022, by = "GEOID")
joinind = joinind |> mutate(indpopchange17_22 = indpop22-indpop17)

joinnepal = full_join(datanepal2017,datanepal2022, by = "GEOID")
joinnepal = joinnepal |> mutate(nepalpopchange17_22 = nepalpop22-nepalpop17)

joinpak = full_join(datapak2017,datapak2022, by= "GEOID")
joinpak = joinpak |> mutate(pakpopchange17_22 = pakpop22-pakpop17)

joinbangladesh = full_join(databangladesh2017,databangladesh2022,by= "GEOID")
joinbangladesh = joinbangladesh |> mutate(bangladeshpopchange17_22 = bangladeshpop22-bangladeshpop17)

joinSri = full_join(dataSri2017,dataSri2022, by = "GEOID")
joinSri = joinSri |> mutate(Sripopchange17_22 = Sripop22-Sripop17)

data = full_join(jointotal,joinind, by = "GEOID")
data = full_join(data,joinnepal, by = "GEOID")
data = full_join(data,joinpak, by = "GEOID")
data = full_join(data,joinbangladesh, by = "GEOID")
data = full_join(data,joinSri, by = "GEOID")

data$GEOID = as.numeric(data$GEOID)
joinhisp$GEOID = as.numeric(joinhisp$GEOID)
joinblackpop$GEOID = as.numeric(joinblackpop$GEOID)
data4 = full_join(data,gdp, by = "GEOID")
data5 = full_join(data4,classifydata, by = "GEOID")
data6 = full_join(data5,joinhisp, by ="GEOID")
data7 = full_join(data6,joinblackpop,by = "GEOID")
#------------------------  

#finaldata
finaldata = data7 |> select(GEOID,NAME.x.x,totalpop17,
                            totalpop22,totalpopchange17_22,
                            indpop17,indpop22,
                            indpopchange17_22,nepalpop17,
                            nepalpop22,nepalpopchange17_22,
                            pakpop17,pakpop22,pakpopchange17_22,
                            bangladeshpop17,bangladeshpop22,bangladeshpopchange17_22,
                            Sripop17,Sripop22,
                            Sripopchange17_22,gdp2017, hisppop22,hisppop17,hisppopchange17_22,
                            blackpop17,blackpop22,blackpopchange17_22,
                            gdp2022,Metropolitan.Micropolitan.Statistical.Area,geometry)
finaldata = finaldata |> rename(NAME = "NAME.x.x")
finaldata = finaldata |> mutate(gdppercap17 = as.numeric(gdp2017)/totalpop17)
finaldata = finaldata |> mutate(gdppercap22 = as.numeric(gdp2022)/totalpop22)
finaldata = finaldata |> mutate(GDPpercapchangerate17_22 = ((gdppercap22-gdppercap17)/gdppercap17)*100)
finaldata = finaldata |> mutate(totalsubindpop17 = indpop17+nepalpop17+pakpop17+bangladeshpop17+Sripop17)
finaldata = finaldata |> mutate(totalsubindpop22 = indpop22+nepalpop22+pakpop22+bangladeshpop22+Sripop22)
finaldata = finaldata |> mutate(totalsubindpopchange17_22 = totalsubindpop22-totalsubindpop17)
finaldata = finaldata |> mutate(percentotalsubind17 = (totalsubindpop17/totalpop17)*100)
finaldata = finaldata |> mutate(percentotalsubind22 = (totalsubindpop22/totalpop22)*100)
finaldata = finaldata |> mutate(percentotalhisp17= (hisppop17/totalpop17)*100)
finaldata = finaldata |> mutate(percentotalhisp22= (hisppop22/totalpop22)*100)
finaldata = finaldata |> mutate(percentotalblack17= (blackpop17/totalpop17)*100)
finaldata = finaldata |> mutate(percentotalblack22= (blackpop22/totalpop22)*100)
#finaldata = finaldata |> mutate(isUrban2017 = if_else(totalpop17>49999,"Yes","No"))
#finaldata = finaldata |> mutate(isUrban2022 = if_else(totalpop22>49999,"Yes","No"))
finaldata1 = finaldata |> filter(totalsubindpop17!=0 & totalsubindpop22 !=0)
#filter to contain only valid states counties
finaldata1 = finaldata |> filter(GEOID <72001 | GEOID > 72153)
finaldata1 = finaldata1 |> filter(GEOID <15001 | GEOID > 15009)
finaldata1 = finaldata1 |> filter(GEOID <2013 | GEOID > 2290)
finaldata1$Metropolitan.Micropolitan.Statistical.Area = finaldata1$Metropolitan.Micropolitan.Statistical.Area |> replace_na("Rural")
finaldata2 = finaldata1 |> filter(GEOID != 11001 & percentotalsubind17>1)
#data is prepared

#------------------------
#Graphical Representation

#plots and histograms

#boxplots

#GDP

ggplot(finaldata2, aes(x = GDPpercapchangerate17_22,y = Metropolitan.Micropolitan.Statistical.Area )) +geom_boxplot() 
##ggplotly(
  ##geom_boxplot() +
    ##geom_text(aes(label = Metropolitan.Micropolitan.Statistical.Area), vjust = 2, size = 3)
#)

ggplot(finaldata2, aes(x = GDPpercapchangerate17_22, y = Metropolitan.Micropolitan.Statistical.Area, label = "NAME")) +
  geom_boxplot() + labs(
        x = "GDP per capita change rate from 2017 to 2022",
        y = "Area"
    )

ggplotly(p, tooltip = "NAME", hoverinfo = "x+y+text")

# Show the plotly plot
plotly_p


#Indian scatter plot
p = ggplot(finaldata2, aes(x =percentotalsubind17,y=GDPpercapchangerate17_22,color = Metropolitan.Micropolitan.Statistical.Area,text = paste("County: ", NAME))) +
  geom_point()+labs(x = "Percentage of Total Subcontinent Indian population in US 2017",
                    y = "GDP per capita change rate 2017-22")+theme(legend.position = "bottom",legend.title =  element_blank())

plotly_p <- ggplotly(p, tooltip = County , hoverinfo = "x+y+text")

# Show the plotly plot
plotly_p

summary(lm(finaldata2$GDPpercapchangerate17_22~finaldata2$percentotalsubind17*finaldata2$Metropolitan.Micropolitan.Statistical.Area))

#hispanic scatter plot
p = ggplot(finaldata2, aes(x =percentotalhisp17,y=GDPpercapchangerate17_22,color = Metropolitan.Micropolitan.Statistical.Area,text = paste("County: ", NAME))) +
  geom_point()+labs(x= "Percentage of Total Hispanic population in US 2017",
                    y = "GDP per capita change rate 2017-22")+theme(legend.position = "bottom",legend.title =  element_blank())

plotly_p <- ggplotly(p, tooltip = "text", hoverinfo = "x+y+text")

# Show the plotly plot
plotly_p

summary(lm(finaldata2$GDPpercapchangerate17_22~finaldata2$percentotalhisp17*finaldata2$Metropolitan.Micropolitan.Statistical.Area))

#black pop scatter plot 
p = ggplot(finaldata2, aes(x =percentotalblack17,y=GDPpercapchangerate17_22,color = Metropolitan.Micropolitan.Statistical.Area,text = paste("County: ", NAME))) +
  geom_point()+labs(x= "Percentage of Total Black population in US 2017",
                    y = "GDP per capita change rate 2017-22")+theme(legend.position = "bottom",legend.title =  element_blank())

plotly_p <- ggplotly(p, tooltip = "text", hoverinfo = "x+y+text")

# Show the plotly plot
plotly_p

summary(lm(finaldata2$GDPpercapchangerate17_22~finaldata2$percentotalblack17*finaldata2$Metropolitan.Micropolitan.Statistical.Area))


#histogram gdp per capita change 17-22 histogram
ggplot(finaldata2, aes(x = GDPpercapchangerate17_22,fill = Metropolitan.Micropolitan.Statistical.Area)) +
  geom_histogram(fill = "red", color = "black",bins = 30) +
  coord_cartesian(xlim = c(0, 80))+labs(x= "GDP per capita change rate 2017-22")


#barplot
ggplot(finaldata2, aes(x = Metropolitan.Micropolitan.Statistical.Area, y = GDPpercapchangerate17_22)) +
  geom_col(stat = "identity")+labs(y = "GDP per capita change rate 2017-22")

#map of rural urban
df = na.omit(finaldata1)
ggplot(data = df, aes(fill = Metropolitan.Micropolitan.Statistical.Area)) +
  geom_sf(aes(geometry = geometry), color = "white") +theme(legend.position = "bottom")
# map of GDPpercapchangerate17_22

ggplot(data = finaldata1, aes(fill = GDPpercapchangerate17_22)) +
  geom_sf(aes(geometry = geometry), color = "white") +
  scale_fill_gradient(low = "red", high = "green", na.value = "white") +
  theme(legend.position = "bottom") +
  labs(title = "GDP per capita change rate 2017-22", fill = "GDP per Capita Change Rate 2017-22")

#map of indpopchange17
ggplot(data = finaldata1,aes(fill = percentotalsubind17))+
  geom_sf(aes(geometry = geometry),color = "white")+scale_fill_gradient(low = "red",
                                                                        high = "green")+theme(legend.position = "bottom") +
  labs(fill = "Percentage of Indians in 2017")
#map of hisppopchange17

ggplot(data = finaldata1, aes(fill = percentotalhisp17)) +
  geom_sf(aes(geometry = geometry), color = "white") +
  scale_fill_gradient(low = "red", high = "green") +
  theme(legend.position = "bottom") +
  labs(fill = "Percentage of Hispanics in 2017")

#map of blackpop17
ggplot(data = finaldata1,aes(fill = percentotalblack17))+
  geom_sf(aes(geometry = geometry),color = "white")+scale_fill_gradient(low = "red",
                                                                        high = "green")+theme(legend.position = "bottom") +
  labs(fill = "Percentage of Blacks in 2017")

#-----------------------------------

#Grouping of data

finaldata3 = na.omit(finaldata2)
#change in gdp by rural and urban
finaldata3 |> group_by(Metropolitan.Micropolitan.Statistical.Area) |> summarise(sum(GDPpercapchangerate17_22))
#counties with most change
finaldata2 |> arrange(desc(GDPpercapchangerate17_22)) |> select(NAME,Metropolitan.Micropolitan.Statistical.Area,GDPpercapchangerate17_22)|> head(5)


#--------------
#Anova test
dt = finaldata |> select(GEOID,NAME,percentotalsubind17,percentotalhisp17,percentotalblack17,GDPpercapchangerate17_22,Metropolitan.Micropolitan.Statistical.Area)
dt = dt |> filter(GEOID <72001 | GEOID > 72153)
dt = dt |> filter(GEOID <15001 | GEOID > 15009)
dt = dt |> filter(GEOID <2013 | GEOID > 2290)
dt$Metropolitan.Micropolitan.Statistical.Area = dt$Metropolitan.Micropolitan.Statistical.Area |> replace_na("Rural")

#for ind 
dtind = dt |> filter(percentotalsubind17 !=0)
summary(aov(dtind$GDPpercapchangerate17_22 ~ dtind$Metropolitan.Micropolitan.Statistical.Area))

#for hisp
dthisp = dt |> filter(percentotalhisp17 != 0)
summary(aov(dthisp$GDPpercapchangerate17_22~dthisp$Metropolitan.Micropolitan.Statistical.Area))

#for African American 
dtafr = dt |> filter(percentotalblack17 != 0 )
summary(aov(dtafr$GDPpercapchangerate17_22~dtafr$Metropolitan.Micropolitan.Statistical.Area))

# for all 
summary(aov(dt$GDPpercapchangerate17_22 ~ dt$Metropolitan.Micropolitan.Statistical.Area))


#testing
ggplot(finaldata2, aes(x =percentotalblack17,y=GDPpercapchangerate17_22,color = Metropolitan.Micropolitan.Statistical.Area)) +
  geom_point()+labs(x= "Percentage of Total Black population in US 2017",
                    y = "GDP per capita change rate 2017-22")+geom_smooth(method = "lm", se = FALSE)

summary(lm(finaldata2$GDPpercapchangerate17_22~finaldata2$percentotalsubind17*finaldata2$Metropolitan.Micropolitan.Statistical.Area))

