library(readr)
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)

daily <- read_csv("data/daily_SPEC_2014.csv.bz2")
names(daily) <- gsub(" ",".",names(daily))
head(daily)

daily_one <- daily %>%
  filter(State.Name == "Wisconsin", Parameter.Name == "Bromine PM2.5 LC")

mean(daily_one$Arithmetic.Mean)

daily_two <- daily %>%
  group_by(Parameter.Name) %>%
  summarise(Avg.Level = mean(Arithmetic.Mean)) %>%
  arrange(desc(Avg.Level))

head(daily_two)

daily_three  <- daily %>%
  filter(Parameter.Name == "Sulfate PM2.5 LC") %>%
  group_by(State.Code,County.Code,Site.Num) %>%
  summarise(Avg.Level = mean(Arithmetic.Mean)) %>%
  arrange(desc(Avg.Level))

head(daily_three)

daily_four  <- daily %>%
  filter(Parameter.Name == "EC PM2.5 LC TOR", State.Name %in% c("Arizona","California")) %>%
  group_by(State.Name) %>%
  summarise(Avg.Level = mean(Arithmetic.Mean)) %>%
  arrange(State.Name)

daily_four$Avg.Level[2]-daily_four$Avg.Level[1]

daily_five  <- daily %>%
  filter(Parameter.Name == "OC PM2.5 LC TOR", Longitude < -100)

median(daily_five$Arithmetic.Mean)
  
head(daily_five)

aqs_sites <- read_excel("data/aqs_sites.xlsx")
names(aqs_sites) <- gsub(" ",".",names(aqs_sites))

sites_six <- aqs_sites %>%
  filter(Land.Use == "RESIDENTIAL", Location.Setting=="SUBURBAN")

dim(sites_six)

daily$State.Code <- as.numeric(daily$State.Code)
daily$County.Code <- as.numeric(daily$County.Code)
daily$Site.Num <- as.numeric(daily$Site.Num)

daily_sites <- inner_join(daily,aqs_sites,by=c("State.Code","County.Code","Site.Num" = "Site.Number"))

sites_seven <- daily_sites %>%
  filter(Land.Use == "RESIDENTIAL", Location.Setting =="SUBURBAN", 
         Parameter.Name == "EC PM2.5 LC TOR", Longitude.x >= -100)

median(sites_seven$Arithmetic.Mean)

sites_eight <- daily_sites %>%
  filter(Land.Use == "COMMERCIAL", Parameter.Name == "Sulfate PM2.5 LC") %>%
  mutate(Mth.Name = month(Date.Local, label=TRUE)) %>%
  group_by(Mth.Name) %>%
  summarise(Avg.Level = mean(Arithmetic.Mean)) %>%
  arrange(desc(Avg.Level))

sites_nine <- daily_sites %>%
  filter(State.Code == 6, County.Code == 65, Site.Num == 8001,
         Parameter.Name %in% c("Sulfate PM2.5 LC","Total Nitrate PM2.5 LC")) %>%
  group_by(Parameter.Name,Date.Local) %>%
  summarise(Avg.Level = mean(Arithmetic.Mean)) %>%
  group_by(Date.Local) %>%
  summarise(Sum.Level = sum(Avg.Level)) %>%
  filter(Sum.Level > 10)

dim(sites_nine)

sites_ten <- daily_sites %>%
  filter(Parameter.Name %in% c("Sulfate PM2.5 LC","Total Nitrate PM2.5 LC"),
         State.Code %in% c(5,2,16,42), County.Code %in% c(113,90,37,45), Site.Num %in% c(3,35,2)) %>%
  group_by(State.Code, County.Code, Site.Num, Parameter.Name,Date.Local) %>%
  summarise(Avg.Level = mean(Arithmetic.Mean))  %>%
  spread(Parameter.Name,Avg.Level) %>%
  group_by(State.Code, County.Code, Site.Num) %>%
  summarise(Cor.Level = cor(`Sulfate PM2.5 LC`,`Total Nitrate PM2.5 LC`)) %>%
  arrange(desc(Cor.Level))
