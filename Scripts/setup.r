#Author: Sam Lee
#04/17/2024

#This script aggregates all the data and creates a clean working data set for analysis.

library(tidyverse)

#setwd(".../Scripts")

crimes <- read_csv("../Data/Crimes.csv")
crimes$DateTime <- mdy_hms(crimes$Date)
crimes$Date <- as.Date(crimes$DateTime)
crimes$hour <- hour(crimes$DateTime)

moons <- read_csv("../Data/full_moon.csv") %>%
  mutate(Date = dmy(FullMoonDates))

holidays <- read_csv("../Data/holidays.csv") %>%
  mutate(Date = ymd(Date))

#Weather from Open-Meteo API
#API Call: https://archive-api.open-meteo.com/v1/archive?latitude=41.881832&longitude=-87.72&start_date=2010-01-01&end_date=2024-02-24&hourly=temperature_2m,relative_humidity_2m,apparent_temperature,rain,snowfall,snow_depth,cloud_cover,wind_speed_10m,wind_gusts_10m,is_day,shortwave_radiation_instant,direct_radiation_instant&timezone=America%2FChicago

weather.data <- fromJSON("../Data/weather.json")["hourly"] %>% as.data.frame()
weather.data$hourly.time = weather.data$hourly.time %>% str_replace("T", " ")
weather.data$hourly.time = ymd_hm(weather.data$hourly.time)
weather.data <- weather.data %>%
  mutate(
    Date = as.Date(hourly.time),
    hour = hour(hourly.time) %>%
      as.numeric()
  )
weather.covariates <- colnames(weather.data)[2:(ncol(weather.data)-2)]

cutoff.date = max(
  c(min(crimes$Date), min(holidays$Date), min(weather.data$Date))
)
upper.cutoff = min(
  c(max(crimes$Date), max(holidays$Date), max(weather.data$Date))
)

crimes <- weather.data %>% left_join(crimes) %>%
  mutate(
    DateTime = hourly.time,
    Year = year(DateTime)
  )

week.days <- c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")

crimes.cleaned <- crimes %>%
  left_join(holidays) %>%
  left_join(moons) %>%
  mutate(
    Holiday = ifelse(is.na(Holiday), "", Holiday),
    DayofWeek = week.days[wday(Date, week_start=1)],
    FullMoon = ifelse(is.na(FullMoonDates),0,1)
  ) %>% filter(
    Date >= cutoff.date & Date <= upper.cutoff
  )

factors = c("DateTime", "Date", "Primary Type", "Location Description", "Arrest",
            "Domestic", "Community Area", "Year", "Latitude", "Longitude",
            "FullMoon", "DayofWeek", "Holiday", "hour", weather.covariates)

crimes.cleaned <- crimes.cleaned[,factors]

#In the FBI's Uniform Crime Reporting (UCR) Program, violent crime is composed 
#of four offenses: murder and nonnegligent manslaughter, forcible rape, 
#robbery, and aggravated assault.

#https://www.chicago.gov/city/en/sites/vrd/home/violence-victimization.html
allcrimes = crimes.cleaned$`Primary Type` %>% unique() %>% sort()
violence.key <- c(0,1,1,1,0,1,0,1,0,0,0,1,1,0,1,1,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,1,0,0,0,0)
mapping <- setNames(seq_along(unique(allcrimes)), unique(allcrimes))
crimes.cleaned$Violent <- violence.key[unname(mapping[crimes.cleaned$`Primary Type`])]

crimes.cleaned <- crimes.cleaned %>%
  mutate(
    #These are the hours where there weren't any violent crimes 
    Violent = ifelse(is.na(Violent), 0, Violent)
  )

crimes.cleaned <- crimes.cleaned %>% group_by(Violent, Date, hour) %>%
  mutate(
    NumViolentCrimes = Violent*n()
  ) %>% ungroup()

#Assume unemployment rate in Chicago's 11th police district is proportional
#to the unemployment rate in the Chicago area
unem <- read_csv("Data/chicago-unemployment.csv") %>%
  dplyr::select(Year, Label, Value) %>%
  mutate(
    Date = ym(Label),
    Month = month.name[month(Date)]
  ) %>% dplyr::select(-Label) %>% setNames(c(
    "Year", "Unemployment", "Date", "Month"
  )) %>% arrange(Date)

crimes.cleaned$Month <- month.name[month(crimes.cleaned$Date)]

#Add in monthly unemployment
crimes.cleaned <- crimes.cleaned %>%
  left_join(
    unem %>% dplyr::select(-Date), by=join_by(Year, Month)
  )

#Write the cleaned data set a file
write_csv(crimes.cleaned, "../Data/crimes_cleaned.csv")