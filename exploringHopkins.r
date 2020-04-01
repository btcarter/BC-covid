# Exploratory script 
#
# This script is an exploration of the Hopkins dataset on COVID-19.
# Will try and use this to see what sorts of data we have and
# and what we can do with it.

# Environment ####
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)

# Paths to data ####
TS <- file.path("https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")




  # load data for montana and make it long ####
ts.confirmed.us <- read.csv(TS)
ts.confirmed.us.mt <- ts.confirmed.us %>%
  filter(
    "Province_State" == "Montana"
  ) %>%
  gather(key = 'date',
         value = 'confirmed',
         -c("UID","iso2","iso3","code3","FIPS","Admin2","Province_State",
            "Country_Region","Lat","Long_","Combined_Key")
         ) %>%
  mutate(
    date = as.POSIXct(gsub("X(\\d+\\.\\d+\\.2020)",
                "\\1",
                date
                ),
                "%m.%d.%Y")
  )

  # what does it look like so far? ####
    # how many in MT so far? ####
ts.confirmed.us.mt %>%
  group_by(date) %>%
  summarise(
    "cumulative" = sum(confirmed)
  ) %>% ggplot(aes(date, cumulative)) +
  geom_point() + 
  geom_smooth(method = "gam") + 
  theme_classic() +
  labs(title = "Total Cases") +
  xlab("Date") +
  ylab("Cases")

ts.confirmed.us.mt %>%
  group_by(Admin2,
           date) %>%
  summarise(
    "cumulative" = sum(confirmed)
  ) %>% 
  ungroup() %>%
  ggplot(aes(date, cumulative, color = Admin2)) +
  geom_point() + 
  geom_smooth() + 
  theme_classic() +
  labs(title = "Total Cases by County") +
    xlab("Date") +
    ylab("Cases")


  # can we predict growth?


# billings data

bc <- file.path("C:","Users","CarteB","Downloads",
                "Yellowstone County 3.31.20 - Best Estimate.xlsx")
bc <- read_xlsx(bc)

ggplot(bc, aes(Date, TotalCensus)) +
  geom_ribbon(aes(ymin=TotalCensus*0.9,
                  ymax=TotalCensus*1.1),
              alpha=0.6) +
  geom_point(color="blue") +
  labs(
    title = "Total census",
    y = "Number of Patients"
  ) +
  theme_classic()

ggplot(bc, aes(Date, ICUCensus)) +
  geom_ribbon(aes(ymin=ICUCensus*0.9,
                  ymax=ICUCensus*1.1),
              alpha=0.6) +
  geom_point(color="blue") +
  labs(
    title = "ICU census",
    y = "Number of Patients"
  ) +
  theme_classic()


bc %>%
  select(
    Date, TotalCensus, ICUCensus, ventilatorsCensus
  ) %>%
  rename(
    "Total Census" = TotalCensus,
    "ICU Census" = ICUCensus,
    "Ventilators Census" = ventilatorsCensus
  ) %>%
  gather(key = "Census", value = "count", -Date) %>%
  ggplot(aes(Date, count, fill = Census)) +
  geom_ribbon(aes(ymin=count*0.9,
                  ymax=count*1.1),
              alpha=0.6) +
  geom_point() +
  labs(
    title = "Census",
    y = "Number of Patients"
  ) +
  theme_classic()
  

