## R ----

setwd("/Users/graham/Dropbox/01_gitR/NHS_england_covid")
## load packages ----
library(tidyverse)
library(lubridate)
library(qcc)
library(cronR)


## import data ----

covid_data <- as_tibble(read_csv("https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv"))

covid_deaths <- as_tibble(read_csv("https://coronavirus.data.gov.uk/downloads/csv/coronavirus-deaths_latest.csv"))

#rename columns

covid_gt <- rename(covid_data, area_name = `Area name`, area_code = `Area code`, area_type = `Area type`, date = `Specimen date`, 
                   daily_confirmed = `Daily lab-confirmed cases`, reported =`Previously reported daily cases`, changed = `Change in daily cases`, 
                   cumulative_confirmed = `Cumulative lab-confirmed cases`, prev_rep_cumulative = `Previously reported cumulative cases`, 
                   delta_cumulative = `Change in cumulative cases`, 
                   cumulative_rate = `Cumulative lab-confirmed cases rate`)

covid_deaths_gt <- rename(covid_deaths, area_name = `Area name`, area_code = `Area code`, area_type = `Area type`, 
                          date = `Reporting date`, new_deaths = `Daily change in deaths`, cumulative_deaths = `Cumulative deaths` )

covid_upper <- filter(covid_gt, area_type == "Upper tier local authority")

covid_lower <- filter(covid_gt, area_type == "Lower tier local authority")



utla_area_names <- unique(select(covid_upper, area_name))

covid_regions <- filter(covid_gt, area_type == "Region")

length(unique(covid_regions$area_name))

region_names <- unique(select(covid_regions, area_name))

utlas <- select(covid_upper, area_name, date, daily_confirmed)

utlas_by_date <- group_by(utlas, date)

ltlas <- select(covid_lower, area_name, date, daily_confirmed)

ltlas_by_date <- group_by(ltlas, date)

daily_max <- slice(utlas_by_date, (which.max(daily_confirmed)))

top_ulta <- top_n(utlas_by_date, 2, daily_confirmed)

top_ltla <- top_n(ltlas_by_date, 2, daily_confirmed)

#plot(top_ltla, daily_confirmed, date)

##Dates for labels----

sys_date <- Sys.Date()
start_date <- as.Date("2020-03-01")
label_start_date <- format(start_date, "%d %b")
label_sys_date <- format(sys_date, "%d %b")
label_dates <- paste0(label_start_date," to ",label_sys_date, " https://github.com/LordGenome/NHS_england_covid")


#add log2() columns
#owid <- mutate(owid, log2tdpm = log2(total_deaths_per_million), log2tcpm = log2(total_cases_per_million))

#local filters
#local_filter <- c("Calderdale", "Kirklees", "Blackburn with Darwen", "Leeds", "North Yorkshire", "Bradford", "Rochdale")

#local_filter <- c("Calderdale", "Kirklees", "North Yorkshire")

local_filter <- c("Kirklees", "Leeds", "North Yorkshire", "Bradford", "Rochdale", "Leicester")

covid_local <- filter(covid_upper, area_name %in% local_filter)

## filtering by date ----

date_v <- seq(as.Date("2020-03-01"), as.Date(sys_date), by = "days") #date window

## date range filter ----
date_range <- filter(covid_local,  date %in% date_v)

#date_range <- mutate(date_range, Date = as.Date(date)) #then back to date format

image_name <- paste("images/utlas_",sys_date, ".png", sep = "")

##plot new deaths or cases per million in the countries selected at date range filter ----
ggplot(date_range) +
  stat_smooth(mapping = aes(x = date, y = daily_confirmed, group = area_code, colour = area_name), span= 0.7, se = FALSE, show.legend = TRUE) +
  #geom_jitter(mapping =  aes(x = date, y = daily_confirmed, colour = area_name, shape =  area_name), show.legend = TRUE) +
  theme_bw() +
  scale_x_date(NULL,
               breaks = scales::breaks_width("1 week"),
               labels = scales::label_date_short()) +
  #scale_y_continuous(name = "new cases", breaks = seq(0, 25, by = 5)) +
  ylim(0, 50) +
  ylab("new cases") +
  labs (title = "Daily Covid-19 new cases in UTLAs",
        subtitle = "Source: (https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv)",
        caption = label_dates) +
  ggsave(image_name)

## scan all upper tiers for spikes then identify any cosum deviants


## Regions ----

date_v <- seq(as.Date("2020-03-01"), as.Date(sys_date), by = "days")
regions <- filter(covid_regions,  date %in% date_v)

image_name <- paste("images/regions_",sys_date, ".png", sep = "")

ggplot(regions) +
  stat_smooth(mapping = aes(x = date, y = daily_confirmed, group = area_code, colour = area_name), span= 0.7, se = FALSE, show.legend = TRUE) +
  #geom_point(mapping =  aes(x = date, y = daily_confirmed, colour = area_name, shape =  area_name), show.legend = TRUE) +
  theme_bw() +
  scale_x_date(NULL,
               breaks = scales::breaks_width("1 week"),
               labels = scales::label_date_short()) +
  #scale_y_continuous(name = "new cases", breaks = seq(0, 25, by = 5)) +
  ylim(0, 350) +
  ylab("new cases") +
  labs (title = "Daily Covid-19 new cases in NHSE Regions",
        subtitle = "Source: (https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv)",
        caption = label_dates) +
  ggsave(image_name)

## deaths regions ---

date_v <- seq(as.Date("2020-03-01"), as.Date(sys_date), by = "days")
covid_deaths_regions <- filter(covid_deaths_regions,  date %in% date_v)

image_name <- paste("images/deaths_by_regions_",sys_date, ".png", sep = "")

#
#ggplot(covid_deaths_nations) +
#  stat_smooth(mapping = aes(x = date, y = new_deaths, group = area_code, colour = area_name), span= 0.7, se = FALSE, show.legend = TRUE) +
#  #geom_point(mapping =  aes(x = date, y = daily_confirmed, colour = area_name, shape =  area_name), show.legend = TRUE) +
#  theme_bw() +
# scale_x_date(NULL,
#               breaks = scales::breaks_width("1 week"),
#               labels = scales::label_date_short()) +
#  #scale_y_continuous(name = "new cases", breaks = seq(0, 25, by = 5)) +
#  ylim(0, 350) +
#  ylab("new cases") +
#  labs (title = "Daily Covid-19 deaths in NHSE Regions",
#        subtitle = "Source: (https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv)",
#        caption = label_dates) +
#  ggsave(image_name)
