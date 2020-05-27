
# In this script i clean the data publicly released by SCB
# for easier analysis in R

# NOTE: to run on your own computer, make sure filepath is assigned so that
# it corresponds to where the excel file is on your personal computer.

library(tidyverse)
library(readxl)
library(lubridate)
library(forecast)

filepath <- "2020-05-22-preliminar_statistik_over_doda_inkl_eng.xlsx"
daily_deaths_sweden <- read_excel(filepath, sheet = "Tabell 1", skip = 6)



daily_deaths_sweden <- daily_deaths_sweden %>% 
  select(DagMånad, "2015", "2016", "2017", "2018", "2019","2020")

# These are the columns we need.

daily_deaths_sweden <- separate(daily_deaths_sweden, 
                                DagMånad, c("Day", "Month"), sep = " ")

# the column DagMånad is separated into days and months.

daily_deaths_sweden <- daily_deaths_sweden %>% 
  mutate(Month = case_when(Month == "januari" ~ 1,Month == "februari" ~2,
                           Month == "mars" ~ 3, Month == "april" ~ 4,
                           Month == "maj" ~ 5, Month == "juni" ~ 6,
                           Month == "juli" ~ 7, Month == "augusti" ~ 8,
                           Month == "september" ~ 9, Month == "oktober" ~ 10,
                           Month == "november" ~ 11, Month == "december" ~ 12))

# The names of the months are in swedish. To process with lubridate, 
# i recode them to numbers.

daily_deaths_sweden <- daily_deaths_sweden %>% 
  mutate(deaths_2015 = `2015`, deaths_2016 = `2016`, 
         deaths_2017 = `2017`, deaths_2018 = `2018`, 
         deaths_2019 = `2019`,
         deaths_2020 = `2020`) %>% 
  select(deaths_2015, deaths_2016, deaths_2017, 
         deaths_2018, deaths_2019, deaths_2020, Month, Day)

# The columns are renamed, so that they are easier to work with. 

unknown_date <- daily_deaths_sweden[367,]

# For some cases, date of death is not known.

daily_deaths_sweden <- daily_deaths_sweden[-367,]

# the row containg how many such deaths there are per yer are removed.

long_data <- daily_deaths_sweden %>% 
  pivot_longer(cols = c(deaths_2015, deaths_2016, 
                        deaths_2017, deaths_2018, 
                        deaths_2019, deaths_2020), 
               names_to = "Year",
               values_to = "Deaths",
               names_prefix = "deaths_")

# the dataframe is pivoted to a longer format

long_data <- long_data %>% 
  unite("date", c(Year, Month, Day), sep = " ") %>% 
  mutate(date = ymd(date))

# the date columns are united into one, and converted to date format

long_data <- long_data %>% 
  arrange(date) %>% 
  mutate(avg_death = ma(Deaths, 7))

# a new column is added; a 7-day moving average to smooth 

long_data <- long_data %>% 
  mutate(year = year(date), year_day = yday(date)) 

# two other columns are added, year, and year day

# The data cleaning is now complete.

long_data



