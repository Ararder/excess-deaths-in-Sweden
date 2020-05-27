
# In this script i clean the data publicly released by SCB
# for easier analysis in R

# This script cleans up deaths broken down by county level.

library(tidyverse)
library(readxl)
library(lubridate)
library(forecast)

filepath <- "2020-05-22-preliminar_statistik_over_doda_inkl_eng.xlsx"
deaths_by_community <- read_excel(filepath, sheet = "Tabell 3", skip = 5)

deaths_by_community <- deaths_by_community[-2,] %>% 
  rename(date = `Datum/Date`, Year = År, Stockholm = `Län / County`, 
         Uppsala = ...4, Södermanland = ...5, Östergötland = ...6,
         Jönköping = ...7, Kronoberg = ...8, Kalmar = ...9, Gotland = ...10,
         Blekinge = ...11, Skåne = ...12, Halland = ...13, VästraGötaland = ...14,
         Värmland = ...15, Örebro = ...16, Västmanland = ...17, Dalarna = ...18, Gävleborg = ...19, 
         VästerNorrland = ...20, Jämtland = ...21, Västerbotten = ...22, Norrbotten = ...23) %>% 
  select(-...24, -...25 , -...26, -...27)

# Columns are renamed to county names. Unncessecary rows and columns are removed

deaths_by_community <- deaths_by_community[-c(1,2),]

# The first two rows does not contain death data.

misc <- deaths_by_community[(846:851),]
deaths_by_community <- deaths_by_community[-(846:851),]

# the last five rows contain sum of deaths per county, 
# and number of deaths without a date.

deaths_by_community <-  deaths_by_community %>% 
  separate(date, into = c("Day", "Month"), sep = " ")

# the date column is separated into two columns, 
# so that months can be renamed from Swedish.

deaths_by_community <- deaths_by_community %>% 
  mutate(Month = case_when(Month == "januari" ~ 1, Month == "februari" ~2,
                           Month == "mars" ~ 3, Month == "april" ~ 4,
                           Month == "maj" ~ 5, Month == "juni" ~ 6,
                           Month == "juli" ~ 7, Month == "augusti" ~ 8,
                           Month == "september" ~ 9, Month == "oktober" ~ 10,
                           Month == "november" ~ 11, Month == "december" ~ 12))

# This allows lubridate to parse it as a date

deaths_by_community <- deaths_by_community %>% 
  mutate_all(~ replace(., . == "..", values = 0))

# in excel arc, zero deaths is coded as ".." This is changed to 0.

deaths_by_community <- deaths_by_community %>% 
  unite(., col = "Date", Day, Month, Year,sep = "-",) %>%
  mutate(Date = dmy(Date))

# the date columns are united into one column, and converted to date format

community_long <- deaths_by_community %>% 
  pivot_longer(cols = -1,
               names_to = "County",
               values_to = "Deaths") 

# dataframe is pivoted to a long format. 

community_long <- community_long %>% 
  mutate(Deaths = as.numeric(Deaths))

# Death column is converted to numeric format.
# One clean version is now complete.

progress <- deaths_by_community %>% 
  arrange(Date) %>% 
  mutate_at(c(2:22), .funs = as.numeric) %>% 
  mutate_at(c(2:22), ma, order = 7) %>% 
  mutate_at(c(2:22), round)

# Another clean dataset is made. This one has a moving average computed
# before its pivoted to long format.
# Here i compute the moving average for each county.

progress <- progress %>% 
  mutate_at(c(2:22), as.numeric)

# Changed from timeseries format to numeric format.

thin <- progress %>% 
  pivot_longer(cols = -1,
               names_to = "County",
               values_to = "Deaths")

# the dataframe is now pivoted to long format.

thin <- thin %>% 
  mutate(year = year(Date), year_day = yday(Date))

# to make visualizing easy, year and year day is added as variables
# The data cleaning is now complete.


