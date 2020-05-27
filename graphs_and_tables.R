
# in this script, i visualize some aspects of excess mortality in Sweden.
#

library(tidyverse)

my_theme <- theme(panel.grid = element_blank(), axis.line = element_line(color = "black"),
                  panel.background = element_blank(), axis.text.x = element_text(color = "black"), 
                  axis.text.y = element_text(color = "black"),
                  legend.key = element_blank(), legend.title = element_blank())

# a theme, with the main purpose of removing gridlines and background.

# graph 1)
# purpose: show how each year's daily deaths compares to the other years.

long_data %>% 
  filter(year_day < 130 ) %>% 
  ggplot(aes(x = year_day, y = avg_death, color = factor(year))) +
  geom_line() +
  my_theme +
  scale_x_continuous(breaks = c(0,30,60,90,120),
                     labels = paste0(c("Jan","Feb", "March","April","May"))) +
  labs(title = "Deaths in Sweden, 2015 - 2020",
       subtitle = "7-day moving average",
       caption = "",
       y = "Daily Deaths",
       x = " ")

# Graph 2)
# Purpose: Show how 2018, 2019 and 2020 compare to each in daily deaths. 

long_data %>% 
  filter(year == 2018 | year == 2019 | year == 2020) %>% 
  filter(year_day < 130) %>% 
  ggplot(aes(x = year_day, y = avg_death, color = factor(year))) +
  geom_line() +
  my_theme +
  scale_x_continuous(breaks = c(0,30,60,90,120),
                     labels = paste0(c("Jan","Feb", "Mars","April","May")))  +
  scale_y_continuous(limits = c(100, 400)) +
  labs( title = "Excess Mortality in Sweden",
        subtitle = "7-day moving average",
        caption = "Data from SCB.se",
        y = "Daily Deaths",
        x = " ")

# Table 1)
# Purpose: How many people have died in 18,19 and 20, between March 11th and May 8th

totex <- long_data %>% 
  filter(year > 2017) %>% 
  filter(year_day < 130, year_day > 70) %>% 
  group_by(year) %>% 
  summarise(sum(Deaths)) %>% 
  select(Year = year, Deaths = `sum(Deaths)`) 

# Graph 3
# Purpose; Visualize the excess mortality by county.
# requires the thin dataframe from excess_death_by_country.R

by_county <- thin %>% 
  filter(year_day < 130) %>% 
  ggplot(aes(x = year_day, y = Deaths, color = factor(year))) +
  geom_line() +
  facet_wrap(~County) +
  my_theme 

# A line plot, between January 1th to May 8th, for each county.

by_county +
  scale_x_continuous(breaks = c(0,30,60,90,120), 
                     labels = paste0(c("Jan","Feb", "Mars","April","May"))) +
  labs(title = "Mortality rates in Swedish Counties",
       subtitle = "7-day moving average",
       x = " ",
       y = "Daily Deaths")

# x axis text is changed to name of months. Labels for x axis and y axis is changed.
# a title is added

# Graph 4
# Purpose: Visualize daily deaths in Stockholm, comparing 18,19 and 20
#


thin %>%
  filter(year_day < 130, County == "Stockholm") %>% 
  ggplot(aes(x = year_day, y = Deaths, color = factor(year))) +
  geom_line() +
  theme(legend.key = element_blank(), legend.title = element_blank()) +
  labs( title = "Excess Mortality in Stockholm",
        subtitle = "7-day moving average",
        x = "",
        y = "Daily Deaths") + 
  my_theme + 
  scale_x_continuous(breaks = c(0,30,60,90,120),
                     labels = paste0(c("Jan","Feb", "Mars","April","May")))


# Table 2)
# How many more people have died in Stockholm comparing 2020 to 2019?
# between March 11th to May 8th

ex_death <- thin %>% 
  filter(year_day < 130, year_day > 71) %>%
  filter(year == 2019 |year == 2020) %>% 
  group_by(County, year) %>% 
  summarise(sum(Deaths))

results <- ex_death %>% 
  mutate(diff = `sum(Deaths)` - lag(`sum(Deaths)`)) %>% 
  mutate(excess = round(diff / lag(`sum(Deaths)`)*100)) %>% 
  filter(year == 2020) %>% 
  arrange(desc(excess)) %>% 
  select(County,excess)

results %>% 
  select(County , '% increase deaths' = excess)









