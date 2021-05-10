# Load readr, ggplot2, and dplyr packages
library(readr)
library(ggplot2)
library(dplyr)

# Read confirmed_cases_worldwide.csv into confirmed_cases_worldwide
(confirmed_cases_worldwide <- read_csv("confirmed_cases_worldwide.csv"))

# Draw a line plot of cumulative cases vs. date 
# Label y axis 
ggplot(confirmed_cases_worldwide, aes (date, cum_cases)) +
  geom_line() +
  labs(y = "Cumulative confirmed cases")

# Plot confirmed Covid cases in China and the rest of the world separately to 
# gain insight

# Read confirmed_cases_china_vs_world.csv 
(confirmed_cases_china_vs_world <- read_csv("confirmed_cases_china_vs_world.csv"))

# Draw a line plot of cumulative cases vs. date, grouped and colored by is_china
# Define aesthetics within the line geom 

(plt_cum_confirmed_cases_cases_china_vs_world <- 
  ggplot(confirmed_cases_china_vs_world) +
  geom_line(aes(date, cum_cases, group = is_china, color = is_china)) +
  ylab("Cumulative confirmed cases"))

# Annotate landmark events

who_events <- tribble (
  ~ date, ~ event,
  "2020-01-30", "Global health\nemergency declared" ,
  "2020-03-11", "Pandemic\ndeclared",
  "2020-02-13", "China reporting \nchange"
  ) %>% 
  mutate(date = as.Date(date))

# Using who_events, add vertical dashed lines with an xintercept at date
# and text at date, labeled by event, and at 100000 on the y-axis
(plt_cum_confirmed_cases_cases_china_vs_world +
  geom_vline(aes(xintercept = date), data = who_events, linetype = "dashed") +
  geom_text(aes(x = date, label = event), data = who_events, y = 1e5))

# Add trend line to China to measure how fast the number of cases is growing

# Filter for China, from Feb 15
(china_after_feb15<- confirmed_cases_china_vs_world %>% 
  filter(is_china == "China", date >= "2020-02-15"))

# Using china_after_feb15, draw a line plot cum_cases vs. date
# Add a smooth trend line using linear regression, no error bars

ggplot(china_after_feb15, aes(date, cum_cases))+
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Cumulative confirmed cases")

# Add trend line for the rest of the world 

# Filter confirmed_cases_china_vs_world for not China
(not_china <- confirmed_cases_china_vs_world %>% 
  filter(is_china == "Not China"))

# Using not_china, draw a line plot cum_cases vs. date
# Add a smooth trend line using linear regression, no error bars
(plt_not_china_trend_lin <- ggplot(not_china, aes(date, cum_cases)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Cumulative confirmed cases"))

# Modify the plot to use a logarithmic scale on the y-axis for better fit
plt_not_china_trend_lin +
  scale_y_log10()

# Cases growing at an exponential rate using log scale. Not all countries
# affected by covid equally. Which countries outside of China have been hit
# hardest?

(confirmed_cases_by_country <- read_csv("confirmed_cases_by_country.csv"))

# Group by country, summarize to calculate total cases, find top 7
(top_countries_by_total_cases <- confirmed_cases_by_country %>% 
  group_by(country) %>% 
  summarize(total_cases = sum(cases)) %>% 
  top_n(7, total_cases))

# Plot hardest hit countries as of Mid-March 2020
(confirmed_cases_top7_outside_china <- read_csv("confirmed_cases_top7_outside_china.csv"))

# Using confirmed_cases_top7_outside_china, draw a line plot of 
# cum_cases vs. date, grouped and colored by country
ggplot(confirmed_cases_top7_outside_china, aes(date, cum_cases, color = country,
                                               group = country)) +
  geom_line() +
  ylab("Cumulative confirmed cases")
