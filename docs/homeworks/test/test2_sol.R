library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

df <- read_csv('data/results.csv')

df$year <- year(df$date)
df <- df %>% filter(year == '2018', tournament == 'FIFA World Cup')
df <- df %>% select(-neutral)
df <- df %>% select(-year, -country, -tournament)

write_csv(df, 'results.csv')

df <- read_csv('results.csv')

head(df)
# 1.1
glimpse(df)

# 1.2
df$city %>% unique() %>% length()

# 1.3
df$date %>% range()

# 1.4
summary(df[c('home_score', 'away_score')])

# 1.5
df %>% filter(home_team == 'Russia', away_team == 'Spain')

# 2
df %>%
  group_by(home_team) %>%
  summarise(sum_missing_scores = sum(away_score)) %>%
  arrange(desc(sum_missing_scores)) %>%
  head(10)


df %>%
  group_by(date) %>%
  summarise(sum_home_score = sum(home_score),
            sum_away_score = sum(away_score)) %>%
  gather(key = "type", value = "count", sum_home_score, sum_away_score) %>%
  ggplot(aes(x = date, y = count, color = type)) + 
  geom_line() + 
  labs(title = "Количество забитых голов домашней и гостевой командами",
       x = "Дата",
       y = "Количество голов",
       color = "Тип")

df %>% 
  filter(city %in% c('Moscow', 'Kazan', 'Sochi')) %>%
  group_by(city, date) %>%
  summarise(sum_home_score = sum(home_score)) %>%
  ggplot(aes(x = date, y = sum_home_score, color = city)) +
  geom_line() + 
  labs(x = "Дата",
       y = "Количество забитых голов домашней командой",
       color = "Город")
  
  


