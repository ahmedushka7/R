packages <- c('readr', 'dplyr', 'tidyr', 'ggplot2')
# install.packages(packages)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)


data <- read_csv('data/StudentsPerformance.csv')

data1 <- data %>% 
  mutate(student_id = 1:1000) %>%
  select(student_id, everything()) %>%
  rename(race = `race/ethnicity`,
         parental_lvl_of_edu = `parental level of education`,
         math_score = `math score`,
         reading_score = `reading score`,
         writing_score = `writing score`,
         prepartion_course = `test preparation course`) %>%
  mutate(lunch = ifelse(lunch == 'standard', 1, 0),
         prepartion_course = ifelse(prepartion_course == 'completed', 1, 0)) %>%
  gather(key = 'type_test', value = 'score', math_score, reading_score, writing_score) %>%
  arrange(student_id)
  
write_csv(data1, 'data/students.csv')