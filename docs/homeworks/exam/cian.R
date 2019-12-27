library(rvest)
library(stringr)
library(tibble)
library(ggplot2)
library(naniar)

parse_page <- function(url){
  # parse html
  cian <- read_html(url)
  
  # parse price
  price <- cian %>%
    html_nodes(xpath = "//div[@class='c6e8ba5398--header--1dF9r' or @class='c6e8ba5398--header--1df-X']") %>%
    html_text() %>% 
    str_replace_all('\u20bd', '') %>%
    str_replace_all(' ', '') %>%
    as.numeric()
  
  
  # parse way time and way type
  way <- cian %>%
    html_nodes(xpath = "//div[@class='c6e8ba5398--remoteness--3bptF']") %>%
    html_text()
  
  way_time <- c()
  way_type <- c()
  
  for(i in 1:length(way)){
    s <- str_split(way[i], pattern = '\\s+', simplify = T)
    way_time <- c(way_time, as.numeric(s[1]))
    if(length(s) == 3){
      way_type <- c(way_type, 'walk')
      next
    }
    if(length(s) == 4){
      way_type <- c(way_type, 'transport')
      next
    }
    way_type <- c(way_type, NA)
  }
  
  # parse room count and metrs
  char <- cian %>%
    html_nodes(xpath = "//a[@class='c6e8ba5398--header--1fV2A']") %>%
    html_text()
  
  ref <- cian %>%
    html_nodes(xpath = "//a[@class='c6e8ba5398--header--1fV2A']") %>%
    html_attr(name = 'href')
  rooms <- c()
  metrs <- c()
  
  for(i in 1:length(char)){
    rooms <- c(rooms, str_extract(char[i], '\\d-') %>% str_replace('-', '') %>% as.numeric())
    metrs <- c(metrs, char[i] %>% 
                 str_extract('[,].*\\sм²') %>% 
                 str_remove('м²') %>%
                 str_remove_all(' ') %>%
                 str_remove(',') %>%
                 str_replace(',', '.') %>%
                 as.numeric())
  }
  
  df <- tibble(price, way_time, way_type, rooms, metrs, ref)
  return(df)
}

pages_count <- 16
urls <- paste0('https://www.cian.ru/cat.php?deal_type=sale&engine_version=2&foot_min=20&metro%5B0%5D=102&offer_type=flat&only_foot=2&room1=1&room2=1&room3=1&p=', 1:pages_count)

data <- tibble()
for(url in urls){
  print(url)
  df <- parse_page(url)
  data <- bind_rows(data, df)
  Sys.sleep(7)
}

# вторичка или нет
type <- c()

for(ref in data$ref){
  flat <- read_html(ref)
  type_item <- flat %>%
    html_node(xpath = "//span[@class='a10a3f92e9--value--3Ftu5']") %>%
    html_text()
  
  
  
  print(type_item)
  type <- c(type, type_item)
}
flat %>%
  html_nodes(xpath = "//div[@class='a10a3f92e9--info-text--2uhvD']")


data$type <- type

########

data %>%
  count(type)

data %>% 
  filter(type == 'Новостройка')

vis_miss(data)

ggplot(data, aes(x = metrs, y = price)) +
  geom_point() + 
  geom_smooth(method = 'lm')

ggplot(data, aes(x = metrs, y = price, color = factor(type))) +
  geom_point()

ggplot(data, aes(x = metrs, y = price, color = factor(rooms))) +
  geom_point()

ggplot(data, aes(x = way_time, y = price, color = way_type)) +
  geom_point()

ggplot(data, aes(x = price, fill = factor(rooms))) +
  geom_histogram()



data1 <- data %>%
  select(-c(stair, max_stair)) %>%
  rename(url = ref) %>%
  unite('subway', c('way_time', 'way_type'), sep = '/') %>%
  mutate(metrs = as.character(metrs)) %>%
  mutate(type = ifelse(type == 'Вторичка', 'second', 'first'))

data1

data1 %>%
  write_csv('Desktop/cian4.csv')

data2 <- data1 %>%
  mutate(price = price / 10**6) %>%
  separate('subway', into = c('subway_time', 'subway_type'), sep = '/', convert = T) %>%
  select(-url) %>%
  mutate(metrs = as.numeric(metrs))

data2


library(rsample)
library(Metrics)
spliter <- initial_split(data2, prop = 0.7)

train <- training(spliter)
test <- testing(spliter)

train %>%
  write_csv('Desktop/train.csv')
test %>%
  write_csv('Desktop/test.csv')

train$type1 <- 'Train'
test$type1 <- 'Test'

df <- bind_rows(train, test)

ggplot(df, aes(as.numeric(metrs), price, color = factor(type1))) +
  geom_point() +
  geom_smooth(data = train, se = F, method = 'lm')