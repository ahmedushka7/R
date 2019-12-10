### packages
library('httr')
library('dplyr')

### get token
app_id <- '6925724' # id приложения
url_get_token <- paste('https://oauth.vk.com/authorize?client_id=', 
                       app_id,'&display=page&redirect_uri=https://oauth.vk.com/blank.html&scope=friends&response_type=token&v=5.52', 
                       sep = '')

###

get_method <- function(method, params, token){
  url <- paste('https://api.vk.com/method/',
               method,
               '?v=5.52&access_token=', 
               token, sep = '')
  request <- GET(url, query = params)
  info <- content(request)
  return(info)
}

### get my frieds

token <- '9c75456aafba5bfd8a54327823573db8d6b6ae4217c5eb93461f4b0885def43b729b2478c487f9d1a83ad'
method <- 'friends.get'
params <- list(fields = 'id,nickname',
               user_id = '')
res <- get_method(method, params, token)

str(res, max.level = 2)

d <- res$response$items
data <- d %>% 
          bind_rows() %>%
          select(id, first_name, last_name)

my_id <- 76
my_name <- 'Зарманбетов Ахмед'
data <- data %>%
  transmute(id1 = my_id,
         id2 = id,
         name1 = my_name,
         name2 = str_c(first_name, last_name, sep = ' '))
list_my_friends <- data$id2

### friends of my friends

token <- '9c75456aafba5bfd8a54327823573db8d6b6ae4217c5eb93461f4b0885def43b729b2478c487f9d1a83ad'
method <- 'friends.get'

for(id in list_my_friends){
  params <- list(fields = 'id,nickname',
                 user_id = as.character(id))
  res <- get_method(method, params, token)
  d <- res$response$items
  df <- d %>% 
    bind_rows() %>%
    select(id, first_name, last_name)
  break
}

library(jsonlite)
library(rlist)
request <- GET(url, query = params)
str(fromJSON(content(request, as = 'text')))


d %>%
  list.select(id, first_name, last_name) %>%
  bind_rows() %>%
  filter(id %in% list_my_friends)



