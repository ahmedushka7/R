library('httr')
library('dplyr')

# app_id <- '6925724' # id приложения
# url_get_token <- paste('https://oauth.vk.com/authorize?client_id=', app_id,'&display=page&redirect_uri=https://oauth.vk.com/blank.html&scope=friends&response_type=token&v=5.52', sep = '')
# get_token <- GET(url_get_token)


token <- '35ff967c5ae479a991d8e0659a3f9aea7df2ede25dcb895df27aeb219b19474886380d04b1661752c4c22'

method <- 'friends.get'
url <- paste('https://api.vk.com/method/',
             method,
             '?v=5.52&access_token=', 
             token, sep = '')
params <- list(fields = 'americans,nickname')
request <- GET(url, query = params)
q <- content(request)
str(q, max.level = 4)
d <- q$response$items
data <- d %>% 
          bind_rows() %>%
          select(id, first_name, last_name)

# get_request <- function()

### Отправка сообщения
method <- 'messages.send'
url <- paste('https://api.vk.com/method/',
             method,
             '?v=5.101&access_token=', 
             token, sep = '')
params <- list(user_id = '73614108',
               random_id = '1',
               message = 'as')
request <- GET(url, query = params)
request

