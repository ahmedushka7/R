library(httr)
library(dplyr)
library(tidyr)
library(rlist)
library(stringr)

my_file <- file('token.txt', "r")
token <- readLines(my_file)

get_method <- function(method, token, params, version = "5.52"){
  url <- paste('https://api.vk.com/method/', method, sep = '')
  params <- append(params, list(access_token = token, v = version))
  request <- GET(url, query = params)
  info <- content(request)
  return(info)
}


params <- list(domain = "hsemem", count = '100', offset = as.character(0))
con <- get_method(method = "wall.get", token = token, params = params)
  
to_df <- function(con){
  
  posts <- con$response$items
  
  reposts <- c()
  likes <- c()
  
  for(i in 1:length(posts)){
    reposts <- append(reposts, posts[[i]]$reposts$count)
    likes <- append(likes, posts[[i]]$likes$count)
  }
  
  df <- tibble('reposts' = reposts, 'likes' = likes)
  
  return(df)
}


n <- con$response$count

for(i in seq(100, n, 100)){
  Sys.sleep(0.4)
  params <- list(domain = "hsemem", count = '100', offset = as.character(i))
  con <- get_method(method = "wall.get", token = token, params = params)
  cur_df <- to_df(con)
  df <- df %>% union(cur_df)
}

library(ggplot2)

df %>%
  ggplot(aes(x = likes)) +
  geom_histogram(bins = 25)

df %>%
  ggplot(aes(x = reposts)) +
  geom_histogram(bins = 25)
