library(rvest)
library(dplyr)
library(ggplot2)
library(tibble)
library(stringr)

# Функция, которая забирает информацию с одной страницы
make_df_from_parse <- function(url){
  
  # подгружаем контент с сайта
  books <- read_html(url)
  
  # здесь лежит ВЕСЬ контент по книгам
  res <- books %>% html_nodes(xpath='//article[@class="product_pod"]')
  
  # достали из контента названия книг
  titles <- res %>% 
    html_nodes(xpath='//h3//a') %>% 
    html_attr(name = 'title')
  
  # достали из контента цены книг
  prices <- res %>% 
    html_nodes(xpath='//p[@class="price_color"]') %>% 
    html_text() %>% 
    str_replace('£', '') %>% 
    as.numeric()
  
  # достали информацию о том, находится ли книга в наличии, по каждой книге
  stocks <- res %>% 
    html_nodes(xpath='//p[@class="instock availability"]') %>% 
    html_text() %>%
    str_extract_all('\\w+ \\w+') %>% 
    unlist()
  
  # достали ссылки на каждую книгу
  hrefs <- res %>% html_nodes(xpath='//h3//a') %>% html_attr(name = 'href')
  
  # достали уникальный код книги (upc)
  # чтобы его получить, нужно перейти по ссылке конкретной книги на сайте
  # сделать так для каждой книги на сайте 
  
  upc <- c()
  
  for(i in 1:length(hrefs)){
    cur_href <- hrefs[i]
    cur_url <- paste0('http://books.toscrape.com/catalogue/', cur_href)
    info <- read_html(cur_url)
    upc <- append(upc, info %>% html_nodes(xpath='//tr[1]//td') %>% html_text())
    Sys.sleep(1)
  }
  
  # добавили все в табличку
  df <- tibble('title' = titles, 'price' = prices, 'stock' = stocks, 'upc' = upc)
  
  return(df)
}

# получили ссылки всех страниц
links <- paste0('http://books.toscrape.com/catalogue/page-',1:50,'.html')

# собрали по 1ой странице информацию
df <- make_df_from_parse(links[1])

# собрали по всем остальным
# код работает долго!
for(i in 2:length(links)){
  link <- links[i]
  df <- df %>% union(make_df_from_parse(link))
  Sys.sleep(1)
}

