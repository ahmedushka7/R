library(tibble)
library(ggplot2)



our_function <- function(x){
  y <- 10 * cos(x) + 0.5*x^2 -5*x
  return(y)
}

df <- tibble(x = seq(-10,10, by = 0.01),
             y = our_function(x))

der_our_function <- function(x){
  y <- -10*sin(x) + x - 5
  return(y)
}

x <- -10
al <- 0.3
for(i in 1:100){
  p <- ggplot(df, aes(x, y)) +
          geom_line() +
          geom_point(x = x , y = our_function(x), color='red')
  print(p)
  x <- x - al * der_our_function(x)
  readline('Enter:')
}

library(datasauRus)
library(ggplot2)
library(gganimate)

ggplot(datasaurus_dozen, aes(x=x, y=y))+
  geom_point()+
  theme_minimal() +
  transition_states(dataset, 3, 10) + 
  ease_aes('cubic-in-out')

datasaurus_dozen

library(plotly)
library(gapminder)
p <- gapminder %>%
  plot_ly(
    x = ~gdpPercap, 
    y = ~lifeExp, 
    size = ~pop, 
    color = ~continent, 
    frame = ~year, 
    text = ~country, 
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )
  )
p
gapminder
