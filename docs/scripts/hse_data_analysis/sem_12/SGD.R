# install.packages('plotly')
library(plotly)
library(dplyr)
library(tibble)
library(ggplot2)
library(tidyr)


n <- 50
data <- tibble(x = runif(n, 0, 10),
               y = 10 + 5 * x + rnorm(n,0, 3))

w0 <- 32
w1 <- 8

ggplot(data, aes(x, y)) +
  geom_point() + 
  geom_abline(intercept = w0, slope = w1, color = 'blue')

loss <- function(df, w0, w1){
  # функция потерь
  l <- 0
  x <- (df$y - (w0 + w1 * df$x))^2
  return(sum(x))
}
# видим, что потери большие, давайте их уменьшать меняя наши коэффициенты
loss(data, w0, w1)

der_w0 <- function(df, w0, w1){
  x <- -2 * (df$y - (w0 + w1 * df$x))
  return(sum(x))
}
der_w0(data, w0, w1)

der_w1 <- function(df, w0, w1){
  x <- -2 * (df$y - (w0 + w1 * df$x)) * df$x
  return(sum(x))
}
der_w1(data, w0, w1)

SGD <- function(df, w0, w1, al = 0.001) {
  x <- c(w0, w1)
  der <- c(der_w0(df, w0, w1), al*der_w1(df, w0, w1))
  x <- x - al*der
  return(x)
}
SGD(data, w0, w1)

stage <- tibble(w0 = numeric(), w1 = numeric(), loss = numeric())

for(i in 1:100){
  new_w <- SGD(data, w0, w1, al = 0.01)
  w0 <- new_w[1]
  w1 <- new_w[2]
  stage <- add_row(stage, w0 = w0, w1 = w1, loss = loss(data, w0, w1))
  # p <- ggplot(data, aes(x, y)) +
  #         geom_point() + 
  #         geom_abline(intercept = w0, slope = w1, color = 'blue') +
  #         ggtitle(paste('Значение потерь:',loss(data, w0, w1)))
  # print(p)
  # readline('Нажмите Enter, чтобы сделать шаг градиентного спуска:')
}

x = seq(-10,40, length.out = 1000)
y = seq(-2,10, length.out = 1000)
z <- matrix(data = 0, nrow = 1000, ncol = 1000)
for(i in 1:nrow(z)){
  for(j in 1:ncol(z)){
    z[i,j] <- loss(data,x[i],y[j])
  }
}

pp <- plot_ly(z = z, x = x, y = y, type = "surface") %>%
        add_markers(x = stage$w0, y = stage$w1, z = stage$loss, size = 0.3) %>%
        add_lines(x = stage$w0, y = stage$w1, z = stage$loss, size = 0.3)
      
pp

