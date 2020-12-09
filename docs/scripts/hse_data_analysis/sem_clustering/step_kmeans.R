library(ggplot2)
library(readr)
library(dplyr)
library(ramify)
library(tweenr)
library(animation)

df <- read_csv("simple_sample.csv")
ggplot(df, aes(x1, x2)) + 
  geom_point()

step <- function(df, centers){
  df_matrix <- df %>% 
    select(x1, x2) %>%
    as.matrix()
  a2 <- as.matrix(rowSums(df_matrix**2))
  a2 <- cbind(a2[, 1], a2[, 1])
  b2 <- rowSums(centers**2)
  b2 <- matrix(b2, nrow=100, ncol=2, byrow = T)
  ab <- df_matrix %*% t(centers)
  
  dist <- a2 - 2 * ab + b2
  
  mins <- argmin(dist)
  new_centers <- df %>%
    mutate(mins = mins) %>%
    group_by(mins) %>%
    summarise(x1 = mean(x1),
              x2 = mean(x2)) %>%
    arrange(mins) %>%
    select(-mins) %>%
    as.matrix()
  return(list(df %>%
             mutate(cluster = factor(mins)), new_centers))
}
k <- 2
c1 <- c(-5.5, -2)
c2 <- c(-2.5, 2)
centers <- matrix(c(c1, c2), nrow = k, byrow = T)
colnames(centers) <- c("x1", "x2")

for(i in 1:5){
    res <- step(df, centers)
    df <- res[[1]]
    old_centers <- centers
    centers <- res[[2]]
    p <- ggplot(df, aes(x1, x2, color=cluster)) + 
      geom_point() +
      geom_point(data = as.data.frame(old_centers), aes(x1, x2), color="blue", size=2)
    
    print(p)
    x <- readline("enter: ")
    if(x == "stop"){
      break
    }
}
