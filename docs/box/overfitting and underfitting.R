library(ggplot2)
library(splines)

x <- seq(0, 1.3, 0.03)
y <- cos(1.5 * pi * x) + 0.4*rnorm(length(x))
df <- data.frame(x,y)

model1 <- lm(y~x, df)

ggplot(df, aes(x=x,y=y)) +
  geom_point() + 
  stat_smooth(method = 'lm', se = FALSE, col='blue') +
  stat_smooth(method = 'lm', formula = y ~ bs(x, 3),se = FALSE, col='red') +
  stat_smooth(method = 'lm', formula = y ~ bs(x, 15),se = FALSE, col='green') 


