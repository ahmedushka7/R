library(caret)
library(Metrics)
library(ggplot2)
library(dplyr)

data(mtcars)


# mape
mapeSummary <- function(data, lev = NULL, model = NULL){
  out <- mape(data$obs, data$pred)  
  names(out) <- "MAPE"
  out
}

mControl <- trainControl(summaryFunction = mapeSummary)

lm_model <- train(mpg ~ wt,
                  data = mtcars, 
                  method = "lm",
                  metric = "MAPE",
                  maximize = 'MAPE',
                  trControl = mControl)

coef <- lm_model$finalModel$coefficients %>% unname()

ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  geom_abline(intercept = coef[1], slope = coef[2], color='red')
coef

#mse
lm_model <- train(mpg ~ wt,
                  data = mtcars, 
                  method = "lm",
                  metric = "MAE",
                  maximize = FALSE)

coef <- lm_model$finalModel$coefficients %>% unname()
  
ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  geom_abline(intercept = coef[1], slope = coef[2], color='red')

q <- lm(mpg ~ wt,
        data = mtcars)
summary(q)

