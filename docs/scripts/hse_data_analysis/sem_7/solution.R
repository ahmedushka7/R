library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

###
bad <- read_csv('bundesliga_bad.csv', col_names = F, skip = 1,
                col_types = cols(X4 = col_number()),
                na = c('', 'NA', 'UNKNOWN'))
###
good <- read_csv('bundesliga.csv')
glimpse(good)
###
good <- good[, -1]
glimpse(good)
###
min(good$Date, na.rm = TRUE)
max(good$Date, na.rm = TRUE)
range(good$Date, na.rm = TRUE)
###
length(unique(c(good$HomeTeam, good$AwayTeam)))
###
h <- table(good$HomeTeam)
a <- table(good$AwayTeam)
mean(h == a)
sort(table(c(good$HomeTeam, good$AwayTeam)),decreasing = T)[1]
###
mean(good$HomeGoals)
mean(good$AwayGoals)
good$TotalGoals <- good$HomeGoals + good$AwayGoals
mean(good$TotalGoals)
### 
good$results <- ifelse(good$HomeGoals > good$AwayGoals,
                       "Победа хозяев",
                       ifelse(good$HomeGoals < good$AwayGoals, "Победа гостей", "Ничья"))
table(good$results)
###
x <- sample(1:nrow(good), 10)
good[x,]
###
ggplot(good, aes(x = HomeGoals)) +
  geom_bar()
ggplot(good, aes(x = AwayGoals)) +
  geom_bar() +
  xlim()

good %>% 
  gather('Who', 'Goals',  c('HomeGoals', 'AwayGoals')) %>%
  ggplot(aes(x = factor(Goals), fill = Who)) +
    geom_bar(position=position_dodge()) + 
    xlab('Goals') +
    ylab('Count')

### 
good$Win <- ifelse(good$HomeGoals > good$AwayGoals, 
                   good$HomeTeam, 
                   ifelse(good$HomeGoals < good$AwayGoals, good$AwayTeam, 'Ничья'))
x <- table(good$Win)
x <- x[names(x)!='Ничья']
sort(x, decreasing = T)[1]
###
good2008 <- good[good$Year==2008, ]
table(c(good2008$HomeTeam, good2008$AwayTeam))

name <- names(table(c(good2008$HomeTeam, good2008$AwayTeam)))
x <-  rep(0, length(name))
names(x) <- name
for(i in 1:nrow(good2008)){
  if (good2008$HomeGoals[i] > good2008$AwayGoals[i]) {
    x[good2008$HomeTeam[i]] <- x[good2008$HomeTeam[i]] + 3
  }else if (good2008$HomeGoals[i] < good2008$AwayGoals[i]) {
    x[good2008$AwayTeam[i]] <- x[good2008$AwayTeam[i]] + 3
  }else{
    x[good2008$HomeTeam[i]] <- x[good2008$HomeTeam[i]] + 1
    x[good2008$AwayTeam[i]] <- x[good2008$AwayTeam[i]] + 1
  }
}
print(sort(x, decreasing = T))
  
}