f <- function(x, action = 'Сумма'){
  if (action == 'Сумма'){
    summa <- 0
    for (i in 1:length(x)){
      summa <- summa + x[i]
    }
    return(summa)
  } else if (action == 'Произведение'){
    proizv <- 1
    for (i in 1:length(x)){
      proizv <- proizv * x[i]
    }
    return(proizv)
  }
}

x <- c(1,2,3,100)
f(x, action = 'Произведение')


my_sum <- function(x){
  summa <- 0
  for (i in 1:length(x)){
    summa <- summa + x[i]
  }
  return(summa)
}

my_proizv <- function(x){
  proizv <- 1
  for (i in 1:length(x)){
    proizv <- proizv * x[i]
  }
  return(proizv)
}

f_new <- function(vector, action='Сумма'){
  if (action == 'Сумма'){
    otvet <- my_sum(vector)
    return(otvet)
  } else if (action == 'Произведение'){
    otvet <- my_proizv(vector)
    return(otvet)
  }
}



# Рекурсия

fact <- function(n){
  if (n == 0){
    return (1)
  } else {
    return (n * fact(n-1))
  }
}

game <- function(h, m, p, z){
  current <- 0 # текущая ступенька, на которой находится герой
  
  while(h > 0 && current < m){
    
    kick <- sample(c('Ударил', 'Не ударил'), size = 1, prob = c(p, 1-p))
    print(kick)
    
    if (kick == 'Ударил'){
      if (current != 0){
        current <- current - 1
      }
      h <- h - z
    } else {
      current <- current + 1
    }
    print(paste('Здоровья осталось:', h))
    print(paste('Cтупенька:', current))
    print('-------------------')
    readline('Нажмите Enter:')
  }
}

game(100, 50, 0.25, 5) # хорошие параметры





for (i in 1:5){
  print(sample(c('Орел', 'Решка'), size = 1))
}






