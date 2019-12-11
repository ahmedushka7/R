install.packages("RSelenium") # установка пакета
library("RSelenium") # подгрузка пакета

vignette("basics", package = "RSelenium") # виньетка
# http://ropensci.github.io/RSelenium/articles/basics.html
# http://ropensci.github.io/RSelenium/articles/saucelabs.html браузеры

# https://ropensci.org/tutorials/rselenium_tutorial/ ГАЙД
# УСТАНОВИТЬ JAVA SE https://www.oracle.com/technetwork/java/javase/downloads/index.html
binman::list_versions("chromedriver")
driver <- rsDriver(browser=c("chrome"), chromever = '77.0.3865.40') # нужно зайти в хром и посмотреть свою версию
remote_driver <- driver[["client"]]
remote_driver$open() # открыть браузер
remote_driver$close() # закрыть браузер
remote_driver$navigate("http://www.google.com") # перейти на указанный адрес
remote_driver$getCurrentUrl() # получить текуший url
remote_driver$navigate("http://www.yandex.com")
remote_driver$goBack() # вернуться на предыдушую страницу
remote_driver$goForward() # перейти на страницу вперед
remote_driver$refresh() # обновить страницу
remote_driver$maxWindowSize() # браузер на весь экран
remote_driver$screenshot(display = FALSE)

remote_driver$navigate("http://www.google.com")
webElem <- remote_driver$findElement(using = 'xpath', "//*[@id='tsf']/div[2]/div[1]/div[1]/div/div[2]/input") # найти элемент по XPATH
webElem$sendKeysToElement(list("Миша")) # ввести текст
webElem$sendKeysToElement(list(key = 'enter'))



