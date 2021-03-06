# Airline Data
#https://www.bts.gov/topics/airlines-and-airports/airline-information-download						
#https://www.bts.gov/product/passenger-travel-facts-and-figures						
#https://www.iata.org/en/programs/covid-19-resources-guidelines/safely-restarting/						
#https://fortune.com/2021/04/21/covid-airline-industry-losses-iata/						


library(readxl)
library(tidyverse)
library(ggplot2)
#setwd('C:/Users/willi/Desktop/Beginner1 - Studio and Syntax/Airline Data')
mysheets <- read_excel("Air Traffic Tables.xlsx", sheet = 1, col_names = T)[1:7,]

sheet2 <- read_excel("Air Traffic Tables.xlsx", sheet = 2, col_names = T)[1:13,]
sheet2$month <- factor(sheet2$Month, levels = month.name)
ggplot(data = sheet2[1:12,], mapping = aes(x = month, y = `2020`))+
  geom_point()+
  scale_x_discrete(limits = month.name)+
  labs(y = "Year", x = "Month", title = "Scheduled Enplanements")+
  theme(axis.text.x = element_text(angle = 45, h = 1, size = 8))+
  theme(plot.title = element_text(h = .5, size = 18))

#?melt


####Part 2

library(readxl)

library(tidyverse)
library(ggplot2)
#setwd('C:/Users/willi/Desktop/Beginner1 - Studio and Syntax/Airline Data')

sheet2 <- read_excel("Air Traffic Tables.xlsx", sheet = 2, col_names = T)[1:13,]
sheet2$Month <- factor(sheet2$Month, levels = month.name)
ggplot(data = sheet2[1:12,], mapping = aes(x = Month, y = `2020`))+
  geom_point()+
  scale_x_discrete(limits = month.name)+
  labs(y = "Year", x = "Month", title = "Scheduled Enplanements")+
  theme(axis.text.x = element_text(angle = 45, h = 1, size = 8))+
  theme(plot.title = element_text(h = .5, size = 18))

####
library(readxl)

library(tidyverse)
library(ggplot2)
#setwd('C:/Users/willi/Desktop/Beginner1 - Studio and Syntax/Airline Data')

sheet2 <- read_excel("Air Traffic Tables.xlsx", sheet = 2, col_names = T)[1:13,]
sheet2$Month <- factor(sheet2$Month, levels = month.name)
# Plot the scheduledenplanements by year
ggplot(data = sheet2[1:12,], mapping = aes(x = Month, y = `2020`))+
  geom_point(color= "green")+
  geom_point(aes(x = Month, y = `2018`), color = "blue")+
  geom_point(aes(x=Month, y=`2019`), color = "Red")+
  scale_x_discrete(limits = month.name)+
  labs(y = "Year", x = "Month", title = "Scheduled Enplanements")+
  theme(axis.text.x = element_text(angle = 45, h = 1, size = 8))+
  theme(plot.title = element_text(h = .5, size = 18))

#Plot the % difference in enplanements

ggplot(data = sheet2[1:12,], mapping = aes (x = Month, y = `2019-2020 Pct. Change`))+
  geom_point(color = "green")+
  geom_point(aes(x=Month, y = `2018-2019 Pct. Change` ), color = "blue")+
  labs(y = "% Change", x = "Month", title = "% Change by Year")+
  theme(axis.text.x = element_text(angle = 45, h = 1, size = 8))+
  theme(plot.title = element_text(h=.5, size = 18))

sheet2$pct1 <- sheet2$`2018-2019 Pct. Change`
sheet2$pct2 <- sheet2$`2019-2020 Pct. Change`

sheet3 <- sheet2 %>%
  slice(1:12) %>%
  mutate(month_num = match(Month,month.name))

reg1 <- lm(month_num~pct1, data=sheet3)
summary(reg1)
with(sheet2, plot('2020'))
abline(reg1)
View(sheet2)


