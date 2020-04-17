library(dplyr)
library(plyr)
library(caret)
library(moments)
library(class)
library(ellipse)
library(ggpubr)
library(ggplot2)
library(cowplot)
library(randomForest)


wd = "C:/Users/lmich/Desktop/data"
setwd(wd)


who_data <- read.csv(file = 'whoData.csv')

who_data$country <- as.character(who_data$country)

who_data$age <- revalue(who_data$age, c("5-14 years"="05-14 years"))
who_data$age <- relevel(who_data$age, "05-14 years")

who_data$log <- log((who_data$suicides_no/who_data$population)*100000)
who_data$log[who_data$log == -Inf] <- NA


cellphones <- read.csv(file = 'cellphones_new.csv')
density <- read.csv(file = 'density_new.csv')
gdp <- read.csv(file = 'gdp_new.csv')
hospital_beds <- read.csv(file = 'hb_new.csv')
internet <- read.csv(file = 'internet_new.csv')
literacy <- read.csv(file = 'literacy_new.csv')
migrate <- read.csv(file = "migration.csv")
salary <- read.csv(file = 'salary.csv')
unemployment <- read.csv(file = 'unemployment_new.csv')



a <- aggregate(who_data$suicides_no, by=list(country=who_data$country,year=who_data$year), FUN=sum)
b <- aggregate(who_data$population, by=list(country=who_data$country,year=who_data$year), FUN=sum)
names(a)[names(a) == "x"] <- "suicides"
names(b)[names(b) == "x"] <- "population"
new_table <-inner_join(a,b, by = c("country" = "country", "year" = "year"))
new_table$log <- log((new_table$suicides/new_table$population)*100000)
new_table$log[new_table$log == -Inf] <- NA




final <-left_join(who_data,density, by = c("country" = "country", "year" = "year"))
final$population_log <- log(final$population_density)
final$population_log[final$population_log == -Inf] <- NA

final <-left_join(final,cellphones, by = c("country" = "country", "year" = "year"))
final$cell_log <- log(final$cell_subs_thousands)
final$cell_log[final$cell_log == -Inf] <- NA

final <-left_join(final,gdp, by = c("country" = "country", "year" = "year"))
final <-left_join(final,hospital_beds, by = c("country" = "country", "year" = "year"))
final <-left_join(final,internet, by = c("country" = "country", "year" = "year"))
final <-left_join(final,literacy, by = c("country" = "country", "year" = "year"))
final <-left_join(final,migrate, by = c("country" = "country", "year" = "year"))
final <-left_join(final,salary, by = c("country" = "country", "year" = "year"))
final <-left_join(final,unemployment, by = c("country" = "country", "year" = "year"))

final$suicides[final$suicides == 0] <- NA


write.csv(final, "final_data.csv")

