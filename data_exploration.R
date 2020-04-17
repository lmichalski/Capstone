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
library(rpart)
library(GGally)
library(ISLR)
library(leaps)



delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
}

delete.value <- function(df, num_col=3, v=0) {
  df[!(df[num_col] == v),]
}

columns.summary <- function(df, col) {
  x <- df[,col]
  summary(x)
}


wd = "C:/Users/lmich/Desktop/Data"
setwd(wd)


who_data <- read.csv(file = 'final_data.csv')

set.seed(136)


#cleaning and normalizing data-set
data_features <- who_data[,c(8,4,5,10,12,13,14,18)]

data_features$sex <- as.numeric(data_features$sex)
data_features$age <- as.numeric(data_features$age)

data_features <- as.data.frame(lapply(delete.na(data_features), normalize))

sample_scaled <- data.frame(t(na.omit(t(data_features))))
sample_scaled$sex <- as.factor(sample_scaled$sex)
sample_scaled$age <- as.factor(sample_scaled$age)



k_test <- kmeans(sample_scaled, 4, iter.max = 10, nstart = 10, trace=FALSE)
ggline(sample_scaled, x = "sex", y = "log", add = "jitter", color = k_test$cluster)



fit <- regsubsets(log~.,sample_scaled,nvmax = 12, method = 'backward')
fit_summary <- summary(fit)
plot(fit_summary$rsq,type = 'l')
plot(fit_summary$rss,type = 'l')
plot(fit_summary$bic,type = 'l')


plot(fit, scale = 'r2')
plot(fit, scale = 'Cp')
plot(fit, scale = 'bic')
coef(fit,12)


train = sample(c(TRUE,FALSE), nrow(sample_scaled), rep=T)
test = (!train)


#examing data
summary(who_data[c(3:7)])

#histograms of log normalized suicide rates by age
par(mfrow = c(3,1))
hist(who_data$log[who_data$age == '05-14 years'], xlim = c(0,11), 
     main = "Age 05-14", xlab = '', breaks = 16)

hist(who_data$log[who_data$age == '15-24 years'], xlim = c(0,11), 
     main = "Age 15-24", xlab = '', breaks = 16)

hist(who_data$log[who_data$age == '25-34 years'], xlim = c(0,11), 
     main = "Age 25-34", xlab = '', breaks = 16)

hist(who_data$log[who_data$age == '35-54 years'], xlim = c(0,11), 
     main = "Age 35-54", xlab = '', breaks = 16)

hist(who_data$log[who_data$age == '55-74 years'], xlim = c(0,11),
     main = "Age 55-74", xlab = '', breaks = 16)

hist(who_data$log[who_data$age == '75+ years'], xlim = c(0,11),
     main = "Age 75+", xlab = '', breaks = 16)



#histogramof suicide rates by gender
gender <- aggregate(who_data$suicides_no, by=list(country=who_data$country,year=who_data$year, sex = who_data$sex), FUN=sum)

par(mfrow = c(2,1))
hist(log(gender$x[who_data$sex == 'male']), , main = "Male", xlab = '', breaks = 8)
hist(log(gender$x[who_data$sex == 'female']), , main = "Female", xlab = '', breaks = 8)
par(mfrow = c(1,1))

suicides.high <- subset(who_data, log > -2)


#summaries of suicides organized by age data
age_new <- who_data[!(who_data$log == 0),]
age_05 <- age_new$log[age_new$age == '05-14 years']
age_15 <- age_new$log[age_new$age == '15-24 years']
age_25 <- age_new$log[age_new$age == '25-34 years']
age_35 <- age_new$log[age_new$age == '35-54 years']
age_55 <- age_new$log[age_new$age == '55-74 years']
age_75 <- age_new$log[age_new$age == '75+ years']

par(mfrow = c(1,6))
boxplot(age_05, ylim = c(0,10))
boxplot(age_15, ylim = c(0,10))
boxplot(age_25, ylim = c(0,10))
boxplot(age_35, ylim = c(0,10))
boxplot(age_55, ylim = c(0,10))
boxplot(age_75, ylim = c(0,10))

par(mfrow = c(1,1))

hist(age_05)
curve(dnorm(x, mean = mean(age_05), sd = sd(age_05)), from = 0, to = 10, col = 'red')
curve(dnorm(x, mean = mean(age_15), sd = sd(age_15)), from = 0, to = 10, col = 'purple', add = TRUE)
curve(dnorm(x, mean = mean(age_25), sd = sd(age_25)), from = 0, to = 10, col = 'blue4', add = TRUE)
curve(dnorm(x, mean = mean(age_35), sd = sd(age_35)), from = 0, to = 10, col = 'blue1', add = TRUE)
curve(dnorm(x, mean = mean(age_55), sd = sd(age_55)), from = 0, to = 10, col = 'green4', add = TRUE)
curve(dnorm(x, mean = mean(age_75), sd = sd(age_75)), from = 0, to = 10, col = 'orange2', add = TRUE)

#combining all age ranges due to low variance
#suicides combined using raw #s then re-normalized 

a <- aggregate(who_data$suicides_no, by=list(country=who_data$country,year=who_data$year), FUN=sum)
b <- aggregate(who_data$population, by=list(country=who_data$country,year=who_data$year), FUN=sum)
names(a)[names(a) == "x"] <- "suicides"
names(b)[names(b) == "x"] <- "population"
new_table <-inner_join(a,b, by = c("country" = "country", "year" = "year"))

new_table$log <- log(new_table$suicides)
new_table <- inner_join(new_table, density, by = c("country" = "country", "year" = "year"))
new_table$pop_log <- log(new_table$population_density)

new_table$log[new_table$log == -Inf] <- 0

pairs(~log+pop_log+year,data=new_table)

#comparing potential independent variables to each other and the dependent

infra <- inner_join(hospital_beds, literacy, by = c("country" = "country", "year" = "year"))
infra <- inner_join(infra, internet, by = c("country" = "country", "year" = "year"))
infra <- inner_join(infra, cellphones, by = c("country" = "country", "year" = "year"))
infra <- inner_join(infra, new_table, by = c("country" = "country", "year" = "year"))

econ <- inner_join(gdp, migrate, by = c("country" = "country", "year" = "year"))
econ <- inner_join(econ, unemployment, by = c("country" = "country", "year" = "year"))
econ <- inner_join(econ, salary, by = c("country" = "country", "year" = "year"))
econ <- inner_join(econ, new_table, by = c("country" = "country", "year" = "year"))

summary(infra)
summary(econ)

pairs(~log+hb_per_thousand+literacy+internet_percent+cell_subs_thousands, data = infra)
pairs(~log+gdp_growth+migration_population+compensation_percent_cost+percent_unemployment, data = econ)

cor(infra$log, infra$hb_per_thousand)
cor(infra$log, infra$literacy)
cor(infra$log, infra$cell_subs_thousands)

cor(econ$log, econ$migration_population)


#creating final dataset

final <-inner_join(a,b, by = c("country" = "country", "year" = "year"))
final$log <- log(final$suicides)
final <- inner_join(final, hospital_beds, by =  c("country" = "country", "year" = "year"))
final <- inner_join(final, cellphones, by =  c("country" = "country", "year" = "year"))
final <- inner_join(final, salary, by =  c("country" = "country", "year" = "year"))

head(final)
summary(final)

pairs(~log+hb_per_thousand+cell_log+compensation_percent_cost, data = final)


#anova on scaled data

data_aov <- aov(log~.,data = sample_scaled)


ggline(blah, x = "sex", y = "log", 
       add = c("mean_se", "jitter"),
       ylab = "", xlab = "")


kruskal.test(log ~ age, data = blah)
kruskal.test(log ~ sex, data = blah)

pairwise.wilcox.test(who_data$log, who_data$age, p.adjust.method = "BH")



#knn

results <- kmeans(sample_scaled, 4)


#pca 

data_pca <- prcomp(sample_scaled, center = TRUE, scale. = TRUE, )




#random forest 

train = sample(c(TRUE,FALSE), nrow(sample_scaled), rep=T)
test = (!train)
rf_data = randomForest(log~., data = sample_scaled, subset = train)

oob.err = double(6)
test.err = double(6)

for(mtry in 1:6){
  fit = randomForest(log~., data = sample_scaled, subset = train, mtry=mtry, ntree = 350)
  oob.err[mtry] = fit$mse[350]
  pred = predict(fit, sample_scaled[-train,])
  test.err[mtry] = with(sample_scaled[-train,], mean( (log-pred)^2 ))
}

matplot(1:mtry, cbind(test.err, oob.err), pch = 23, col = c("red", "blue"), type = "b", ylab="Mean Squared Error")
legend("topright", legend = c("OOB", "Test"), pch = 23, col = c("red", "blue"))




n <- length(who_data$suicides_no)
temp <- sort(who_data$suicides_no, decreasing = TRUE)



dt <- aggregate(who_data$suicides_no,by=list(country=who_data$country,year=who_data$year), FUN=sum)
dt2 <- aggregate(who_data$population,by=list(country=who_data$country,year=who_data$year), FUN=sum)

dt<- left_join(dt,dt2, by=c('country' = 'country', 'year' = 'year'))



dt2_A <- aggregate(who_data$suicides_no, by=list(year = who_data$year), FUN=sum)
dt2_B <- aggregate(who_data$population, by=list(year = who_data$year), FUN=sum)

dt2 <- left_join(dt2_A, dt2_B)


age <- aggrcegate(who_data$suicides_no, by=list(country=who_data$country,year=who_data$year, gender = who_data$sex), FUN=sum)
a <- delete.na(a)


blah[,c(8,4,5)] %>%
  ggpairs()

blah[,c(8,10,12:19)] %>%
  ggpairs()



var.test(log[sex == 'female']~ log[sex == 'male'], who_data, alternative = "two.sided")

temp <- who_data[,c(8, 12:19)]
temp <- delete.na(temp)

res <- cor(temp)
round(res, 2)
