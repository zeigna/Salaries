#I forgot to note where I got the salaries.csv file from
#see if I can refind the website


library(Rcmdr)
library(ggm)
library(QuantPsyc)
library(Hmisc)
library(ggplot2)
library(polycor)
library(tidyverse)
library(lattice)
library(qcc)
library(dplyr)
library(e1071)
library(moments)
library(pastecs)




#read in the .csv file into R
salaries <- read.csv("salaries.csv")

#display the data and examine it
salaries

#number of rows and number of columns in the datset, respectively
nrow(salaries)
ncol(salaries)

#a listing of the names of the columns
colnum(salaries)

#information about the data structure for 
#quick look at data and how it is organized
str(salaries)

#summary of the variables in the dataset
#(including descriptive stats for numerical variables)
summary(salaries)

#mean and standard deviation for the numerical columns
mean(salaries$salary)
mean(salaries$yrs.service)
mean(salaries$yrs.since.phd)

sd(salaries$salary)
sd(salaries$yrs.service)
sd(salaries$yrs.since.phd)

#using gglot to make scatterplots
library(tidyverse)
sc <- ggplot(salaries)
sc <- ggplot(salaries, aes(yrs.service, salary))
sc + geom_point()


#facet allow you to break down the overall graph 
#into separate graphs based on a discrete/categorical variable
#like gender

sc <- ggplot(salaries)
sc <- ggplot(salaries, aes(yrs.service, salary))
sc + geom_point()
sc + geom_point() + facet_grid(sex~.)


sc <- ggplot(salaries, aes(yrs.since.phd, salary))
sc + geom_point()

 
sc <- ggplot(salaries, aes(yrs.since.phd, salary))
sc + geom_point()
sc + geom_point() + facet_grid(sex~.)




sc <- ggplot(salaries, aes(yrs.since.phd, yrs.service))
sc + geom_point()
 
sc <- ggplot(salaries, aes(yrs.since.phd,yrs.service))
sc + geom_point()
sc + geom_point() + facet_grid(sex~.)


#histograms (and its layers) using ggplot2
hist <- ggplot(salaries, aes(x = salary))
hist + geom_histogram()

hist <- ggplot(salaries, aes(x = salary))
hist + geom_histogram(binwidth = 25000, color = "darkslategray", fill = "darkslategray4") + ggtitle("Salary Distribution of Professors") + labs(y = "Number of Professors", x = "Salary") + theme_minimal()





#make a two-way table with row and column sums
T <- table(salaries$rank, salaries$sex)
addmargins(T, c(1, 2))


spineplot(T, )

barplot(T, col = c('pink', 'blue', 'green'), beside = TRUE, legend.text = TRUE)
str(salaries)
hist(salaries$salary)
hist(salaries$salary, main = "Overall Professor Salary", xlab = "Salary")
hist(salaries$yrs.since.phd, main = "Overall Years Since Ph.D.", xlab = "years since Ph.D.")
hist(salaries$yrs.service, main = "Overall of Service", xlab = "years of service")

mean(salaries$salary)
mean(salaries$yrs.service)
mean(salaries$yrs.since.phd)

sd(salaries$salary)
sd(salaries$yrs.service)
sd(salaries$yrs.since.phd)

skewness(salaries$salary)
4*sqrt(6/length(salaries$salary))

skewness(salaries$yrs.service)
4*sqrt(6/length(salaries$yrs.service))

skewness(salaries$yrs.since.phd)
4*sqrt(6/length(salaries$yrs.since.phd))


# Simple Bar Plot
counts <- table(salaries$rank)
barplot(counts, main="Academic Rank",
        xlab="Rank") 

counts <- table(salaries$discipline)
barplot(counts, main="Theoretical vs Applied",
        xlab="Discipline")

counts <- table(salaries$sex)
barplot(counts, main="Gender",
        xlab="gender")

#single variable descriptive statisics
mean(salaries$salary)
var(salaries$salary)
median(salaries$salary)
mode(salaries$salary)
sd(salaries$salary)

#two variable descriptive statistics



#correlation

#put the two "years" variables into a data frame
yrs.data <- data.frame(salaries$yrs.service, salaries$yrs.since.phd)
cor(yrs.data, use = "pairwise.complete.obs", method = "pearson")

#simple regression
salariessincelm<- lm(salary ~ yrs.since.phd, data = salaries)
salariesincelm
summary(salariessincelm)

salariesservicelm<- lm(salary ~ yrs.service, data = salaries)
salarieservicelm
summary(salariesservicelm)


#multiple regression
salarieslm<- lm(salary ~ yrs.since.phd + yrs.service, data = salaries)
salarieslm
summary(salarieslm)

#making the scatterplot


#summary statsitics
salaries <- read.csv("salaries.csv")
summary(salaries)

#number of columns and number of rows
ncol(salaries)
nrow(salaries)

#names of the columns (does not really make sense to get a list of the names of the rows)
colnames(salaries)
#rownames(salaries)


aggregate(salaries[c(salaries$salary,salaries$yrs.since.phd, salaries$yrs.service)],by=list(gender=salaries$sex), mean, na.rm=TRUE)
aggregate(salaries[c("Salary","Years Since Ph.D", "Years of Service")],mydata["Gender"], mean, na.rm=TRUE)
aggregate(salaries,by=list(gender=salaries$sex), mean, na.rm=TRUE)
aggregate(salaries,by=list(gender=salaries$sex, phd=salaries$yrs.since.phd, service=salaries$yrs.service), mean, 
          na.rm=TRUE)
aggregate(salaries$salary,by=list(gender=salaries$sex, phd=salaries$yrs.since.phd, service=salaries$yrs.service), mean, 
          na.rm=TRUE)
aggregate(salaries[c("Salary")],by=list(gender=salaries$sex, phd=salaries$yrs.since.phd, service=salaries$yrs.service), 
          mean, na.rm=TRUE)








