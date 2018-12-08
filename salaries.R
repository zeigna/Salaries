#I forgot to note where I got the salaries.csv file from
#see if I can refind the website

# working google sites link:  https://sites.google.com/s/1h_Tb6dwLKJR_jY4kApzYVmqW3-lj-N51/p/1OPJZQLSQ3X3xkZnGJCfqg4gOocnp8gej/edit

# Google sites published at: https://sites.google.com/view/data-analysis-with-r/home

# Github username:  zeigna

#Github repo:  https://github.com/zeigna/Salaries




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

hist <- ggplot(salaries, aes(x = yrs.service))
hist + geom_histogram(binwidth = 10, color = "darkslategray", fill = "darkslategray4") + ggtitle("Years of Service") + labs(y = "Number of Professors", x = "Years of Service") + theme_minimal()


hist <- ggplot(salaries, aes(x = yrs.since.phd))
hist + geom_histogram(binwidth = 10, color = "darkslategray", fill = "darkslategray4") + ggtitle("Years since Professor Earned Ph.D.") + labs(y = "Number of Professors", x = "Years Since Ph.D") + theme_minimal()

#make bar chart with ggplot
bar <- ggplot(salaries, aes(x = sex, fill = rank))
bar + geom_bar() + theme_light() + labs(y = "Number of Professors", title = "Academic Rank")

bar <- ggplot(salaries, aes(x = rank, fill = sex))
bar + geom_bar() + theme_light() + labs(y = "Number of Professors", title = "Academic Rank")

bar <- ggplot(salaries, aes(x = yrs.since.phd, fill = sex))
bar + geom_bar() + theme_light() + labs(y = "Number of Professors", title = "AYears Since Ph.D.")

bar <- ggplot(salaries, aes(x = yrs.service, fill = sex))
bar + geom_bar() + theme_light() + labs(y = "Number of Professors", title = "Years of Service")

bar <- ggplot(salaries, aes(x = sex, fill = discipline))
bar + geom_bar() + theme_light() + labs(y = "Number of Professors", x = "Gender", title = "Gender breakdown by Discpline and Rank") + facet_wrap(~ rank)

bar <- ggplot(salaries, aes(x = discipline, fill = sex))
bar + geom_bar() + theme_light() + labs(y = "Number of Professors", title = "Years of Service")

#making boxplots with ggplot

box <- ggplot(salaries, aes(x = sex, y = salary))
box + geom_boxplot() + labs(title = "Salary by Gender") + theme_light()

box <- ggplot(salaries, aes(x = rank, y = salary))
box + geom_boxplot(outlier.color = "red", outlier.shape = 4) + labs(title = "Salary by Academic Rank") + theme_light() + geom_jitter(width = 0.2, aes (color = sex) )


#making scatterplots with ggplot

salsc <- ggplot(salaries, aes(yrs.since.phd, salary))
salsc + geom_point(aes(color = sex)) + theme_light() + labs(x = "Years Since Ph.D", y = "Professor's Salary", title = "Relationship between Years Since Ph.D. and Salary") + stat_smooth()


salsc <- ggplot(salaries, aes(yrs.service, salary))
salsc + geom_point(aes(color = sex)) + theme_light() + labs(x = "Years of Service", y = "Professor's Salary", title = "Relationship between Years of Service. and Salary") + stat_smooth()


salsc <- ggplot(salaries, aes(yrs.since.phd, yrs.service))
salsc + geom_point(aes(color = sex)) + theme_light() + labs(x = "Years Since Ph.D", y = "Years of Service", title = "Relationship between Years Since Ph.D. and Years of Service") + stat_smooth(se = FALSE)

#for fun and giggles :-)

salsc <- ggplot(salaries, aes(yrs.since.phd, yrs.service))
salsc + geom_point(aes(color = sex), shape = 21, fill = "white", size = 3, stroke = 2) + theme_light() + labs(x = "Years Since Ph.D", y = "Years of Service", title = "Relationship between Years Since Ph.D. and Years of Service")


#correlations 

cor(salaries$yrs.service, salaries$salary)
(cor(salaries$yrs.service, salaries$salary))^2
cor.test(salaries$yrs.service, salaries$salary)

cor(salaries$yrs.since.phd, salaries$salary)
(cor(salaries$yrs.since.phd, salaries$salary))^2
cor.test(salaries$yrs.since.phd, salaries$salary)

cor(salaries$yrs.since.phd, salaries$yrs.service)
(cor(salaries$yrs.since.phd, salaries$yrs.service))^2
cor.test(salaries$yrs.since.phd, salaries$yrs.service)


#select (subset) only males so I can do a t-test
#on male full professors in applied disciplines compared to
#male full professors in theoretical disciplines

sex <- salaries$sex
discip <- salaries$discipline
salary <- salaries$salary
rank <- salaries$rank
newdata <- data.frame(sex, discip, rank, salary)
#newdata includes BOTH genders and all three ranks currently
males.data1 <- subset(newdata, sex == "Male")
males.data <- subset(males.data1, rank != "AssocProf"|rank != "AsstProf")

#having difficulty filtering out AsstProf and AssocProf to get only 
#Prof data so I hit this site:
# https://blog.exploratory.io/filter-with-text-data-952df792c2ba

library(stringr)

males.data <- males.data1[!grepl("Ass", males.data1$rank),]
#finally!! only took over an hour :-) 
# https://stackoverflow.com/questions/22249702/delete-rows-containing-specific-strings-in-r

#independent measures t-test
ind.t.test <- t.test(salary~discip, data = males.data, mu = 0)
ind.t.test


#correlation

#put the two "years" variables into a data frame
yrs.data <- data.frame(salaries$yrs.service, salaries$yrs.since.phd)
cor(yrs.data, use = "pairwise.complete.obs", method = "pearson")

yrs.data <- data.frame(salaries$yrs.service, salaries$yrs.since.phd, salaries$salary)
cor(yrs.data, use = "pairwise.complete.obs", method = "pearson")
#it makes a matrix of all three variables -- YAY!!!!


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

#I did not use most of these but need to check them out more :-)




#make a two-way table with row and column sums
T <- table(salaries$rank, salaries$sex)
addmargins(T, c(1, 2))

T <- table(salaries$discipline, salaries$sex)
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





#making the scatterplot not using ggplot


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








