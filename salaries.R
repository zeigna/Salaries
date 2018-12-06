#I forgot to note where I got the salaries.csv file from
#see if I can refind the website

#read in the .csv file into R
salaries <- read.csv("salaries.csv")

#display the data and examine it
salaries

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

#single variable statisics
mean(salaries$salary)
var(salaries$salary)
median(salaries$salary)
mode(salaries$salary)
sd(salaries$salary)







