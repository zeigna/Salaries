#I forgot to note where I got the salaries.csv file from
#see if I can refind the website

#read in the .csv file into R
salaries <- read.csv("salaries.csv")

#display the data and examine it
salaries

#make a two-way table with row and column sums
T <- table(salaries$rank, salaries$sex)
addmargins(T, c(1, 2))


spineplot(T, col = c('pink', 'blue'))

barplot(T, col = c('pink', 'blue', 'green'), beside = TRUE, legend.text = TRUE)

