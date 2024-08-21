#Use the raw data for probit analysis provided in the class webpage, and
#follow the steps of Finney to produce a linear scatterplot.

library(MASS) # installing MASS library for using functions like qnorm
a = read.table('C:\\Users\\91836\\OneDrive\\Desktop\\Notes\\Stat (AC)\\codes in R\\probit.txt') # reading probit model file
x = as.numeric(a$V3) #extracting the data of daeds and removing non numeric values with NA
y = as.numeric(a$V2) #extracting the size of data and removing non numeric values with NA
x = x[!is.na(x)] #removing NA values
y = y[!is.na(y)] #  removing NA values
plot(qnorm(x/y),pch=20, xlab = "d(i)'s", ylab = 'qnorm(p)') # plotting probit function of dead/size for each doses using qnorm(x) function

#    V1   V2   V3
#1 dose size dead
#2   12  100    0
#3   13  100    4
#4   14  100    8
#5   15  100   50
#6   16  100   82
#7   17  100   99
#8   18  100  100