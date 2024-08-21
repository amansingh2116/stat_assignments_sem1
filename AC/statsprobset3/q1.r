#We have a random sample of size 101 from a N(µ, 1) population, where µ ∈ R is unknown. 
#We want to estimate µ. There are two contending estimators: the sample mean and the sample median. 
#We want to approximate the standard errors of these estimators. Do this using simulation for µ = 10 and µ = 20.

#for mean = 10
y = numeric(1000) # generating a vector containing 1000 zeros for updating mean
z = numeric(1000) # generating a vector containing 1000 zeros for updating median
for (i in 1:1000){x = rnorm(101,10,1) #generating 101 normal distribution random numbers
y[i]=mean(x) # updating mean of 101 random deviates in y for the current iteration
z[i]=median(x) # updating median of 101 random deviates in y for the current iteration
}
seme1 = sd(y) #average standard error of mean
semd1 = sd(z) #average standard error of median


#for mean = 20
y = numeric(1000) # generating a vector containing 1000 zeros for updating mean
z = numeric(1000)# generating a vector containing 1000 zeros for updating median
for (i in 1:1000){x=rnorm(101,20,1) #generating 101 normal distribution random numbers
y[i]=mean(x) # updating mean of 101 random deviates in y for the current iteration
z[i]=median(x) # updating median of 101 random deviates in y for the current iteration
}
seme2 = sd(y) #average standard error of mean
semd2 = sd(z) #average standard error of median

# when we compare the standard errors of mean and median for mean = 10 and 20
# we found that usually the error increase as we increase the mean, also standard error of mean is less than that of median.