#A point, P, is chosen at random on the circumference of the unit circle
#centered at the origin. All points are equally likely. Let (X, 0) be the point
#where the tangent hits the x-axis. Take X = 0 if the P is at (0, −1) or
#(0, 1). Use simulation to form an idea about the distribution of X. Is the
#distribution normal? Answer this question by overlaying the best normal
#PDF on the histogram, and then visually ascertaining the fit

p = runif(1000,0,2*pi) #generating 1000 random values between 0 to 2*pi as angles formed by joining a point and the origin of the unit circle

#tangent at (a,b) will have equation ax+by=1, so for y=0 we have x as 1/a where (a,b)~(cos(p),sin(p)) so x-intercept of tangent at the point for a given angle is 1/cos(p)

t = numeric(1000) # generating a vector containing 1000 zeros for updating x-intercept values

# updating x intercept values for each angle according to condition given in the question
for (i in 1:1000){
    if ((p[i] == pi/2) | (p[i] == 3*pi/2)){
        t[i] = 0}
    else{t[i] = 1/cos(p[i])
    }} 

hist(t,1000,prob=T,xlim=c(-200,200),main='Histogram of X',xlab='X') # plotting the histogram of x-intercepts
library(MASS) # importing MASS library for using fitdistr
a = fitdistr(t,"normal") # using fitdistr to find mean and standard deviation of the data for normal distribution
b = a$estimate # extracting values of mean and sd
curve(dnorm(x,b[1],b[2]),add=T,col = 'red') # plotting the normal distribution curve for the founded mean and sd, trying to fit it in the distribution of x-intercepts


# we can see from the plot of histogram and the curve that the x-intercepts do not fit in normal distribution for the same mean and standard deviation.
# also our distribution of X have no value between -1 and 1 , because of the range of 1/cos(x) = sec(x) function, 
# and it can't take value zero also because of the computer limitation that it our P can't take value exactly pi/2 and 3*pi/2 for which it could have been zero