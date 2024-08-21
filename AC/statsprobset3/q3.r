#The same set up as above. Find (using simulation) two numbers L and
#U such that X lies between them with 90% probability. The smaller is
#U −L, the happier I would be. Also find (using simulation) the probability
#that X exceeds 5.

k = numeric(1000) #generating 1000 random values for U
z = numeric(1000) #generating 1000 random values for L
for (j in 1:1000){
p = runif(1000,0,2*pi) #generating 1000 random values between 0 to 2*pi as angles formed by joining a point and the origin of the unit circle

#tangent at (a,b) will have equation ax+by=1, so for y=0 we have x as 1/a where (a,b)~(cos(p),sin(p)) so x-intercept of tangent at the point for a given angle is 1/cos(p)

t = numeric(1000) # generating a vector containing 1000 zeros for updating x-intercept values
# updating x intercept values for each angle according to condition given in the question
for (i in 1:1000){
    if ((i==pi/2) | (i==3*pi/2)){
        t[i]=0}
    else{t[i]=1/cos(p[i])
    }}

m = numeric(100) # generating a vector containing 100 zeros for U-L values
for (i in 1:100){q1 = quantile(t,(900+i)/1000) # will give a number such that 900+i% of x-intercepts are below it
q2 = quantile(t,i/1000) # will give a number such that i% of x-intercepts are below it
m[i]=abs(q2-q1)} # updating U-L
c = which.min(m) # finding index of minimum U-L
k[j] = quantile(t,(900+c)/1000) # will give the min U such that 900+c % of x-intercepts are below it
z[j] = quantile(t,c/1000) # will give the min L such that c % of x-intercepts are below it
}

qa = mean(k) # mean of U
qb = mean(z) # mean of L
minc = qa-qb # minimum of U-L

temp=numeric(1000) # generating a vector containing 1000 zeroes for storing probability that x-intercept exceeds 5 at a given iteration
for (i in 1:1000){r= runif(1000,0,2*pi)
l = 1/cos(r)
temp[i]=(sum(l>5))/1000 #probability that x-intercept exceeds 5 in this iteration
}
prob5 = mean(temp) # average probability that x-intercept exceeds 5


#thus we found the U(that is qa) and L(that is qb) such that X lies between them with 90% probability and U-L(that is minc) being minimum , and it vary everytime we take different observations
#also we found the probability that X exceeds 5 (prob5) which is approximately 0.06%