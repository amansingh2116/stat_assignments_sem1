#Use R to toss 1000 times a biased coin with probability of head 0.7 and
#make a line chart of the running relative frequencies of heads againsts
#number of tosses. Repeat the entire experiment once more (i.e., toss the
#same coin 1000 more times) and overlay the new line on top of the old
#chart. Both the lines should be completely visible (not get out of the
#screen). Use different colours for the two lines.

ab1 = sample(c(0,1),1000,replace=T,prob=c(0.3,0.7))
z=1:1000
plot(z,cumsum(ab1)/z,type='l',col='red',ylim=c(0,1))
ab2 = sample(c(0,1),1000,replace=T,prob=c(0.3,0.7))
lines(z,cumsum(ab2)/z,col="blue")