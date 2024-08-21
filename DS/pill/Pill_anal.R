# Read tabular data into R
library("readxl")
pill <- read_excel("DS\\pill\\Pillnumerical.xlsx")
head(pill)
k <- dim(pill)[1]

pills <- NULL
for (i in 1:k) {
  rowicol234 <- as.matrix(pill[i,-1])
  ni <- pill$Individuals[i]
  append <- rep(1,ni) %*% rowicol234
  pills <- rbind(pills,append)
}

head(pills)

dim(pills)

pills <- as.data.frame(pills)
# pills is a fictitious data set that would produce the summary given in the book

BPcode <- pills[,1]
Agecode <- pills[,2]
Categorycode <- pills[,3]

table(BPcode[Agecode==2],Categorycode[Agecode==2])

par(mfrow=c(2,1))
cond1 <- ((Agecode==2)&(Categorycode==1))&((BPcode>1)&(BPcode<16))
cond2 <- ((Agecode==2)&(Categorycode==2))&((BPcode>1)&(BPcode<16))
hist(BPcode[cond1], freq = F, xlab = "BP code", 
     main = "Histogram for Age 25-34, Non-users", xlim = c(2,15))
hist(BPcode[cond2], freq = F, xlab = "BP code", 
     main = "Histogram for Age 25-34, Users", xlim = c(2,15))

hist(80+5*BPcode[cond1], freq = F, xlab = "BP code", 
     main = "Histogram for Age 25-34, Non-users", xlim = c(90,160))
hist(80+5*BPcode[cond2], freq = F, xlab = "BP code", 
     main = "Histogram for Age 25-34, Users", xlim = c(90,160))

