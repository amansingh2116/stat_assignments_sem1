#Repeat the audio recording example done in class. Once in a silent room,How
#do the histograms differ?
library(tuneR)
s2 = readWave('C:\\Users\\91836\\OneDrive\\Documents\\Audacity\\realsilent.wav')
str(s2)
hist(s2@left,prob=T)