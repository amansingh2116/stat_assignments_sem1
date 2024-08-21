#Repeat the audio recording example done in class. while you are shouting in front of the microphone. How
#do the histograms differ?
library(tuneR)
s1 = readWave('C:\\Users\\91836\\OneDrive\\Documents\\Audacity\\noisy.wav')
str(s1)
hist(s1@left,prob=T)
