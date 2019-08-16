###################################################################################################
## PROGRAM: SoDA_Examples.r   
## PURPOSE: show the examples from Software for Data Analysis: Programming with R
## AUTHOR:  Dave Jansz
## DATA:    Mars.csv data obtained from 
##          http://www.pafko.com/tycho/observe.html
###################################################################################################
# set global options and provide the location of the working directory
getOption("digits")
1.2345
options(digits=3)
1.2345
setwd("C:/cygwin64/lib/R/data")
mars<-read.csv("mars.csv",skip=5,as.is=TRUE)
# Load the mars data into separate vectors for Date and Declination
Declination=mars[["Declination"]]
Date=mars$Date
plot(Date,Declination)
for(what in c("p","l","b")) plot(Date, Declination, type = what)
# page 13 about the quantile function
quantile(Declination)
# On page 20 abot the ? operator to display documentation on a topic
?jitter
# add some random noise to a vector with the jitter function
x1<-c(0,1,0,-1)
jitter(x1)
#name the arguments to the function
formalArgs(jitter)
# from page 176 about using the simplified list apply (sapply) function
dim(mars)
#for loop to mimic the sapply function
for(j in names(mars)) print(class(mars[,j]))
#apply the function class to all variables of mars and print the output
sapply(mars,class)
#apply the function max to all variables of mars and print the output
sapply(mars,max)