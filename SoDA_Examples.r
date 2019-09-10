###################################################################################################
## TITLE:   SoDA Examples
## AUTHOR:  Dave J
## PURPOSE: To consolidate the examples from the book
##          Software for Data Analysis: Programming with R
## NOTES:   Install the following packages before running this code:
##          install.packages(c("gam","HSAUR","SoDA"));
###################################################################################################
library(SoDA)
library(HSAUR)
library(gam)
#data from pafko.com/tycho about the declination of Mars (the angle the planet makes with the equator)
setwd("C:/Users/SJF/OneDrive - Economical Insurance/Documents/Data")
getwd()
mars<-read.csv("mars.csv",skip=5,as.is=TRUE)
#help(package = "gam")
data(clouds)
data(kyphosis)
class(kyphosis)
typeof(kyphosis)
names(kyphosis)
kyphosis[1:3,] # select the top three rows
#  Kyphosis Age Number Start
#1   absent  71      3     5
#2   absent 158      3    14
#3  present 128      4     5
n<-length(kyphosis$Age) #row count
# 81
kyphosis[1,]$Kyphosis # Unique values
#[1] absent
#Levels:
#[1] "absent"  "present"

packageDescription("gam") # Short description
#library(help=gam)
ls("package:gam") # List functions
lsf.str("package:gam") # List functions with parameters
list.files(system.file(package="gam")) # List system files for package
#demo(package = .packages(all.available = TRUE)) # List all available demos
#demo(package = "SoDA") # List demo for the SoDA package
demo(searchList) # Run Demo
#RSiteSearch("gam") # Search R site for string
#data(package="gam") # Get list of available datasets for package


# Data from an experiment investigating the use of massive amounts of silver iodide (100 to 1000 grams per cloud) in cloud seeding to increase rainfall. 
Declination<-mars$Declination
Date<-mars$Date
Date2<-mars[["Date"]]
Age<-kyphosis$Age
Number<-kyphosis$Number
plot(Date,Declination)
dev.off(2) #close the graphics device
quantile(Declination)
#?quantile
#fit a GAM:
fit1<-gam(Kyphosis~lo(Age)+lo(Number)+lo(Start), family=binomial, data=kyphosis)
summary(fit1)
# get the option for hte number of digits
getOption("digits")
options(digits=8)
formalArgs(jitter)
for(j in names(mars)) print(class(mars[,j]))
sapply(mars,class)
#pause between plots
pause <- function(sec) for (i in 1:sec)
{ cat(format(Sys.time(), "%X"),"\n")
  date_time<-Sys.time()
  while((as.numeric(Sys.time()) - as.numeric(date_time))<1){} 
}
for(what in c("p","l","b")) 
{plot(Date,Declination,type=what)
pause(3)}
dev.off(2)
# Dotplot is a function in the Lattice package
find("dotplot")
library(lattice)
find("dotplot")
#package:lattice
library(mgcv) #package that also has the gam function
fit1<-mgcv::gam(Kyphosis~lo(Age)+lo(Number)+lo(Start), family=binomial, data=kyphosis) #specifies that the mgcv package's gam() function should be used and not the gam package's gam function with the :: operator
##CHAPTER 3
formula<-rainfall~seeding+(sne+cloudcover+prewetness+echomotion)+time
model<-lm(formula,data=clouds)
model

lm(formula = formula, data = clouds)

#Coefficients:
#         (Intercept)            seedingyes                   sne  
#             6.82235               1.01320              -0.91086  
#          cloudcover            prewetness  echomotionstationary  
#             0.00565               1.84379               2.16799  
#                time  
#            -0.03212  
model2<-update(model,~ . - sne -seeding:sne) # update the (.) precending model by taking out two variables
plot(resid(model),resid(model2))
pause(2)
abline(0,1)
#identify(resid(model),resid(model2))
dev.off(2)

upd<-function(drop){
model2<-update(model,drop)
plot(resid(model),resid(model2))
abline(0,1)
model2
}
modelSne<-upd(~ . - sne - seeding:sne)
modelSne
pause(2)
modelCover<-upd(~ . - cloudcover - seeding:cloudcover)
modelCover
pause(2)
modelCover<-upd(~ . - echomotion - seeding:echomotion)
modelCover
pause(2)
dev.off(2)
#object that describes all of the terms implied by this model formula
terms(~ x0 * (x1+x2))
