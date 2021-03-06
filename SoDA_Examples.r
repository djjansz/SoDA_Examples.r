###################################################################################################
## TITLE:   SoDA Examples                                                                        ##
## AUTHOR:  Dave J                                                                               ##
## PURPOSE: To consolidate the examples from the book                                            ##
##          Software for Data Analysis: Programming with R                                       ##
## NOTES:   Pay attention to the setwd() below and change the working directory depending        ##
##          on which computer you are working on.                                                ##
##          Install the following packages before running this code:                             ##
##          install.packages(c("gam","ggfortify","HSAUR","SoDA"));                               ##
###################################################################################################
rm(list=ls(all=TRUE)) 
library(gam);library(ggfortify);library(HSAUR);library(SoDA);library(hash)
#data from pafko.com/tycho about the declination of Mars (the angle the planet makes with the equator)
setwd("C:/Users/SJF/OneDrive - Economical Insurance/Documents/Data")
#setwd("C:/R")
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
###################################################################################################
## FUNCTION OBJECTS AND FUNCTION CALLS                                                           ##
###################################################################################################
#pause(x) pauses for x seconds and prints the time to the window each second
pause <- function(sec) for (i in 1:sec)
{ cat(format(Sys.time(), "%X"),"\n")
  date_time<-Sys.time() #prints the time each second for x seconds
  while((as.numeric(Sys.time()) - as.numeric(date_time))<1){} 
}
#function to reverse a string
strReverse <- function(x)
{
  sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "")
}
stopiferror <- function(cond) 
{  if (cond==1) 
	{message(deparse(substitute(cond)), " has an error")  
		 while (TRUE) {}
	}
}
#define some objects related to the Canadian provinces
provinces<-c("AB","BC","MB","NB","NL","NS","NU","NT","ON","PE","QC","SK","YT")
region_provinces<-hash(c("CW","AT","AB","ON"),c("'AB','NB','NS','ON','PE','QC'","'NB','NS','PE'","'AB'","'ON'"))
region_names<-hash(c("CW","AT","AB","ON"),c("Country Wide","Atlantics","Alberta","Ontario"))
#show the region name of CW
region_names[["CW"]]
#show the provinces in CW
region_provinces[["CW"]]
#computing an additive summary for all nodes of a binary tree
binaryCount<-function (nodes, leafValues) 
{
    nL <- length(leafValues)
    nN <- nrow(nodes)
    left <- nodes[, 1]
    right <- nodes[, 2]
    left <- ifelse(left < 0, -left, left + nL)
    right <- ifelse(right < 0, -right, right + nL)
    count <- c(leafValues, rep(NA, nN))
    while (any(is.na(count))) 
	#message(iter, ": ", sum(is.na(count)))
    count <- c(leafValues, count[left] + count[right])
    count[-seq(length = nL)]
}
###################################################################################################
#edit the pause function in a separate editor with the trace() function and the edit=TRUE optional argument
#trace(pause,edit=TRUE)
#trace on both the entry into the function and exit out of the function
#trace(pause,browser,exit=browser)
pause(2)
#trace-and-browse session with the function pause() - step through subexpressions with the "n" command
#trace(pause,browser)
#pause(2)
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
formula<-rainfall~seeding+(sne+cloudcover+prewetness+echomotion)+time
model<-lm(formula,data=clouds)
model

terms(formula)
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
dropFormula<-function(original, drop) 
{
    factors <- attr(terms(as.formula(original)), "factors")
    row <- match(drop, rownames(factors))
    whichTerms <- factors[row, ] == 1
    labels <- colnames(factors)[whichTerms]
    text <- paste("~ . ", paste("-", labels, collapse = " "))
    eval(parse(text = text))
}
formula<-y~x0*(x1+x2)
terms(formula)
fMatrix<-attr(terms(formula),"factors")
fMatrix
fMatrix["x1",]
whichTerms<-fMatrix["x1",]==1
whichTerms
colnames(fMatrix)
colnames(fMatrix)[whichTerms]
dropModel<-function(model,drop){
    model2 <- update(model, dropFormula(model, drop))
    plot(resid(model), resid(model2), xlab = "Original Residuals", 
        ylab = paste("Residuals after dropping", drop))
    abline(0, 1)
    model2
}
# Original call to remove the sne variable from the linear model
modelSne<-upd(~ . - sne - seeding:sne)
modelSne
# After creating the dropFormula and dropModel function we can call the procedure in a more simple way
modelSne2<-dropModel(model,"sne")
modelSne2
pause(4)
###################################################################################################
## TRACING, EDITING, AND INTERACTIVE DEBUGGING                                                   ##
###################################################################################################
#call recover from the lowest relevant function call to debug interactively at the time of error
#options(error=recover)
#save allthe data in the calls that were active at the time the error occurred then call debugger later on
#options(error=dump.frames);
#debugger()
#send the function to the text editor
#trace(zapsmall,edit=TRUE)
#reassign zapsmall so that it will start with the browser and with the call to trace()
#trace(zapsmall,browser) 
#send the function to the text editor once again and notice the change in the function definition
#trace(zapsmall,edit=TRUE)
zapsmall(c(1.001,2.006,.006,.005),digits=2) #press "n" at each subexpression when in browse mode
#undo the open with browser behavior using the untrace() function
#untrace(zapsmall)
binaryCount <- function(nodes, leafValues) {
nL <- length(leafValues)
nN <- nrow(nodes)
left <- nodes[,1]; right <- nodes[, 2]
left <- ifelse(left<0, -left, left + nL)
right <- ifelse(right<0, -right , right + nL)
count <- c(leafValues, rep(NA, nN))
while(any(is.na(count)))
count <- c(leafValues, count[left] + count[right])
count[-seq(length=nL)]
}
# the binaryCount function uses the hclust example from the stats package
#example(hclust)
head(UScitiesD)
head(USArrests)
#nodeArea <- binaryCount(usTree@nodes, Area)
library(ggfortify)
autoplot(kmeans(USArrests, 3), data = USArrests, label = TRUE, label.size = 3)
pause(2)
devoff(3)
#Using the trace() function
#trace(aov,recover)
#trace(aov,exit=browser)
#trace(aov,browser,exit=browser)
#trace(.Fortran,recover)
#trace(aov,edit=TRUE)
#untrace(aov)
aov(yield ~ N + K + Error(block/(N + K)), data=npk)
###################################################################################################
## CONDITIONS: ERRORS AND WARNINGS                                                               ##
###################################################################################################
1+xyz
withCallingHandlers(1+xyz, error=function(e) cat("<error>"))
#define xyz and repeat
xyz=2
1+xyz
withCallingHandlers(1+xyz, error=function(e) cat("<error>"))
#remove all of the objects
rm(list=ls());
#xyz has not been defined and is used in the expr argument of the tryCatch handler function
1+xyz
tryCatch(1+xyz, error=function(e) cat("<you've got an error>\n"))
#define xyz and repeat
xyz=2
1+xyz
tryCatch(1+xyz)
# tryCatch.W.E taken from the demo(error.catching)
tryCatch.W.E <- function(expr){
W <- NULL
    w.handler <- function(w){ # warning handler
      W <<- w
       invokeRestart("muffleWarning")
     }
  list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),warning = w.handler),warning = W)
}
log(2)
str( tryCatch.W.E( log( 2 ) ) )
log(-1)
str( tryCatch.W.E( log( -1) ) )
log("a")
str( tryCatch.W.E( log("a") ) )
###################################################################################################
## R PACKAGES: CREATE A PACKAGE AND THEN DELETE IT                                               ##
###################################################################################################
SoDA2Objects <- c("geoXY", "geoDist", "packageAdd")
package.skeleton("SoDA2", SoDA2Objects, path = "C:/R")
dir.create("C:/R/SoDA2/data")
mars<-read.csv("mars.csv",skip=5,as.is=TRUE)
dowNames <-c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday")
save(mars,dowNames, list = character(),file = "C:/R/SoDA2/data/marsDOW.rda")
rm(list=ls(all=TRUE)) 
load("C:/R/SoDA2/data/marsDOW.rda")
head(mars)
dowNames
#delete the folder
unlink("C:/R/SoDA2", recursive=TRUE) 
#documentation on the class ts
class?ts 
#methods for the function show()
methods?show
#documention on the Matrix package
package?SoDA
#see examples from the documentation page on lm
example(lm)
#the evaluator will search the namespace when functions are called
search()
#show the location of the base R package
system.file()
#show the location of the SoDA package
system.file(package="SoDA")
#show the path of the chunks program in the perl subdirectory of the SoDA package
system.file("perl",c("chunks.perl"),package="SoDA")
#list the files and folders under the SoDA package's root directory
list.files(system.file(package="SoDA"))
