setwd("/media/mandy/Volume/transcend/mpicbs/2015kurs/session2")

## install the package ggplot2
## either via the menu
## or via command line

## install.packages("faraway")
install.packages("ggplot2")

## set working directory


################################ our data frame #####################################
### read presentation files

pre1 <- read.table(file = "../session1/session1data/pre001.txt",
                sep = "\t",
                header = T,
                skip = 3,
                na.strings = "")


#####################################################################################
###################### Exercises Read in Data #######################################
#####################################################################################

## read in the files datatable1.txt and datatable2.txt (both are contained in the
## session1data directory! First look on the both of them, they need different
## specification of arguments! Assign them to a variable

data1 <- read.table("../session1/session1data/datatable1.txt",
                    sep="|",
                    dec=",",
                    header=T,
                    na.strings = "missing")

data2 <- read.table("../session1/session1data/datatable2.txt",
                    sep="\t",
                    header=T,
                    skip=1)



##############################################################################
###################### Exercises Indices      ################################
##############################################################################

########## part A

## run the following commands!
## try to understand whats going on in each case!
x <- c(2, 7, 0, 9, 10, 23, 11, 4, 7, 8, 6, 0)
x[4]
x[3:5]
x[c(1, 5, 8)]
x[x > 10]
x[(1:6) * 2]
all(x > 0)
x[x == 0] <- 1
x

all(x > 0)
ifelse(round(x/2) == x/2, "even", "odd")

sum(x>4)

########## part B

## install and load the package faraway!
## use the names() function to list all column names of the data frame pima

install.packages("faraway")
require(faraway)
names(pima)

## use indexing to extract the bmi value of row 15-17
pima[15:17,"bmi"]

## use the sum() function to obtain the number
## of bmi values greater than 30 (like in the
## last line of part A)

sum(pima$bmi > 30)

## display rows 7-10 
pima[7:10,]

## display rows 7-10 but without column 2 and 3
pima[7:10,-c(2:3)]


### New exercises
x <- c(2, 7, 0, 9, 10, 23, 11, 4, 7, 8, 6, 0)

## 1. Display every third element in x
x[c(3,6,9,12)]

x[(1:4)*3]

x[seq(3,length(x),by=3)]


## 2. Display elements that are less than 10, but greater than 4
x[ x < 10 & x > 4]

## 3. Modify the vector x, replacing by 10 all values that are greater than 10
x[ x > 10] <- 10

## 4. Modify the vector x, multiplying by 2 all elements that are smaller than 5
x[x < 5] <- x[x < 5] * 2

## 5. Create a new vector y with elements 0,1,0,1, . . . (12 elements, use the rep() function) and a vector z that equals x when y=0 and 3x when y=1. (You can do it using ifelse, but there are other possibilities)
y <- rep(0:1,6)

z <- 3 * y * x + x

ifelse(y==1, 3*x, x)
ifelse(y==0, x, 3*x)


################################ back to our data frame #####################

## remove the first line 
pre1 <- pre1[-1,] 

########################## apply family #####################################

tapply(quine$Days,quine$Lrn,mean)
tapply(quine$Days,list(quine$Eth,quine$Lrn),mean)

#############################################################################
###################### Exercises Apply     ##################################
#############################################################################

## the class() function shows the class of an object
## use it in combination with lapply() to get the
## classes of the columns of the quine data frame
lapply(quine,class)

## do the same with sapply()
## what is the difference
sapply(quine,class)

## try to combine this with what you learned about indexing
## and create a new data frame quine2 only containing the columns
## which are factors
quine2 <- quine[,sapply(quine,class)=="factor"]

## calculate the row and column means of the below defined
## matrix m using the apply function
## PS: in real life application use the rowMeans() and
## colMeans() function 
m <- matrix(rnorm(100),nrow=10)
apply(m,1,mean)
apply(m,2,mean)

## use tapply() to summarise the number of missing days at school
## per Ethnicity and/or per Sex (three lines)
tapply(quine$Days,quine$Eth,mean)
tapply(quine$Days,quine$Sex,mean)

tapply(quine$Days,list(quine$Sex,quine$Eth),mean)

## sometimes the aggregate function is more convenient
## note the use of ~ ; it is read as 'is dependent on'
## it is extensively used in modelling
aggregate(Days ~ Sex + Eth, data=quine,mean)

aggregate(Days ~ Sex + Eth, data=quine,summary)

## compare the result of the last line to this one
tapply(quine$Days,list(quine$Sex,quine$Eth),summary)

tmp <- tapply(quine$Days,list(quine$Sex,quine$Eth),summary)
dim(tmp) <- NULL


#####################################################################################
###################### Exercises Functions    #######################################
#####################################################################################


## Write a function to compute the average distance from the mean for some data vector.
## (distance > 0)

avg.dist <- function(x){
    xbar <- mean(x)
    mean(abs(x-xbar))
}

## Write a function f() which finds the average of the x values after squaring
##  and substracts the square of the average of the numbers. Verify
## this output will always be non-negative by computing f(1:10)

f <- function(x){
    xbar <- mean(x**2)
    xbar2 <- mean(x)**2
    xbar - xbar2
}


f <- function(x){
    mean(x**2) - mean(x)**2
}

f(1:10)


## An integer is even if the remainder upon dividing it by 2 is 0.
## This remainder is given by R with the syntax  x \%\% 2.
## Use this to write a function iseven().
## How would you write isodd()?

iseven <- function(x){
    x %% 2 == 0
}

## we do not need it...
isodd <- function(x){
    !iseven(x)
}

## Write a function isprime() that checks if a number x is prime by
## dividing x by all values 2,...,x-1 then checking to see if
## there is a remainder of 0. Ignore cases x < 2.

isprime <- function(x){
    if(x == 2) return(TRUE)
    !(0 %in% (x %% (2:(x-1))))
}


#####################################################################################
###################### Put it all together    #######################################
#####################################################################################


file <- "../session1/session1data/pre001.txt"
skip <- 3
tmp <- read.table(file,skip = skip,sep = "\t",
                  header=T,na.strings = c(" +",""),
                  fill=T)

## Remove empty line
tmp <- tmp[!is.na(tmp$Subject),] 

## Remove unnecessary spaces
## in addition to the use of lapply() we have to define a
## function on our own

tmp <- lapply(tmp,function(x) {
        if( class(x) %in% c("character","factor") ){
            x <- factor(gsub(" ","",as.character(x)))
            return(x)}else{ return(x) }})
tmp <- as.data.frame(tmp)


pause <- which(tmp$Event.Type=="Picture" & tmp$Code=="Pause")
if(length(pause)>0){
    drei <- which(tmp$Code==3 & !is.na(tmp$Code))
    drei <- drei[drei > pause][1:2]
    if(pause + 1 < drei[1]){
        tmp <- tmp[-(pause:drei[2]),]
    }}

    
tmp <- tmp[!(tmp$Event.Type %in% c("Pause","Resume")), ]

first.pic <- min(which(tmp$Event.Type=="Picture" & !is.na(tmp$Event.Type) )) - 1 
tmp <- tmp[-(1:first.pic),]

last.pic <- min(which(tmp$Event.Type=="Picture" & !is.na(tmp$Event.Type) &
                          tmp$Code=="Fertig!" & !is.na(tmp$Code)))
tmp <- tmp[-(last.pic:nrow(tmp)),]

zeilen <- which(tmp$Event.Type %in% c("Response"))
zeilen <- sort(unique(c(zeilen,zeilen-1)))
zeilen <- zeilen[zeilen>0]
tmp <- tmp[zeilen,]
    
responses <- which(tmp$Code %in% c(1,2))
events <- responses-1
tmp$Type <- NA
tmp$Type[responses] <- as.character(tmp$Event.Type[events])

if(length(tmp$Type[responses])!=length(tmp$Event.Type[events])) { print(file)}

tmp$Event.Code <- NA
tmp$Event.Code[responses] <- as.character(tmp$Code[events])
tmp$Stim.Type[responses] <- as.character(tmp$Stim.Type[events])
tmp$Duration[responses] <- as.character(tmp$Duration[events])
tmp$Uncertainty.1[responses] <- as.character(tmp$Uncertainty.1[events])
tmp$ReqTime[responses] <- as.character(tmp$ReqTime[events])
tmp$ReqDur[responses] <- as.character(tmp$ReqDur[events])
tmp$Pair.Index[responses] <- as.character(tmp$Pair.Index[events])

tmp <- tmp[tmp$Event.Type=="Response" & !is.na(tmp$Type),]
tmp <- tmp[tmp$Type=="Picture" & !is.na(tmp$Type),]

## run and use the new function
## the function is contained in the file function.r 
## you can run the content of the file using source("function.r")

source("function.r")


file <- "../session1/session1data/pre001.txt"
pre1 <- read.file(file,skip=3)

file <- "../session2/data/pretest/pre_001.txt"
pre1v2 <- read.file(file,skip=0)


table(pre1$Subject)
table(pre1$Event.Type)
table(pre1$Code)

pre1 <- droplevels(pre1)
table(pre1$Code)
table(pre1$Code,pre1$Stim.Type)


########################## combining data frames  #####################################
### rbind

x <- data.frame(id=1:3,score=rnorm(3))
y <- data.frame(id=13:15,score=rnorm(3))
rbind(x,y)


## cbind
x <- data.frame(id=1:5,score1=rnorm(5))
y <- data.frame(score2=rnorm(5),score3=rnorm(5))
cbind(x,y)


## merge
(d1 <- data.frame(id=LETTERS[c(1,2,3)],day1=sample(10,3)))
(d2 <- data.frame(id=LETTERS[c(1,3,5,6)],day2=sample(10,4)))

merge(d1,d2)

merge(d1,d2,all.x = T)

merge(d1,d2,all.y = T)

zz <- merge(d1,d2,all = T)


################################ back to our data frame ###############################
## join the person data with the pre1 data frame

#####################################################################################
############################### Exercises  ##########################################
#####################################################################################

## now read in the file personendaten.txt using the appropriate command
## join the demographics with our pre1 data frame (even it does not make sense
## now)

persdat <- read.table("../session1/session1data/personendaten.txt",
                      sep="\t",
                      header=T)

pre1 <- merge(persdat,pre1,all.y = T)
head(pre1)
summary(pre1)

################################ the functional Reduce ###############################

## Reduce

(d1 <- data.frame(id=LETTERS[c(1,2,3)],day1=sample(10,3)))
(d2 <- data.frame(id=LETTERS[c(1,3,5,6)],day2=sample(10,4)))
(d3 <- data.frame(id=LETTERS[c(2,4:6)],day3=sample(10,4)))
(d4 <- data.frame(id=LETTERS[c(1:5)],day4=sample(10,5)))

### example 1
Reduce(merge,list(d1,d2,d3,d4))
Reduce(function(x,y) { merge(x,y, all=T) } ,list(d1,d2,d3,d4))

na.omit(Reduce(function(x,y) { merge(x,y, all=T) } ,list(d1,d2,d3,d4)))


### example 2
d4$day <- names(d4)[2]
names(d4)[2] <- "score"
Reduce(function(x,y) { y$day <- names(y)[2]
                       names(y)[2] <- "score"
                       rbind(x,y) } ,list(d1,d2,d3), init = d4)

## dir
dir()

dir("data")

dir("data",recursive = T)

dir("data",recursive = T, full.names = T)

dir(pattern = "\\.r$")

## exercise dir()
## create a variable files containing the names of all text files
## in the data directory, my editor creates temporary files beginning
## and ending by a #, make sure they are not contained in the list

files <- dir("data",full.names = T, recursive = T,pattern = "txt$")

df.list <- lapply(files,read.file,skip=0)

length(df.list)
sapply(df.list,class)

data <- Reduce(rbind,df.list)
nrow(data)
table(data$Subject)


read.files <- function(filesdir,skip=3,recursive=F,pattern="."){
    files <- dir(filesdir,
                 full.names = T,
                 recursive = recursive,
                 pattern = pattern)
    Reduce(rbind,lapply(files,read.file,skip=skip))}


data <- read.files("data",recursive = T,skip=0,pattern = "\\.txt$")


#####################################################################################
############################### Exercises  ##########################################
#####################################################################################

## if you look at the result of table() - what is the problem?
table(data$Subject)


### create two new variables persid and testid using the following line
## look at the resulting column and try to understand
## what is going on
## PS: the str_split() function is part of the stringr package
data$persid <- sapply(data$Subject,function(x)
    str_split(x,pattern = "_")[[1]][1])

data$testid <- sapply(data$Subject,function(x)
    str_split(x,pattern = "_")[[1]][2])

data$persid[data$persid=="CHGU"] <- "007"
data$persid[data$persid=="RMK"] <- "011"
data$persid[data$persid=="IJ2K"] <- "017"
data$persid[data$persid=="GA3K"] <- "004"
data$persid[data$persid=="Kj6K"] <- "006"

data$Subject <- NULL

## now read in the file subjectsdemographics.txt using the appropriate command
## join the demographics with our pre1 data frame (there is a little problem left
## - compare the persid and Subject columns)


persdat <- read.table("data/subjectdemographics.txt",
                      sep="\t",
                      header=T)

data$persid <- as.numeric(data$persid)

data <- merge(persdat,data,by.x = "Subject",by.y = "persid",all=T)

head(data)
summary(data)


## to see at least one graphic
require(ggplot2)
ggplot(data,aes(x=factor(Subject),fill=..count..)) +
    geom_bar() +
    facet_wrap(~testid)


table(data$Subject,data$testid)
data$testid <- str_replace(data$testid,"[a-z]$","")
data$testid <- factor(data$testid,
                      levels=c("test1","1","2","3","4","5","6","7","8","test2"))
table(data$Subject,data$testid)

ggplot(data,aes(x=factor(Subject),fill=..count..)) +
    geom_bar() +
    facet_wrap(~testid)



ggplot(data,aes(x=testid,fill=Stim.Type)) +
    geom_bar(position=position_fill()) +
    facet_wrap(~Subject)
