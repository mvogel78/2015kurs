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


##############################################################################
###################### New Exercises          ################################
##############################################################################

x <- c(2, 7, 0, 9, 10, 23, 11, 4, 7, 8, 6, 0)

## 1. Display every third element in x







## 2. Display elements that are less than 10, but greater than 4


## 3. Modify the vector x, replacing by 10 all values that are greater than 10


## 4. Modify the vector x, multiplying by 2 all elements that are smaller than 5


## 5. Create a new vector y with elements 0,1,0,1, . . . (12 elements, use the rep() function) and a vector z that equals x when y=0 and 3x when y=1. (You can do it using ifelse, but there are other possibilities)









########################## apply family #####################################
require(MASS)


tapply(quine$Days,quine$Lrn,mean)
tapply(quine$Days,list(quine$Eth,quine$Lrn),mean)


x <- 1:12


#############################################################################
###################### Exercises Apply     ##################################
#############################################################################

## the class() function shows the class of an object
## use it in combination with lapply() to get the
## classes of the columns of the quine data frame


lapply(quine,class)



## do the same with sapply()
## what is the difference

quine2 <- quine[,lapply(quine,class)=="factor"]


## try to combine this with what you learned about indexing
## and create a new data frame quine2 only containing the columns
## which are factors


## calculate the row and column means of the below defined
## matrix m using the apply function
## PS: in real life application use the rowMeans() and
## colMeans() function 
m <- matrix(rnorm(100),nrow=10)
apply(m,1,mean)

apply(MARGIN = 2,X=m,FUN=mean)


apply(m,2,mean)




## use tapply() to summarise the number of missing days at school
## per Ethnicity and/or per Sex (three lines)


tapply(quine$Days,quine$Lrn,mean)
tapply(quine$Days,list(quine$Eth,quine$Lrn),mean)

tapply(quine$Days,list(quine$Eth,quine$Lrn),summary)


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


my.function <- function(x){
    mean(abs(x - mean(x)))
}

my.function2 <- function(x){
    my.mean <- mean(x)
    dists <- abs(x-mean(x))
    mean(dists)
}



## Write a function f() which finds the average of the x values after squaring
##  and substracts the square of the average of the numbers. Verify
## this output will always be non-negative by computing f(1:10)



f(1:10)


## An integer is even if the remainder upon dividing it by 2 is 0.
## This remainder is given by R with the syntax  x %% 2.
## Use this to write a function iseven().
## How would you write isodd()?



4 %% 1:3




## Write a function isprime() that checks if a number x is prime by
## dividing x by all values 2,...,x-1 then checking to see if
## there is a remainder of 0. Ignore cases x < 2.


isprime <- function(x){
    if(x==2 ) return(TRUE)
    list(!(0 %in%  c( x %% (2:(x-1) ))),x,matrix(1:10,nrow=10))


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


(d1 <- data.frame(id=LETTERS[c(1,2,3)],day1=sample(10,3)))
(d2 <- data.frame(id2=LETTERS[c(1,3,5,6)],day2=sample(10,4)))


merge(d1,d2,by.x = "id", by.y = "id2", all = T)

#####################################################################################
############################### Exercises  ##########################################
#####################################################################################

## now read in the file personendaten.txt using the appropriate command
## join the demographics with our pre1 data frame (even it does not make sense
## now)






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





## now we read all these files at once
df.list <- lapply(files,read.file,skip=0)

length(df.list)
sapply(df.list,class)


## and bind them together
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



### create two new variables persid and testid using the following line
## look at the resulting column and try to understand
## what is going on
## PS: the str_split() function is part of the stringr package
data$persid <- sapply(data$Subject,function(x)
    str_split(x,pattern = "_")[[1]][1])

data$testid <- sapply(data$Subject,function(x)
    str_split(x,pattern = "_")[[1]][2])


## delete the Subject column
data$Subject <- NULL

data$persid[data$persid=="CHGU"] <- "007"


## there are some more wrong person ids:
## RMK - 011, IJ2K - 017, GA3K - 004, Kj6K - 006. Correct them!




## now read in the file subjectsdemographics.txt using the appropriate command
## join the demographics with our data data frame (there is a little problem left
## - compare the persid and Subject columns)










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
