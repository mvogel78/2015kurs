## Revision
m <- matrix(1:100, nrow=10)
l <- list(a=1:10,b=rep(c(T,F),2),c=letters)

## use lapply() to get the class and the length of each element of l (two steps

lapply(l,class)
lapply(l,length)

## get the maximum of each column in m

apply(m,2,max)

## read in the data files
files <- dir("../session2/data",full.names = T, recursive = T,pattern = "[0-9]{3}\\.txt$")


source("function.r")
df.list <- lapply(files,read.file,skip=0)

length(files)
length(df.list)


table(sapply(df.list,class))

###########################################################################
################################ the functional Reduce ####################
###########################################################################

## Reduce

(d1 <- data.frame(id=LETTERS[c(1,2,3)],day1=sample(10,3)))
(d2 <- data.frame(id=LETTERS[c(1,3,5,6)],day2=sample(10,4)))
(d3 <- data.frame(id=LETTERS[c(2,4:6)],day3=sample(10,4)))
(d4 <- data.frame(id=LETTERS[c(1:5)],day4=sample(10,5)))

### example 1
Reduce(merge,list(d1,d2,d3,d4))

mymerge <- function(x,y) { merge(x,y, all=T)}
na.omit(Reduce(function(x,y) { merge(x,y, all=T) } ,list(d1,d2,d3,d4)))

Reduce(mymerge ,list(d1,d2,d3,d4))
### example 2

d4$day <- names(d4)[2]
names(d4)[2] <- "score"
Reduce(function(x,y) { y$day <- names(y)[2]
                       names(y)[2] <- "score"
                       rbind(x,y) } ,list(d1,d2,d3), init = d4)


## exercises
ml <- list(vl <- c(TRUE,FALSE),
           vn <- 1:10,
           vc <- letters)

## get the class of each of the vectors


## coerce them into one vector. Of which class is the resulting vector?

class(Reduce(c,ml))

## Combine all data frames
### exercise





read.files <- function(filesdir,skip=3,recursive=F,pattern="."){
    files <- dir(filesdir,
                 full.names = T,
                 recursive = recursive,
                 pattern = pattern)
    Reduce(rbind,lapply(files,read.file,skip=skip))}


data <- read.files("../session2/data",recursive = T,skip=0,pattern = "[0-9]{3}\\.txt$")


## read subsets
test <- read.files("../session2/data",
                   skip = 0, recursive = T,pattern="p[ro].+\\.txt$")
sub1 <- read.files("../session2/data",
                   skip = 0, recursive = T,pattern="\\002\\.txt$")

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

## alternative
data$testid <- str_replace(data$Subject,"^.+_","")
data$persid <- str_replace(data$Subject,"_.+$","")


data$Subject <- NULL

## now table the personid column
## what is left to do?





data$persid[data$persid=="CHGU"] <- "007"
data$persid[data$persid=="RMK"] <- "011"
data$persid[data$persid=="IJ2K"] <- "017"
data$persid[data$persid=="GA3K"] <- "004"
data$persid[data$persid=="Kj6K"] <- "006"


## there are some more wrong person ids: RMK - 011, IJ2K - 017,
## GA3K - 004, Kj6K - 006. Correct them!

## now read in the file subjectsdemographics.txt using the appropriate command
## join the demographics with our data data frame (there is a little problem left
## - compare the persid and Subject columns) 

persdat <- read.table("../session2/data/subjectdemographics.txt",
                      sep="\t",
                      header=T)

data$persid <- as.numeric(data$persid)


data <- merge(persdat,data,by.x = "Subject", by.y = "persid",all = T)


## first graph
require(ggplot2)

ggplot(data,aes(x=factor(Subject),fill=..count..)) +
    geom_bar() +
    facet_wrap(~testid)


table(data$Subject,data$testid)

## remove letters from testid
data$testid <- str_replace(data$testid,"[a-z]$","")
table(data$Subject,data$testid)

## change the order of levels
data$testid <- factor(data$testid,
                      levels=c("test1","1","2","3","4","5","6","7","8","test2"))
table(data$Subject,data$testid)

## save(data,file="201503data.rdata")


## redo the plot
ggplot(data,aes(x=factor(Subject),fill=..count..)) +
    geom_bar() +
    facet_wrap(~testid)
 
ggplot(data[data$testid=="test1",],aes(x=factor(Subject), fill=Stim.Type)) +
    geom_bar() 


###########################################################################
#########################  graphics #######################################
###########################################################################
require(ggplot2)
## create a new object
po <- ggplot()
summary(po)

## show structure of the object
str(po)

## example data
x1 <- 1:10; y1 <- 1:10; z1 <- 10:1
l1 <- LETTERS[1:10]
a <- 10; b <- (0:-9)/10:1
ex <- data.frame(x=x1,y=y1,z=z1,l=l1,a=a,b=b)

## create a new ggplot object containing the data
po <- ggplot(ex,aes(x=x1,y=y1))
summary(po)

## scatter plot
p1 <- po + geom_point()


## second example data frame
ex2 <- data.frame(x1=sample(1:20),
                  y1=sample(1:10),
                  l=letters[1:20])
head(ex2,10)

## replace data in po
pn <- po %+% ex2 
pn + geom_line()


## add a text layer
my.text <- geom_text(aes(label=l), 
                         hjust=1.1, 
                         vjust=-0.2)
pn + geom_path() + my.text


### add lines
## one line
p1 + geom_abline(intercept=10,slope=-1,
                colour=rgb(.5,.5,.9))
## two lines
p1 + geom_abline(intercept=c(10,9),slope=c(-1,-2),
                colour=rgb(.5,.5,.9))
## more lines
p1 + geom_abline(intercept=10:1,slope=-(10:1)/10,
                colour=rgb(.5,.5,.9))

p1 +
  geom_abline(aes(slope=b,intercept=a,colour=x1)) + 
  scale_x_continuous(limits=c(0,10))

p1 + geom_hline(yintercept=1:10)
p1 + geom_hline(yintercept=1:10) + 
    geom_vline(xintercept=1:10)


###########################################################################
###################### Exercises ##########################################
###########################################################################

## Now use our data frame
## create a new variable EC1 containing the first 2 letters of the Event.Code
## column, use the function str_sub() from the stringr package (type ?str_sub
## to get help)

tt <- load("201503data.rdata")



## now create a plot using ggplot
## map the variable EC1 to x and use geom_bar()




ggsave("plot1.png")

## now to the plot again, but add another aesthetic: fill (colour of the filling)
## map fill to Stim.Type




ggsave("plot2.png")

## add the position argument to geom_bar(), set it to "fill"



ggsave("plot3.png")

## now add facet_wrap(~testid) to show the same graph per time







ggsave("plot4.png")




## make a graph per child showing stacked hit/incorrect bars
## with time on the x axis




ggsave("plot5.png")


## show the distribution of the variable TTime using geom_boxplot()






ggsave("plotbp.png")

###################################################################################
########################           dplyr             ##############################
###################################################################################

require(dplyr)

## filter()
sub1 <- filter(data, Subject == 1)
table(sub1$Subject)

### and
sub1 <- filter(data, Subject == 1, Stim.Type == "incorrect")
table(sub1$Subject,sub1$Stim.Type)

### or
subframe <- filter(data, Age_PRETEST < 3.5 | Sex == "m" )
table(subframe$Age_PRETEST < 3.5, subframe$Sex)


## select()
subframe <- select(data, Subject, Sex, Age_PRETEST)
head(subframe)

### combining with filter() 
subframe <- select(data, Subject, Sex, Age_PRETEST) %>%
    filter(Age_PRETEST < 3.2)
table(subframe$Subject)

## arrange()
arr.frame <- arrange(data, TTime, Time)
head(arr.frame)

## mutate()
mut.frame <- mutate(data,
                    Event.Code = str_replace(Event.Code,".jpg",""),
                    TTime.calc = Time - Time1)
head(mut.frame)


subframe <- filter(data, Subject == 1) %>%
    mutate(Event.Code = str_replace(Event.Code,".jpg",""),
           TTime.calc = Time - Time1)

head(subframe)
table(subframe$Subject)

## transmute
mut.frame <- transmute(data,
                    Event.Code = str_replace(Event.Code,".jpg",""),
                    TTime.calc = Time - Time1)
head(mut.frame)

## summarise
sum.frame <- summarise(data, mean.ttime=mean(TTime), sd.ttime = sd(TTime))
sum.frame

## group_by/summarise

sum.frame <- group_by(data, Subject) %>%
    summarise(mean.ttime=mean(TTime), sd.ttime = sd(TTime))


sum.frame <- group_by(data, Subject, testid) %>%
    summarise(mean.ttime=mean(TTime), sd.ttime = sd(TTime))
head(sum.frame)

###################################################################################
########################  Exercises          dplyr   ##############################
###################################################################################

## use select() and filter() in combination (%>%) to
## select all rows belonging to the post or the pre test, keep the Subject,
## Sex, Age_PRETEST and Stim.Type column. Create a new data frame named
## data2 or something like this.





## add two new variables containing the counts of hit and incorrect.
## Use mutate() and sum(Stim.Type=='hit').





## use group\_by() and summarise() to extract the minimum and maximum TTime
## per person from the original data frame






## repeat the last exercise, but now group per person and EC1





###################################################################################
########################  ggplot2 scales and themses ##############################
###################################################################################
require(ggplot2)

ggplot(data,aes(x=EC1,fill=Stim.Type)) +
    geom_bar(position = "fill") +
    facet_wrap(~testid,scales = "free") +
    scale_fill_manual(values=c("forestgreen","firebrick"))

ggsave("ggp10.png")


ggplot(data,aes(x=EC1,fill=Stim.Type)) +
    geom_bar(position = "fill") +
    facet_wrap(~testid,scales = "free") +
        scale_fill_grey(start=0.3,end=0.6)


ggplot(data,aes(x=EC1,fill=Stim.Type)) +
    geom_bar(position = "fill") +
    facet_wrap(~testid,scales = "free") +
        scale_fill_hue(h=c(180,360))


ggplot(data,aes(x=EC1,fill=Stim.Type)) +
    geom_bar(position = "fill") +
    facet_wrap(~testid,scales = "free") +
    scale_fill_brewer(type = "div",palette = 4)


ggplot(data,aes(x=Trial,y=TTime)) +
    geom_point() 
    
ggsave("ggp11.png")

ggplot(data,aes(x=Trial,y=TTime)) +
    geom_point() +
    scale_y_continuous(trans="sqrt")
ggsave("ggp12.png")

xt1_3_trans <- function() trans_new("xt1_3", function(x) x**(1/3), function(x) x**3)

ggplot(data,aes(x=Trial,y=TTime)) +
    geom_point() +
    scale_y_continuous(trans="xt2_3")

ggsave("ggp13.png")

ggplot(data,aes(x=Trial,y=TTime)) +
    geom_point() +
    scale_y_sqrt()


###########################################################################
################################## Exercises  #############################
###########################################################################

## Create a scatter plot with Trial on the x-axis and TTime on the y-axis.
## Map colour to to age column. Looking at the pattern in the graph,
## is there relation between age and reaction time?




ggsave("ggp14.png")


## Make a plot which has a facet for each child containing the histogram
## of TTime, map fill to ..count.. ( fill=..count.. inside of aes() )
## then add scale_fill_gradient() and set its arguments low and high to
## say green and red resp. (or make your own choice)










## do the same but now do the facetting by testid









## (and second per Stim.Type level)






