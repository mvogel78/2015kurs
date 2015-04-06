setwd("/media/mandy/Samsung_T1/mpicbs/2015kurs/session5/")
require(ggplot2)
require(dplyr)
require(ez)
require(reshape2)

load("../session4/session4data.rdata")
data$Duration <- as.numeric(data$Duration)

######################################################################
####################### Exercises  ###################################
######################################################################

## use a t-test to compare TTime according to Stim.Type,
## visualize it. What is the problem?





## now do the same for Subject 1 on pre and post test (use filter()
## or indexing to get the resp. subsets)








## use the following code to do the test on every subset Subject
## and testid, try to figure what is happening in each step:

data$Stim.Type <- droplevels(data$Stim.Type)

data.l <- split(data,list(data$Subject,data$testid))

tmp.l <- lapply(data.l,function(x) {
    if(min(table(x$Stim.Type)) < 5) return(NULL)
    tob <- t.test(x$TTime ~ x$Stim.Type)
    tmp <- data.frame(
        Subject = unique(x$Subject),
        testid = unique(x$testid),
        mean.group.1 = tob$estimate[1],
        mean.group.2 = tob$estimate[2],
        name.test.stat = tob$statistic,
        conf.lower = tob$conf.int[1],
        conf.upper = tob$conf.int[2],
        pval = tob$p.value,
        alternative = tob$alternative,
        tob$method)})

res <- Reduce(rbind,tmp.l)

## plots






## how many tests have a statistically significant result?




## Is there a tendency? What could be the next step?



## hypothesis: if the child tries to answer correctly it thinks about it 
## if it does not know the answer immediately. So the difference of
## TTime resp. correct/incorrect should be higher as the percentage of
## correct answers is higher


tmp.l <- lapply(data.l,function(x) {
    if(min(table(x$Stim.Type)) < 5) return(NULL)
    tob <- t.test(x$TTime ~ x$Stim.Type)
    tmp <- data.frame(
        Subject = unique(x$Subject),
        testid = unique(x$testid),
        perc.corr = sum(x$Stim.Type=="hit")/sum(!is.na(x$Stim.Type)),
        mean.group.1 = tob$estimate[1],
        mean.group.2 = tob$estimate[2],
        name.test.stat = tob$statistic,
        conf.lower = tob$conf.int[1],
        conf.upper = tob$conf.int[2],
        pval = tob$p.value,
        alternative = tob$alternative,
        tob$method)})

res <- Reduce(rbind,tmp.l)

require(grid)

ggplot(res,aes(x=perc.corr,y=mean.group.1 - mean.group.2)) +
    geom_point() +
    geom_smooth(se=F) +
    geom_smooth(data = res[res$perc.corr < 0.65,],se=F, method="lm",colour="black") +
    geom_smooth(data = res[res$perc.corr > 0.65,],se=F, method="lm",colour="black") +
    annotate("rect",xmin=0.65,xmax=Inf,ymin=-Inf,ymax = Inf,fill="blue",alpha=0.1) + annotate("segment",x=0.65,xend=0.65,y=-20000,yend=-5000,size=3,arrow=arrow())




## Anova
oneway <- read.table("anova.data.txt",header = T)
names(oneway) <- c("ozone","garden")

oneway

t(oneway %>% group_by(garden) %>% summarise(mean=mean(ozone)))

mean(oneway$ozone)
sum((oneway$ozone-mean(oneway$ozone))**2)

(oneway$ozone-mean(oneway$ozone))**2


### Anova Grafiken
plot(oneway$ozone,pch=19)
abline(h=mean(oneway$ozone))
for(i in 1:14){lines(c(i,i),c(mean(oneway$ozone),oneway$ozone[i]))}


plot(oneway$ozone,pch=19,ylim=c(4,12))
abline(h=mean(oneway$ozone))
for(i in 1:14){lines(c(i,i),c(mean(oneway$ozone),oneway$ozone[i]))}
text(x=1:14,y=4,labels=(oneway$ozone-mean(oneway$ozone))**2,cex = 1.8)

oneway <- oneway %>% group_by(garden) %>% mutate(mean=mean(ozone))

par("mar")
par(mar=c(5,4,0,2)+0.1)
plot(oneway$ozone,pch=21,bg=oneway$garden)
abline(h=tapply(oneway$ozone,oneway$garden,mean),col=c(1,2))
for(i in 1:14){lines(c(i,i),c(oneway$mean[i],oneway$ozone[i]),col=oneway$garden[i])}


plot(oneway$ozone,pch=21,bg=oneway$garden,ylim = c(4,12))
abline(h=tapply(oneway$ozone,oneway$garden,mean),col=c(1,2))
for(i in 1:14){lines(c(i,i),c(oneway$mean[i],oneway$ozone[i]),col=oneway$garden[i])}
text(x=1:14,y=4,labels=(oneway$ozone-oneway$mean)**2,cex = 1.8)

mm <- aov(ozone ~ garden, data=oneway)

plot(seq(0.1,20,by=0.1),
     df(seq(0.1,20,by=0.1),1,12),
     type="l",
     xlab = "F",
     ylab = "density")
x <- seq(15.75,20,by=0.1)
polygon(c(x,rev(x)),c(df(x,1,12),rep(0,length(x))),col="firebrick2")
x <- seq(0.01,15.75,by=0.1)
polygon(c(x,rev(x)),c(df(x,1,12),rep(0,length(x))),col="darkgreen")

abline(v=15.75)

## anova syntax R
mm <- lm(ozone ~ garden, data=oneway)
anova(mm)

m2 <- aov(ozone ~ garden, data=oneway)
summary(m2)

########################################################################
######################## Exer Anova    #################################
########################################################################

## Look at the help of the TukeyHSD function. What is its purpose?
## Execute the code of the example near the end of the help page,
## interpret the results!



## install and load the granovaGG package (a package for visualization
## of ANOVAs)
require(granovaGG)

## load the arousal data frame and use the stack()} command to bring
## the data in the long form.



## Do a anova analysis. Is there a difference at least 2 of the groups?



## If indicated do a post-hoc test.



## Visualize your results





########################################################################
######################## stats ggplot2 #################################
########################################################################


## stat_bin
ggplot(data,aes(x=EC1)) +
    geom_bar() +
    stat_bin(geom="text", aes(label=..count..),
             colour="red", size=14, vjust=1)

ggsave("img/statbin.png")

ggplot(data,aes(x=EC1)) +
    geom_bar() +
    stat_bin(geom="point",
             colour="blue", size=40, vjust=1) +
    stat_bin(geom="text", aes(label=..count..),
             colour="red", size=14, vjust=0.5)


ggplot(data,aes(x=EC1)) +
    geom_bar() +
    stat_bin(geom="point",
             colour="blue", size=40, vjust=1) +
    stat_bin(aes(group=Stim.Type,label=..count..,),geom="text", 
             colour="red", size=14, vjust=0.5,position = position_stack())

### 2d variant
ggplot(data,aes(x=EC1,y=factor(Subject))) +
    stat_bin2d() +
    stat_bin2d(geom="text",aes(label=..count..),
               position = position_jitter(width=0.2,height = 0)) +
    facet_wrap(~testid,scales = "free_x")

## stat_ecdf
ggplot(data,aes(x=seq_along(TTime),y=TTime/max(data$TTime))) +
    geom_point() +
    stat_ecdf()



## stat_density/stat_function
p1 <- ggplot(data,aes(x=TTime)) +
    geom_density() +
    stat_function(fun=dnorm,
                  args = list(mean=mean(data$TTime),
                              sd=sd(data$TTime))
                  )

ggsave("statdens.png")

ggplot(data,aes(x=TTime**(1/3))) +
    geom_density() +
    stat_function(fun=dnorm,
                  args = list(mean=mean(data$TTime**(1/3)),
                              sd=sd(data$TTime**(1/3)))
                  )

ggplot(data,aes(x=log(TTime))) +
    geom_density(position = position_identity()) +
    stat_function(fun=dnorm,
                  args = list(mean=mean(log(data$TTime)),
                              sd=sd(log(data$TTime))),
                  colour="red"
                  )

ggplot(data,aes(x=log(TTime))) +
    geom_line(stat="density") +
    stat_function(fun=dnorm,
                  args = list(mean=mean(log(data$TTime)),
                              sd=sd(log(data$TTime))),
                  colour="red"
                  )

## stat_summary
data$Duration <- as.numeric(data$Duration)

ggplot(data,aes(x=EC1,y=Duration)) +
    stat_summary(fun.data="mean_se",geom = "pointrange")

ggsave("img/sum1.png")


ggplot(data,aes(x=EC1,y=Duration)) +
    stat_summary(fun.data="mean_se",geom = "pointrange") +
    facet_wrap(~Subject)


trimed.mean <- function(x){
    data.frame(y=mean(x,na.rm = T,trim = 0.1))
}

stat_meanlabel <- function(angle=0,vjust=0.5,hjust=0,...){
    stat_summary(fun.y="mean",
                 geom="text",
                 aes(label=round(..y..)),
                 hjust=hjust,
                 vjust=vjust,
                 angle=angle, ...)}
    
ggplot(data,aes(x=EC1,y=Duration)) +
    stat_summary(fun.data="mean_se",geom = "pointrange") +
    stat_summary(fun.data="trimed.mean",geom = "point", colour="blue") +
    stat_summary(fun.y="median",geom = "point", colour="green") +
    stat_meanlabel(angle = 90,vjust = 0 )

ggsave("img/sum2.png")


########################################################################
######################## annotation ggplot2 ############################
########################################################################


ggplot(data,aes(x=EC1,y=Duration)) +
    stat_summary(fun.data="mean_se",geom = "pointrange") +
    annotate(geom="text",x=3,y=15000,label="here we are") +
    annotate(geom="segment",x = 2.5,xend = 3.5, y = 15000,yend=16000,colour="red") +
    annotate(geom="rect",xmin = 4.8,xmax = 6.2, ymin = 19100,ymax=20100,fill="red",alpha=0.3)

## table
require(gridExtra)

my.table <- tableGrob(as.data.frame(table(data$EC1)))

ggplot(data,aes(x=EC1,y=Duration)) +
    stat_summary(fun.data="mean_se",geom = "pointrange") +
    annotation_custom(xmin = 1, xmax = 3, ymin = 18000, ymax = 20000,
                      grob = my.table) +
    ylim(13000,20000)


my.subplot <- ggplotGrob(
    p1 +
        theme(plot.background=element_rect(fill="transparent",
                                           colour="transparent"),
              panel.background=element_rect(fill="transparent"),
              panel.grid=element_blank()))

ggplot(data,aes(x=EC1,y=Duration)) +
    stat_summary(fun.data="mean_se",geom = "pointrange") +
    annotation_custom(xmin = 1, xmax = 3, ymin = 18000, ymax = 20000,
                      grob = my.table) +
    annotation_custom(xmin = 4, xmax = 6, ymin = 13000, ymax = 18000,
                      grob = my.subplot) +
    ylim(13000,20000)


########################################################################
############## Exercises stats ggplot2    ##############################
########################################################################

## use stat_summary() to plot the mean inclusive the 95 percent
## confidence interval per time (testid)
## use stat_bin() to add the number of observations













## add a new column to the data data frame containing a 1 if Stim.Type
## equals hit and 0 otherwise, now use stat\_summary() to plot a line
## (time on the x-axis and the percentage of correct hits on the
## y-axis. Hint: you have to transform testid into a numeric variable.





## now colour the background of the time points test1 and test2 different
## from the training time. use \texttt{annotate()}















#########################################################################
################### Rep Meas ANOVA ######################################
#########################################################################

xx <- data.frame(subject=1:6,
                 t1=c(45,42,36,39,51,44),
                 t2=c(50,42,41,35,55,49),
                 t3=c(55,45,43,40,59,56))
n <- 6
k <- 3


require(reshape2)
xx <- melt(xx,id.vars = "subject")

## sstime
(tmp <- aggregate(value ~ variable,FUN = "mean",data=xx))
(sstime <- sum(6*(tmp$value - mean(xx$value))**2))

## ssw
require(dplyr)
(xx <- xx %>% group_by(variable) %>% mutate(gr.mean=mean(value)))
(ssw <- sum((xx$value - xx$gr.mean)**2))

## sssub
(sub.xx <- xx %>% group_by(subject) %>% summarise(sub.mean=mean(value)))
(sssub <- k*sum((sub.xx$sub.mean - mean(xx$value))**2))

## sserror
(sserror <- ssw - sssub)

## mean squares
(mstime <- sstime/(k-1))
(mserror <- sserror/((n-1)*(k-1)))

(F <- mstime/mserror)

1-pf(F,k-1,((n-1)*(k-1)))


require(ez)
xx <- as.data.frame(xx)
(an <- ezANOVA(data=xx,dv = value,wid= subject, within = variable))
