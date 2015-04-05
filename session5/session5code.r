setwd("/media/mandy/Samsung_T1/mpicbs/2015kurs/session5/")
require(ggplot2)
require(dplyr)

load("../session4/session4data.rdata")

######################################################################
####################### Exercises  ###################################
######################################################################

## use a t-test to compare TTime according to Stim.Type,
## visualize it. What is the problem?

t.test(data$TTime ~ data$Stim.Type)

ggplot(data,aes(x=Stim.Type,y=TTime)) +
    geom_boxplot()

## now do the same for Subject 1 on pre and post test (use filter()
## or indexing to get the resp. subsets)

t.test(data$TTime[data$Subject==1 & data$testid=="test1"] ~
       data$Stim.Type[data$Subject==1 & data$testid=="test1"])

t.test(data$TTime[data$Subject==1 & data$testid=="test2"] ~
       data$Stim.Type[data$Subject==1 & data$testid=="test2"])




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

ggplot(data,aes(x=testid,y=TTime)) +
    geom_boxplot(aes(fill=Stim.Type)) +
    facet_wrap(~Subject)

ggsave("boxplotfacets.png")

ggplot(data,aes(x=factor(Subject),y=TTime)) +
    geom_boxplot(aes(fill=Stim.Type)) +
    facet_wrap(~testid)


## how many tests have a statistically significant result?
table(res$pval < 0.05)

prop.table(table(res$pval < 0.05))

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
ggplot(data,aes(x=TTime)) +
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




########################################################################
############## Exercises stats ggplot2    ##############################
########################################################################


require(Hmisc)
ggplot(data,aes(x=testid,y=TTime)) +
    stat_summary(fun.data="mean_se",mult=1.96,geom="pointrange",aes(colour=EC1),position = position_dodge(width = 0.5)) +
    stat_bin(y=0,aes(label=..count..),geom="text") +
    scale_y_continuous(limits=c(0,75000)) +
    facet_wrap(~Subject)

tdf <- data.frame(x=runif(1000),y=runif(1000))

ggplot(tdf,aes(x=x,y=y)) + stat_quantile()



require(zoo)
tmp <- rollmean(data$Stim.Type[data$Subject==2] == "hit",40)
plot(tmp,pch=19,ylim=c(0,1))

for(i in 2:10){
    tmp <- rollmean(data$Stim.Type[data$Subject==i] == "hit",40)
    points(tmp,col=i,pch=19)
}


