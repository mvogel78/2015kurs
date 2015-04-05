setwd("/media/mvogel/Volume/transcend/mpicbs/2015kurs/session4")

## load the data (file: session4data.rdata)
## make a new summary data frame (per subject and time) containing:
###### the number of trials
###### the number correct trials (absolute and relative)
###### the mean TTime and the standard deviation of TTime
###### the respective standard error of the mean
##  keep the information about Sex and Age\_PRETEST

load("session4data.rdata")

require(dplyr)
sumdf <- data %>%
    group_by(Subject,Sex,Age_PRETEST,testid) %>%
    summarise(count=n(),
              n.corr = sum(Stim.Type=="hit"),
              perc.corr = n.corr/count,
              mean.ttime = mean(TTime),
              sd.ttime = sd(TTime),
              se.ttime = sd.ttime/sqrt(count))


## make a plot with time on the x-axis and TTime on the y-axis
## showing the means and the 95\% confidence intervals (geom_pointrange())
## hint: you can use operations inside aes()

require(ggplot2)

p1 <- ggplot(sumdf,aes(x=testid,
                 y=mean.ttime,
                 ymin=mean.ttime - 1.96*se.ttime,
                 ymax=mean.ttime + 1.96*se.ttime)) +
       geom_pointrange() +
       facet_wrap(~Subject)

## second variant

sumdf$conlow <- sumdf$mean.ttime - 1.96 * sumdf$se.ttime
sumdf$conup <- sumdf$mean.ttime + 1.96 * sumdf$se.ttime

require(ggplot2)
p1 <- ggplot(sumdf,aes(x=testid,
                 y=mean.ttime,
                 ymin=conlow,
                 ymax=conup)) +
       geom_pointrange() +
       facet_wrap(~Subject)


## add the number of trials and the percentage of
## correct ones using geom\_text()

p1 + geom_text(y=0,aes(label=n.corr),angle=90,size=3) +
    geom_text(y=60000,aes(label=round(perc.corr,2)),angle=90,size=3) +
    scale_y_continuous(limits=c(0,75000))


require(stringr)
p1 + geom_text(y=0, aes(label=n.corr),size=4) +
    geom_text(y=70000,
              aes(label=str_pad(round(perc.corr,2),width = 4,side = "right",pad=0)),
              angle=90,hjust=1,size=4) +
    scale_y_continuous(limits=c(0,70000))


## Write a function which takes a vector, the poulation standard deviation
## and the population mean as arguments
## and which gives the Z score as result. 


ztest <- function(x,x.sd,mu=0){
    sqrt(length(x)) * (mean(x)-mu)/x.sd
}

set.seed(1)
ztest(rnorm(100),x.sd = 1)

## add a line to your function that allows you to also process numeric
## vectors containing missing values!

ztest <- function(x,x.sd,mu=0){
    x <- x[!is.na(x)]
    if(length(x) < 3) stop("too few values in x")
    sqrt(length(x)) * (mean(x)-mu)/x.sd
}

set.seed(1)
ztest(rnorm(100),x.sd = 1)

## the function pnorm(Z) gives the probability of x <= Z. Change your
## function so has the p-value as result.

ztest <- function(x,x.sd,mu=0){
    x <- x[!is.na(x)]
    if(length(x) < 3) stop("too few values in x")
    z <- sqrt(length(x)) * (mean(x)-mu)/x.sd
    2*pnorm(-abs(z))
}

set.seed(1)
ztest(rnorm(100),x.sd = 1)

## now let the result be a named vector containing the estimated
## difference, Z, p and the n.


ztest <- function(x,x.sd,mu=0){
    x <- x[!is.na(x)]
    if(length(x) < 3) stop("too few values in x")
    est.diff <- mean(x)-mu
    z <- sqrt(length(x)) * (est.diff)/x.sd
    c(diff=est.diff,Z=z,pval=2*pnorm(-abs(z)),n=length(x))
}

set.seed(1)
ztest(rnorm(100),x.sd = 1)

######################################################################
####################### Simulation exerc #############################
######################################################################

## Now sample 100 values from a Normal distribution with mean 10
## and standard deviation 2 and use a z-test to compare it against
## the population mean 10. What is the p-value?

ztest(rnorm(100,mean=10,sd=2),x.sd=2,mu=10)["pval"]
ztest(rnorm(100,mean=10,sd=2),x.sd=2,mu=10)["diff"]
ztest(rnorm(100,mean=10,sd=2),x.sd=2,mu=10)[c("pval","diff")]



## Now do the sampling and the testing 1000 times, what would be the
## number of statistically significant results? Use replicate()
## (which is a wrapper of tapply()) or a for() loop! Record at
## least the p-values and estimated differences! Transform the
## result into a data frame.

### replicate()
res <- replicate(1000, ztest(rnorm(100,mean=10,sd=2),x.sd=2,mu=10))
res <- as.data.frame(t(res))


### replicate(,simplify=F) 
res <- replicate(1000, ztest(rnorm(100,mean=10,sd=2),x.sd=2,mu=10),simplify = F)
res <- as.data.frame(Reduce(rbind,res))


## for() loop
res <- matrix(numeric(2000),ncol=2)
for(i in seq.int(1000)){
    res[i,] <- ztest(rnorm(100,mean=10,sd=2),x.sd=2,mu=10)[c("pval","diff")] }
res <- as.data.frame(res)
xnames(res) <- c("pval","diff")

## Use table() to count the p-vals below 0.05.
table(res$pval < 0.05)

## What is the smallest absolute difference with a p-value below 0.05?
tapply(abs(res$diff),res$pval < 0.05,summary)
min(abs(res$diff[res$pval<0.05]))

## Repeat the last exercise, only change the sample size to 1000 in each of the 1000 samples! How many p-value below 0.05? What is now the smallest absolute difference with a p-value below 0.05?
res2 <- replicate(1000, ztest(rnorm(1000,mean=10,sd=2),
                             x.sd=2,mu=10))
res2 <- as.data.frame(t(res2))

## Use table() to count the p-vals below 0.05.
table(res2$pval < 0.05)

## What is the smallest absolute difference with a p-value below 0.05?
tapply(abs(res2$diff),res$pval < 0.05,summary)

## Concatenate the both resulting data frames from above using rbind()
res <- rbind(res,res2)

## Plot the distributions of the pvals and the difference per
## sample size. Use \texttt{ggplot2} with an appropriate
## geom (density/histogram)


require(ggplot2)
ggplot(res,aes(x=pval)) +
    geom_histogram(bin=0.1,fill="forestgreen") +
    facet_grid(~ n)

ggsave("hist.png")

ggplot(res,aes(x=diff,colour=factor(n))) +
    geom_density(size=3)

ggsave("dens.png")


ggplot(res,aes(x=diff,y=pval)) +
    geom_point() +
    geom_hline(yintercept=0.05) +
    facet_grid(~ n)

ggsave("point.png")

ggplot(res,aes(x=diff,y=pval)) +
    geom_hex() +
    geom_hline(yintercept=0.05) +
    facet_grid(~ n)

ggsave("dens2d.png")

######################################################################
####################### T-Test #######################################
######################################################################


## one sample
set.seed(1)
x <- rnorm(12)

t.test(x,mu=0)

t.test(x,mu=1)


## two sample Welch or Satterthwaite test 
set.seed(1)
x <- rnorm(12)
y <- rnorm(12)
g <- sample(c("A","B"),12,replace = T)
t.test(x, y)
t.test(x ~ g)
t.test(x, y, var.equal = T)


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

ggplot(res,aes(x=perc.corr,y=mean.group.1 - mean.group.2)) +
    geom_point() +
    geom_smooth(se=F) +
    geom_smooth(data = res[res$perc.corr < 0.65,],se=F, method="lm",colour="black") +
    geom_smooth(data = res[res$perc.corr > 0.65,],se=F, method="lm",colour="black") +
    annotate("rect",xmin=0.65,xmax=Inf,ymin=-Inf,ymax = Inf,fill="blue",alpha=0.1) + annotate("segment",x=0.65,xend=0.65,y=-20000,yend=-5000,size=3,arrow=arrow())

