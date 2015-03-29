setwd("/media/mandy/Volume/transcend/mpicbs/2015kurs/session4")

## load the data (file: session4data.rdata)
## make a new summary data frame (per subject and time) containing:
###### the number of trials
###### the number correct trials (absolute and relative)
###### the mean TTime and the standard deviation of TTime
###### the respective standard error of the mean
##  keep the information about Sex and Age\_PRETEST














## make a plot with time on the x-axis and TTime on the y-axis
## showing the means and the 95\% confidence intervals (geom_pointrange())
## hint: you can use operations inside aes()













## add the number of trials and the percentage of
## correct ones using geom\_text()










## Write a function which takes a vector, the poulation standard deviation
## and the population mean as arguments
## and which gives the Z score as result. 


ztest <- function(x,x.sd,mu=0){


}

set.seed(1)
ztest(rnorm(100),x.sd = 1)

## add a line to your function that allows you to also process numeric
## vectors containing missing values!





set.seed(1)
ztest(rnorm(100),x.sd = 1)

## the function pnorm(Z) gives the probability of x <= Z. Change your
## function so has the p-value as result.







set.seed(1)
ztest(rnorm(100),x.sd = 1)

## now let the result be a named vector containing the estimated
## difference, Z, p and the n.






set.seed(1)
ztest(rnorm(100),x.sd = 1)

######################################################################
####################### Simulation exerc #############################
######################################################################

## Now sample 100 values from a Normal distribution with mean 10
## and standard deviation 2 and use a z-test to compare it against
## the population mean 10. What is the p-value?





## Now do the sampling and the testing 1000 times, what would be the
## number of statistically significant results? Use replicate()
## (which is a wrapper of tapply()) or a for() loop! Record at
## least the p-values and estimated differences! Transform the
## result into a data frame.









## Use table() to count the p-vals below 0.05.


## What is the smallest absolute difference with a p-value below 0.05?





## Repeat the last exercise, only change the sample size to 1000 in each of the 1000 samples! How many p-value below 0.05? What is now the smallest absolute difference with a p-value below 0.05?




## Use table() to count the p-vals below 0.05.


## What is the smallest absolute difference with a p-value below 0.05?


## Concatenate the both resulting data frames from above using rbind()


## Plot the distributions of the pvals and the difference per
## sample size. Use \texttt{ggplot2} with an appropriate
## geom (density/histogram)






















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







## now do the same for Subject 1 on pre and post test (use filter()
## or indexing to get the resp. subsets)








## use the following code to do the test on every subset Subject
## and testid, try to figure what is happening in each step:

data.l <- split(data,list(data$Subject,data$testid),drop=T)

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


























########################################################################
############## Exercises stats ggplot2    ##############################
########################################################################


require(Hmisc)
ggplot(data,aes(x=testid,y=TTime)) +
    stat_summary(fun.data="mean_se",mult=1.96,geom="pointrange") +
    stat_bin(y=0,aes(label=..count..),geom="text",position="identity") +
    scale_y_continuous(limits=c(0,75000)) +
    facet_wrap(~Subject)



