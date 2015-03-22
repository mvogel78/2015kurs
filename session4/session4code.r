setwd("/media/mandy/Volume/transcend/mpicbs/2015kurs/session4")

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
names(res) <- c("pval","diff")

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



