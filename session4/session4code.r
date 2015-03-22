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
    round(c(diff=est.diff,Z=z,pval=2*pnorm(-abs(z)),n=length(x)),4)
}

set.seed(1)
ztest(rnorm(100),x.sd = 1)


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



