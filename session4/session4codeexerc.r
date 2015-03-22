setwd("/media/mandy/Volume/transcend/mpicbs/2015kurs/session4")

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



