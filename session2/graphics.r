require(ggplot2)
po <- ggplot()
summary(po)

str(po)

x1 <- 1:10; y1 <- 1:10; z1 <- 10:1
l1 <- LETTERS[1:10]
a <- 10; b <- (0:-9)/10:1
ex <- data.frame(x=x1,y=y1,z=z1,l=l1,a=a,b=b)


p <- ggplot(ex,aes(x=x1,y=y1))
summary(p)


p1 <- p + geom_point()
