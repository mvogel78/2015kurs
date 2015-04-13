load("guinea.rdata")

## for plotting
require(reshape2)
require(tidyr)
dl <- melt(guinea,id.vars = c("Dose","Animal"))
dl$variable <- extract_numeric(dl$variable)
dl$group <- factor(dl$Dose)
dl$Animal <- factor(dl$Animal)


require(ggplot2)
ggplot(dl,aes(x=variable,y=value,colour=group)) +
    geom_boxplot(aes(x=variable,group=factor(variable))) +
    stat_summary(fun.y="mean",geom = "line") 


## model
guinea$Dosef <- factor(guinea$Dose)

### Basic multivariate linear model
(mod <- lm(cbind(X1,X3,X4,X5,X6,X7) ~ 1,data=guinea))

## test
require(car)
(av <- Anova(mod))
summary(av)

## extract H and E
av$SSP
av$SSPE

## HE plot
require(heplots)
heplot(mod,terms = "(Intercept)")
points(mean(dl$value),mean(dl$value))

savePlot("img/rma2.png")

## testing within-S effects
idata <- data.frame(time=factor(c(1,3:7)))
(av2 <- Anova(mod, idata = idata, idesign = ~time))
summary(av2)

### another test
(av2 <- Anova(mod, test="Wilks", idata = idata, idesign = ~time))

### 
source("functions.r")
get.p.vals.all(av2)


## a multivariate linear model for repeated-measures data
## and between-S effect
(mod2 <- lm(cbind(X1,X3,X4,X5,X6,X7) ~ Dosef,data=guinea))
(av2.mod2 <- Anova(mod2, idata = idata, idesign = ~time))

get.p.vals.all(av2.mod2)

## eigenvalues E-1H
Re(eigen(solve(av$SSPE) %*% av$SSP[[1]])$values)

Re(eigen(solve(av2.mod2$SSPE$time) %*% av2.mod2$SSP$time)$values)

## Mixed Models
dl$variable <- factor(dl$variable)

require(nlme)
Lme.mod <- lme(value ~ group * variable, random = ~ 1 | Animal, data = dl)
anova(Lme.mod)

require(lme4)
Lme.mod <- lmer(value ~ group * variable + (1 | Animal), data = dl)
anova(Lme.mod)

require(multcomp)
summary(glht(Lme.mod, linfct=mcp(group="Tukey")))

## consider interaction terms
summary(glht(Lme.mod, linfct=mcp(group="Tukey",interaction_average = TRUE)))

## without interaction term
Lme.mod <- lmer(value ~ group + variable + (1 | Animal), data = dl)
anova(Lme.mod)

summary(glht(Lme.mod, linfct=mcp(group="Tukey")))

######################################################################
############################# Exercises ##############################
######################################################################

## use the guinea data
## try to set up an unconditional means model using lme or lmer from
## the respective package. Extract the coefficients using coef()
## interpret the results.
## add time as fixed effect.
## add time as random effect. Again: extract the coefficients












## in the help pages of Anova() you find a example of repeated
## measurement anova. Redo this example.
## which effects are significant? What would you remove from the
## model...
















####################################################################
#######################  ggplot 2 themes ###########################
####################################################################

## text
p1 <- ggplot(dl,aes(x=variable,y=value,colour=group)) +
    geom_boxplot(aes(x=variable,group=factor(variable))) +
    stat_summary(fun.y="mean",geom = "line") +
    ggtitle("Plot title")



p1 + theme(
    title = element_text(colour="red", face="bold", angle = 10),
    axis.title.y = element_text(colour="blue", face="italic",size=25)
)


ggsave("img/ggtext.png")

p1 + theme(
    axis.line = element_line(colour="green", linetype = 3, size = 3)
)

ggsave("img/ggline.png")

p1 + theme(
    axis.line = element_line(colour="green", linetype = 3, size = 3),
    axis.ticks = element_line(colour="red", linetype = 3, size = 3),
    panel.grid.major = element_line(colour="deeppink", linetype = 2, size = 1)
)

ggsave("img/ggline2.png")

p1 + theme(
    legend.background = element_rect(fill = "darkslategray1"),
    panel.background = element_rect(fill = "darkslategray3"),
    plot.background =  element_rect(fill = "darkslateblue")
)

ggsave("img/ggrect.png")

require(grid) ## for unit
p1 + theme(
    axis.ticks = element_line(colour="red", linetype = 2, size = 1),
    axis.ticks.length = unit(1,"cm"),
    axis.ticks.margin = unit(1,"cm"),
    legend.position = c(0.5,0.5),
    legend.background = element_rect(fill="transparent"),
    legend.key.size = unit(2,"cm")
)

ggsave("img/ggother.png")

#####################################################################
############### Exercises dplyr, ggplot #############################
#####################################################################

## download the data from the nih:

brfss <- read.csv("http://watson.nci.nih.gov/~sdavis/tutorials/IntroToR/BRFSS-subset.csv")

## use dplyr to create a new data frame summarising Age, Height, Weight
## per Year and Sex, calculate means, min, max, medians, number of
## observations, number of missing ages











## calculate BMI in the brfss data set 
## make an appropriate plot to compare Weight, Height and Age of
## the sample per Year and sex
## hint: maybe you want transform the data before plotting
## using melt() from the reshape2 package












## customize the plot
## try to remove all elemets except the boxplot and the text
## elements











## change the colour scale (if you used one)
