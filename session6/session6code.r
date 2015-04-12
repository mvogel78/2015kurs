
load("guinea.rdata")

## for plotting
require(reshape2)
require(tidyr)
dl <- melt(guinea,id.vars = c("Dose","Animal"))
dl$variable <- extract_numeric(dl$variable)
dl$group <- factor(dl$Dose)



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


## a multivariate linear model for repeated-measures data
## and between-S effect
(mod2 <- lm(cbind(X1,X3,X4,X5,X6,X7) ~ Dosef,data=guinea))
idata <- data.frame(time)
(av2.mod2 <- Anova(mod2, idata = idata, idesign = ~time))

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

