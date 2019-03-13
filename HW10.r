library(lsmeans)
library(car)
library(multcompView)
library(lme4)
library(lmerTest)
options(contrasts =c("contr.sum", "contr.poly"))

##1
cars <- mtcars[,c("am","mpg","hp")]
head(cars)

fit.ANCOVA=aov(mpg~am+hp+hp:am,data=cars)

par(mfrow=c(1,1))
plot(fit.ANCOVA)

anova(fit.ANCOVA)
library(jtools)
interact_plot(fit.ANCOVA,pred="hp",modx="am")

##2
hsb2=read.table("hsb2.csv")
head(hsb2)

fit.ANCOVA=aov(math~ses+schtyp+ses:schtyp+write+write:ses+write:schtyp+write:ses:schtyp,data=hsb2)
fit.ANCOVA=aov(math~ses+schtyp+write,data=hsb2)

par(mfrow=c(1,1))
plot(fit.ANCOVA)

anova(fit.ANCOVA)

library(jtools)
interact_plot(fit.ANCOVA,pred="write",modx="ses")

