displays=read.table("displays.dat",header=TRUE)
displays$store=as.factor(displays$store)
displays$week=as.factor(displays$week)
displays

library(lsmeans)
library(car)
library(multcompView)
library(lme4)
library(lmerTest)
options(contrasts =c("contr.sum", "contr.poly"))

model=lmer(sales~store+(1|display:store)+(1|week:display), data=displays)
model

fitted=fitted(model)
resid=residuals(model)
plot(fitted,resid,main="Fitted vs Residuals",pch=1,cex=2)
qqnorm(resid)
qqline(resid)

Anova(model,type="III")
rand(model)
