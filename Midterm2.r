##1
cue=c(rep("auditory",9),rep("visual",9))
elapsed.time=rep(rep(1:3,each=3),2)
reaction.time=c(204,170,181,167,182,187,202,198,236,257,279,269,283,235,260,256,281,258)
data=data.frame(cue,elapsed.time,reaction.time)
data

model1=aov(reaction.time~cue+elapsed.time+cue:elapsed.time, data=data)
library(car)
Anova(model1,type="III")

par(mfrow=c(2,2))
plot(model1)

interaction.plot(x.factor = elapsed.time, trace.factor = cue,
                 response = reaction.time, type ="b",col = 2:5,
                 xlab ="Elapsed Time", ylab ="Mean Reaction Time", trace.label ="Cue")

library(lsmeans)
lsm.Cue=lsmeans(model1,~cue)
contrast(lsm.Cue,method="pairwise")
cld(lsm.Cue)

##2
Brand<-c(rep("1",10),rep("2", 10),rep("3",10),rep("4", 10))
Times<-c(167, 171, 178, 175, 184, 176, 185, 172, 178, 178,
         231, 233, 236, 252, 233, 225, 241, 248, 239, 248,
         176, 168, 171, 172, 178, 176, 169, 164, 169, 171,
         201, 199, 196, 211, 209, 223, 209, 219, 212, 210)
data<-data.frame(Brand,Times)
data

aov(Times~Brand)

model2<-aov(Times~Brand,data=data)
anova(model2)

library(lsmeans)
lsmBrand<-lsmeans(model2, ~ Brand)
summary(contrast(lsmBrand, method="pairwise", adjust="tukey"),
        infer=c(T,T), level=0.95, side="two-sided")

##3
wood=read.table("wood.csv",header=TRUE)
wood

options(contrasts = c("contr.sum", "contr.poly"))

model3=aov(Nconc~Type+Species:Type,data=wood)
anova(model3)

par(mfrow=c(2,2))
plot(model3)

library(lsmeans)
lsm.type=lsmeans(model3,~Type)
cld(lsm.type)

lsm.diff=lsmeans(model3,~Species:Type)
contrast(lsm.diff,method="pairwise")

##4
beans=read.table("Beans.csv",header=TRUE)
beans

options(contrasts = c("contr.sum", "contr.poly"))

model4=aov(Rating~SoakTim+Crock:SoakTim+Recipe+SoakTim:Recipe,data=beans)
Anova(model4,type="III")

library(lme4)
library(lmerTest)
model4=lmer(Rating~SoakTim+(1|Crock:SoakTim)+Recipe+SoakTim:Recipe,data=beans)
Anova(model4,type="III")
rand(model4)

difflsmeans(model4,"SoakTim")
difflsmeans(model4,"Recipe")
difflsmeans(model4,"SoakTim:Recipe")
