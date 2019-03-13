Brand=c(rep("Advantage",10),rep("Target",10))
Size=c(rep(c(rep("Thin", 5),rep("Thick", 5)),2))
Length=c(55.3,55.6,57.7,57.7,57,62.4,68.9,65.3,61.5,63.6,
         51.2,52.3,54.6,57.2,59.6,63.2,65.2,60,61.7,64.3)
data=data.frame(Brand,Size,Length)
data

model1=aov(Length~Brand+Size+Brand:Size, data=data)

fitted=fitted(model1)
resid=residuals(model1)
plot(fitted,resid,main="Fitted vs Residuals",pch=1,cex=2)
qqnorm(resid)
qqline(resid)

library(car)
Anova(model1,type="III")

interaction.plot(x.factor = Size, trace.factor = Brand,
                 response = Length, type ="b",col = 2:5,
                 xlab ="Size", ylab ="Mean Breaking Length (cm)", trace.label ="Brand")

library(lsmeans)
lsm.Size=lsmeans(model1,~Size)
contrast(lsm.Size,method="pairwise")
cld(lsm.Size)