widgets=read.table("widgets.txt",header=TRUE)
Batch=as.factor(widgets$Batch)
Supplier=widgets$Supplier
WidgetSize=widgets$WidgetSize
df<-data.frame(Batch=as.factor(widgets$Batch), Supplier=widgets$Supplier, WidgetSize=widgets$WidgetSize)
df

library(lsmeans)
library(multcompView)
library(car)
options(contrasts =c("contr.sum", "contr.poly"))

model1<-aov(WidgetSize~Supplier+Batch+Batch:Supplier, data=df)
anova(model1)

par(mfrow=c(2,2))
plot(model1)

lsm.batch=lsmeans(model1,~Batch)
cld(lsm.batch)

lsm.diff=lsmeans(model1,~Batch:Supplier)
cld(lsm.diff)
