type<-c("AlkName","AlkName","AlkName","AlkName","AlkGen","AlkGen","AlkGen","AlkGen",
        "HDName","HDName","HDName","HDName","HDGen","HDGen","HDGen","HDGen")
life<-c(100.668, 77.734, 79.210, 95.063, 206.880, 153.347, 165.980, 196.000,
        14.951, 18.063, 11.111, 12.840, 15.340, 22.090, 15.734, 14.440)
batt<-data.frame(type=type, life=life)
batt

fit1<-aov(life~type, data=batt)
summary(fit1)

par(mfrow=c(2,2))
plot(fit1)

##3
batt$SqrtLife<-sqrt(life)
fit2<-aov(SqrtLife~type, data=batt)
summary(fit2)

par(mfrow=c(2,2))
plot(fit2)

model1<-aov(SqrtLife~type, data = batt)
anova(model1)

library(lsmeans)
lsmType=lsmeans(model1,~type)
summary(contrast(lsmType, method="pairwise", adjust="tukey"),
        infer=c(T,T), level=0.95, side="two-sided")

##4
batt$LogLife<-log(life)
fit3<-aov(LogLife~type, data=batt)
summary(fit3)

par(mfrow=c(2,2))
plot(fit3)

##5
batt$SqrLife<-(life)^2
fit4<-aov(SqrLife~type, data=batt)
summary(fit4)

par(mfrow=c(2,2))
plot(fit4)


##8
type<-c(rep("Beef",20),rep("Pork",17),rep("Chicken",17))
calories<-c(186,181,176,149,184,190,158,139,175,148,152,111,141,153,190,157,131,149,135,132,
            173,191,182,190,172,147,146,139,175,136,179,153,107,195,135,140,138,
            129,132,102,106,94,102,87,99,107,113,135,142,86,143,152,146,144)
hotdog<-data.frame(type=type, calories=calories)

fit5<-aov(calories~type, data=hotdog)
summary(fit5)

par(mfrow=c(2,2))
plot(fit5)

hotdog$InvCalories<-1/(calories)
fit6<-aov(InvCalories~type, data=hotdog)
summary(fit6)

par(mfrow=c(2,2))
plot(fit6)

model2<-aov(InvCalories~type, data = hotdog)
anova(model2)

library(lsmeans)
lsmType=lsmeans(model2,~type)
summary(contrast(lsmType, method="pairwise", adjust="tukey"),
        infer=c(T,T), level=0.95, side="two-sided")