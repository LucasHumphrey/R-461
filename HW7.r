Fert<-c(rep("control", 12),rep("f1", 12),
        rep("f2", 12),rep("f3", 12))
Species<-c(rep(c(rep("SppA", 6),rep("SppB", 6)),4))
Height<-c(21.0, 19.5, 22.5, 21.5, 20.5, 21.0,
          23.7, 23.8, 23.8, 23.7, 22.8, 24.4,
          32.0, 30.5, 25.0, 27.5, 28.0, 28.6,
          30.1, 28.9, 30.9, 34.4, 32.7, 32.7,
          22.5, 26.0, 28.0, 27.0, 26.5, 25.2,
          30.6, 31.1, 28.1, 34.9, 30.1, 25.5,
          28.0, 27.5, 31.0, 29.5, 30.0, 29.2,
          36.1, 36.6, 38.7, 37.1, 36.8, 37.1)
df<-data.frame(Fert=Fert, Species=Species, Height=Height)

modelAB<-aov(Height~Fert+Species+Fert:Species, data=df)
library(car)
Anova(modelAB,type="III")

par(mfrow=c(2,2))
plot(modelAB)

interaction.plot(x.factor = df$Species, trace.factor = df$Fert,
                 response = df$Height, type ="b",col = 2:5,
                 xlab ="Species", ylab ="Mean", trace.label ="Fertilizer")

lsm.Fert=lsmeans(modelAB,~Fert)
contrast(lsm.Fert,method="pairwise")
cld(lsm.Fert)

lsm.Species=lsmeans(modelAB,~Species)
contrast(lsm.Species,method="pairwise")
cld(lsm.Species)

library(multcompView)
lsminter=lsmeans(modelAB, ~ Fert:Species )
cld(lsminter, alpha=0.05)



##2
A<-c(rep("1", 10),rep("2", 10))
B<-rep(c(c(rep("1", 5),rep("2", 5))), 2)
resp<-c(12.9, 11.3, 11.7, 12.1, 12.3,
        13.7, 12.8, 13.6, 13.1, 13.5,
        14.2, 14.5, 13.9, 13.6, 14.4,
        13.5, 13.1, 13.3, 13.1, 13.4)
df<-data.frame(A=A, B=B, resp=resp)

modelAB<-aov((resp)^2~A+B+A:B, data=df)
Anova(modelAB,type="III")

par(mfrow=c(2,2))
plot(modelAB)

interaction.plot(x.factor = df$B, trace.factor = df$A,
                 response = df$resp, type ="b",col = 2:5,
                 xlab ="Factor B", ylab ="Mean", trace.label ="Factor A")

lsminter=lsmeans(modelAB, ~ A:B )
cld(lsminter, alpha=0.05)

lsm.FactorA=lsmeans(modelAB,~A)
contrast(lsm.FactorA,method="pairwise")
cld(lsm.FactorA)

lsm.FactorB=lsmeans(modelAB,~B)
contrast(lsm.FactorB,method="pairwise")
cld(lsm.FactorB)