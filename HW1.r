##1
soap=c(rep("Reg",4),rep("Deo",4),rep("Moist",4))
soap
weight=c(-0.30,-0.10,-0.14,0.40,2.63,2.61,2.41,3.15,1.86,2.03,2.26,1.82)
weight
weight.lost=data.frame(soap,weight)
weight.lost

##2
weight=c(-0.30,-0.10,-0.14,0.40,2.63,2.61,2.41,3.15,1.86,2.03,2.26,1.82)*0.001
weight
weight.lost=data.frame(soap,weight)
weight.lost

##3
mean(weight)
sd(weight)
mean(weight[soap=="Reg"])
sd(weight[soap=="Reg"])
mean(weight[soap=="Deo"])
sd(weight[soap=="Deo"])
mean(weight[soap=="Moist"])
sd(weight[soap=="Moist"])

##4
hist(weight,main="Histogram of Weight Loss (in kilograms)")

##5
boxplot(weight~soap,data=weight.lost,
        main="Boxplot of Weight Loss With Different Soaps",
        xlab="Soap",ylab="Weight Lost (kg)")
