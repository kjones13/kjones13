field.data<- read.csv('Field.data4r.csv')
attach(field.data)

fit.grass<-lm(LUSUKI~ Site+Scarified + Av..GrassCover)
anova(fit.grass)
summary(fit.grass)

fit3.lm<-lm(LUSUKI~Site+Scarified+Av..GrassCover+Av..LitterDepth+
	Av..GrassCover:Av..LitterDepth)
summary(fit3.lm)
anova(fit3.lm)
confint(fit3.lm)

fit.litter<- lm(LUSUKI~ Site+Scarified+Av..LitterDepth)
anova(fit.litter)
summary(fit.litter)

fit.scar<- lm(LUSUKI~Site+Scarified)
summary(fit.scar)
anova(fit.scar)

####

plot(fit3.lm, which=4)

library(car)
win.graph()
cr.plots(lm(LUSUKI ~ Site+ Av..LitterDepth + Av..GrassCover +
	 Scarified, data=field.data),
variable=Av..LitterDepth,line=TRUE,smooth=FALSE)

win.graph()
cr.plots(lm(LUSUKI ~ Site+ Av..LitterDepth + Av..GrassCover +
	 Scarified, data=field.data),
variable=Scarified,line=TRUE,smooth=FALSE)

win.graph()
cr.plots(lm(LUSUKI ~ Site+ Av..LitterDepth + Av..GrassCover +
	 Scarified, data=field.data),
variable=Site,line=TRUE,smooth=FALSE)

scar.code<-ifelse(Scarified=="n",1,0)

win.graph()
plot(LUSUKI ~ Av..LitterDepth, pch=(scar.code +16),
	col=(scar.code+3),xlab="Litter Depth (cm)",
	ylab="% of established Lupine seedlings",
	main="Lupine Establishment vs. Litter Depth")

fit.scar<-lm(LUSUKI~Av..LitterDepth, subset=(Scarified=="n"))
abline(fit.scar,col="blue", lwd=2 )

fit.noscar<-lm(LUSUKI~Av..LitterDepth, subset=(Scarified=="y"))
abline(fit.noscar, col="green", lwd=2, lty=2)

legend("topright",legend=c("Unscarified Seeds","Scarified Seeds"),
	pch=c(17,16),
	col=c(4,3),cex=1)