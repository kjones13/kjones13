sivi<- read.csv('sivigerm.csv')
attach(sivi)
names(sivi)

sivi.ns<-sivi[which(sivi$scarified=='n'),]
sivi.s<-sivi[which(sivi$scarified=='y'),]

plot(propgerm ~ cold)

site.code<-ifelse(scarified=="n",1,0)

plot((propgerm*100) ~ cold, ylim=c(0,70), pch=(site.code+16), col=(site.code+3), 
	xlab= "Weeks Cold", ylab="Germination (%)")

	scar<-sivi[which(sivi$scarified=='y'),]
	lines(lowess(scar$cold, scar$propgerm*100), col="green", lwd=3)

	noscar<-sivi[which(sivi$scarified=='n'),]
	lines(lowess(noscar$cold, noscar$propgerm*100), col="blue", lwd=3)

fit1<-aov(propgerm~scarified*cold)
summary(fit1)

fit1<-aov(propgerm~treatment)
TukeyHSD(fit1)
plot(TukeyHSD(fit1, "treatment"))
plot(TukeyHSD(fit1, "treatment", ordered=TRUE))