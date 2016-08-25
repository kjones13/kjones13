irte<-read.csv('IRTE4r.csv')

###Survival###

attach(irte)


detach(IRTE)
attach(IRTE.bf)
t.test(Survival, Max.survival)
anova(lm(Survival~Site))

summary(lm(Max.survival~ Site + G2011))
	bf.survival<-lm(Max.survival ~ G2011,subset=(Site=="Bellfountain"))
	abline(bf.survival,col="blue", lwd=2 )

	pb.survival<-lm(Max.survival ~ G2011, subset=(Site=="Pigeon Butte"))
	abline(pb.survival, col="cyan", lwd=2, lty=2)

	fh.survival<-lm(Max.survival ~ G2011, subset=(Site=="Ft. Hoskins"))
	abline(fh.survival, col="green", lwd=3, lty=3)

summary(lm(Max.survival~ Site + F2011)
	plot(Max.survival~Site+F2011)

	bf.survival<-lm(Max.survival ~ F2011,subset=(Site=="Bellfountain"))
	abline(bf.survival,col="blue", lwd=2 )

	pb.survival<-lm(Max.survival ~ F2011, subset=(Site=="Pigeon Butte"))
	abline(pb.survival, col="cyan", lwd=2, lty=2)

	fh.survival<-lm(Max.survival ~ F2011, subset=(Site=="Ft. Hoskins"))
	abline(fh.survival, col="green", lwd=3, lty=3)

#####

irte.pb<-irte[which(irte$Site=='Pigeon Butte'),]
dim(IRTE.bf)
detach(irte)
attach(irte.pb)

summary(lm(Survival~Site+G2011))
summary(lm(Survival~Site+F2011))
summary(lm(Survival~Site+M2011))
summary(lm(Survival~Site+B2011))
summary(lm(Survival~Site+D2011))	
summary(lm(Survival~Site+L2011))
summary(lm(Survival~Site+LD2011))
summary(lm(Survival~Site+Vole))	
summary(lm(Survival~Site+Veg2011))

summary(lm(MaxLeafLength~Site+G2011))
summary(lm(MaxLeafLength~Site+F2011))
summary(lm(MaxLeafLength~Site+M2011))
summary(lm(MaxLeafLength~Site+B2011))
summary(lm(MaxLeafLength~Site+D2011))	
summary(lm(MaxLeafLength~Site+L2011))
summary(lm(MaxLeafLength~Site+LD2011))
summary(lm(MaxLeafLength~Site+Vole))	
summary(lm(MaxLeafLength~Site+Veg2011))

IRTE.bf.AIC<-lm(Survival~G2011+F2011+M2011+
	B2011+D2011+LD2011+L2011+Vole)
step(IRTE.fh.AIC, direction = c("both"))
	
	summary(lm(  Max.survival ~ G2011 + F2011 + M2011 + LD2011 +     L2011 + Vole))
plot(lm( Max.survival ~ D2011 + Vole))

detach(IRTE.bf)

#######

attach(IRTE)

IRTE.matrix<-cbind(Survival2, Site, G2011, F2011, Veg2011,
	M2011, B2011, D2011, LD2011, L2011, Vole)

IRTE.matrix<-as.data.frame(IRTE.s.matrix)
pairs(IRTE.matrix)

boxplot(Survival~Site, ylab= "Survival (%)")


