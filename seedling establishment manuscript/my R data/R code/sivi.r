sivi<-read.csv('SIVI4r.csv')

	#### Establishment ####

attach(sivi)

sivi.matrix<-cbind(SIVI2010, Site, G2010, F2010, Veg2010,
	M2010, B2010, D2010, LD2010)

sivi.matrix<-as.data.frame(sivi.matrix)
pairs(sivi.matrix)

dim(sivi)

anova(lm(SIVI2010~Site))
summary(lm(SIVI2010~Site))

summary(lm(SIVI2010~Site+G2010))
summary(lm(SIVI2010~Site+F2010))
summary(lm(SIVI2010~Site+LD2010))
summary(lm(SIVI2010~Site+M2010))
summary(lm(SIVI2010~Site+Veg2010))
summary(lm(SIVI2010~Site+D2010))
summary(lm(SIVI2010~Site+B2010))


sivi.bf<-sivi[which(sivi$Site=='Bellfountain'),]
dim(sivi.bf)
detach(sivi.bf)
attach(sivi.pb)de

summary(lm(SIVI2010~G2010))
summary(lm(SIVI2010~F2010))
summary(lm(SIVI2010~LD2010))
summary(lm(SIVI2010~M2010))
summary(lm(SIVI2010~Veg2010))
summary(lm(SIVI2010~D2010))
summary (lm(SIVI2010~B2010))

sivi.AIC<-lm(SIVI2010~G2010+F2010+Veg2010+M2010+B2010+D2010+LD2010)
step(sivi.AIC, direction = c("both"))

summary(lm(SIVI2010 ~ G2010 + F2010 + B2010 + D2010))

detach(sivi.pb)

###Survival###

attach(sivi)


detach(sivi)
attach(sivi.bf)
t.test(Observed.survival, Max.survival)
anova(lm(Observed.survival~Site))

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

sivi.fh<-sivi[which(sivi$Site=='Ft. Hoskins'),]
dim(sivi.fh)
detach(sivi.pb)
attach(sivi.fh)

t.test(Max.survival, Observed.survival)
summary(lm(Observed.survival~G2011))
summary(lm(Observed.survival~F2011))
summary(lm(Observed.survival~M2011))
summary(lm(Observed.survival~B2011))
summary(lm(Observed.survival~D2011))	
summary(lm(Observed.survival~L2011))
summary(lm(Observed.survival~LD2011))
summary(lm(Observed.survival~Vole))	
summary(lm(Observed.survival~Veg2011))

sivi.fh.AIC<-lm(Observed.survival~G2011+F2011+Veg2011+M2011+
	B2011+D2011+LD2011+L2011+Vole)
step(sivi.fh.AIC, direction = c("both"))
	
	summary(lm(  Max.survival ~ G2011 + F2011 + M2011 + LD2011 +     L2011 + Vole))
plot(lm( Max.survival ~ D2011 + Vole))

detach(sivi.fh)

#######

attach(sivi)

sivi.matrix<-cbind(Observed.survival, Site, G2011, F2011, Veg2011,
	M2011, B2011, D2011, LD2011, L2011, Vole)

sivi.matrrix<-as.data.frame(sivi.matrix)
pairs(sivi.matrix)

plot(Observed.survival~Site)

########

summary(lm(Max.survival~Site+G2011))
summary(lm(Max.survival~Site+F2011))
summary(lm(Max.survival~Site+M2011))
summary(lm(Max.survival~Site+B2011))
summary(lm(Max.survival~Site+D2011))	
summary(lm(Max.survival~Site+L2011))
summary(lm(Max.survival~Site+LD2011))
summary(lm(Max.survival~Site+Vole))	
summary(lm(Max.survival~Site+Veg2011))

summary(lm(Max.survival~G2011))
summary(lm(Max.survival~F2011))
summary(lm(Max.survival~M2011))
summary(lm(Max.survival~(log(M2011+.0001))))
summary(lm(Max.survival~B2011))
summary(lm(Max.survival~D2011))	
summary(lm(Max.survival~L2011))
summary(lm(Max.survival~LD2011))
summary(lm(Max.survival~Vole))	
summary(lm(Max.survival~Veg2011))


summary(lm(Observed.survival~Site+G2011))
summary(lm(Observed.survival~Site+F2011))
summary(lm(Observed.survival~Site+M2011))
summary(lm(Observed.survival~Site+(log
	(M2011+0.001))))
summary(lm(Observed.survival~Site+B2011))
summary(lm(Observed.survival~Site+D2011))	
summary(lm(Observed.survival~Site+L2011))
summary(lm(Observed.survival~Site+LD2011))
summary(lm(Observed.survival~Site+Vole))	
summary(lm(Observed.survival~Site+Veg2011))


sivi.fh.AIC<-lm(Max.survival~G2011+F2011+Veg2011+M2011+
	B2011+D2011+LD2011+L2011+Vole)
step(sivi.fh.AIC, direction = c("both"))

######## Community components ##########

sivi.pb<-sivi[which(sivi$Site=='Pigeon Butte'),]

detach(sivi.pb)
attach(sivi.pb)
 
summary(sivi.bf)




