luor<-read.csv('LUOR4r.csv')
attach(luor)


summary(lm(LUOR2010~Site+Scarified+LD2010))
anova(lm(LUOR2010~Site+Scarified+LD2010))

anova(lm(LUOR2010~Site))
anova(lm(LUOR2010~Site+Scarified))

summary(lm(Max.survival~Site+D2011+Veg2011))
summary(lm(Max.survival~Site+Scarified))

###Scarified###

	#### Establishment ####
detach(luor)
luor.s<-luor[which(luor$Scarified=='y'),]

attach(luor.s)

luor.s.matrix<-cbind(LUOR2010, Site, G2010, F2010, Veg2010,
	M2010, B2010, D2010, LD2010)

luor.s.matrix<-as.data.frame(luor.s.matrix)
pairs(luor.s.matrix)

dim(luor.s)  ## 60 rows = only scarified data

summary(lm(LUOR2010~Site+B2010))


anova(lm(LUOR2010~Site))
anova(lm(Max.survival~Site+G2011))
anova(lm(Max.survival~Site+F2011))
anova(lm(Max.survival~Site+LD2011))
anova(lm(Max.survival~Site+B2011))
anova(lm(Max.survival~Site+M2011))
summary(lm(Max.survival~Site+Veg2011))
anova(lm(Max.survival~Site+D2011))

luor.s.AIC<-lm(LUOR2010~G2010+F2010+Veg2010+M2010+B2010+D2010+LD2010)
step(luor.s.AIC, direction = c("both"))

summary(lm(LUOR2010~F2010+LD2010))
summary(lm(LUOR2010~Site+F2010+LD2010))

	###Survival###

t.test(Observed.survival, Max.survival)
anova(lm(Max.survival~Site))

summary(lm(Max.survival~ Site + G2011))
	plot(Max.survival~G2011)
	bf.survival<-lm(Max.survival ~ G2011,subset=(Site=="Bellfountain"))
	abline(bf.survival,col="blue", lwd=2 )

	pb.survival<-lm(Max.survival ~ G2011, subset=(Site=="Pigeon Butte"))
	abline(pb.survival, col="cyan", lwd=2, lty=2)

	fh.survival<-lm(Max.survival ~ G2011, subset=(Site=="Ft. Hoskins"))
	abline(fh.survival, col="green", lwd=3, lty=3)

summary(lm(Max.survival~ Site + F2011))
	plot(Max.survival~Site+F2011)

	bf.survival<-lm(Max.survival ~ F2011,subset=(Site=="Bellfountain"))
	abline(bf.survival,col="blue", lwd=2 )

	pb.survival<-lm(Max.survival ~ F2011, subset=(Site=="Pigeon Butte"))
	abline(pb.survival, col="cyan", lwd=2, lty=2)

	fh.survival<-lm(Max.survival ~ F2011, subset=(Site=="Ft. Hoskins"))
	abline(fh.survival, col="green", lwd=3, lty=3)

summary(lm(Max.survival~Site+G2011*F2011))
anova(lm(Max.survival~Site+D2011+Veg2011))

summary(lm(LUOR2010~Site+G2010*F2010))

##Bellfountain, scarified only###

luor.s.bf<-luor.s[which(luor.s$Site=='Bellfountain'),]
dim(luor.s.bf)
detach(luor.s)
attach(luor.s.bf)

t.test(Max.survival, Observed.survival)
summary(lm(Max.survival~G2011))
summary(lm(Max.survival~F2011))
summary(lm(Max.survival~M2011))
	plot(Max.survival~M2011)
	abline(lm(Max.survival~M2011))
	plot(lm(Max.survival~M2011))
summary(lm(Max.survival~B2011))
summary(lm(Max.survival~D2011))	
	plot(Max.survival~D2011)
	abline(lm(Max.survival~D2011))
summary(lm(Max.survival~L2011))
summary(lm(Max.survival~LD2011))
summary(lm(Max.survival~Vole))	
summary(lm(Max.survival~Veg2011))

summary(lm(Max.survival~M2011*D2011))
	plot(Max.survival~M2011:D2011)
	abline(lm(Max.survival~M2011:D2011))
	abline(lm(Max.survival~M2011))
	abline(lm(Max.survival~D2011))



luor.s.bf.AIC<-lm(Max.survival~G2011+F2011+Veg2011+M2011+
	B2011+D2011+LD2011+L2011+Vole)
step(luor.s.bf.AIC, direction = c("both"))
	
	summary(lm(Max.survival ~ M2011 + B2011 + D2011 + LD2011))
plot(lm(Max.survival ~ M2011 + B2011 + D2011 + LD2011))

summary(lm(Max.survival~D2011+LD2011))

luor.s.bf.AIC<-lm(Max.survival~G2011*F2011*Veg2011*M2011*
	B2011*D2011*LD2011*L2011*Vole)
step(luor.s.bf.AIC, direction = c("both"))

detach(luor.s.bf)


#### Pigeon Butte #####

luor.s.pb<-luor.s[which(luor.s$Site=='Pigeon Butte'),]
dim(luor.s.pb)
attach(luor.s.pb)

t.test(Max.survival, Observed.survival)
summary(lm(Max.survival~G2011))
summary(lm(Max.survival~F2011))
summary(lm(Max.survival~M2011))
summary(lm(Max.survival~B2011))
summary(lm(Max.survival~D2011))	
summary(lm(Max.survival~L2011))
summary(lm(Max.survival~LD2011))
summary(lm(Max.survival~Vole))	
summary(lm(Max.survival~Veg2011))

luor.s.pb.AIC<-lm(Max.survival~G2011+F2011+Veg2011+M2011+
	B2011+D2011+LD2011+L2011+Vole)
step(luor.s.pb.AIC, direction = c("both"))

summary(lm(Max.survival ~ G2011 + M2011 + B2011 + D2011 + L2011 + Vole))

summary(lm(Max.survival~D2011+LD2011))

detach(luor.s.pb)

##### Ft. Hoskins #####

luor.s.fh<-luor.s[which(luor.s$Site=='Ft. Hoskins'),]
dim(luor.s.fh)
attach(luor.s.fh)

t.test(Max.survival, Observed.survival)
summary(lm(Max.survival~G2011))
summary(lm(Max.survival~F2011))
summary(lm(Max.survival~M2011))
summary(lm(Max.survival~B2011))
summary(lm(Max.survival~D2011))	
summary(lm(Max.survival~L2011))
summary(lm(Max.survival~LD2011))
summary(lm(Max.survival~Vole))	
summary(lm(Max.survival~Veg2011))

summary(lm(Max.survival~D2011+LD2011))
summary(lm(Max.survival~D2011*LD2011))

luor.s.fh.AIC<-lm(Max.survival~G2011+F2011+Veg2011+M2011+
	B2011+D2011+LD2011+L2011+Vole)
step(luor.s.fh.AIC, direction = c("both"))

summary(lm(Max.survival ~ D2011 + LD2011))
summary(lm(Max.survival~ D2011*LD2011))
plot(Max.survival~LD2011)
abline(lm(Max.survival~D2011:LD2011))
abline(lm(Max.survival~D2011))
abline(lm(Max.survival~LD2011))

detach(luor.s.fh)

#######

attach(luor.s)

luor.s.matrix<-cbind(Max.survival, Site, G2011, F2011, Veg2011,
	M2011, B2011, D2011, LD2011, L2011, Vole)

luor.s.matrix<-as.data.frame(luor.s.matrix)
pairs(luor.s.matrix)

boxplot(Max.survival~Site)



