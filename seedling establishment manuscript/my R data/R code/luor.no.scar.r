luor<-read.csv('LUOR4r.csv')
attach(luor)
summary(lm (Max.survival~LUOR2010))

factor(luor$treatment,c("control", "MM", "MBG", "SM", "SBG"))
as.integer(factor(luor$treatment,c("control", "MM", "MBG", "SM", "SBG")))
luor$treatment<-ordered(luor$treatment, levels=c("control", "MM", "MBG", "SM", "SBG"))


plot(G2010~treatment, xlab= "Combination", ylab="Grass Cover (%)")


###All, scar & no-scar - for survival analysis###
luor<-read.csv('LUOR2011.csv')
attach(luor)

summary(lm(Max.survival~Site))
summary(lm(Max.survival~Site+F2011))
summary(lm(Max.survival~Site+G2011))
summary(lm(Max.survival~Site+M2011))
summary(lm(Max.survival~Site+B2011))
summary(lm(Max.survival~Site+D2011))
summary(lm(Max.survival~Site+LD2011))
summary(lm(Max.survival~Site+Veg2011))

summary(lm(Max.survival~LUOR2010))
anova(lm(Max.survival~Scarified))


#### log.odds, scar - noscar

luor2<-read.csv('2LUOR4r.csv')
attach(luor2)

site.code<-ifelse(Site=="Bellfountain",1,0)
site.code2<-ifelse(Site=="Pigeon Butte",2,0)
site.code3<-site.code+site.code2

plot(log.odds.~trt.unit, pch=(site.code3+16), col=(site.code3+3))
summary(lm(log.odds.~trt.unit))

plot(odds~trt.unit, pch=(site.code3+16), col=(site.code3+3))

plot(log.odds.~G2010, pch=18, col=5)
abline(lm(log.odds.~G2010))

	bf.odds<-lm(log.odds. ~ G2010,subset=(Site=="Bellfountain"))
	abline(bf.odds,col="blue", lwd=2 )

	pb.odds<-lm(log.odds. ~ G2010, subset=(Site=="Pigeon Butte"))
	abline(pb.odds, col="cyan", lwd=2, lty=2)

	fh.odds<-lm(log.odds. ~ G2010, subset=(Site=="Ft. Hoskins"))
	abline(fh.odds, col="green", lwd=3, lty=3)

plot(log.odds.~Site)


#####

anova(lm(LUOR2010~Site+Scarified))


###Not Scarified###

	#### Establishment ####

luor.ns<-luor[which(luor$Scarified=='n'),]
luor.s<-luor[which(luor$Scarified=='y'),]

t.test(luor.ns$LUOR2010,luor.s$LUOR2010, paired=TRUE)

attach(luor.ns)

luor.ns.matrix<-cbind(LUOR2010, Site, G2010, F2010, Veg2010,
	M2010, B2010, D2010, LD2010)

luor.ns.matrix<-as.data.frame(luor.ns.matrix)
pairs(luor.ns.matrix)

dim(luor.ns)  ## 60 rows = only un-scarified data

LUOR20102<-LUOR2010/100
summary(lm(LUOR20102~Site+G2010))

anova(lm(LUOR2010~Site))
summary(lm(LUOR2010~Site+G2010))
summary(lm(LUOR2010~Site+F2010))
summary(lm(LUOR2010~Site+LD2010))
summary(lm(LUOR2010~Site+M2010))
summary(lm(LUOR2010~Site+Veg2010))
summary(lm(LUOR2010~Site+D2010))
summary(lm(LUOR2010~Site+B2010))

summary(lm(LUOR2010~G2010))
summary(lm(LUOR2010~F2010))
summary(lm(LUOR2010~LD2010))
summary(lm(LUOR2010~M2010))
summary(lm(LUOR2010~Veg2010))
summary(lm(LUOR2010~D2010))
summary(lm(LUOR2010~B2010))

luor.ns.AIC<-lm(LUOR2010~G2010+F2010+Veg2010+M2010+B2010+D2010+LD2010)
step(luor.ns.AIC, direction = c("both"))


anova(lm(LUOR2011~Site))
summary(lm(LUOR2011~Site+G2011))
summary(lm(LUOR2011~Site+F2011))
summary(lm(LUOR2011~Site+LD2011))
summary(lm(LUOR2011~Site+M2011))
summary(lm(LUOR2011~Site+Veg2011))
summary(lm(LUOR2011~Site+D2011))

		# LUOR2011 ~ Site + any variable is significant
		

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




##Bellfountain, scarified only###

luor.ns.bf<-luor.ns[which(luor.ns$Site=='Bellfountain'),]
dim(luor.ns.bf)
detach(luor.ns)
attach(luor.ns.bf)

## All scar + no-scar ##
luor.bf<-luor[which(luor$Site=='Bellfountain'),]
detach(luor)
attach(luor.bf)
##

t.test(Max.survival, Observed.survival)
summary(lm(Max.survival~G2011))
summary(lm(Max.survival~F2011))
summary(lm(Max.survival~M2011))
	plot(Max.survival~M2011)
	abline(lm(Max.survival~M2011))
summary(lm(Max.survival~B2011))
summary(lm(Max.survival~D2011))	
	plot(Max.survival~D2011)
	abline(lm(Max.survival~D2011))
summary(lm(Max.survival~L2011))
summary(lm(Max.survival~LD2011))
summary(lm(Max.survival~Vole))	
summary(lm(Max.survival~Veg2011))

luor.bf.AIC<-lm(Max.survival~G2011+F2011+Veg2011+M2011+
	B2011+D2011+LD2011+L2011)
step(luor.bf.AIC, direction = c("both"))
	
summary(lm(Max.survival ~ D2011 + LD2011 + L2011))
plot(lm(Max.survival ~ D2011 + LD2011 + L2011))

summary(lm(Max.survival ~ D2011 * LD2011 * L2011))
summary(lm(Max.survival ~ LD2011+ L2011+ LD2011:L2011))

summary(lm(Max.survival ~ D2011 + L2011))

summary(lm(Max.survival ~ M2011 + D2011 + LD2011 + L2011))



detach(luor.bf)


#### Pigeon Butte #####

luor.pb<-luor[which(luor$Site=='Pigeon Butte'),]
attach(luor.pb)
##

luor.ns.pb<-luor.ns[which(luor.ns$Site=='Pigeon Butte'),]
dim(luor.ns.pb)
attach(luor.ns.pb)

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
	plot(Max.survival~Veg2011)
	abline(lm(Max.survival~Veg2011))

luor.pb.AIC<-lm(Max.survival~G2011+F2011+Veg2011+M2011+
	B2011+D2011+LD2011+L2011)
step(luor.pb.AIC, direction = c("both"))

summary(lm(Max.survival ~ G2011 + F2011 + B2011))

summary(lm(Max.survival ~ G2011 + L2011))

summary(lm(Max.survival ~ G2011 * F2011 * B2011))
detach(luor.ns.pb)

##### Ft. Hoskins #####

##
luor.fh<-luor[which(luor$Site=='Ft. Hoskins'),]
attach(luor.fh)
##

luor.ns.fh<-luor.ns[which(luor.ns$Site=='Ft. Hoskins'),]
dim(luor.ns.fh)
attach(luor.ns.fh)

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

luor.fh.AIC<-lm(Max.survival~G2011+F2011+Veg2011+M2011+
	B2011+D2011+LD2011+L2011)
step(luor.fh.AIC, direction = c("both"))

summary(lm(Max.survival ~ G2011 + M2011 + LD2011 + Vole))
plot(Max.survival ~ G2011 + M2011 + LD2011 + Vole)

summary(lm(Max.survival ~ G2011 + F2011 + M2011))

detach(luor.ns)

####### 

attach(luor.ns)

luor.ns.matrix<-cbind(Max.survival, Site, G2011, F2011, Veg2011,
	M2011, B2011, D2011, LD2011, L2011, Vole)

luor.ns.matrix<-as.data.frame(luor.ns.matrix)
pairs(luor.ns.matrix)

luor.ns.matrix<-cbind(Max.survival, Site)


###

summary(lm(Max.survival~Site+G2011))
summary(lm(Max.survival~Site+F2011))
summary(lm(Max.survival~Site+M2011))
summary(lm(Max.survival~Site+B2011))
summary(lm(Max.survival~Site+D2011))	
summary(lm(Max.survival~Site+L2011))
summary(lm(Max.survival~Site+LD2011))
summary(lm(Max.survival~Site+Vole))	
summary(lm(Max.survival~Site+Veg2011))
