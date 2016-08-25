luor.noscar<- read.csv('luor 4r no scar.csv')
names(luor.noscar)
attach(luor.noscar)


treat.lm<-lm(GrassAV~Site+treatment)
plot(ForbAV~treatment)
anova(treat.lm)
summary(treat.lm)
### treatment is partially correlated with grass cover after accounting
	for Site ###

***************Plants2011***************

anova(lm(Plants2011~Site))

anova(lm(Plants2011~Scarified))

anova(lm(Plants2011~Site+Scarified))


********** Max.survival ***************

max.matrix<- cbind(Max.survival, Site, Scarified, treatment, GrassAV, ForbAV, 
	VegAv, MossAV, BareAV, DisturbedAV, LDAV)



max.matrix<-as.data.frame(max.matrix)
pairs(max.matrix)

plot(Max.survival~Site)
	anova(lm(Max.survival~Site))
	summary(lm(Max.survival~Site))
	###p<0.0001, R2=0.29

plot(Max.survival~treatment)
	anova(lm(Max.survival~treatment))
	### p=0.654

plot(Max.survival~LDAV)
	anova(lm(Max.survival~LDAV))
	### p=0.4777

mean(my.bf$DisturbedAV)
	#
	sd(my.bf$DisturbedAV)
	#
mean(my.pb$DisturbedAV, na.rm=TRUE)
	#
	sd(my.pb$DisturbedAV, na.rm=TRUE)
	#
mean(my.fh$DisturbedAV, na.rm=TRUE)
	#
	sd(my.fh$DisturbedAV, na.rm=TRUE)
	#


**** interactions w/ site ******

lm(Max.survival ~ Site + Scarified + GrassAV + ForbAV + BareAV + 
    DisturbedAV + LDAV + Vole)

anova(lm(Max.survival~Site*GrassAV))
	#p=.8590
anova(lm(Max.survival~Site*ForbAV))
	#p=.2960
anova(lm(Max.survival~Site*VegAV))
	#p=.073
anova(lm(Max.survival~Site*BareAV))
	#p=.09862
anova(lm(Max.survival~Site*DisturbedAV))
	#p=.0617
anova(lm(Max.survival~Site*LDAV))
	#p=.8834
anova(lm(Max.survival~Site*Vole))
	#p=.9161



max.full<-lm(Max.survival~Site+ GrassAV+ ForbAV+ 
	VegAV+ MossAV+ BareAV + DisturbedAV +LDAV)
step(max.full, direction = c("both"))


*** identifies MossAV, ForbAV and VegAV in best model  ***

max.step<-lm(Max.survival ~ Site+ MossAV +ForbAV +VegAV) 
summary(lm(Max.survival~ Site+ MossAV +ForbAV +VegAV))
	p=0.0004363, R2=0.33
anova(max.step)
plot(Max.survival~DisturbedAV)

	summary(lm(Max.survival~DisturbedAV, subset=(Site=="Ft. Hoskins")))
	## 
	summary(lm(Max.survival~DisturbedAV, subset=(Site=="Bellfountain")))
	## 
	summary(lm(Max.survival~DisturbedAV, subset=(Site=="Pigeon Butte")))
	## 



/////////////// plots - disturbed //////////////

library(car)
win.graph()
cr.plots(lm(Max.survival ~ Site+DisturbedAV, data=luor.data),
variable=DisturbedAV,line=TRUE,smooth=FALSE)


site.code<-ifelse(Site=="Bellfountain",1,0)
site.code2<-ifelse(Site=="Pigeon Butte",2,0)
site.code3<-(site.code+site.code2)

win.graph()
plot(Max.survival ~ DisturbedAV,pch=(site.code3 +16), 
	col=(site.code3+3),xlab="Disturbed area (% cover)",
	ylab="Survival (%)",
	main="Lupine Survival vs. Disturbance")

bf.survival<-lm(Max.survival ~ DisturbedAV,subset=(Site=="Bellfountain"))
abline(bf.survival,col="blue", lwd=2 )

pb.survival<-lm(Max.survival ~ DisturbedAV, subset=(Site=="Pigeon Butte"))
abline(pb.survival, col="cyan", lwd=2, lty=2)

fh.survival<-lm(Max.survival ~ DisturbedAV, subset=(Site=="Ft. Hoskins"))
abline(fh.survival, col="green", lwd=3, lty=3)

legend("right",legend=c("Bellfountain","Ft. Hoskins","Pigeon Butte" ),
	pch=c(17,16,18),
	col=c(4,3,69),cex=1)


////////////////

max.step2<- lm(Max.survival ~ Site + Scarified + GrassAV + ForbAV + BareAV + 
    DisturbedAV + LDAV + Vole)
anova (max.step2)
max.dist.ld<-lm(Max.survival~ Site+ DisturbedAV+ LDAV)
anova(max.dist.ld)
summary(max.dist.ld)

plot(LDAV~GrassAV)
abline(lm(LDAV~GrassAV))
litter.grass<-lm(LDAV~Site+GrassAV+DisturbedAV)
summary(litter.grass)

plot(DisturbedAV~Site)
plot(Vole~Site)



****************** Observed.survival  **************  CSV NOT Updated
t.test(Max.survival, Observed.survival)
	p-value = 0.338

plot(Observed.survival~Site)
	anova(lm(Observed.survival~Site))
	summary(lm(Observed.survival~Site))
	###p<0.0001 R2=0.2079


obs.matrix<- cbind(Observed.survival, Site, Scarified, treatment, GrassAV, ForbAV, 
	VegAv, MossAV, BareAV, DisturbedAV, LDAV)
obs.matrix<-as.data.frame(obs.matrix)
pairs(obs.matrix)

plot(Observed.survival~Site)
plot (Observed.survival~Scarified)
plot(Observed.survival~GrassAV)

obs.full<-lm(Observed.survival~Site+ Scarified+ treatment+ GrassAV+ ForbAV+ 
	VegAv +MossAV+ BareAV + DisturbedAV +LDAV)
step(obs.full, direction = c("both"))
		## identifies grass, disturbed and site 

obs.grass<-lm(Observed.survival~Site+DisturbedAV+GrassAV)
summary(obs.grass)
anova(obs.grass)
## Dist, p=0.001855, grass, p=.156  R2= .2886

summary(lm(Observed.survival~Site+DisturbedAV))
	## dist, p=0.00194 ; 
plot(Observed.survival~DisturbedAV)
abline(lm(Observed.survival~DisturbedAV))

summary(lm(Observed.survival~LDAV))
 
********************** 

## Bellfountain  only ##

my.bf<- subset(luor.noscar, Site=="Bellfountain")
levels(my.bf$Site)
table(my.bf$Site)
detach(luor.noscar)
attach(my.bf)

max.matrix<- cbind(Max.survival, Site, Scarified, treatment, GrassAV, ForbAV, 
	VegAv, MossAV, BareAV, DisturbedAV, LDAV)
	max.matrix<-as.data.frame(max.matrix)
pairs(max.matrix)

max.full<-lm(Max.survival~ GrassAV+ ForbAV+ 
	VegAV +MossAV+ BareAV + DisturbedAV +LDAV)
step(max.full, direction = c("both"))

*****identifies Disturbed, ForbAV and LDAV************

summary(lm(Max.survival ~ ForbAV + DisturbedAV + LDAV))
	p=0.01253 r2=.4829

library(car)
win.graph()
cr.plots(lm(Max.survival ~ ForbAV + DisturbedAV + LDAV, data=my.bf),
variable=LDAV,line=TRUE,smooth=FALSE)

win.graph()
plot(Max.survival~LDAV, pch=17, col=4,	
	xlab="Average Litter Depth (cm)", ylab="Survival (%)",
	main="Survival vs. Litter Depth")
abline(lm(Max.survival~LDAV),col="blue", lwd=2,)

plot(Max.survival ~ DisturbedAV,pch=(site.code3 +16), 
	col=(site.code3+3),xlab="Disturbed area (% cover)",
	ylab="Survival (%)",
	main="Lupine Survival vs. Disturbance")



*****identifies litter depth, disturbed and treatment************

summary(lm(formula = Max.survival ~ ForbAV + MossAV + DisturbedAV))
summary(lm(formula = Max.survival ~ MossAV + DisturbedAV))


summary(lm(Max.survival~LDAV+treatment+DisturbedAV))
plot(Max.survival~LDAV)

summary(lm(Max.survival~DisturbedAV+treatment+LDAV))
	##p=, R2=
summary(lm(Max.survival~DisturbedAV+LDAV))
	## p=  R2= 

summary(lm(Observed.survival~DisturbedAV+treatment+LDAV))
	## p=, R2=

summary(lm(Max.survival~VegAv+DisturbedAV+LDAV))
summary(lm(Max.survival~VegAv*DisturbedAV*LDAV))

mean(DisturbedAV)

summary(lm(Max.survival~GrassAV))
	##p=0.565
		summary(lm(Max.survival~log(GrassAV)))
			p=
summary(lm(Max.survival~ForbAV))
	p=0.860  R2=0.001774
summary(lm(Max.survival~VegAV))
	p=0.0608  R2= 0.1818
	summary(lm(Max.survival~log(VegAv+0.00001)))
			p=0.  R2=0.
summary(lm(Max.survival~LDAV))
	p=0.07803 R2=.1625
summary(lm(Max.survival~DisturbedAV))
	p=0.00507  R2=0.3612
summary(lm(Max.survival~Vole))
	p=
anova(lm(Max.survival~treatment))
	p=0.1430
summary(lm(Max.survival~BareAV))
	p=0.0783 R2=0.1622
summary(lm(Max.survival~MossAV))
	p=0.12522  R2=

summary (lm(Max.survival~DisturbedAV*MossAV))
	p=0.0001699  R2=0.4221

summary(lm(Max.survival~LDAV+VegAV))
	p=0.1104
*************Pigeon Butte**********

my.pb<- subset(luor.noscar, Site=="Pigeon Butte")
levels(my.pb$Site)
table(my.pb$Site)
detach(my.bf)
attach(my.pb)

max.matrix<- cbind(Max.survival, Site, Scarified, treatment, GrassAV, ForbAV, 
	VegAV, MossAV, BareAV, DisturbedAV, LDAV)
	max.matrix<-as.data.frame(max.matrix)
pairs(max.matrix)

max.full<-lm(Max.survival~ GrassAV+ ForbAV+ 
	VegAv +MossAV+ BareAV + DisturbedAV +LDAV)
step(max.full, direction = c("both"))

	summary(lm( Max.survival ~ treatment + GrassAV + ForbAV 
	+ MossAV + BareAV + DisturbedAV + LDAV)
			p=0.1072, R2=0.723
	anova(lm(Max.survival~Vole+BareAV+MossAV+DisturbedAV+GrassAV+
		treatment+Scarified))

summary(lm(Max.survival~GrassAV))
	##p=0.804
		summary(lm(Max.survival~log(GrassAV)))
#			p=0.
summary(lm(Max.survival~ForbAV))
#	#p=0.212
summary(lm(Max.survival~VegAV))
#	p=0.0892 R2=0.1521
		summary(lm(Max.survival~log(VegAV+0.00001)))
#			p=0.
summary(lm(Max.survival~LDAV))
#	p=0.633
summary(lm(Max.survival~DisturbedAV))
#	p=0.0449

anova(lm(Max.survival~treatment))
#	p=0.1961
summary(lm(Max.survival~BareAV))
#	p=0.878
summary(lm(Max.survival~MossAV))
#	p=0.968


*****************  FT. Hoskins  *****************
my.fh<- subset(luor.noscar, Site=="Ft. Hoskins")
levels(my.fh$Site)
table(my.fh$Site)
detach(my.pb)
attach(my.fh)

max.matrix<- cbind(Max.survival, Site, Scarified, treatment, GrassAV, ForbAV, 
	VegAv, MossAV, BareAV, DisturbedAV, LDAV)
	max.matrix<-as.data.frame(max.matrix)
pairs(max.matrix)

max.full<-lm(Max.survival~  GrassAV+ ForbAV+ 
	VegAV +MossAV+ BareAV + DisturbedAV +LDAV)
step(max.full, direction = c("both"))



summary(lm(Max.survival ~VegAv + MossAV))

summary(lm(Max.survival~GrassAV))
	##p=0.619
		summary(lm(Max.survival~log(GrassAV)))
			# p=0.
summary(lm(Max.survival~ForbAV))
	# p= 0.3391
summary(lm(Max.survival~VegAV))
	# p= 0.510
summary(lm(Max.survival~LDAV))
	# p= 0.355
summary(lm(Max.survival~DisturbedAV))
	# p= 0.023  R2=0.2555

anova(lm(Max.survival~treatment))
	# p= 0.0826
summary(lm(Max.survival~BareAV))
	# p= 0.1224

summary(lm(Max.survival~MossAV))
	# p= 0.222



