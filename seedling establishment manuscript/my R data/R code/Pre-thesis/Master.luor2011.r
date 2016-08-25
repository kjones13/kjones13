luor.data<- read.csv('luor data 4r.csv')
names(luor.data)
attach(luor.data)

treat.lm<-lm(GrassAV~Site+treatment)
plot(ForbAV~treatment)
anova(treat.lm)
summary(treat.lm)
### treatment is partially correlated with grass cover after accounting
	for Site ###

luor.noscar<- read.csv('luor 4r no scar.csv')
names(luor.noscar)
attach(luor.noscar)


***************Plants2011***************

anova(lm(Plants2011~Site))
	p=0.8687
anova(lm(Plants2011~Scarified))
	p=0.853
anova(lm(Plants2011~Site+Scarified))
	p=0.8541

********** Max.survival ***************

max.matrix<- cbind(Max.survival, Site, Scarified, treatment, GrassAV, ForbAV, 
	VegAv, MossAV, BareAV, DisturbedAV, LDAV)



max.matrix<-as.data.frame(max.matrix)
pairs(max.matrix)

plot(Max.survival~Site)
	anova(lm(Max.survival~Site))
	summary(lm(Max.survival~Site))
	###p<0.0001, R2=0.203

plot(Max.survival~Scarified)
	anova(lm(Max.survival~Scarified))
	### p=0.3322
	anova(lm(Plants2011~Site+Scarified))

plot(Max.survival~treatment)
	anova(lm(Max.survival~treatment))
	### p=0.4903

plot(Max.survival~LDAV)
	anova(lm(Max.survival~LDAV))
	### p=8568


mean(my.bf$DisturbedAV)
	#6.56
	sd(my.bf$DisturbedAV)
	#8.82
mean(my.pb$DisturbedAV, na.rm=TRUE)
	#10.55
	sd(my.pb$DisturbedAV, na.rm=TRUE)
	#8.84
mean(my.fh$DisturbedAV, na.rm=TRUE)
	# 1.82
	sd(my.fh$DisturbedAV, na.rm=TRUE)
	#2.26


**** interactions w/ site ******

lm(Max.survival ~ Site + Scarified + GrassAV + ForbAV + BareAV + 
    DisturbedAV + LDAV + Vole)
anova(lm(Max.survival~Site*Scarified))
	#p=.2476
anova(lm(Max.survival~Site*GrassAV))
	#p=.9424
anova(lm(Max.survival~Site*ForbAV))
	#p=.2345
anova(lm(Max.survival~Site*VegAv))
	#p=.04468
anova(lm(Max.survival~Site*BareAV))
	#p=.04509
anova(lm(Max.survival~Site*DisturbedAV))
	#p=.019
anova(lm(Max.survival~Site*LDAV))
	#p=.9341
anova(lm(Max.survival~Site*Vole))
	#p=.9161

max.scar<-lm(Max.survival~Site+Scarified)
summary(max.scar)
anova(max.scar)
	summary(lm(Max.survival~Scarified, subset=(Site=='Pigeon Butte')))
	##PB, p=0.0843 ; FH & BF, n/s

max.full<-lm(Max.survival~Site+ Scarified+ treatment+ GrassAV+ ForbAV+ 
	VegAv +MossAV+ BareAV + DisturbedAV +LDAV)
step(max.full, direction = c("both"))


*** identifies Site and Disturbed only in best model  ***

max.step<-lm(Max.survival ~ Site+DisturbedAV) 
summary(lm(Max.survival~Site+DisturbedAV))
	p=0.00358, R2=0.2616
anova(max.step)
plot(Max.survival~DisturbedAV)

	summary(lm(Max.survival~DisturbedAV, subset=(Site=="Ft. Hoskins")))
	## p=0.03026  R2=0.1176
	summary(lm(Max.survival~DisturbedAV, subset=(Site=="Bellfountain")))
	## p=0.000749  R2=0.2613
	summary(lm(Max.survival~DisturbedAV, subset=(Site=="Pigeon Butte")))
	## p=.403  R2=0.01953

summary(lm(Max.survival~Site+Scarified+DisturbedAV))
	## R2=0.2688
	summary(lm(Max.survival~Scarified+DisturbedAV, subset=(Site=="Ft. Hoskins")))
	## p=0.0277  R2=0.1273
	summary(lm(Max.survival~Scarified+DisturbedAV, subset=(Site=="Bellfountain")))
	## p=0.000915  R2=0.2616
	summary(lm(Max.survival~Scarified+DisturbedAV, subset=(Site=="Pigeon Butte")))
	## p=0.5224  R2=0.09863


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



****************** Observed.survival  **************
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

my.bf<- subset(luor.data, Site=="Bellfountain")
levels(my.bf$Site)
table(my.bf$Site)
detach(luor.data)
attach(my.bf)

max.matrix<- cbind(Max.survival, Site, Scarified, treatment, GrassAV, ForbAV, 
	VegAv, MossAV, BareAV, DisturbedAV, LDAV)
	max.matrix<-as.data.frame(max.matrix)
pairs(max.matrix)

max.full<-lm(Max.survival~ Scarified+ GrassAV+ ForbAV+ 
	VegAv +MossAV+ BareAV + DisturbedAV +LDAV+ Vole)
step(max.full, direction = c("both"))
*****identifies litter depth, disturbed and treatment************

obs.full<-lm(Observed.survival~ Scarified+ treatment+ GrassAV+ ForbAV+ 
	VegAv +MossAV+ BareAV + DisturbedAV +LDAV+ Vole)
step(obs.full, direction = c("both"))

*****identifies litter depth, disturbed and treatment************

summary(lm(formula = Max.survival ~ ForbAV + MossAV + DisturbedAV))
summary(lm(formula = Max.survival ~ MossAV + DisturbedAV))

survival2<-



summary(lm(Max.survival~LDAV+treatment+DisturbedAV))
plot(Max.survival~LDAV)

summary(lm(Max.survival~DisturbedAV+treatment+LDAV))
	##p= 0.0004924, R2=0.4998
summary(lm(Max.survival~DisturbedAV+LDAV))
	## p=0.2774, R2= 0.2774

summary(lm(Observed.survival~DisturbedAV+treatment+LDAV))
	## p=0.001263, R2=0.4665

summary(lm(Max.survival~VegAv+DisturbedAV+LDAV))
summary(lm(Max.survival~VegAv*DisturbedAV*LDAV))

mean(DisturbedAV)

summary(lm(Max.survival~GrassAV))
	##p=0.957
		summary(lm(Max.survival~log(GrassAV)))
			p=0.7996
summary(lm(Max.survival~ForbAV))
	p=0.252  R2=0.1225
summary(lm(Max.survival~VegAv))
	p=0.0269  R2= 0.1225
		summary(lm(Max.survival~log(VegAv+0.00001)))
			p=0.0225  R2=0.1296
summary(lm(Max.survival~LDAV))
	p=0.3473
summary(lm(Max.survival~DisturbedAV))
	p=0.0007492  R2=0.2613
summary(lm(Max.survival~Vole))
	p=0.985
anova(lm(Max.survival~treatment))
	p=0.1426
summary(lm(Max.survival~BareAV))
	p=0.0992
summary(lm(Max.survival~Scarified))
	p=0.7709
summary(lm(Max.survival~MossAV))
	p=0.005353  R2=0.1868

summary (lm(Max.survival~DisturbedAV*MossAV))
	p=0.0001699  R2=0.4221

*************Pigeon Butte**********

my.pb<- subset(luor.data, Site=="Pigeon Butte")
levels(my.pb$Site)
table(my.pb$Site)
detach(luor.data)
attach(my.pb)

max.matrix<- cbind(Max.survival, Site, Scarified, treatment, GrassAV, ForbAV, 
	VegAv, MossAV, BareAV, DisturbedAV, LDAV)
	max.matrix<-as.data.frame(max.matrix)
pairs(max.matrix)

max.full<-lm(Max.survival~ Scarified+ treatment+ GrassAV+ ForbAV+ 
	VegAv +MossAV+ BareAV + DisturbedAV +LDAV+ Vole)
step(max.full, direction = c("both"))

	summary(lm(Max.survival~Vole+BareAV+MossAV+DisturbedAV+GrassAV+
		treatment+Scarified))
			p=0.3054
	anova(lm(Max.survival~Vole+BareAV+MossAV+DisturbedAV+GrassAV+
		treatment+Scarified))

summary(lm(Max.survival~GrassAV))
	##p=0.85
		summary(lm(Max.survival~log(GrassAV)))
			p=0.606
summary(lm(Max.survival~ForbAV))
	p=0.403
summary(lm(Max.survival~VegAv))
	p=0.429
		summary(lm(Max.survival~log(VegAv+0.00001)))
			p=0.69
summary(lm(Max.survival~LDAV))
	p=0.732
summary(lm(Max.survival~DisturbedAV))
	p=0.403
summary(lm(Max.survival~Vole))
	p=0.917
anova(lm(Max.survival~treatment))
	p=0.09398
summary(lm(Max.survival~BareAV))
	p=0.751
summary(lm(Max.survival~Scarified))
	p=0.0843
summary(lm(Max.survival~MossAV))
	p=0.741


*****************  FT. Hoskins  *****************
my.fh<- subset(luor.data, Site=="Ft. Hoskins")
levels(my.fh$Site)
table(my.fh$Site)
detach(my.pb)
attach(my.fh)

max.matrix<- cbind(Max.survival, Site, Scarified, treatment, GrassAV, ForbAV, 
	VegAv, MossAV, BareAV, DisturbedAV, LDAV)
	max.matrix<-as.data.frame(max.matrix)
pairs(max.matrix)

max.full<-lm(Max.survival~ Scarified+ treatment+ GrassAV+ ForbAV+ 
	VegAv +MossAV+ BareAV + DisturbedAV +LDAV+ Vole)
step(max.full, direction = c("both"))

	summary(lm(Max.survival ~ treatment + GrassAV + ForbAV + 
		MossAV +BareAV + DisturbedAV + LDAV + Vole))
			p=0.0001372 R2=0.6817
	anova(lm(Max.survival ~ treatment + GrassAV + ForbAV + 
		MossAV +BareAV + DisturbedAV + LDAV + Vole))
			treatment p=0.0007484
	anova(lm(Max.survival ~ treatment + GrassAV + ForbAV + 
		MossAV +BareAV + DisturbedAV + LDAV))


summary(lm(Max.survival ~ treatment + GrassAV + ForbAV + 
	MossAV + DisturbedAV + LDAV + Vole))
		p=0.0001818 R2=0.6488

summary(lm(Max.survival ~ GrassAV + ForbAV + 
	MossAV + DisturbedAV + LDAV + Vole))
		p=0004696  R2=0.5014

summary(lm(Max.survival ~ GrassAV + ForbAV + 
	MossAV + LDAV + Vole))
		p=0.000783 R2=0.4487

summary(lm(Max.survival ~VegAv + MossAV))

summary(lm(Max.survival~GrassAV))
	##p=0.71
		summary(lm(Max.survival~log(GrassAV)))
			# p=0.615
summary(lm(Max.survival~ForbAV))
	# p= 0.140
summary(lm(Max.survival~VegAv))
	# p= 0.163
summary(lm(Max.survival~LDAV))
	# p= 0.143
summary(lm(Max.survival~DisturbedAV))
	# p= 0.030
summary(lm(Max.survival~Vole))
	# p= 0.522
anova(lm(Max.survival~treatment))
	# p= 0.1275
summary(lm(Max.survival~BareAV))
	# p= 0.106
summary(lm(Max.survival~Scarified))
	# p= 0.721 
summary(lm(Max.survival~MossAV))
	# p= 0.117



