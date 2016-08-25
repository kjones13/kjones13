cale<- read.csv('CALE4r.csv')
names(cale)
attach(cale)

Veg2010<-G2010+F2010
Veg2011<-G2011+F2011

survival.10.2<-survival2010*100
persist2<-persistence2011*100


max.matrix<- cbind(Survival,GrassAV, ForbAV, 
	VegAV, MossAV, BareAV, DisturbedAV, LDAV)

max.matrix<-as.data.frame(max.matrix)
pairs(max.matrix)

max.matrix2<- cbind(AVstemlenth,GrassAV, ForbAV, 
	VegAV, MossAV, BareAV, DisturbedAV, LDAV)

max.matrix2<-as.data.frame(max.matrix2)
pairs(max.matrix2)

#########  Survival

summary(lm(Survival~GrassAV))
	# p=0.167
summary(lm(Survival~ForbAV))
	# p=0.337
summary(lm(Survival~VegAV))
	# p=0.165
summary(lm(Survival~MossAV))
	# p=0.289
summary(lm(Survival~BareAV))
	# p=0.062
summary(lm(Survival~DisturbedAV))
	# p=0.826
summary(lm(Survival~LDAV))
	# p=0.0349  *
summary(lm(Survival~X2011Litter))
	# p=0.210
summary(lm(Survival~X2011Vole))
	# p=0.288

max.full<-lm(Survival~ GrassAV+ ForbAV+ 
	VegAV+ MossAV+ BareAV + DisturbedAV +LDAV+ X2011Litter + X2011Vole)
step(max.full, direction = c("both"))
	#lm(Survival ~ ForbAV + DisturbedAV + X2011Litter +X2011Vole)

summary(lm(Survival ~ ForbAV + DisturbedAV + X2011Litter +X2011Vole))
	# p=0.006897 R2=0.5889

########### Stem Length

summary(lm(AVstemlenth~GrassAV))
	# p=0.987
summary(lm(AVstemlenth~ForbAV))
	# p=0.728
summary(lm(AVstemlenth~VegAV))
	# p=0.441
summary(lm(AVstemlenth~MossAV))
	# p=0.219
summary(lm(AVstemlenth~BareAV))
	# p=0.422
summary(lm(AVstemlenth~DisturbedAV))
	# p=0.836
summary(lm(AVstemlenth~LDAV))
	# p=0.2102
summary(lm(AVstemlenth~X2011Litter))
	# p=0.8814
summary(lm(AVstemlenth~X2011Vole))
	# p=0.238

max.full2<-lm(AVstemlenth~ GrassAV+ ForbAV+ 
	VegAV+ MossAV+ BareAV + DisturbedAV +LDAV+ X2011Litter + X2011Vole)
step(max.full2, direction = c("both"))
	# lm(formula = AVstemlenth ~ GrassAV + ForbAV + LDAV)

summary(lm(formula = AVstemlenth ~ GrassAV + ForbAV + LDAV))
	# p=0.07466  R2=0.3429

###### Persistence

summary(lm(persistence2011~G2011))
summary(lm(persistence2011~F2011))
summary(lm(persistence2011~M2011))
summary(lm(persistence2011~B2011))
summary(lm(persistence2011~D2011))
summary(lm(persistence2011~LD2011))
summary(lm(persistence2011~Veg2011))

summary(lm(survival2011~LD2011))

summary(lm(survival2010~G2010))
summary(lm(survival2010~F2010))
summary(lm(survival2010~M2010))
summary(lm(survival2010~B2010))
summary(lm(survival2010~D2010))
summary(lm(survival2010~LD2010))
summary(lm(survival2010~Veg2010))

summary(lm(survival2011~LD2011))