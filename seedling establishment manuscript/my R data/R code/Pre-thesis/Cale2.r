cale<- read.csv('CALE4r.csv')
names(cale)
attach(cale)

stemage<-AVstemlenth


max.matrix<- cbind(survival2011,G2011, F2011, 
	Veg2011, M2011, B2011, D2011, LD2011, AVstemlenth)

max.matrix<-as.data.frame(max.matrix)
pairs(max.matrix)

max.matrix2<- cbind(AVstemlenth,GrassAV, ForbAV, 
	VegAV, MossAV, BareAV, DisturbedAV, LDAV)

max.matrix2<-as.data.frame(max.matrix2)
pairs(max.matrix2)

#########  Survival 2010

summary(lm(survival2010~G2010))
summary(lm(survival2010~F2010))
summary(lm(survival2010~LD2010))
summary(lm(survival2010~M2010))
summary(lm(survival2010~Veg2010))
summary(lm(survival2010~D2010))
summary(lm(survival2010~B2010))

cale2010.AIC<-lm(survival2010~G2010+F2010+Veg2010+M2010+B2010+D2010+LD2010)
step(cale2010.AIC, direction = c("both"))

##########  Survival 2011

summary(lm(survival2011~G2011))
summary(lm(survival2011~F2011))
summary(lm(survival2011~LD2011))
summary(lm(survival2011~M2011))
summary(lm(survival2011~Veg2011))
summary(lm(survival2011~D2011))
summary(lm(survival2011~B2011))
summary(lm(survival2011~Vole))

cale2011.AIC<-lm(survival2011~G2011+F2011+Veg2011+M2011+B2011+D2011+LD2011)
step(cale2011.AIC, direction = c("both"))


##########  loss

summary(lm(changeCALE~G2011))
summary(lm(changeCALE~F2011))
summary(lm(changeCALE~LD2011))
summary(lm(changeCALE~M2011))
summary(lm(changeCALE~Veg2011))
summary(lm(changeCALE~D2011))
summary(lm(changeCALE~B2011))
summary(lm(changeCALE~Vole))

change.AIC<-lm(changeCALE~G2011+F2011+Veg2011+M2011+B2011+D2011+LD2011)
step(change.AIC, direction = c("both"))

##########  stemage

summary(lm(stemage~G2011))
summary(lm(stemage~F2011))
summary(lm(stemage~LD2011))
summary(lm(stemage~M2011))
summary(lm(stemage~Veg2011))
summary(lm(stemage~D2011))
summary(lm(stemage~B2011))
summary(lm(stemage~Vole))

stem.AIC<-lm(stemage~G2011+F2011+Veg2011+M2011+B2011+D2011+LD2011)
step(stem.AIC, direction = c("both"))

############## long stem

summary(lm(Avlongstem~G2011))
summary(lm(Avlongstem~F2011))
summary(lm(Avlongstem~LD2011))
summary(lm(Avlongstem~M2011))
summary(lm(Avlongstem~Veg2011))
summary(lm(Avlongstem~D2011))
summary(lm(Avlongstem~B2011))
summary(lm(Avlongstem~Vole))

longstem.AIC<-lm(Avlongstem~G2011+F2011+Veg2011+M2011+B2011+D2011+LD2011)
step(longstem.AIC, direction = c("both"))

