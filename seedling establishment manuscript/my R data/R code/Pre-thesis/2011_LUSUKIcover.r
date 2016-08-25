luor.data<-read.csv('LUSUKIsummary.csv')
names(luor.data)
attach(luor.data)

my.matrix<- cbind(luor.change, Site, Scarified, 
	Grass, Forb, Moss, LD)
my.matrix<-as.data.frame(my.matrix)
pairs(my.matrix)


my.matrix<- cbind(luor2011, Leaves2011, Site, Scarified, 
	Grass, Forb, Moss, Litter, Vole, LD)
my.matrix<-as.data.frame(my.matrix)
pairs(my.matrix)

step1<- lm(luor2011~ Site+ Scarified+ 
	Grass+ Forb+ Moss+ Litter+ Bare+ Disturbed+ Vole+ LD)
step(step1, direction = c("both"))

step1.lm<-lm(luor2011 ~ Site + Scarified + Moss + Litter + Bare +Disturbed)
anova(step1.lm)

step2<- lm(luor2011~ Site+ Scarified+ 
	Grass+Forb+ Vole+LD)
step(step2, direction = c("both"))

step2<- lm(luor2011 ~ Site + Scarified + Forb + LD)
anova(step2)
summary(step2)

fit1<-lm(Leaves2011~Site+Scarified+Litter)
anova(fit1)

boxplot(Leaves2011~Site)
win.graph()
boxplot(luor2011~Site)
boxplot(luor2011~Scarified, subset=(Site=='Pigeon Butte'))
boxplot (Leaves2011~ Scarified, subset=(Site=='Ft. Hoskins'))


luor.change<-(luor2011-luor2010)
luor.change
leaves.change<-(Leaves2011-Leaves2010)
leaves.change

leaves.change.lm<-lm(leaves.change~Site+Scarified+Grass+Forb+LD+Litter+Vole)
anova(leaves.change.lm)
summary(leaves.change.lm)

leaves.change.lm2<-lm(leaves.change~Site+Scarified+Grass+Forb+LD)
anova(leaves.change.lm2)
summary(leaves.change.lm2)


leaves.change.lm2<-lm(leaves.change~Site+Forb+LD+Litter+Vole)
anova(leaves.change.lm2)
summary(leaves.change.lm2)

leaves.ratio<-(Leaves2011/luor2011)
ratio.lm<- lm(leaves.ratio~ Site+Scarified+Grass+Forb+Moss+Bare
	+Litter+Disturbed+LD)
anova(ratio.lm)
summary(ratio.lm)
*********ratio is not a useful summary)

leaves.ratio.change<- ((Leaves2010/luor2010)-(Leaves2011/luor2011))
ratio.change.lm<- lm(leaves.ratio.change~ Site+Scarified+Grass+Forb
	+Disturbed+LD)
anova(ratio.change.lm)
summary(ratio.change.lm)



######   SIVI  ######
sivi.scar<-lm(sivi2010~ Scarified)
anova(sivi.scar)
   	***There is no difference between SIVI establishment between scarified
	and non-scarified plots at any site or all sites grouped******

boxplot(sivi2010~ Scarified)
