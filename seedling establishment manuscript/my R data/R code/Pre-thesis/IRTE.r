irte<-read.csv('IRTE 4r.csv')
names(irte)
attach(irte)

anova(lm(Survival~Site))  ### p< 0.019
summary(lm(Survival~Site+Grass))  	# p=0.454
summary(lm(Survival~Site+Forb))		# P=0.326
summary(lm(Survival~Site+ Disturbed))	# p=0.657
summary(lm(Survival~Site+ LD))		# p=0.353
summary(lm(Survival~Site+Vole))	# p=0.643
summary(lm(Survival~Site+Litter))	# p=0.350
anova(lm(Survival~Site+treatment))	# p=0.354
summary(lm(Survival~Moss))		# p=0.804
summary(lm(Survival~Bare))		# p=0.85

vegAV<- (Grass+Forb)
summary(lm(IRTE~Site+vegAV))		# p=0.422

detach(irte)

max.full.irte<-lm(Survival~Site+ Grass+ Forb+ 
	vegAV+ Moss+ Bare + Disturbed +LD)
step(max.full.irte, direction = c("both"))
	#lm(formula = Survival ~ Site + Moss)
summary(lm(Survival~Site+Moss))
	# p=0.01674  R2=0.1656


