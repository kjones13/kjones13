irte<-read.csv('IRTE 4r.csv')
names(cale)
attach(cale)

anova(lm(AVstemlenth~Site))  ### p< 0.019
summary(lm(AVstemlenth~Site+Grass))  	# p=0.454
summary(lm(AVstemlenth~Site+Forb))		# P=0.326
summary(lm(AVstemlenth~Site+ Disturbed))	# p=0.657
summary(lm(AVstemlenth~Site+ LD))		# p=0.353
summary(lm(AVstemlenth~Site+Vole))	# p=0.643
summary(lm(AVstemlenth~Site+Litter))	# p=0.350
anova(lm(AVstemlenth~Site+treatment))	# p=0.354
summary(lm(AVstemlenth~Moss))		# p=0.804
summary(lm(AVstemlenth~Bare))		# p=0.85

vegAV<- (Grass+Forb)
summary(lm(IRTE~Site+vegAV))		# p=0.422

detach(cale)

full<-lm(AVstemlenth~Site+Grass+Forb+Moss

*************************


