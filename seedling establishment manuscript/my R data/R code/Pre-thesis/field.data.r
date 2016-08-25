field.data<- read.csv('Field.data4r.csv')
names(field.data)
attach(field.data)

my.matrix<- cbind(LUSUKI,Av..GrassCover, Av..ForbCover, Av..MossCover, Av..LitterDepth)
my.matrix<-as.data.frame(my.matrix)
pairs(my.matrix)

detach(field.data)
attach(field.data)


					##LUSUKI##

##Step AIC##
step1<-lm(LUSUKI~ Site+ trt.unit+ Scarified+ Av..GrassCover+ Av..ForbCover+
	Av..MossCover+ Av..BareGround+ Av..Disturbance+ Av..LitterDepth)
step(step1, direction = c("both"))

fit1.lm<-lm(LUSUKI ~ Site + Av..GrassCover +
	 Av..MossCover * Av..ForbCover * Av..LitterDepth * Scarified)

step(fit1.lm, direction = c("both"))

fit2.lm<-lm(LUSUKI~Site+Scarified+Av..ForbCover+Av..LitterDepth)
summary(fit2.lm)
anova(fit2.lm)

fit2i.lm<-lm(LUSUKI~Site+Scarified+Av..ForbCover+Av..LitterDepth+
	Av..ForbCover:Av..LitterDepth)
summary(fit2i.lm)


fit3.lm<-lm(LUSUKI~Site+Scarified+Av..GrassCover+Av..LitterDepth+
	Av..GrassCover:Av..LitterDepth)
summary(fit3.lm)

anova(fit2.lm, fit2i.lm, fit3.lm)

plot(fit3.lm)

grass.lm<-lm(LUSUKI~Av..GrassCover)
library(car)
cr.plots(lm(LUSUKI ~ Site+ Av..LitterDepth + Av..GrassCover +
	 Scarified, data=field.data),
variable=Av..LitterDepth,line=TRUE,smooth=FALSE)


cr.plots(lm(LUSUKI ~ Site+ Av..LitterDepth + Av..GrassCover +
	 Scarified, data=field.data),
variable=Scarified,line=TRUE,smooth=FALSE)

cr.plots(lm(LUSUKI ~ Site+ Av..LitterDepth + Av..GrassCover +
	 Scarified, data=field.data),
variable=Site,line=TRUE,smooth=FALSE)



##Best GLM##
data.glm<- cbind(LUSUKI, Site, trt.unit, Scarified, Av..GrassCover, Av..ForbCover,
	Av..MossCover, Av..BareGround, Av..Disturbance, Av..LitterDepth)
data.glm<-as.data.frame(data.glm)
	
library(bestglm)
bestglm.out1<-bestglm(data.glm, IC="BIC")
bestglm.out1
summary(bestglm.out1)

bestglm.lm<- lm(LUSUKI~Av..GrassCover + Av..BareGround + Av..Disturbance)
summary(bestglm.lm)

data.glm2<- cbind(LUSUKI, Site, trt.unit, Scarified, Av..GrassCover, Av..ForbCover,
	 Av..LitterDepth)
data.glm2<-as.data.frame(data.glm2)
bestglm.out2<-bestglm(data.glm2, IC="BIC")
bestglm.out2
summary(bestglm.out2)


step1<-lm(LUSUKI~ Site* Scarified* Av..GrassCover* Av..ForbCover*
	Av..MossCover* Av..BareGround* Av..Disturbance* Av..LitterDepth)
step(step1, direction = c("both"))
