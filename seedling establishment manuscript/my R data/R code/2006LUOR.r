luor.data<- read.csv('2011_LUOR_2006planting.csv')
names(luor.data)
attach(luor.data)
veg<-(Grass+Forb)

plot(Grass~Present)

summary(lm(leaves~Site))
summary(lm(leaves~Site+Grass))
summary(lm(leaves~Site*Grass))
	plot(((log(leaves))~Grass))
	abline(lm(((log(leaves))~Grass)))

log.leaves<-log(leaves+0.001)


leaves.present<-subset(luor.data, (leaves>0))

tiff(filename="figure6A-2006LUORflower.tiff", res=160, width=960, height=960)
plot(leaves~Flowering, log="y", xlab="Flowering", 
	ylab="# of leaves (log scale)")

plot(Grass~Present, xlab="2006 LUOR present", ylab="Grass Cover (%)")



summary(lm(log.leaves~Site+Grass))
summary((lm(log.leaves~Site+Forb))
summary(lm(log.leaves~Site+AvLD))
summary(lm(log.leaves~Site+Disturbed))
summary(lm(log.leaves~Site+Litter))
summary(lm(log.leaves~Site+Bare))
summary(lm(log.leaves~Site+Moss))
veg<-Grass+Forb
summary(lm(log.leaves~Site+veg))


*****Bellfountain*********
bf.luor<-subset(luor.data, (Site=='Bellfountain'))
detach(luor.data)
attach(bf.luor)

names(bf.luor)
log.leaves<-log(leaves+0.001)

my.matrix<- cbind(log.leaves, Grass, Forb, Litter, AvLD)
my.matrix<-as.data.frame(my.matrix)
pairs(my.matrix)

plot(log.leaves~Grass)


bf4.luor<-subset(bf.luor, log.leaves > 0)
detach(bf3.luor)
attach(bf4.luor)

bf.grass.lm<-lm(leaves~Grass)
summary(bf.grass.lm)
confint(bf.grass.lm)
plot(bf.grass)
abline(log.leaves~Grass)

bf.forb.lm<-lm(leaves~Forb)
summary(bf.forb.lm)

bf.both.lm<-lm(leaves~Forb+Grass)
summary(bf.both.lm)

confint(bf.both.lm)

plot(leaves~Grass)
abline(bf.grass.lm)

bf.vole.lm<- lm(leaves~Vole)
summary(bf.vole.lm)

library(car)
win.graph()
cr.plots(lm(log.leaves~Forb+Grass, data=bf.luor),
	variable=Grass,line=TRUE,smooth=FALSE)

plot(bf.both.lm)

omit35<-bf.luor[which(bf.luor$leaves<200),]
detach(bf.luor)
attach(omit35)
adj.both<-lm(leaves~Forb+Grass)
summary(adj.both)
plot(adj.both

detach(omit35)

omit15<-omit35[which(omit35$leaves<100),]
detach(omit35)
attach(omit15)
adj.both<-lm(leaves~Forb+Grass)
summary(adj.both)
plot(adj.both)

omit18<-omit15[which(omit15$leaves<80),]
detach(omit15)
attach(omit18)
adj.both<-lm(leaves~Forb+Grass)
summary(adj.both)
plot(adj.both)

	*too many influential points*

bf.both.lm<-lm(log.leaves~Forb+Grass)
summary(bf.both.lm)

library(car)
win.graph()
cr.plots(lm(log.leaves~Forb+Grass, data=bf.luor),
	variable=Grass,line=TRUE,smooth=FALSE)


bf.AIC<-lm(log.leaves~Grass+Forb+Moss+Litter+Bare+Disturbed+AvLD+Litter)
step(bf.AIC, direction = c("both"))

summary(lm(log.leaves ~ Grass + Litter + Disturbed + AvLD))

*****Ft. Hoskins*****

fh.luor<-subset(luor.data, (Site=='Ft. Hoskins'))
detach(luor.data)
attach(fh.luor)
names(luor.data)
summary(lm(leaves~Grass))
summary(lm(leaves~Forb))
summary(lm(leaves~AvLD))
summary(lm(leaves~Litter))
summary(lm(leaves~Bare))


log.leaves<-log(leaves+0.001)

summary(lm(log.leaves~Grass))
summary(lm(log.leaves~Forb))
summary(lm(log.leaves~AvLD))
summary(lm(log.leaves~Litter))
summary(lm(log.leaves~Bare))
summary(lm(log.leaves~Disturbed))
summary(lm(log.leaves~Moss))
summary(lm(log.leaves~Vole))
summary(lm(log.leaves~veg))

veg<-Grass+Forb


fh.lm<- lm(leaves~Grass+Forb+Moss+Litter+Bare+Disturbed+Vole+AvLD,
	 subset=(Site=="Ft. Hoskins"))
anova(fh.lm)

fh.AIC<-lm(log.leaves~Grass+Forb+Moss+Litter+Bare+Disturbed+AvLD+Litter)
step(fh.AIC, direction = c("both"))


fh.grass.lm<-lm(leaves~Grass, subset=(Site=="Ft. Hoskins"))
summary(fh.grass.lm)

fh.forb.lm<-lm(leaves~Forb, subset=(Site=="Ft. Hoskins"))
summary(fh.forb.lm)

fh.both.lm<-lm(leaves~Forb+ Grass, subset=(Site=="Ft. Hoskins"))
summary(fh.both.lm)

*****************

omit200<-omit0[which(omit0$leaves<200),]
omit0<-luor.data[which(luor.data$leaves>1),]


omitA<-subset(luor.data, (luor.data$leaves>1))
omitB<-subset(luor.data, (luor.data$leaves<1))
t.test(omitA$Grass, omitB$Grass)

t.test(omitA$Grass)
t.test(omitB$Grass)


