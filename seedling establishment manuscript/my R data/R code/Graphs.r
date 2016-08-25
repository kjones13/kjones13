
		######## LUOR #########

luor<-read.csv('LUOR4r.csv')
attach(luor)

plot(LUOR2010~ Scarified, ,xlab="Scarified",
	ylab="Lupine Establishment (%)")

plot(LD2011~G2011, ,xlab="Grass Cover (%)",
	ylab="Average Litter Depth (cm)")

site.code<-ifelse(Site=="Bellfountain",1,0)
site.code2<-ifelse(Site=="Pigeon Butte",2,0)
site.code3<-site.code+site.code2

luor.ns<-luor[which(luor$Scarified=='n'),]

plot(LUOR2010~LD2010, ylim=c(0,60), pch=(site.code3+16), col=(site.code3+3), 
	xlab= "Litter Depth (cm)", ylab="Establishment (%)")

	bf.ns.estab<-lm(LUOR2010 ~ LD2010, subset=(Site=="Bellfountain"))
	abline(bf.ns.estab,col="blue", lwd=2 )

	pb.ns.estab<-lm(LUOR2010 ~ LD2010, subset=(Site=="Pigeon Butte"))
	abline(pb.ns.estab, col="cyan", lwd=2, lty=2)

	fh.ns.estab<-lm(LUOR2010 ~ LD2010, subset=(Site=="Ft. Hoskins"))
	abline(fh.ns.estab, col="green", lwd=3, lty=3)

legend("bottomright", inset=.05, legend=c("Bellfountain", "Pigeon Butte", "Ft. Hoskins"),
	 merge=F,, col=c(4,5,3), pch=c(17,18,16), lty=c(1,2,3), lwd=c(2,2,3))
	


Max.survival<-Max.survival*100

plot(Max.survival~LD2011, pch=(site.code3+16), col=(site.code3+3), 
	xlab= "Litter Depth (cm)", ylab="Survival (%)")

	bf.survival<-lm(Max.survival ~ LD2011,subset=(Site=="Bellfountain"))
	abline(bf.survival,col="blue", lwd=2 )

	pb.survival<-lm(Max.survival ~ LD2011, subset=(Site=="Pigeon Butte"))
	abline(pb.survival, col="cyan", lwd=2, lty=2)

	fh.survival<-lm(Max.survival ~ LD2011, subset=(Site=="Ft. Hoskins"))
	abline(fh.survival, col="green", lwd=3, lty=3)

plot(Max.survival~D2011, pch=(site.code3+16), col=(site.code3+3),
	xlab= "Disturbance (%)", ylab="Survival (%)")
	bf.survival<-lm(Max.survival ~ D2011,subset=(Site=="Bellfountain"))
	abline(bf.survival,col="blue", lwd=2 )

	pb.survival<-lm(Max.survival ~ D2011, subset=(Site=="Pigeon Butte"))
	abline(pb.survival, col="cyan", lwd=2, lty=2)

	fh.survival<-lm(Max.survival ~ D2011, subset=(Site=="Ft. Hoskins"))
	abline(fh.survival, col="green", lwd=3, lty=3)

luor2<-read.csv('2LUOR4r.csv')
attach(luor2)

site.code<-ifelse(Site=="Bellfountain",1,0)
site.code2<-ifelse(Site=="Pigeon Butte",2,0)
site.code3<-site.code+site.code2

plot(log.odds.~trt.unit, pch=(site.code3+16), col=(site.code3+3))
plot(log.odds.~G2010, pch=18, col=5)
abline(lm(log.odds.~G2010))

	bf.odds<-lm(log.odds. ~ G2010,subset=(Site=="Bellfountain"))
	abline(bf.odds,col="blue", lwd=2 )

	pb.odds<-lm(log.odds. ~ G2010, subset=(Site=="Pigeon Butte"))
	abline(pb.odds, col="cyan", lwd=2, lty=2)

	fh.odds<-lm(log.odds. ~ G2010, subset=(Site=="Ft. Hoskins"))
	abline(fh.odds, col="green", lwd=3, lty=3)

plot(log.odds.~Site)


luor.data<- read.csv('2011_LUOR_2006planting.csv')
attach(luor.data)
veg<-(Grass+Forb)
log.leaves<-log(leaves+0.1)

plot(log.leaves~Grass)
	bf.leaves<-lm(log.leaves ~ Grass,subset=(Site=="Bellfountain"))
	abline(bf.leaves,col="blue", lwd=2 )	

	fh.leaves<-lm(log.leaves ~ Grass, subset=(Site=="Ft. Hoskins"))
	abline(fh.leaves, col="green", lwd=3, lty=3)

plot(log.leaves~veg)
	bf.leaves<-lm(log.leaves ~ veg,subset=(Site=="Bellfountain"))
	abline(bf.leaves,col="blue", lwd=2 )	

	fh.leaves<-lm(log.leaves ~ veg, subset=(Site=="Ft. Hoskins"))
	abline(fh.leaves, col="green", lwd=3, lty=3)

				### SIVI ###

sivi<-read.csv('SIVI4r.csv')
attach(sivi)

site.code<-ifelse(Site=="Bellfountain",1,0)
site.code2<-ifelse(Site=="Pigeon Butte",2,0)
site.code3<-site.code+site.code2

plot(Max.survival~LD2011, pch=(site.code3+16), col=(site.code3+3), 
	xlab= "Litter Depth (cm)", ylab="Establishment (%)")

	bf.survival<-lm(Max.survival ~ LD2011,subset=(Site=="Bellfountain"))
	abline(bf.survival,col="blue", lwd=2 )

	pb.survival<-lm(Max.survival ~ LD2011, subset=(Site=="Pigeon Butte"))
	abline(pb.survival, col="cyan", lwd=2, lty=2)

	fh.survival<-lm(Max.survival ~ LD2011, subset=(Site=="Ft. Hoskins"))
	abline(fh.survival, col="green", lwd=3, lty=3)

Observed.survival<-Observed.survival*100

plot(Observed.survival~LD2011, pch=(site.code3+16), col=(site.code3+3), 
	xlab= "Litter Depth (cm)", ylab="Survival (%)")
axis(2,

	bf.survival<-lm(Observed.survival ~ LD2011,subset=(Site=="Bellfountain"))
	abline(bf.survival,col="blue", lwd=2 )

	pb.survival<-lm(Observed.survival ~ LD2011, subset=(Site=="Pigeon Butte"))
	abline(pb.survival, col="cyan", lwd=2, lty=2)

	fh.survival<-lm(Observed.survival ~ LD2011, subset=(Site=="Ft. Hoskins"))
	abline(fh.survival, col="green", lwd=3, lty=3)

plot(Max.survival~D2011)
	bf.survival<-lm(Max.survival ~ D2011,subset=(Site=="Bellfountain"))
	abline(bf.survival,col="blue", lwd=2 )

	pb.survival<-lm(Max.survival ~ D2011, subset=(Site=="Pigeon Butte"))
	abline(pb.survival, col="cyan", lwd=2, lty=2)

	fh.survival<-lm(Max.survival ~ D2011, subset=(Site=="Ft. Hoskins"))
	abline(fh.survival, col="green", lwd=3, lty=3)



plot(Observed.survival~D2011)
	bf.survival<-lm(Observed.survival ~ D2011,subset=(Site=="Bellfountain"))
	abline(bf.survival,col="blue", lwd=2 )

	pb.survival<-lm(Observed.survival ~ D2011, subset=(Site=="Pigeon Butte"))
	abline(pb.survival, col="cyan", lwd=2, lty=2)

	fh.survival<-lm(Observed.survival ~ D2011, subset=(Site=="Ft. Hoskins"))
	abline(fh.survival, col="green", lwd=3, lty=3)

sivi.bf<-sivi[which(sivi$Site=='Bellfountain'),]
attach(sivi.bf)


			####### CALE ######

persistence2011<-persistence2011*100

plot(persistence2011~G2011, pch=17, col="blue",
	xlab= "Grass Cover (%)", ylab="Survival (%)")
	abline(lm(persistence2011~G2011), col="blue", lwd=2,)

plot(persistence2011~Veg2011, pch=17, col="blue",
	xlab= "Total Vegetative Cover (%)", ylab="Survival (%)")
	abline(lm(persistence2011~G2011), col="blue", lwd=2,)

plot(persistence2011~LD2011, pch=17, col="blue",
	xlab= "Litter Depth (cm)", ylab="Survival (%)")
	abline(lm(persistence2011~G2011), col="blue", lwd=2,)


			####### IRTE ########





