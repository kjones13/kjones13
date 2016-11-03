luor <- read.csv('C:/Users/kjones/Documents/GitHub/kjones13/seedling establishment manuscript/LUOR4r.csv', 
                stringsAsFactors = F)

library(dplyr)

#luor<-read.csv('LUOR4r.csv')
attach(luor)

site.code<-ifelse(Site=="Bellfountain",1,0)
site.code2<-ifelse(Site=="Pigeon Butte",2,0)
site.code3<-site.code+site.code2

########### Figure 5 - pg 14

luor.ns<-luor[which(luor$Scarified=='No'),]
detach(luor)
attach(luor.ns)


tiff(filename="figure5-LUORestab.tif", res=160, width=900, height=900, 
     units="px", compression = "lzw")

plot(LUOR2010~LD2010, ylim=c(0,60), xlim=c(0,7), pch=(site.code3+1),  
	xlab= "Litter Depth (cm)", ylab="Establishment (%)")

	bf.ns.estab<-lm(LUOR2010 ~ LD2010, subset=(Site=="Bellfountain"))
	abline(bf.ns.estab, col="black", lwd=2)

	pb.ns.estab<-lm(LUOR2010 ~ LD2010, subset=(Site=="Pigeon Butte"))
	abline(pb.ns.estab, col="black", lwd=2, lty=2)

	fh.ns.estab<-lm(LUOR2010 ~ LD2010, subset=(Site=="Ft. Hoskins"))
	abline(fh.ns.estab, col="black", lwd=2, lty=4)

legend("bottomright", inset=.05, legend=c("Bellfountain", "Pigeon Butte", 
	"Ft. Hoskins"), merge=F, col=c("black"), pch=c(2,1,3), pt.cex=1.5, 
	cex=.9, pt.lwd=1.5, lty=c(1,2,4), lwd=c(1.5))

dev.off()
	

############ Figure 6

## still nonscarified (luor.ns) ##

Max.survival<-Max.survival*100

tiff(filename="figure6-LUORsurvivetif", res=160, width=960, height=960)

plot(Max.survival~LD2011, pch=(site.code3+1), col=("black"), 
	xlab= "Litter Depth (cm)", ylab="Survival (%)")

	bf.survival<-lm(Max.survival ~ LD2011,subset=(Site=="Bellfountain"))
	abline(bf.survival,col="black", lwd=2 )

	pb.survival<-lm(Max.survival ~ LD2011, subset=(Site=="Pigeon Butte"))
	abline(pb.survival, col="black", lwd=2, lty=2)

	fh.survival<-lm(Max.survival ~ LD2011, subset=(Site=="Ft. Hoskins"))
	abline(fh.survival, col="black", lwd=2, lty=4)

legend("bottomright", inset=.025, legend=c("Bellfountain", "Pigeon Butte", 
	"Ft. Hoskins"), merge=F, col=c("black"), pch=c(2,3,1), pt.cex=1.3, 
	cex=.9, pt.lwd=1.5, lty=c(1,2,4), lwd=c(1.5))
	
dev.off()


########### Figure 7

tiff(filename="figure7LUOR-survive_disttif", res=160, width=960, height=960)

plot(Max.survival~D2011, pch=(site.code3+1), col=("black"),
	xlab= "Disturbance (%)", ylab="Survival (%)")
	bf.survival<-lm(Max.survival ~ D2011,subset=(Site=="Bellfountain"))
	abline(bf.survival,col="black", lwd=2 )

	pb.survival<-lm(Max.survival ~ D2011, subset=(Site=="Pigeon Butte"))
	abline(pb.survival, col="black", lwd=2, lty=2)

	fh.survival<-lm(Max.survival ~ D2011, subset=(Site=="Ft. Hoskins"))
	abline(fh.survival, col="black", lwd=2, lty=4)

legend("topright", inset=.05, legend=c("Bellfountain", "Pigeon Butte", 
	"Ft. Hoskins"), merge=F, col=c("black"), pch=c(2,3,1), pt.cex=1.3, 
	cex=.9, pt.lwd=1.5, lty=c(1,2,4), lwd=c(1.5))
	
dev.off()

detach(luor.ns)
######## Figure 8

sivi<-read.csv('SIVI4r.csv')
attach(sivi)

site.code<-ifelse(Site=="Bellfountain",1,0)
site.code2<-ifelse(Site=="Pigeon Butte",2,0)
site.code3<-site.code+site.code2

Observed.survival<-Observed.survival*100

tiff(filename="figure8-sivitif", res=160, width=960, height=960)

plot(Observed.survival~LD2011, pch=(site.code3+1), col=("black"), 
	xlab= "Litter Depth (cm)", ylab="Survival (%)")

	bf.survival<-lm(Observed.survival ~ LD2011,subset=(Site=="Bellfountain"))
	abline(bf.survival,col="black", lwd=2 )

	pb.survival<-lm(Observed.survival ~ LD2011, subset=(Site=="Pigeon Butte"))
	abline(pb.survival, col="black", lwd=2, lty=2)

	fh.survival<-lm(Observed.survival ~ LD2011, subset=(Site=="Ft. Hoskins"))
	abline(fh.survival, col="black", lwd=2, lty=3)

legend("bottomright", inset=.025, legend=c("Bellfountain", "Pigeon Butte", 
	"Ft. Hoskins"), merge=F, col=c("black"), pch=c(2,3,1), pt.cex=1.3, 
	cex=.9, pt.lwd=1.5, lty=c(1,2,4), lwd=c(1.5))
	
dev.off ()

detach(sivi)
############### Figure 9

cale<- read.csv('CALE4r.csv')
attach(cale)

persistence2011<-persistence2011*100

tiff(filename="figure9a-CALEtif", res=160, width=960, height=960)
plot(persistence2011~G2011, pch=8, col="black",
	xlab= "Grass Cover (%)", ylab="Survival (%)")
	abline(lm(persistence2011~G2011), col="black", lwd=2,)
dev.off()

tiff(filename="figure9b-CALEtif", res=160, width=960, height=960)
plot(persistence2011~Veg2011, pch=8, col="black",
	xlab= "Total Vegetative Cover (%)", ylab="Survival (%)")
	abline(lm(persistence2011~G2011), col="black", lwd=2,)
dev.off()

tiff(filename="figure9c-CALEtif", res=160, width=960, height=960)
plot(persistence2011~LD2011, pch=8, col="black",
	xlab= "Litter Depth (cm)", ylab="Survival (%)")
	abline(lm(persistence2011~G2011), col="black", lwd=2,)
dev.off()
