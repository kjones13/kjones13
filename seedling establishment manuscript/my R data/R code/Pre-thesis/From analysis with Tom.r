##Based on previous analysis with Tom###

litter.lm<-lm(LUSUKI~Av..LitterDepth, subset=(Scarified=="n" & Site=="Ft. Hoskins"))
summary(litter.lm)
## p=0.0518 ##
 
plot(LUSUKI~Av..LitterDepth, subset=(Scarified=="n" & Site=="Ft. Hoskins"))
abline(litter.lm)


litter.grass.lm<-lm(LUSUKI~ + Av..GrassCover+Av..LitterDepth, subset=(Scarified=="n" & Site=="Ft. Hoskins"))
summary(litter.grass.lm)
## p = 0.1590 ##

sum.sivi<-(SIVI ) + (SIVI )

litter.sivi.pb.lm<-lm(SIVI~Av..LitterDepth, subset=(Scarified=="n" & Site=="Pigeon Butte"))
summary(litter.sivi.pb.lm)
##p =0.04567 ##
plot(SIVI~Av..LitterDepth)
abline(litter.sivi.pb.lm)