meta<- read.csv('metaLUOR.csv')
attach(meta)
names(meta)

summary(meta)

levels(SiteID)

summary(lm(proportion~Scarified))
win.graph()
boxplot(proportion~SiteID)
boxplot(proportion~SiteID+Scarified)
win.graph()
boxplot(proportion~Scarified)

meta.ns<-meta[which(meta$Scarified=='no'),]
meta.s<- meta[which(meta$Scarified=='yes'),]

##
summary(lm(proportion~Site+Scarified))
Confidence Interval for Scarified
	-0.08041+(-6.256*.975)*0.01285
	-0.08041-(-6.256*.975)*0.01285	
##

t.test(meta.ns$proportion, meta.s$proportion)


(mean(meta.s$proportion))- (mean(meta.ns$proportion))


meta.GO<-meta[which(meta$SiteID=='Green Oaks'),]
meta.GO.ns<-meta.GO[which(meta.GO$Scarified=='no'),]
meta.GO.s<-meta.GO[which(meta.GO$Scarified=='yes'),]

meta.EE<-meta[which(meta$SiteID=='EE wilson2008'),]
meta.EE.ns<-meta.EE[which(meta.EE$Scarified=='no'),]
meta.EE.s<-meta.EE[which(meta.EE$Scarified=='yes'),]

meta.FG09<-meta[which(meta$SiteID=='Fitton Green2009'),]
meta.FG09.ns<-meta.FG09[which(meta.FG09$Scarified=='A'),]
meta.FG09.s<-meta.FG09[which(meta.FG09$Scarified=='B'),]

meta.FG10<-meta[which(meta$SiteID=='Fitton Green2010'),]
meta.FG10.ns<-meta.FG10[which(meta.FG10$Scarified=='no'),]
meta.FG10.s<-meta.FG10[which(meta.FG10$Scarified=='yes'),]

meta.I<-meta[which(meta$SiteID=='Isabelle2000'),]
meta.I.ns<-meta.I[which(meta.I$Scarified=='no'),]
meta.I.s<-meta.I[which(meta.I$Scarified=='yes'),]

meta.PP10<-meta[which(meta$SiteID=='Philomath Praire2010'),]
meta.PP10.ns<-meta.PP10[which(meta.PP10$Scarified=='no'),]
meta.PP10.s<-meta.PP10[which(meta.PP10$Scarified=='yes'),]

meta.PB<-meta[which(meta$SiteID=='Pigeon Butte2003'),]
meta.PB.ns<-meta.PB[which(meta.PB$Scarified=='no'),]
meta.PB.s<-meta.PB[which(meta.PB$Scarified=='yes'),]

meta.Rain<-meta[which(meta$SiteID=='Raindance2010'),]
meta.Rain.ns<-meta.Rain[which(meta.Rain$Scarified=='no'),]
meta.Rain.s<-meta.Rain[which(meta.Rain$Scarified=='yes'),]

meta.ST03<-meta[which(meta$SiteID=='Starck2003'),]
meta.ST03.ns<-meta.ST03[which(meta.ST03$Scarified=='no'),]
meta.ST03.s<-meta.ST03[which(meta.ST03$Scarified=='yes'),]

meta.ST04<-meta[which(meta$SiteID=='Starck2004'),]
meta.ST04.ns<-meta.ST04[which(meta.ST04$Scarified=='no'),]
meta.ST04.s<-meta.ST04[which(meta.ST04$Scarified=='yes'),]

meta.BF<-meta[which(meta$SiteID=='Bellfountain2010'),]
meta.BF.ns<-meta.BF[which(meta.BF$Scarified=='no'),]
meta.BF.s<-meta.BF[which(meta.BF$Scarified=='yes'),]

meta.PB03<-meta[which(meta$SiteID=='Pigeon Butte2010'),]
meta.PB03.ns<-meta.PB03[which(meta.PB03$Scarified=='no'),]
meta.PB03.s<-meta.PB03[which(meta.PB03$Scarified=='yes'),]

meta.FH<-meta[which(meta$SiteID=='Ft. Hoskins2010'),]
meta.FH.ns<-meta.FH[which(meta.FH$Scarified=='no'),]
meta.FH.s<-meta.FH[which(meta.FH$Scarified=='yes'),]

meta.C02<-meta[which(meta$SiteID=='Coble2002'),]
meta.C02.ns<-meta.C02[which(meta.C02$Scarified=='no'),]
meta.C02.s<-meta.C02[which(meta.C02$Scarified=='yes'),]

meta.C03<-meta[which(meta$SiteID=='Coble2003'),]
meta.C03.ns<-meta.C03[which(meta.C03$Scarified=='no'),]
meta.C03.s<-meta.C03[which(meta.C03$Scarified=='yes'),]



t.test(meta.EE.s$Germinants, meta.EE.ns$Germinants, paired=FALSE)
t.test(meta.FG09.s$Germinants, meta.FG09.ns$Germinants, paired=FALSE)
t.test(meta.FG10.s$Germinants, meta.FG10.ns$Germinants, paired=FALSE)
t.test(meta.I.s$Germinants, meta.I.ns$Germinants, paired=FALSE)
t.test(meta.PP10.s$Germinants, meta.PP10.ns$Germinants, paired=FALSE)
t.test(meta.PB.s$Germinants, meta.PB.ns$Germinants, paired=FALSE)
t.test(meta.Rain.s$Germinants, meta.Rain.ns$Germinants, paired=TRUE)
t.test(meta.ST03.s$Germinants,meta.ST03.ns$Germinants, paired=TRUE)
t.test(meta.ST04.s$Germinants, meta.ST04.ns$Germinants,  paired=FALSE)
t.test(meta.BF.s$Germinants, meta.BF.ns$Germinants,  paired=TRUE)
t.test(meta.PB03.s$Germinants, meta.PB03.ns$Germinants,  paired=TRUE)
t.test(meta.FH.s$Germinants, meta.FH.ns$Germinants,  paired=FALSE)

t.test(meta.EE.s$proportion, meta.EE.ns$proportion, paired=FALSE)
	t.test(meta.EE.s$proportion, meta.EE.ns$proportion, paired=TRUE, alternative=c("greater"))

t.test(meta.FG10.s$proportion, meta.FG10.ns$proportion, paired=FALSE)
t.test(meta.I.s$proportion, meta.I.ns$proportion, paired=TRUE)
t.test(meta.PP10.s$proportion, meta.PP10.ns$proportion, paired=FALSE)
t.test(meta.PB.s$proportion, meta.PB.ns$proportion, paired=TRUE,)
t.test(meta.Rain.s$proportion, meta.Rain.ns$proportion)
t.test(meta.ST03.s$proportion,meta.ST03.ns$proportion, paired=FALSE,)

t.test(meta.BF.s$proportion, meta.BF.ns$proportion,  paired=TRUE)
t.test(meta.PB03.s$proportion, meta.PB03.ns$proportion,  paired=TRUE)
t.test(meta.FH.s$proportion, meta.FH.ns$proportion,  paired=TRUE)
t.test(meta.C02.s$proportion, meta.C02.ns$proportion,  paired=TRUE)
t.test(meta.C03.s$proportion, meta.C03.ns$proportion,  paired=TRUE)
t.test(meta.GO.s$proportion, meta.GO.ns$proportion,  paired=FALSE)



hist(meta$proportion[which(meta$Scarified=='no')])
hist(meta$proportion[which(meta$Scarified=='yes')])
var(meta$proportion[which(meta$Scarified=='no')])
boxplot(proportion~Scarified)
## assumptions are suspect  ##


wilcox.test(x=proportion[Scarified=='yes'], y=proportion[Scarified=='no'])

plot(proportion~Scarified)
plot(proportion~SiteID)

