germ<-read.csv('IRTEgerm.csv')
attach(germ)
names(germ)


anova(lm(percentgerm~treatment))
summary(lm(percentgerm~treatment))
plot(percentgerm~treatment)
win.graph()
summary(lm(percentgerm~warm+ cold))
summary(lm(percentgerm~warm*cold))
anova(lm(percentgerm~warm*cold))


plot(cold, percentgerm, pch=(warm+16), col=(warm+9))

	warm.0<-germ[which(germ$warm=='0'),]
	lines(lowess(warm.0$cold, warm.0$percentgerm), col="black")

	warm.2<-germ[which(germ$warm=='2'),]
	lines(lowess(warm.2$cold, warm.2$percentgerm), col="green")

	warm.4<-germ[which(germ$warm=='4'),]
	lines(lowess(warm.4$cold, warm.4$percentgerm), col="cyan")
	
warm.2




library(car)
win.graph()
cr.plots(lm(percentgerm ~ warm+cold, data=germ),
variable=cold,line=TRUE,smooth=FALSE)

fit1<-aov(percentgerm~treatment)
TukeyHSD(fit1)
plot(TukeyHSD(fit1, "treatment"))
plot(TukeyHSD(fit1, "treatment", ordered=TRUE))

germ.aov<-aov(percentgerm~cold*warm)
summary(germ.aov)
print(model.tables(germ.aov, "means"),)

barplot
germ.table<-table(warm.0$percentgerm, warm.2$percentgerm, 
	warm.4$percentgerm)

barplot(germ.table)

warm2<-as.factor(warm)
cold2<-as.factor(cold)
treatment2<-as.factor(treatment

aov<-aov(percentgerm~ cold2, data=germ)

> local({pkg <- select.list(sort(.packages(all.available = TRUE)))
+ if(nchar(pkg)) library(pkg, character.only=TRUE)})


tuk2 <- glht(aov, linfct = mcp(cold2 = "Tukey"))
tuk.cld2 <- cld(tuk2)
 old.par <- par( mai=c(1,1,1.25,1))
  plot(tuk.cld2, col=c("green", "red", "blue"))
  par(old.par)



  ### now using covariates
  data(warpbreaks)
  amod2 <- aov(breaks ~ tension + wool, data = warpbreaks)
  ### specify all pair-wise comparisons among levels of variable "tension"
  tuk2 <- glht(amod2, linfct = mcp(tension = "Tukey"))
  ### extract information
  tuk.cld2 <- cld(tuk2)
  ### use sufficiently large upper margin
  old.par <- par( mai=c(1,1,1.25,1))
  ### plot using different colors
  plot(tuk.cld2, col=c("black", "red", "blue"))
  par(old.par)






