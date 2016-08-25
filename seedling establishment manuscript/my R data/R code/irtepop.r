pop<-read.csv('irtepop.csv')
attach(pop)
names(pop)

sourcetreat<-("Source","treatment")

sourcetreat


anova(lm(prop~treatment+Source))
anova(lm(prop~treatment+Source+treatment:Source))
anova(lm(adj.prop~treatment+Source+treatment:Source))

fit1<-aov(prop~sourcetreat)
TukeyHSD(fit1)
plot(TukeyHSD(fit1, "sourcetreat"))
plot(TukeyHSD(fit1, "sourcetreat", ordered=TRUE))