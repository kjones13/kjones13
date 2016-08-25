luor<-read.csv('KL seed scarification Severns.csv')
attach(luor)
names(luor)

S<-luor[which(luor$Source=='Green S'),]
U<-luor[which(luor$Source=='Green US'),]

t.test(S$propger,U$propgerm)



