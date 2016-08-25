meta<-read.csv('meta.LUOR.summary.csv')
attach(meta)
names(meta)

t.test(d, alternative=c("greater"))
barplot(d)
hist(d)

##Fixed effects model##

# Weights
w <- 1/r

# Combined effect
est.fixef = sum(d*w)/sum(w)
est.fixef

# Standard error of the combined effect
se.fixef = sqrt(1/sum(w))
se.fixef

# The Z-value
z.fixef = est.fixef/se.fixef
z.fixef

# p-values
fe_p1t = pnorm(z.fixef,lower=F)   #1-tailed p-value
fe_p1t
fe_p2t = 2*pnorm(z.fixef,lower=F) #2-tailed p-value
fe_p2t

# 95% confidence interval
fe.lowerconf =  est.fixef + qnorm(0.025)*se.fixef
fe.upperconf =  est.fixef + qnorm(0.975)*se.fixef

############## Random Effects Model #############