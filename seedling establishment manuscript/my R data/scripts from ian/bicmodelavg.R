#This is a simple function to calculate coefficients and their associated standard errors of an average linear model from BIC weights of a inputted list of models based on Buckland, S. T., K. P. Burnham, and N. H. Augustin. 1997. Model selection: an integral part of inference. Biometrics 53:603-618.

bicmodelavg<-function(model1,...){ #inputs each fitted model
	bics<-BIC(model1,...) #calculates each model's BIC statistic
	mods<-list(model1,...) #lists the models
	mod.smry<-sapply(mods,summary) #lists each model's summary
	coef.errs<-mod.smry[4,] #collects each model's coefficients, standard errors, and t statistics
	var.labl<-sapply(coef.errs,row.names) #lists the coefficients from all models
	avg.labl<-unique(unlist(var.labl)) #lists coefficients without duplicates
	ord.mat<-matrix(nrow=length(avg.labl),ncol=length(mods)) #creates an empty coefficient matrix
	coef.mat<-ord.mat #to store coefficients
	errs.mat<-ord.mat #to store coef std err
	for(i in 1:length(mods)){
		ord.mat[,i]<-match(avg.labl,var.labl[[i]]) #matches coefficients across models
		coef.mat[,i]<-coef.errs[[i]][,1][ord.mat[,i]] #sorts coefficients relative to each model
		errs.mat[,i]<-coef.errs[[i]][,2][ord.mat[,i]]} #sorts coef std errors relative to each model
	coef.mat<-data.frame(coef.mat, row.names=avg.labl) #labels the coefficients
	errs.mat<-data.frame(errs.mat, row.names=avg.labl) #labels the coefficient standard errors
	expnegbics<-exp(-(bics$BIC)/2) #BIC weight of each model
	wts<-expnegbics/sum(expnegbics) #BIC weights proportional to one	
	new.coef<-t(coef.mat)*wts #weight each model's coefficients
	new.errs<-wts^2*(t(errs.mat)^2+(t(coef.mat)-new.coef)^2) #unconditional std err based on the conditional std err and the variation in the weighted coefficients	
	Coef_Avg<-rowSums(t(new.coef),na.rm=T) #sum the weighted coefficients across all models, with values of 0 for models lacking any of the coefficients
	Coef_SE<-sqrt(rowSums(t(new.errs),na.rm=T)) #sum the unconditional standard errors across all models, with values of 0 for models lacking any of the coefficients
 	mod.data<-as.data.frame(do.call('cbind',sapply(mods,model.frame))) #collects all the model data
	mod.vars<-match(avg.labl,names(mod.data)) #finds redundant data
	ma.data<-as.matrix(cbind(mod.data[1],mod.data[,mod.vars[-1]])) #eliminates redundant data
	ma.pred<-cbind(1,ma.data[,-1])%*%Coef_Avg #averaged model predicted response
	t.stat<-Coef_Avg/Coef_SE #coefficient t statistic
	t.pval<-(1-pt(abs(t.stat),df=length(ma.pred)-length(Coef_Avg)))*2 #coefficient t p-value
	mse.avg<-sum((ma.data[,1]-ma.pred)^2)/(length(ma.pred)-length(Coef_Avg)) #averaged model mean squared error
	mse<-as.numeric(cbind(sqrt(mse.avg),rbind(mod.smry[6,])))^2 #combine model mean squared error
	rsq.avg<-cor(ma.data[,1],ma.pred)^2 #averaged model r-squared
	rsq<-as.numeric(cbind(rsq.avg,rbind(mod.smry[8,]))) #combine model r-squared values
	mod.sts<-do.call('rbind',mod.smry[10,]) #collect model F test statistics
	f.stat.avg<-(length(ma.pred)-length(Coef_Avg))/(length(Coef_Avg)-1)*rsq.avg/(1-rsq.avg) #averaged model F statistic
	f.stat<-c(f.stat.avg,mod.sts[,1]) #combine F statistics
	f.pval.avg<-1-pf(f.stat.avg,length(Coef_Avg)-1,length(ma.pred)-length(Coef_Avg)) #averaged model F p-value
	f.pval<-c(f.pval.avg,1-pf(mod.sts[,1],mod.sts[,2],mod.sts[,3])) #combine F p-values
	coef.stats<-cbind(Coef_Avg,Coef_SE,t.stat,t.pval) #combine the coefficient statistics
	coef.stats<-data.frame(coef.stats,row.names=avg.labl) #label coefficient statistics
	mod.stats<-t(rbind(mse,rsq,f.stat,f.pval)) #combine the model statistics
	mod.stats<-data.frame(mod.stats,row.names=c('modelavg',paste('model',1:length(mods),sep=""))) #label model statistics
	final.stats<-list(coef.stats,mod.stats) #list coefficient and model statistics
	assign("modelavg",final.stats,envir=.GlobalEnv) #store the output in the global environment
	plot(ma.pred,ma.data[,1],main='Averaged Model Fit',xlab='Fitted Values',ylab='Observed Values',xlim=c(0,max(ma.data[,1])),ylim=c(0,max(ma.data[,1])));abline(0,1) #plot fit of averaged model
	return(modelavg)}