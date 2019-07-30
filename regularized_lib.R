### REGULARIZED REGRESSIONS #####
# How does a regularized regression impact our forecast? 
# Makes it smoother.

require("glmnet") 
library("broom")

# glmnet likes at least 2 cols (this is silly with 1, but we are learning)
# glmnet fusses over NA's


# This is a bit sloppy as Remove.NA is twice
# the as.matrix is needed to get "double" (otherwise something goofy in there)
x<- as.matrix(cbind(0,Remove.NA(Data)[,colnames(Data)%in%c("DPratio")]))
x.all<- as.matrix(cbind(0,(Data)[,colnames(Data)%in%c("DPratio")]))  # to get 2018 in forecast
y<- Remove.NA(Data)[,colnames(Data)%in%c("eR.Market.tplush")]

# ALPHA=1 -->LASSO  ALPHA=0-->ridge l2
# the glmnet package returns the results for many Lambdas (the regularlization path)
# we can pick a few


Interesting<-c(.01,.1,.25,.5,1)
for (a in c(0,0.05,0.1,0.5,1.0)){

	
	fit <- cv.glmnet(x=(x),y=y, family = "gaussian", alpha=a)

	results <- tidy(fit) 
	Lambda <- results$lambda 

	hat<-predict(fit,newx=(x.all),s=Lambda)
	sigmas <- apply(hat,2,sd)
	

	keep<-sapply(quantile(sigmas,probs=Interesting),function(x){which.min(abs(sigmas - x))})
	
	hat<-data.frame(hat[,keep])
	a.text<-paste("eR.Predict.a=",a,sep="")
	colnames(hat)<-paste(a.text,"L=",Lambda[keep],sep="")
	colnames(hat)<-sprintf(paste(a.text,":L=",'%.3e',sep=""), Lambda[keep])
	hat<-cbind(Time=Data$Time,hat)
	
	p<-ggplot(data=
			melt(
				data=hat[,grepl("(Time|Predict.a)",colnames(hat))],
				,id="Time"
				)  %>% arrange(Time)
	)	
	p<-p+geom_line(aes(x=Time, y=as.numeric(value)*12,color=variable)) 
	p<-p+scale_color_manual(values=Pallet)
	p<-p+labs(
		color="eR_Market",
		x="Time",	
		y="r (annaul)",
		title=""
	)
	print(p)	
	f<-paste("PredictER.",a,sep="")
	print.plot(p,file=f.pdf(f,dir=""),dir=Gdir,aspect.ratio=Aratio)	
	
}