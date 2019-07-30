# Code to predict returns using d/p of shiller
# 
#
#
#
#
# TO DO:
# Load data      DONE
# Make $1 plots  DONE 
# Define annual returns (cum-sum)
# run predictive regression
# plot predicted ER
# use glmnet for a regularized regresson

# ALSO -- GET --  FRED DATA 

# change for your setting.  You can copy/paste this line and then
# source("Main.PredicR.R")
# to run 
setwd("./")  # Set this as you like
# Send graphics/plots to ....
Gdir <-"./"   #  Graphics printed to local dir
Aratio <- 1.5 # For my plots (1=square)


# usuals
require("ggplot2")
require("grid")
require("dplyr")
require("reshape2")
require('RColorBrewer')

# Libraries - specific to this task
require("MASS")     # for robust regression http://r-statistics.co/Robust-Regression-With-R.html
require("glmnet")   # for LASSO / ELASTIC NET

# a few libs for this project
source("misc_lib.R")




# Load the data
Data.File <- "PredictERsmalldata.csv"
Data <- read.table(Data.File,
        header=TRUE,
        stringsAsFactors = FALSE,  # make strings real strings
        sep=",",
        #nrows=400  # to load part of file
)
Data<-data.frame(Data)    
# quick look   
print(head(Data))
print(tail(Data))

# Some pre-processing .... 
# ditch rows that look to have NA's
# Looks like we are OK - I already removed them by stopping at Dec 2018
Data<-Remove.NA(Data)

# "T" turns out to be a bad name for a variable (means TRUE in R)
colnames(Data)[colnames(Data)=="T"]<-"Time"


# define invest $1 and track (log) of wealth.  
# use cumsum since these are log(1+r) returns.  
Data$logW_Market=cumsum(Data$R_Market)
Data$logW_Rf=cumsum(Data$R_Rf)
# the 60/40 fund from the assigment
Data$logW_LTEF=cumsum((1-0.6)*Data$R_Rf + 0.6*Data$R_Market)

# Plot INVEST 1$ PLOTS
# (lots of other ways to do the plotting here- this is quick for me.)
# https://rpubs.com/euclid/343644

# I try not to use just "red" and "blue"
some.color<-"Set1"
display.brewer.pal(8, some.color)
Pallet <- brewer.pal(8, some.color)

# Simple /Tedious way
p<-ggplot(data=Data)
p<-p+geom_line(aes(x=Time,y=logW_Market), color=Pallet[1])
p<-p+geom_line(aes(x=Time,y=logW_Rf), color=Pallet[1])
p<-p+geom_line(aes(x=Time,y=logW_Rf), color=Pallet[2])
p<-p+geom_line(aes(x=Time,y=logW_LTEF), color=Pallet[3])
# add title and legend / text etc.
print(p)


# here is a better way to do that.  
# "melt" the data into one big line and then keep "logW*"
# If this throws an error try "as.numeric(values)" -- sometimes "values" comes back as string
# The as.numeric to fix "characters"; not sure why it was returning number in " "
p<-ggplot(data=
			melt(
				data=Data[,grepl("(Time|logW)",colnames(Data))],
				,id="Time"
				)  %>% arrange(Time)
	)
p<-p+geom_line(aes(x=Time, y=as.numeric(value),color=variable)) 
p<-p+scale_color_manual(values=Pallet)
p<-p+labs(
	color="portfolio",
	x="Time",
	y="log Wealth",
	title="$1 in 1960"
)
print(p)
print.plot(p,"OneDollar.pdf",dir=Gdir,aspect.ratio=Aratio)

# Before preticting things, get a guess at R2.  
# Suppose you were perfect(!) and could put your $ into R_RF when eR_market<0 (and vice versa.)
# This is impossible (as you will see)
Data$R_Max <-  (apply(Data[,colnames(Data)%in%c("R_Rf","R_Market")], 1, max))  # I hate apply!
Data$logW_MAX=cumsum(Data$R_Max)

#re-plot that (need a function for plotting)
p<-ggplot(data=
			melt(
				data=Data[,grepl("(Time|logW)",colnames(Data))],
				,id="Time"
				)  %>% arrange(Time)
	)
p<-p+geom_line(aes(x=Time, y=as.numeric(value),color=variable)) 
p<-p+scale_color_manual(values=Pallet)
p<-p+labs(
	color="portfolio",
	x="Time",
	y="log Wealth",
	title="$1 in 1960"
)
print(p)
print.plot(p,"OneDollarMAX.pdf",dir=Gdir,aspect.ratio=Aratio)

Wealth <- exp(Data[,grepl("(logW)",colnames(Data))])
colnames(Wealth)<-sub("log","",colnames(Wealth))
Wealth <- cbind(Data$TIME,Data$Time,Wealth)
print(head(Wealth))
print(tail(Wealth))



### PREDICTIVE REGRESSIONS ### 
# the trick here is to align the LHS and RHS variables.  Then we can
# run a simple regression ("linear model") or (later) a fancy one.
#
# Our predicors
Data$DPratio <- (1/Data$PriceDividendRatio)

# Generate FUTURE reutrn (as of date t what will be return t -> t+n)
horizon <- 12 # 
A<- cumsum(Data$eR_Market)  # note excess returns
Data$eR.Market.tplush<- (lead(A,n=horizon) - A )/horizon

print(
	head(
		Data[,colnames(Data)%in%c("TIME","Time","eR.Market.tplush","DPratio")],
		n=20
		)
	)
	
# Simple linear regression	
C<-lm(eR.Market.tplush ~ DPratio,data=Remove.NA(Data))
print(summary(C))
Data$eR.Predict.lm <- predict(C, newdata=Data)

# The cumulative sum gives us lots of auto-correlated errors so a "robust" regression
# is a bit better
C<-rlm(eR.Market.tplush ~ DPratio,data=Remove.NA(Data))
print(summary(C))

Data$eR.Predict.rlm <- predict(C, newdata=Data) 
p<-ggplot(data=
			melt(
				data=Data[,grepl("(Time|Predict.rlm)",colnames(Data))],
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
print.plot(p,"PredictER1.pdf",dir=Gdir,aspect.ratio=Aratio)

# Note low R2 by including "actual"
Data$eR.Predict.rlm <- predict(C, newdata=Data) 
p<-ggplot(data=
			melt(
				data=Data[,grepl("(Time|Predict.rlm|eR.Market.tplush)",colnames(Data))],
				,id="Time"
				)  %>% arrange(Time)
	)	
p<-p+geom_line(aes(x=Time, y=as.numeric(value)*12,color=variable)) 
p<-p+scale_color_manual(values=Pallet)
p<-p+labs(
	color="eR_Market",
	x="Time",
	y="r (annual)",
	title=""
)
print(p)
print.plot(p,"PredictER2.pdf",dir=Gdir,aspect.ratio=Aratio)

p<-ggplot(data=
			melt(
				data=Data[,grepl("(DPratio|Predict.rlm|eR.Market.tplush)",colnames(Data))],
				,id="DPratio"
				)  %>% arrange(DPratio)
	)	
p<-p+geom_point(aes(x=DPratio, y=as.numeric(value)*12,color=variable)) 
p<-p+scale_color_manual(values=Pallet)
p<-p+labs(
	color="eR_Market",
	x="DPratio",
	y="r (annual)",
	title=""
)
print(p)
print.plot(p,"PredictER3.pdf",dir=Gdir,aspect.ratio=Aratio)


#### TRY REGULARIZED REGRESSION
source("regularized_lib.R")

	

