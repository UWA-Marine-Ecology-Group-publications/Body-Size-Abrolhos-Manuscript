# TODO: Re reun with different smoothing parameters - sheather jones and CV
# 
# Author: rohan.sadler
###############################################################################

library(sm)
library(parallel)
# library(multicore)
library(mgcv)
library(fda)
#setwd("/home/johndog64/Desktop/fish_assemblages")
# setwd("C:/Users/johndog64/workspace/fish_assemblages")
setwd("~/ownCloud/Rr/Assemblage FDA")
rm(list=ls())


load("Langlois S2 Appendix Dataset used in the current study")
Assemb3<-na.omit(Assemb)
Sample.Sizes.Depths=array(data=rep('a',length(unique(Assemb3$Year))*length(unique(Assemb3$Status))*length(unique(Assemb3$Location))*length(unique(Assemb3$Op.Depth))*5),dim=c(length(unique(Assemb3$Year))*length(unique(Assemb3$Status))*length(unique(Assemb3$Location))*length(unique(Assemb3$Op.Depth)),5)) #arrays are the way to go for pasting in factor levels from loops without them being coerced into numeric variables I convert it to a dataframe later

counter=0

for(i in 1:length(unique(Assemb3$Year))){
	for(j in 1:length(unique(Assemb3$Status))){
		for(k in 1:length(unique(Assemb3$Location))){
			
			counter=counter+1
			year.i=unique(Assemb3$Year)[i]
			status.ij=unique(Assemb3[Assemb3$Year==year.i,]$Status)[j]
			location.ijk=unique(Assemb3[Assemb3$Year==year.i&Assemb3$Status==status.ij,]$Location)[k]
			
			for(m in 1:length(unique(Assemb3[Assemb3$Year==year.i&Assemb3$Status==status.ij&Assemb3$Location==location.ijk,]$Op.Depth))){
				
				depth.ijkm=unique(Assemb3[Assemb3$Year==year.i&Assemb3$Status==status.ij,]$Op.Depth)[m]
				
				Sample.Sizes.Depths[counter,1]=(year.i)
				Sample.Sizes.Depths[counter,2]=paste(as.character(status.ij))
				Sample.Sizes.Depths[counter,3]=paste(as.character(location.ijk))
				Sample.Sizes.Depths[counter,4]=paste(as.character(depth.ijkm))
				Sample.Sizes.Depths[counter,5]=length(Assemb3[Assemb3$Year==year.i&Assemb3$Status==status.ij&Assemb3$Location==location.ijk&Assemb3$Op.Depth==depth.ijkm,]$Length)
				
			}
		}
	}
}

##########
# ro comment
# alternative: expand.grid(unique(Assemb3$Year),unique(Assemb3$Status),etc)
#    then do a single for loop using: length(which(Assemb3$Year==expand.grid.object$year[i] & ... ))
#    and then exclude zero counts before converting to data.frame
# i.e. about five lines of code :)
##########

Sample.Sizes.Depths<-data.frame(Sample.Sizes.Depths)

colnames(Sample.Sizes.Depths)<-c('Year','Status','Location','Depth','Sample.Size')

Sample.Sizes.Depths

#######
# ro comment
# Sample.Sizes.Depths only has 40 meaningful entries
########

#check:

Assemb3[Assemb3$Year=='2006'&Assemb3$Status=='MPA'&Assemb3$Location=='Pelsaert'&Assemb3$Op.Depth=='D',]$Length

length(Assemb3[Assemb3$Year=='2006'&Assemb3$Status=='MPA'&Assemb3$Location=='Pelsaert'&Assemb3$Op.Depth=='D',]$Length) #same as Sample.Sizes.Depths row 1, good

#so we do not have enough data to consider Deep and Shallow sites separately- we'll have to pool them

#but...

unique(Assemb[Assemb$Location=='North',]$Op.Depth) #...there are no Location=North Depth=Deep sites

#so we shall exclude Location=North from the analysis

##########
# Ro comment
#   or allow an unbalanced factor design as North constitues a lot of data
#     may be more efficient to exclude 2006 (as you have done)
#     but I would go for an unbalanced design.



Sample.Sizes=array(data=rep('a',5*2*4*2),dim=c(5*2*4,4)) #arrays are the way to go for pasting in factor levels from loops without them being coerced into numeric variables. I convert it to a dataframe later

counter=0

for(i in 1:length(unique(Assemb3$Year))){
	for(j in 1:length(unique(Assemb3$Status))){
		for(k in 1:length(unique(Assemb3$Location))){
			counter=counter+1
			year.i=unique(Assemb3$Year)[i]
			status.ij=unique(Assemb3[Assemb3$Year==year.i,]$Status)[j]
			location.ijk=unique(Assemb3[Assemb3$Year==year.i&Assemb3$Status==status.ij,]$Location)[k]
			Sample.Sizes[counter,1]=(year.i)
			Sample.Sizes[counter,2]=print(paste(as.character(status.ij)),quote=F)
			Sample.Sizes[counter,3]=as.character(location.ijk)
			Sample.Sizes[counter,4]=length(Assemb3[Assemb3$Year==year.i&Assemb3$Status==status.ij&Assemb3$Location==location.ijk,]$Length)
		}
	}
}

Sample.Sizes<-data.frame(Sample.Sizes)
########
# Ro Comment
#   the <NA>s for Location for 2006 are simply a total of the preceding
#      three counts, hence remove
########

colnames(Sample.Sizes)<-c('Year','Status','Location','Sample.Size')

Sample.Sizes #this lists the number of fish length observations we have at each Year:Protection Status:Island group combination

s.size<-as.numeric(attr(unclass(Sample.Sizes$Sample.Size),"levels")[unclass(Sample.Sizes$Sample.Size)])
# turning factors to numeric is painfall - best to define different variables
#    using their correct mode in data.frame as array to data.frame turns
#    everything into factor if but one variable value is of mode character.

aggregate(s.size~Sample.Sizes$Location,FUN=sum)
# 1623 for north
aggregate(s.size~Sample.Sizes$Year,FUN=sum)
aggregate(s.size~Sample.Sizes$Year,FUN=sum)[1,2]-sum(s.size[is.na(Sample.Sizes$Location)])
# 1960 for 2006

# hence excluding north is a better option

# aggregate can be applied also to the below code, and perhaps can be applied
#   to your previous loop as well by settting s.size<-rep(1,nrow(Assemb3))
as.data.frame(aggregate(s.size~Sample.Sizes$Year+Sample.Sizes$Status+Sample.Sizes$Location,FUN=mean))



Assemb3[Assemb3$Year=='2009'&Assemb3$Location=='North'&Assemb3$Status=='MPA',]$Length

length(Assemb3[Assemb3$Year=='2009'&Assemb3$Location=='North'&Assemb3$Status=='MPA',]$Length) #matches corresponding entry in Sample.Size

#write.table(Sample.Sizes,sep=',',file='Abrols_Sample.Sizes.csv',row.names=F)

length(Assemb3[Assemb3$Year=='2009'&Assemb3$Status=='MPA'&Assemb3$Location=='Pelsaert'&Assemb3$Op.Depth=='S',]$Length)

#write.table(Sample.Sizes.Depth,sep=',',file='Abrols_Sample_Sizes_Depth.csv',row.names=F)

#summary(Assemb2$Location) # three locations within each Year:Status combination these are our base level of replication

#a condition of using these data is that we do not use more than 3 years worth of it

#using island groups Easter,Pelsaert & Wallabi and years 2009, 2008 2010 gives largest minimum sample size (106) of all possible sets of three years and island groups

Assemb2<-Assemb[Assemb$Year==2008|Assemb$Year==2009|Assemb$Year==2010,] #taking just years 2009, 2008 and 2010 for the analysis

Assemb2<-Assemb2[!Assemb2$Location=='North',] #excluding Location North from the analysis and thus using E,P & W 2009, 2008 2010 gives minimum sample size of 106 #this is the best we can get if we are to use three years and three locations

Assemb2<-na.omit(Assemb2)

unique(Assemb2$Year)

unique(Assemb2$Status)

summary(Assemb2$Length)

min.length<-min(Assemb2$Length)

max.length<-2000-(26*50) #max.length=700 #this comes from the size-spectra analysis so we are comparing the same data
max.length<-1000 #max.length=700 #this comes from the size-spectra analysis so we are comparing the same data

# just take this default, though we do not need to: 
length(which(Assemb2$Length>max.length))/nrow(Assemb2)

Assemb5<-Assemb2[Assemb2$Length>min.length&Assemb2$Length<max.length,]

Assemb5<-na.omit(Assemb5)

Data<-Assemb5

Yr<-as.character(unique(Data$Year))

Sta<-as.character(unique(Data$Status))

Loc<-as.character(unique(Data$Location))

  ######
  # ro comment
  # faster alternative to your 29 minutes: 

  xx<-expand.grid(Yr,Sta,Loc)
  x.1<-rep(1,nrow(Assemb5))
  x.agg<-as.data.frame(aggregate(x.1~Assemb5$Year+Assemb5$Status+Assemb5$Location,FUN=sum)) # demo of a comment above, but needless here

  x.seq<-seq(min(Data$Length)-0.045,max(Data$Length)+0.045,by=0.05) # 0.045 chosen to offset the 0.05 steps
  x.seq<-x.seq[-length(x.seq)] # check that this last is redundant - a function of how the sequence x.seq is constructed.
  yy<-matrix(0,nrow=length(x.seq),ncol=nrow(xx))
  for(i in 1:nrow(xx)) {
     x.length<-Data$Length[Data$Year==as.character(xx[i,1]) & Data$Status==as.character(xx[i,2]) & Data$Location==as.character(xx[i,3])]
     yy[,i]<-sapply(x.seq,function(x) length(which(x.length<x)))
     # results in a cumsum count of observations at each "quantile" defined by x.seq
  }
  # yy.1<-yy    # for debugging
  #  x.seq.1<-x.seq  # for debugging
#  yy<-yy.1
 # x.seq<-x.seq.1
  b<-5              # same parameter as before
  knots<-x.seq[1]
  flip<-FALSE
  counter<-1
system.time(
  while(flip==FALSE) {
    cut.which<-which(apply(yy,1,min)==b)

   # warning message control
   if(length(cut.which)==0) {
      cat("Cut doesn't make it: ",x.seq[cut],"\n")   
      break
   }

   cut<-min(cut.which)  #  b enters here as a minimum number of observations
                               #     note that you are choosing max here, not min.

  #   silly value control
  #   if(is.na(x.seq[cut])) {
  #      cat("Cut doesn't make it: ",cut,"\n")   
  #      break
  #   }

     knots<-c(knots,x.seq[cut])
#     yy<-yy-b                      # parameter b features here again.
     yy<-t(t(yy)-yy[cut,])
   
     cat("this is knot number:  ",counter,"\n")
     counter<-counter+1
     if( max(yy)<=5) flip<-TRUE
  }
)

 yy[x.seq<=knots[3] & x.seq>knots[2],]


 for(i in 1:18) {
	 x.length<-Data$Length[Data$Year==as.character(xx[i,1]) & Data$Status==as.character(xx[i,2]) & Data$Location==as.character(xx[i,3])]
	 #cat("For combo ",i,"  there are: ",length(x.length[x.length>=knots[11]]),"  counts.\n")
	 cat("For combo ",i,"  there are: ",length(x.length[x.length<=knots[11] & x.length>knots[10]]),"  counts.\n")
 }
 
#   user  system elapsed 
#   3.25    0.03    3.26 

knots
length(Data$Length[Data$Length>700])
########
# Ro comment
# to validate, we try and do the above more simply - which will help to get a cross validation error value.

# from page 53: Ramsay, Hooker, Graves 2009, Funcational DAta Analysis with R and MATLAB
#    this extends naturally to calculating F-statistics for a bootstrap sample due to violations of
#    anova distributional assumptions with spline density regularisation.
#     however, permutation test is better a la Cardot et al. (2007). No effect and lack-of-fit permutation tests
#                       for functional regression. Computational Statistics, 22:371-390. 

# Note to self: subjects are the different sites, hence three subjects for each status*year "treatment"
#    easily adapt this for unbalanced data, leading to model for 2006-2010.

  #  knots[length(knots)]<-ceiling(knots[length(knots)-1]+1) 
    knots[1]<-floor(knots[1])
    knots<-c(knots,700,800,900,1000)
  #  lambda.start<-1500   # before undertaking cross-validation to choose lambda
  # after undertaking cross-validation to choose lambda
  # see code below definition of function draw.density()
  #  lambda.start<-lambda.list[which.min(cv.list)]   #1513.561

    assemb.ro<-Assemb5[Assemb5$Length<knots[length(knots)],]
    ro.basis<-create.bspline.basis(breaks=knots,norder=4)# creating a basis system of order 4 gives cubic basis functions which ensure smooth joins between knots (we are not examining derrivatives of effects so cubics are sufficient- (if smooth joins between knots of derivatives are required higher order splines are required))
#    ro.WfdParobj<- fdPar(fdobj=ro.basis, Lfdobj=2, lambda=lambda.start)# Lfdobj=2 means that the roughness penalty is integrated squared second derivative
#  depending on boot scheme; but this one assumes time consistent catch efficiency, which is different at different locations*year.    
    # regularise each data set by using density.
    # means that standard anova assumptions are voided; so do as Ben does and get at p-values with bootstrap.
    year.unique<-unique(assemb.ro$Year)
    status.unique<-unique(assemb.ro$Status)
    location.unique<-unique(assemb.ro$Location)
    xx<-expand.grid(year.unique,status.unique)  # alter to consider Location
    EvPts<-seq(min(knots),max(knots))
    ro.eval.basis<-eval.basis(EvPts,ro.basis)  # smooth.predictions given by density.fd
    ro.predictor.mat<-matrix(0,ncol=ncol(ro.eval.basis),nrow=length(EvPts)*nrow(xx))
    for(i in 1:ncol(ro.predictor.mat)) {   # replicate basis evaluation
      ro.predictor.mat[,i]<-rep(ro.eval.basis[,i],nrow(xx))
    }
    # include main effects of year and status
    ro.predictor.mat<-cbind(ro.predictor.mat,rep(ifelse(xx[,1]==year.unique[1],1,0),each=length(EvPts)),
               rep(ifelse(xx[,1]==year.unique[2],1,0),each=length(EvPts)),rep(ifelse(xx[,1]==year.unique[3],1,0),each=length(EvPts)),
               rep(ifelse(xx[,2]==status.unique[1],1,0),each=length(EvPts)),rep(ifelse(xx[,2]==status.unique[2],1,0),each=length(EvPts)))

    # include interactive effect
    ro.predictor.mat<-cbind(ro.predictor.mat,ro.predictor.mat[,18]*ro.predictor.mat[,21],ro.predictor.mat[,19]*ro.predictor.mat[,21],
       ro.predictor.mat[,20]*ro.predictor.mat[,21],ro.predictor.mat[,18]*ro.predictor.mat[,22],ro.predictor.mat[,19]*ro.predictor.mat[,22],
       ro.predictor.mat[,20]*ro.predictor.mat[,22])
    
    ro.predictor.mat<-cbind(ro.predictor.mat,rep(1,nrow(ro.predictor.mat)))
    ro.constraint<-ro.predictor.mat[1:(nrow(ro.predictor.mat)/nrow(xx)),]   
    ro.constraint[,18:ncol(ro.constraint)]<-1
    ro.constraint.response<-rep(-700,length(EvPts))  # on logistic scale

    colnames(ro.predictor.mat)<-c("Bspline.bs1","Bspline.bs2","Bspline.bs3","Bspline.bs4","Bspline.bs5","Bspline.bs6",
                        "Bspline.bs7","Bspline.bs8","Bspline.bs9","Bspline.bs10","Bspline.bs11","Bspline.bs12","Bspline.bs13",
                         "Bspline.bs14","Bspline.bs15","Bspline.bs16","Bspline.bs17",
                         "Year.2008","Year.2009","Year.2010","Status.Fished","Status.MPA","Fished.2008","Fished.2009","Fished.2010",
                        "MPA.2008","MPA.2009","MPA.2010","Intercept")

    # equates to the three sites.
    ro.predictor.mat<-rbind(ro.predictor.mat,ro.predictor.mat,ro.predictor.mat)  # one for each location.

    dim(ro.predictor.mat) # function of length of EvPts

	
	# set up kernel density smooths
    xx.agg<-aggregate(Length~Year+Status+Location,data=assemb.ro,FUN=length)
    xx.stat.year<-aggregate(Length~Year+Status,data=assemb.ro,FUN=length)
	ro.density.estimates<-matrix(nrow=length(EvPts),ncol=nrow(xx.agg))
    ro.smooth.pars<-numeric(nrow(xx.agg))	
#	ro.density.estimates<-matrix(nrow=length(EvPts),ncol=nrow(xx.stat.year))
#   ro.smooth.pars<-numeric(nrow(xx.stat.year))


## calculate the geometric mean of the individual smoothing paramaters
## change criterion for selecting smoothing parameters with option method
##  in sm.density in the below loop
##   method = "normal"    - smoother smoothing parameter
##   method = "cv"        - cross validation - rougher
##   method = "sj"        - sheather jones - rougher

 for(i in 1:ncol(ro.density.estimates)) {
		d.which<-which(assemb.ro$Year==xx.agg$Year[i] & assemb.ro$Status==xx.agg$Status[i] & 
    						assemb.ro$Location==xx.agg$Location[i])
	#d.which<-which(assemb.ro$Year==xx.stat.year$Year[i] & assemb.ro$Status==xx.stat.year$Status[i])# & 
	
	d.all<-sm.density(assemb.ro$Length[d.which],eval.points=EvPts,display="none",positive=TRUE,method="sj")
		ro.smooth.pars[i]<-d.all$h		     
		ro.density.estimates[,i]<-d.all$estimate 
	}
	
	# overall smoothing parameter: geometric mean of individual smoothing parameters.
ro.mean.pars<-prod(ro.smooth.pars)^(1/length(ro.smooth.pars))
#[1] 0.2908812
#[1] 0.2065473   for 18 data points
mean(ro.smooth.pars)
#[1] 0.2975866
par(mfrow=c(1,2))
sm.density(assemb.ro$Length, positive=TRUE)$h
#[1] 0.3263127

sm.density(assemb.ro$Length,positive=TRUE,h=ro.mean.pars)

par(mfrow=c(2,3))
 plot(apply(ro.density.estimates[,c(1,7,13)],1,mean)~EvPts,type="l",main="2008 Fished")
 plot(apply(ro.density.estimates[,c(2,8,14)],1,mean)~EvPts,type="l",main="2009 Fished")
 plot(apply(ro.density.estimates[,c(3,9,15)],1,mean)~EvPts,type="l",main="2010 Fished")
 plot(apply(ro.density.estimates[,c(4,10,16)],1,mean)~EvPts,type="l",main="2008 MPA")
 plot(apply(ro.density.estimates[,c(5,11,17)],1,mean)~EvPts,type="l",main="2009 MPA")
 plot(apply(ro.density.estimates[,c(6,12,18)],1,mean)~EvPts,type="l",main="2010 MPA")
par(mfrow=c(1,1))

##############
## FUNCTIONS FOR ANOVA
 ro.xmat<-ro.predictor.mat
 ro.density.data<-ro.density.estimates
 resp.dat<-ro.density.data
 resp.dat<-ro.density.estimates

fmodel.estimate<-function(resp.dat=ro.density.data,ro.xmat=ro.predictor.mat,model="no.effect",unit.value=TRUE) {  
 #  purpose: to provide predicted curves for each factor level of different models. 
 #  returns: model coefficients (in logistic domain) , residual sum of squares, R, model df and fitted values (all in response domain)
 #  allows: calculation of F-statistic and R-statistc for the comparison of models. (Cardot et al 2007).
 #  model options
 #   model = "equiprob"  :   spurious case of one where all fish lengths are equally likely
 #   model = "no.effect" :   no effect of status or year
 #   model = "year"      :   main effect of year
 #   model = "status"    :   main effect of status
 #   model = "year.and.status" :  effect of year + status
 #   model = "year.by.status"  :  effect of year * status   (i.e. interaction effect).

 # unit.value standardises to volume = 1; best off standardising input data to volume = 1 first (as well).

 # do on logistic scale to be strictly positive and between 0,1
 # although logistic scale means that residuals on response scale are no longer centred, using
 #    logistic scale residuals for calculation of R and F leads to funny values.
 # there is a difference between logistic and log transforms - introduces bias into residuals on response scale.
 #    logistic is more "wriggly" than log; this will be balanced out by choice of appropriate smoothing parameter
 #      smoothing parameter applied to null model, in this case "no.effect"
 #    due to perturbation of anova assumptions with logistic transform and density regularisation
#           then permutation test is the only way to go

 # could perhaps use "switch" or "case" here, but prefer if (as that is how my brain is currently working)

 # ro.log<-log(ro.response+1e-5)  # alternative transform
 
 #ro.response<-as.vector(resp.dat)[-(c(1:nrow(xx)*length(location.unique))*680)]
 #ro.xmat<-ro.xmat[-(c(1:nrow(xx)*length(location.unique))*680),]
	ro.response<-as.vector(resp.dat)
	ro.newdata<-rep(EvPts,nrow(xx)*length(location.unique))
	if(model=="equiprob") {
     ro.model<-lm(ro.response~1) # evaluated in response domain to compare with the other models.
     ro.sm<-sm.regression(x=ro.newdata,y=ro.model$residuals,eval.points=EvPts,display="none",h=ro.mean.pars)
     ro.R<-sum(ro.model$residuals^2)
     ro.model<-list(coefficients=ro.model$coefficients,rss=sum((ro.model$residuals)^2),R=ro.R,
               fitted=matrix(fitted(ro.model)[1:length(EvPts)],ncol=1))
    }
 if(model=="no.effect") {
     ro.logis<-qlogis(ro.response)  # to keep values strictly between 0 and 1
     ro.inverse<-t(ro.xmat[,1:17])%*%ro.xmat[,1:17]
     ro.cross<-t(ro.xmat[,1:17])%*%ro.logis
     ro.if.zero<-sort(which(ro.cross==0))
     if(length(ro.if.zero)>0) {
         ro.inverse<-ro.inverse[-ro.if.zero,-ro.if.zero]  
         ro.cross<-ro.cross[-ro.if.zero]
         ro.dump<-solve(ro.inverse,ro.cross)
         ro.coeff<-numeric(17)
         ro.coeff[-ro.if.zero]<-ro.dump
     }else{
         ro.coeff<-solve(ro.inverse,ro.cross)
     }
     ro.fd<-fd(ro.coeff,ro.basis)
     ro.pred<-predict(ro.fd,newdata=ro.newdata)
     ro.pred.logis<-plogis(ro.pred)
     ro.resids<-ro.response-ro.pred.logis

     ro.sm<-sm.regression(x=ro.newdata,y=ro.resids,eval.points=EvPts,display="none")
     ro.R<-sum(ro.sm$estimate^2)
     ro.model<-list(coefficients=ro.coeff,rss=sum((ro.resids)^2),R=ro.R,fitted=matrix(ro.pred.logis[1:length(EvPts)],ncol=1),model.df=1)
 }
 if(model=="year") {
     ro.logis<-c(qlogis(ro.response),ro.constraint.response)  # to keep values strictly between 0 and 1
     ro.pred.mat<-rbind(ro.xmat[,1:17],ro.constraint[,1:17])
     ro.fac.mat<-rbind(ro.xmat[,c(18:20,29)],ro.constraint[,c(18:20,29)])  # year
  # add in constraint of different curves for different factor levels - met approximately
     ro.all.terms<-ro.pred.mat*ro.fac.mat[,1]
      
     for(i in 2:ncol(ro.fac.mat)) {
        ro.all.terms<-cbind(ro.all.terms,ro.pred.mat*ro.fac.mat[,i])
     }
     ro.inverse<-t(ro.all.terms)%*%ro.all.terms
     ro.cross<-t(ro.all.terms)%*%ro.logis
     ro.if.zero<-sort(which(ro.cross==0))
	 
	 
     if(length(ro.if.zero)>0) {
         ro.inverse<-ro.inverse[-ro.if.zero,-ro.if.zero]  
         ro.cross<-ro.cross[-ro.if.zero]
         ro.dump<-solve(ro.inverse,ro.cross)
         ro.coeff<-numeric(ncol(ro.all.terms))
         ro.coeff[-ro.if.zero]<-ro.dump
     }else{
         ro.coeff<-solve(ro.inverse,ro.cross)
     }

     ro.fac.mat<-ro.fac.mat[1:nrow(ro.xmat),]

    ro.pred.mat<-matrix(0,ncol=length(year.unique),nrow=length(EvPts))
    ro.R<-ro.rss<-numeric(1)
    ro.coeff<-ro.coeff[1:51]+rep(ro.coeff[52:68],3)    # additive model
    for(i in 1:length(year.unique)) {
     pred.which<-which(ro.fac.mat[,i]==1)
     ro.fd<-fd(ro.coeff[(i-1)*17+c(1:17)],ro.basis)
     ro.pred<-predict(ro.fd,newdata=ro.newdata[1:length(pred.which)])
     ro.pred.logis<-plogis(ro.pred)
     ro.resids<-ro.response[pred.which]-ro.pred.logis
     ro.pred.mat[,i]<-ro.pred.logis[1:length(EvPts)]
     ro.sm<-sm.regression(x=ro.newdata[pred.which],y=ro.resids,eval.points=EvPts,display="none")
     ro.R<-ro.R + sum(ro.sm$estimate^2)
     ro.rss<-ro.rss + sum((ro.resids)^2)
    }
    colnames(ro.pred.mat)<-colnames(ro.xmat)[18:20]
    ro.model<-list(coefficients=ro.coeff,rss=ro.rss,R=ro.R,fitted=ro.pred.mat,model.df=length(year.unique)-1)
}

 if(model=="status") {
     ro.logis<-c(qlogis(ro.response),ro.constraint.response)  # to keep values strictly between 0 and 1
     ro.pred.mat<-rbind(ro.xmat[,1:17],ro.constraint[,1:17])
     ro.fac.mat<-rbind(ro.xmat[,c(21:22,29)],ro.constraint[,c(21:22,29)])  # status: fished, mpa
  # add in constraint of different curves for different factor levels - met approximately
     ro.all.terms<-ro.pred.mat*ro.fac.mat[,1]
     
     for(i in 2:ncol(ro.fac.mat)) {
        ro.all.terms<-cbind(ro.all.terms,ro.pred.mat*ro.fac.mat[,i])
     }
     ro.inverse<-t(ro.all.terms)%*%ro.all.terms
     ro.cross<-t(ro.all.terms)%*%ro.logis
     ro.if.zero<-sort(which(ro.cross==0))
     if(length(ro.if.zero)>0) {
         ro.inverse<-ro.inverse[-ro.if.zero,-ro.if.zero]  
         ro.cross<-ro.cross[-ro.if.zero]
         ro.dump<-solve(ro.inverse,ro.cross)
         ro.coeff<-numeric(ncol(ro.all.terms))
         ro.coeff[-ro.if.zero]<-ro.dump
     }else{
         ro.coeff<-solve(ro.inverse,ro.cross)
     }

    ro.fac.mat<-ro.fac.mat[1:nrow(ro.xmat),]
    ro.pred.mat<-matrix(0,ncol=length(status.unique),nrow=length(EvPts))
    ro.R<-ro.rss<-numeric(1)
    ro.coeff<-ro.coeff[1:34]+rep(ro.coeff[35:51],2)    # additive model
    for(i in 1:length(status.unique)) {
     pred.which<-which(ro.fac.mat[,i]==1)
     ro.fd<-fd(ro.coeff[(i-1)*17+c(1:17)],ro.basis)
     ro.pred<-predict(ro.fd,newdata=ro.newdata[1:length(pred.which)])
     ro.pred.logis<-plogis(ro.pred)
     ro.resids<-ro.response[pred.which]-ro.pred.logis
     ro.pred.mat[,i]<-ro.pred.logis[1:length(EvPts)]

     ro.sm<-sm.regression(x=ro.newdata[pred.which],y=ro.resids,eval.points=EvPts,display="none")
     ro.R<-ro.R + sum(ro.sm$estimate^2)
     ro.rss<-ro.rss + sum((ro.resids)^2)
    }
    colnames(ro.pred.mat)<-colnames(ro.xmat)[21:22]
    ro.model<-list(coefficients=ro.coeff,rss=ro.rss,R=ro.R,fitted=ro.pred.mat,model.df=length(status.unique)-1)
 }
 
 if(model=="year.and.status") {
     ro.logis<-c(qlogis(ro.response),ro.constraint.response)  # to keep values strictly between 0 and 1
     ro.pred.mat<-rbind(ro.xmat[,1:17],ro.constraint[,1:17])
     ro.fac.mat<-rbind(ro.xmat[,c(19:22,29)],ro.constraint[,c(19:22,29)])  # year
  #  base year is 2008, other year effects are additive, while two curves are estimated for fished and mpa
  #    add in year effects to get year trends for fished and mpa
  # add in constraint of different curves for different factor levels - met approximately
     ro.all.terms<-ro.pred.mat*ro.fac.mat[,1]
     
     for(i in 2:ncol(ro.fac.mat)) {
        ro.all.terms<-cbind(ro.all.terms,ro.pred.mat*ro.fac.mat[,i])
     }
     ro.inverse<-t(ro.all.terms)%*%ro.all.terms
     ro.cross<-t(ro.all.terms)%*%ro.logis
     ro.if.zero<-sort(which(ro.cross==0))
     if(length(ro.if.zero)>0) {
         ro.inverse<-ro.inverse[-ro.if.zero,-ro.if.zero]  
         ro.cross<-ro.cross[-ro.if.zero]
         ro.dump<-solve(ro.inverse,ro.cross)
         ro.coeff<-numeric(ncol(ro.all.terms))
         ro.coeff[-ro.if.zero]<-ro.dump
     }else{
         ro.coeff<-solve(ro.inverse,ro.cross)
     }

    ro.fac.mat<-ro.xmat[1:nrow(ro.xmat),18:22]
    ro.pred.mat<-matrix(0,ncol=ncol(ro.fac.mat)+1,nrow=length(EvPts))
    ro.R<-ro.rss<-numeric(1)
 # matrix(ro.coeff,nrow=13)
 #   this is really the only tricky line - check above matrix
#    ro.coeff.2008<-ro.coeff[35:68]+rep(ro.coeff[69:85],2)    # additive model
#    ro.coeff<-c(ro.coeff.2008,ro.coeff.2008+ro.coeff[1:17],ro.coeff.2008+ro.coeff[18:34])
	ro.coeff.2008<-ro.coeff[35:68]+rep(ro.coeff[69:85],2)    # additive model
	#ro.coeff<-c(ro.coeff.2008,ro.coeff.2008+ro.coeff[1:17],ro.coeff.2008+ro.coeff[18:34])
	ro.coeff<-c(ro.coeff.2008,ro.coeff.2008+ro.coeff[1:17],ro.coeff.2008+ro.coeff[18:34])
	# naming order = (fished.2008,mpa.2008,fished.2009,mpa.2009,fished.2010,mpa.2010) ; sets of 13 basis variables
    for(i in 1:(ncol(ro.fac.mat)+1)) {
     comp.fac.status<-2-i%%2    # use of modulus, as a demo :)
     comp.fac.year<-(i-1)%/%2+1
     comp.fac<-ro.fac.mat[,comp.fac.year]*ro.fac.mat[,comp.fac.status+3]
     pred.which<-which(comp.fac==1)
     ro.fd<-fd(ro.coeff[(i-1)*17+c(1:17)],ro.basis)
     ro.pred<-predict(ro.fd,newdata=ro.newdata[1:length(pred.which)])
     ro.pred.logis<-plogis(ro.pred)
     ro.resids<-ro.response[pred.which]-ro.pred.logis
     ro.pred.mat[,i]<-ro.pred.logis[1:length(EvPts)]
 
     ro.sm<-sm.regression(x=ro.newdata[pred.which],y=ro.resids,eval.points=EvPts,display="none")
     ro.R<-ro.R + sum(ro.sm$estimate^2)
     ro.rss<-ro.rss + sum((ro.resids)^2)

    }
    colnames(ro.pred.mat)<-colnames(ro.xmat)[c(23,26,24,27,25,28)]
    ro.model<-list(coefficients=ro.coeff,rss=ro.rss,R=ro.R,fitted=ro.pred.mat,model.df=length(status.unique)+length(year.unique)-2)
 }
 if(model=="year.by.status") {
#options(contrasts=c("contr.sum","contr.poly"))
#model.matrix(~xx.agg$Year*xx.agg$Status)
	 ro.logis<-c(qlogis(ro.response),ro.constraint.response)  # to keep values strictly between 0 and 1
     ro.pred.mat<-rbind(ro.xmat[,1:17],ro.constraint[,1:17])
   #  ro.fac.mat<-rbind(ro.xmat[,c(18:22,23:29)],ro.constraint[,c(18:22,27:29)])  # year
#	 ro.fac.mat<-rbind(ro.xmat[,c(23:29)],ro.constraint[,c(23:29)])  # year
	ro.fac.mat<-rbind(ro.xmat[,c(19:20,22,26:29)],ro.constraint[,c(19:20,22,26:29)])  # year
	
	 #  base year is 2008, other year effects are additive, while two curves are estimated for fished and mpa
  #    add in year effects to get year trends for fished and mpa
  # add in constraint of different curves for different factor levels - met approximately
     ro.all.terms<-ro.pred.mat*ro.fac.mat[,1]
     
     for(i in 2:ncol(ro.fac.mat)) {
        ro.all.terms<-cbind(ro.all.terms,ro.pred.mat*ro.fac.mat[,i])
     }
     ro.inverse<-t(ro.all.terms)%*%ro.all.terms
     ro.cross<-t(ro.all.terms)%*%ro.logis
     ro.if.zero<-sort(which(ro.cross==0))
     if(length(ro.if.zero)>0) {
         ro.inverse<-ro.inverse[-ro.if.zero,-ro.if.zero]  
         ro.cross<-ro.cross[-ro.if.zero]
         ro.dump<-solve(ro.inverse,ro.cross)
         ro.coeff<-numeric(ncol(ro.all.terms))
         ro.coeff[-ro.if.zero]<-ro.dump
     }else{
	#	 ro.coeff<-ginv(ro.inverse)
	#	 ro.coeff<-ro.coeff%*%ro.cross
		 		 ro.coeff<-solve(ro.inverse,ro.cross,LINPACK = TRUE)
     }
 
	 
	ro.fac.mat<-ro.xmat[1:nrow(ro.xmat),23:28]
    ro.pred.mat<-matrix(0,ncol=ncol(ro.fac.mat),nrow=length(EvPts)) # overwrites previous
    ro.R<-ro.rss<-numeric(1)
  #   this is really the only tricky line - check above matrix
	ro.coeff.2008<-rep(ro.coeff[103:119],2)+c(rep(0,17),ro.coeff[35:51])    # additive model
	ro.coeff<-c(ro.coeff.2008+c(rep(0,17),ro.coeff[52:68]),ro.coeff.2008+c(ro.coeff[1:17],ro.coeff[69:85]),
			   ro.coeff.2008+c(ro.coeff[18:34],ro.coeff[86:102]))
    # naming order = (fished.2008,mpa.2008,fished.2009,mpa.2009,fished.2010,mpa.2010) ; sets of 13 basis variables
    for(i in 1:(ncol(ro.fac.mat))) {
     #comp.fac.status<-2-i%%2    # use of modulus, as a demo :)
    # comp.fac.year<-(i-1)%/%2+1
     #comp.fac<-ro.fac.mat[,comp.fac.year]*ro.fac.mat[,comp.fac.status+3]
	 comp.fac<-ro.fac.mat[,i]
	 pred.which<-which(comp.fac==1)
     ro.fd<-fd(ro.coeff[(i-1)*17+c(1:17)],ro.basis)
     ro.pred<-predict(ro.fd,newdata=ro.newdata[1:length(pred.which)])
     ro.pred.logis<-plogis(ro.pred)
     ro.resids<-ro.response[pred.which]-ro.pred.logis
     ro.pred.mat[,i]<-ro.pred.logis[1:length(EvPts)]

     ro.sm<-sm.regression(x=ro.newdata[pred.which],y=ro.resids,eval.points=EvPts,display="none")
     ro.R<-ro.R + sum(ro.sm$estimate^2)
     ro.rss<-ro.rss + sum((ro.resids)^2)
    }
    colnames(ro.pred.mat)<-colnames(ro.xmat)[c(23,26,24,27,25,28)]
    ro.model<-list(coefficients=ro.coeff,rss=ro.rss,R=ro.R,fitted=ro.pred.mat,
                 model.df=length(status.unique)+length(year.unique)-2+(length(status.unique)-1)*(length(year.unique)-1))
 }

 if(unit.value) {
     ro.model$fitted<-t(t(ro.model$fitted)/apply(ro.model$fitted,2,sum))
 }
 return(ro.model)
 }

 rm(ro.model,ro.logis,ro.response)
 rm(ro.sm,ro.newdata,ro.R,ro.rss,ro.inverse,ro.cross,ro.dump,ro.if.zero,ro.coeff,ro.fd,ro.pred)
 rm(ro.resids,ro.pred.logis,ro.pred.mat,ro.fac.mat)
 rm(ro.all.terms,diagnostic.mat,ro.xmat,resp.dat)

# Plot functional ---------------------------------------------------------


# plot functional
plot.fmodel<-function(fmodel) {
 if(ncol(fmodel$fitted)==1) {
     n.plot<-1
     plot(EvPts,fmodel$fitted[1:length(EvPts)],type="l",xlab="Length (mm)",ylab="Probability Density",ylim=c(0,0.008))
  }else{
     n.plot<-ncol(fmodel$fitted)
     plot(EvPts,fmodel$fitted[,1],type="n",xlab="Length (mm)",ylab="Probability Density",ylim=c(0,0.008))
     for(i in 1:n.plot) {
       lines(EvPts,fmodel$fitted[,i],col=ifelse(n.plot>3,rep(2:3,3)[i],i+1),lty=ifelse(n.plot>3,rep(1:3, each=2)[i],1))
     }
  }
# red is fished (danger); green is mpa (conservation)
# return(n.plot)
}


permute.fmodel<-function(ro.data=ro.data,n.permute=100,effects=c("no.effect","status")) {
# permutes at level of subject i.e. permutes curves only
 data.length<-nrow(xx)*length(location.unique)
 f1<-fmodel.estimate(resp.dat=ro.density.data,model=effects[1])
 f2<-fmodel.estimate(resp.dat=ro.density.data,model=effects[2])
 ro.data<-matrix(ro.data,ncol=data.length)
 f.stat.dat<-(f1$rss-f2$rss)/f2$rss  # scale of response does not help
 r.stat.dat<-f2$R
 n.permute<-n.permute-1
 r.permute<-numeric(n.permute)
 f.permute<-numeric(n.permute)
 for(i in 1:(n.permute)) { 
   s.permute<-sample(1:data.length)   
   ro.perm<-as.numeric(ro.data[,s.permute])    # permutes the permutation; saves on objects
   f1<-fmodel.estimate(resp.dat=ro.data,model=effects[1])
   f2<-fmodel.estimate(resp.dat=ro.data,model=effects[2])
   f.permute[i]<-(f1$rss-f2$rss)/f2$rss
   r.permute[i]<-f2$R
   if(i%%10 == 0) print(paste("This is permuation: ",i))
}
  pv.Fstat<-length(which(f.permute>f.stat.dat))/(n.permute+1)
  pv.Rstat<-length(which(f.permute>f.stat.dat))/(n.permute+1)
  pv.vec<-c(pv.Fstat,pv.Rstat)
  names(pv.vec)<-c("Pvalue.Fstat","Pvalue.Rstat")
  return(list(p.values=pv.vec,f.stat=f.permute,r.stat=r.permute,stat.original=c(f.stat.dat,r.stat.dat)))
}
 
 
#   set up bootstrap samples
n.boot<-99

boot.lookup<-matrix(nrow=length(EvPts),ncol=nrow(xx)*length(location.unique))
sm.pars<-numeric(nrow(xx))

for(i in 1:ncol(boot.lookup)) {
    d.which<-which(assemb.ro$Year==xx.agg$Year[i] & assemb.ro$Status==xx.agg$Status[i] & 
                     assemb.ro$Location==xx.agg$Location[i])
    d.all<-sm.density(assemb.ro$Length[d.which],eval.points=EvPts,display="none",positive=TRUE)
    sm.pars[i]<-d.all$h		     
    boot.lookup[,i]<-d.all$estimate 
}

 # run anova regression - without p-value test
ro.density.data<-as.numeric(t(t(boot.lookup)/apply(boot.lookup,2,sum)))


boot.lookup<-apply(boot.lookup,2,cumsum)
boot.lookup<-t(t(boot.lookup)/apply(boot.lookup,2,max))


xx.agg.cumsum    <- cumsum(xx.agg$Length)
xx.agg.start     <- c(1,xx.agg.cumsum[1:(length(xx.agg.cumsum)-1)]+1)
xx.agg.facts   <-cbind(xx.agg$Length, xx.agg.start,xx.agg.cumsum)

xx.agg.design <- cbind(rep(1,nrow(xx.agg)),xx.agg$Year-2007,ifelse(xx.agg$Status=="Fished",1,2))
xx.agg.design<-cbind(xx.agg.design,xx.agg.design[,3]*3 + xx.agg.design[,2] -3)

 
f.equi<-fmodel.estimate(resp.dat=ro.density.data,model="equiprob") 
f.status<-fmodel.estimate(resp.dat=ro.density.data,model="status") 
f.year<-fmodel.estimate(resp.dat=ro.density.data,model="year") 
f.yands<-fmodel.estimate(resp.dat=ro.density.data,model="year.and.status") 
f.ybys<-fmodel.estimate(resp.dat=ro.density.data,model="year.by.status") 
f.noe<-fmodel.estimate(resp.dat=ro.density.data,model="no.effect") 


##################################   PLOT MODELS
# amend as you wish

par(mfrow=c(2,3))
plot.fmodel(f.equi)
title(main="equiprobability")
plot.fmodel(f.noe)
title(main="no effect")
plot.fmodel(f.status)
title(main="status",sub="fished = red ; mpa = green")
#lines(EvPts,f.noe$fitted,lty=2)
plot.fmodel(f.year)
title(main="year",sub="2008 = red ; 2009 = green; 2010 = blue")
plot.fmodel(f.yands)
title(main="status + year",sub="fished = red ; mpa = green")
plot.fmodel(f.ybys)
title(main="status * year",sub="fished = red ; mpa = green")
par(mfrow=c(1,1))


# apply multicore
############################  PERMUTE F.STAT

# rm(ro.data,s.permute,f.permute,r.permute,n.permute,r.stat.dat,f.stat.dat,f1,f2,data.length)
#
system.time(
 permute.fmodel(ro.data=ro.density.data,n.permute=10,effects=c("no.effect","status"))
)
# Pvalue.Fstat Pvalue.Rstat 
#           0            0 
#     user  system elapsed 
 #  5.01    0.06    5.10 

#  about 1 sec a permutation: about 3 hours. in total for one comparison

#system.time(
# permute.fmodel(ro.data=ro.curves,n.permute=100,effects=c("no.effect","year"))
#)

# permute.fmodel(ro.data=ro.curves,n.permute=100,effects=c("year.and.status","year.by.status"))

 ############################  PERMUTE F.STAT   WITH BOOTSTRAP STEP
# all permutation tests are statistically significant; but does not consider the
#   error embodied in density curve estimation - need to boot and then permute to respect
#   error structure of data.


# boot.permute.hypothesis.test<-function(n.boot=99,n.perm=2,perm.effects=c("no.effect","year"),raw.data=FALSE){
# #  boot.dump<-matrix(0,ncol=n.boot,nrow=nrow(ro.predictor.mat))
#  
#   # raw data is the data that you wish to permute
#   #   if false then generate a smooth bootstrap sample, which is then permuted
#   
#   # permutation not among individual strata; and no boot then permute step.
#    
#   boot.generate.fishsize<- function(k) {
# #      env<-parent.frame()
# #      n.boot<-get("n.boot",env)[,k]
# 
#        rand.boot<-          runif(xx.agg$Length[k]*n.boot)
#        rand.boot<-   approx(c(0,boot.lookup[,k]),c(20,EvPts),xout=rand.boot)$y
#        rand.boot<-   matrix(rand.boot,ncol=n.boot)
#        rand.apply<-  apply(rand.boot,2,function(x) sm.density(x,display="none",positive=TRUE,
#                                eval.points=EvPts)$estimate)
#        rand.apply<- t(t(rand.apply)/apply(rand.apply,2,sum))
#        #boot.dump[(k-1)*680+1:680,]<-rand.apply
#         return(rand.apply)
#      }
#   #rm(f.stat.dump,boot.dump,n.boot,perm.effects,n.perm,r.permute,f.permute,f2,f1,ro.perm,s.permute,n.permute)
#   #rm(r.stat.dat,f.stat.dat,ro.data,rand.apply,rand.boot)
#   
#    permute.fishsize<-function(k) {
#   # permutes at level of subject i.e. permutes curves only
#     data.length<-nrow(xx)*length(location.unique)
#   #  env<-parent.frame()
#   #  ro.data<-get("boot.dump",env)[,k]
#   #  perm.effects<-get("perm.effects",env)
#   #  n.perm<-get("n.perm",env)
#     ro.data<-boot.dump[,k]
#  #   f1<-fmodel.estimate(resp.dat=ro.data,model=perm.effects[1])
#  #   f2<-fmodel.estimate(resp.dat=ro.data,model=perm.effects[2])
#     ro.data<-matrix(ro.data,ncol=data.length)
#  #   f.stat.dat<-(f1$rss-f2$rss)/f2$rss  # scale of response does not help
#  #   r.stat.dat<-f2$R
#     n.permute<-n.perm-1
#     r.permute<-numeric(n.permute)
#     f.permute<-numeric(n.permute)
#     for(i in 1:(n.permute)) { 
#       s.permute<-sample(1:data.length)   
#       ro.perm<-as.numeric(ro.data[,s.permute])    # permutes the permutation; saves on objects
#       f1<-fmodel.estimate(resp.dat=ro.perm,model=perm.effects[1])
#       f2<-fmodel.estimate(resp.dat=ro.perm,model=perm.effects[2])
#       f.permute[i]<-(f1$rss-f2$rss)/f2$rss
#       r.permute[i]<-f2$R
#     #  if(i%%10 == 0) print(paste("This is permuation: ",i))
#     }
#    #pv.Fstat<-length(which(f.permute>f.stat.dat))/(n.permute+1)
#    #pv.Rstat<-length(which(f.permute>f.stat.dat))/(n.permute+1)
#    #pv.vec<-c(pv.Fstat,pv.Rstat)
#    #names(pv.vec)<-c("Pvalue.Fstat","Pvalue.Rstat")
#    return(f.permute)
#    # return(list(p.values=pv.vec,f.stat=f.permute,r.stat=r.permute,stat.original=c(f.stat.dat,r.stat.dat)))
#   }
# 
#   
#   if(raw.data[1]==FALSE){
# 
#  # system.time(
#     boot.dump.2<-mclapply(1:nrow(xx.agg),boot.generate.fishsize)
#  #  )  # 38 secs vs 68 for 100 # 23 seconds for 99
#   
#     boot.dump<-boot.dump.2[[1]]
#     for(i in 2:length(boot.dump.2)) {
#        boot.dump<-rbind(boot.dump,boot.dump.2[[i]])
#     }
#   }else{
#     boot.dump<-matrix(raw.data,ncol=1)
#     n.boot<-1
#   } 
#   #system.time(
#   fstat.dump<-unlist(mclapply(1:n.boot,permute.fishsize))  
#   #  system.time(mclapply(1,function(k) permute.fishsize(k,n.permute=n.perm,effects=effects)))  # 35 secs for 100
# 
#   # 1/2 hr for 100 boots * 100 perms
#  #)
#   return(fstat.dump)
# }

rm(boot.generate.fishsize,permute.fishsize,r.permute,f.permute,f1,f2,ro.perm,s.permute,s.which,s.max,s.index,m1,m2,
     perm.list,data.lenbgth,ro.data,n.perm,n.permute,perm.effects)

boot.permute.hypothesis.test<-function(n.boot=99,n.perm=2,perm.effects=c("no.effect","year"),raw.data=FALSE){
  #  boot.dump<-matrix(0,ncol=n.boot,nrow=nrow(ro.predictor.mat))
  
  boot.generate.fishsize<- function(k) {
    #      env<-parent.frame()
    #      n.boot<-get("n.boot",env)[,k]
    
    rand.boot<-   runif(xx.agg$Length[k]*n.boot)
    rand.boot<-   approx(c(0,boot.lookup[,k]),c(20,EvPts),xout=rand.boot)$y
    rand.boot<-   matrix(rand.boot,ncol=n.boot)
    rand.apply<-  apply(rand.boot,2,function(x) sm.density(x,display="none",positive=TRUE,h=ro.mean.pars,
                                                           eval.points=EvPts)$estimate)
    rand.apply<- t(t(rand.apply)/apply(rand.apply,2,sum))
    #boot.dump[(k-1)*680+1:680,]<-rand.apply
    return(rand.apply)
  }
  #rm(f.stat.dump,boot.dump,n.boot,perm.effects,n.perm,r.permute,f.permute,f2,f1,ro.perm,s.permute,n.permute)
  #rm(r.stat.dat,f.stat.dat,ro.data,rand.apply,rand.boot)
  
  permute.fishsize<-function(k) {
    # permutes at level of subject i.e. permutes curves only
    perm.list<-c("no.effect","status","year","year.and.status","year.by.status")
    
    data.length<-nrow(xx)*length(location.unique)
    #  env<-parent.frame()
    #  ro.data<-get("boot.dump",env)[,k]
    #  perm.effects<-get("perm.effects",env)
    #  n.perm<-get("n.perm",env)
    if(n.boot==1) {
      ro.data<-boot.dump[,1]
    }else{
    ro.data<-boot.dump[,k]
    }
    #   f1<-fmodel.estimate(resp.dat=ro.data,model=perm.effects[1])
    #   f2<-fmodel.estimate(resp.dat=ro.data,model=perm.effects[2])
    ro.data<-matrix(ro.data,ncol=data.length)
    #   f.stat.dat<-(f1$rss-f2$rss)/f2$rss  # scale of response does not help
    #   r.stat.dat<-f2$R
    n.permute<-n.perm-1
    r.permute<-numeric(n.permute)
    f.permute<-numeric(n.permute)
    m1<-sort(match(perm.effects,perm.list))
    m2<-m1[2]  # sort models into increasing complexity
    m1<-m1[1]
    if(m1==1){
      for(i in 1:(n.permute)) {
        
        s.permute<-sample(1:data.length)   
        ro.perm<-as.numeric(ro.data[,s.permute])    # permutes the permutation; saves on objects
        f1<-fmodel.estimate(resp.dat=ro.perm,model=perm.list[m1])
        f2<-fmodel.estimate(resp.dat=ro.perm,model=perm.list[m2])
        f.permute[i]<-(f1$rss-f2$rss)/f2$rss
        r.permute[i]<-f2$R
        #  if(i%%10 == 0) print(paste("This is permuation: ",i))
      }
    }else{
      s.index<-xx.agg.design[,m1]
      s.max<-max(s.index)
      s.permute<-numeric(length(data.length))
      
      # within group permutation on the null model
      for(i in 1:(n.permute)) {
        for(n in 1:s.max){
          s.which<-which(s.index==n)
          s.permute[s.which]<-sample(s.which)
        }
        
        s.permute<-sample(1:data.length)   
        ro.perm<-as.numeric(ro.data[,s.permute])    # permutes the permutation; saves on objects
        f1<-fmodel.estimate(resp.dat=ro.perm,model=perm.list[m1])
        f2<-fmodel.estimate(resp.dat=ro.perm,model=perm.list[m2])
        f.permute[i]<-(f1$rss-f2$rss)/f2$rss
        r.permute[i]<-f2$R
        #  if(i%%10 == 0) print(paste("This is permuation: ",i))
      }
      
    }
    #pv.Fstat<-length(which(f.permute>f.stat.dat))/(n.permute+1)
    #pv.Rstat<-length(which(f.permute>f.stat.dat))/(n.permute+1)
    #pv.vec<-c(pv.Fstat,pv.Rstat)
    #names(pv.vec)<-c("Pvalue.Fstat","Pvalue.Rstat")
    return(f.permute)
    # return(list(p.values=pv.vec,f.stat=f.permute,r.stat=r.permute,stat.original=c(f.stat.dat,r.stat.dat)))
  }
  
    
  if(raw.data[1]==FALSE){
    
    # system.time(
    boot.dump.2<-mclapply(1:nrow(xx.agg),boot.generate.fishsize)
    #boot.dump.2<-sapply(1:nrow(xx.agg),boot.generate.fishsize)
    
    #  )  # 38 secs vs 68 for 100 # 23 seconds for 99
    
    boot.dump<-boot.dump.2[[1]]
    for(j in 2:length(boot.dump.2)) {
      boot.dump<-rbind(boot.dump,boot.dump.2[[j]])
    }
  }else{
    boot.dump<-matrix(raw.data,ncol=1)
    n.boot<-1
  } 
  #system.time(
  fstat.dump<-unlist(mclapply(1:n.boot,permute.fishsize))  
  #  system.time(mclapply(1,function(k) permute.fishsize(k,n.permute=n.perm,effects=effects)))  # 35 secs for 100
  
  # 1/2 hr for 100 boots * 100 perms
  #)
  return(fstat.dump)
}


system.time(
  f.permute.boot<-boot.permute.hypothesis.test()
)
# 40 seconds for 100


system.time(
f.permute.boot<-boot.permute.hypothesis.test(n.boot=99,n.perm=100,perm.effects=c("no.effect","year"),raw.data=ro.density.data)
)
# 35 seconds for 100




# 10 hours of processing
# started 11:30 pm Thurs 21 June 2012

#  noe vs year
system.time(
f.permute.noe.year.2<-boot.permute.hypothesis.test(n.boot=99,n.perm=10000,perm.effects=c("no.effect","year"),raw.data=ro.density.data)
)
#800 seconds elapsed for 1000 permutations of raw data only
system.time(
f.permute.boot.noe.year.2<-boot.permute.hypothesis.test(n.boot=9999,n.perm=2,perm.effects=c("no.effect","year"),raw.data=FALSE)
)

#  noe vs status
system.time(
f.permute.noe.status.2<-boot.permute.hypothesis.test(n.boot=99,n.perm=10000,perm.effects=c("no.effect","status"),raw.data=ro.density.data)
)

system.time(
f.permute.boot.noe.status.2<-boot.permute.hypothesis.test(n.boot=9999,n.perm=2,perm.effects=c("no.effect","status"),raw.data=FALSE)
)

#  status vs yands
system.time(
f.permute.status.yands<-boot.permute.hypothesis.test(n.boot=99,n.perm=10000,perm.effects=c("status","year.and.status"),raw.data=ro.density.data)
)

system.time(
f.permute.boot.status.yands<-boot.permute.hypothesis.test(n.boot=9999,n.perm=2,perm.effects=c("status","year.and.status"),raw.data=FALSE)
)


#  status vs ybys
system.time(
f.permute.status.ybys<-boot.permute.hypothesis.test(n.boot=99,n.perm=10000,perm.effects=c("status","year.by.status"),raw.data=ro.density.data)
)
#f.permute.status.ybys<-boot.permute.hypothesis.test(n.boot=99,n.perm=5,perm.effects=c("status","year.by.status"),raw.data=ro.density.data)

system.time(
f.permute.boot.status.ybys<-boot.permute.hypothesis.test(n.boot=9999,n.perm=2,perm.effects=c("status","year.by.status"),raw.data=FALSE)
)


#  yands vs ybys
system.time(
f.permute.yands.ybys<-boot.permute.hypothesis.test(n.boot=99,n.perm=10000,perm.effects=c("year.and.status","year.by.status"),raw.data=ro.density.data)
)

system.time(
f.permute.boot.yands.ybys<-boot.permute.hypothesis.test(n.boot=9999,n.perm=2,perm.effects=c("year.and.status","year.by.status"),raw.data=FALSE)
)


#redo: f.permute.boot.status.yands ; f.permute.boot.status.ybys

########## TODO

# 1. calc table of p-values  ## DONE - may need checking/debugging
# 2. do bootstrap confidence intervals based on results of p-value table.
#        read up on julian faraway's stuff on anova comparison of means - Tukey/fibonacci? for multiple comparisons.  ## GO FIBONACCI - NOT DONE
# 3. readup on permutation - ter braak - constraining for different levels - e.g. bootstrap only from year etc ...
#      work from xx.agg - indicator columns for each effect noe, year etc, to drive bootstrap sample - smoothing parameters ??    ## DONE
#       - smoothing parameters are geometric mean of number of samples and individual smoothing parameters: bowman and azzalini.  ## DONE
# 4. Think of random effects.

# samples from conditional distribution given fish sizes between 20 and 700 mm only. 3% chucked.  ## NOW FROM FULL 1000m length

#f.equi<-fmodel.estimate(resp.dat=ro.density.data,model="equiprob") 
#f.status<-fmodel.estimate(resp.dat=ro.density.data,model="status") 
#f.year<-fmodel.estimate(resp.dat=ro.density.data,model="year") 
#f.yands<-fmodel.estimate(resp.dat=ro.density.data,model="year.and.status") 
#f.ybys<-fmodel.estimate(resp.dat=ro.density.data,model="year.by.status") 

fstat.report.table<-data.frame(M0=character(5),M1=character(5),fstat.permute=numeric(5),fstat.boot.permute=numeric(5))

fstat.report.table$M0<-c("noe","noe","status","status","status.and.year")
fstat.report.table$M1<-c("year","status","status.and.year","status.by.year","status.by.year")

# FInd the F stats here 

f.stat.noe.year<-(f.noe$rss-f.year$rss)/f.year$rss  # scale of response does not help
fstat.report.table$fstat.permute[1]<-length(which(f.permute.noe.year.2>f.stat.noe.year))/(length(f.permute.noe.year.2+1))
fstat.report.table$fstat.boot.permute[1]<-length(which(f.permute.boot.noe.year.2>f.stat.noe.year))/(length(f.permute.boot.noe.year.2+1))

f.stat.noe.status<-(f.noe$rss-f.status$rss)/f.status$rss  # scale of response does not help
fstat.report.table$fstat.permute[2]<-length(which(f.permute.noe.status>f.stat.noe.status))/(length(f.permute.noe.status+1))
fstat.report.table$fstat.boot.permute[2]<-length(which(f.permute.boot.noe.status.2>f.stat.noe.status))/(length(f.permute.boot.noe.status.2+1))

f.stat.status.yands<-(f.status$rss-f.yands$rss)/f.yands$rss  # scale of response does not help
fstat.report.table$fstat.permute[3]<-length(which(f.permute.status.yands>f.stat.status.yands))/(length(f.permute.status.yands+1))
fstat.report.table$fstat.boot.permute[3]<-length(which(f.permute.boot.status.yands>f.stat.status.yands))/(length(f.permute.boot.status.yands+1))

f.stat.status.ybys<-(f.status$rss-f.ybys$rss)/f.ybys$rss  # scale of response does not help
fstat.report.table$fstat.permute[4]<-length(which(f.permute.status.ybys>f.stat.status.ybys))/(length(f.permute.status.ybys+1))
fstat.report.table$fstat.boot.permute[4]<-length(which(f.permute.boot.status.ybys>f.stat.status.ybys))/(length(f.permute.boot.status.ybys+1))

f.stat.yands.ybys<-(f.yands$rss-f.ybys$rss)/f.ybys$rss  # scale of response does not help
fstat.report.table$fstat.permute[5]<-length(which(f.permute.yands.ybys>f.stat.yands.ybys))/(length(f.permute.yands.ybys+1))
fstat.report.table$fstat.boot.permute[5]<-length(which(f.permute.boot.yands.ybys>f.stat.yands.ybys))/(length(f.permute.boot.yands.ybys+1))






############
## OLD TODO/COMMENT
# 1. Validation - trends seem to be pretty much the same as with excel spreadsheet from eyeballing;
#                   predictions can be checked - but be careful which model is chosen!! (year.and.status vs year.by.status)
#               - conclusion - Ben has done a good job :)
#               - biggest difference is that my range has been truncated to 510, not 700
#               
# 2. Display: In the excel file effects were displayed as contrasts [centred at 2009) of all things. This doesn't necessarily
#     give you a good overall idea of the differences as you are yet to add them all up. Better options are:
#     i) centre effects by differencing all fitted trends defined in ro.model$fitted from that of the "no.effect" model
#         this is perhaps better for doing bootstrap confidence regions, especially if you want a CR for a test of difference between trends.
#         It is also useful in identifying divergence points (i.e. where CR first excludes the zero trend line).
#    ii) plot all effects without contrasts: get big picture - i.e. more middle sized fish on average for fished areas than mpa. (model="Status")
#          this is what I have done here. Required a different constrast set than the one Ben used - but contrasts being contrasts means 
#          that any contrast set can be used to generate the same plot.
# 
# 3. Permutation test of significance: needs to be two tiered (boot then permute) to respect data structure
# 
# 4. CV selection of appropriate smoothing parameter needs to be done on whole data set (null hypothesis of no.effect).
#     fda package doesn't seem to provide CV function for estimated densities.
# 
# 5. option to add weights for the 18 kernel density estimates relative to number of catches - however, if bootstrap is used then this 
#     taken into account given that lower sample sizes lead to greater variability in the density estimates.

#####
#  todo 
#    1. cv choice of smoothing parameter
#    2. bootstrap intervals on trends   


boot.interval.model<-function(n.boot=1000,b.effect="status",raw.data=ro.density.data) {
	# rm(f1,f.permute,f.permute1,f.permute2,f.permute3)
	calc.model<-function(j) {
		f1<-fmodel.estimate(resp.dat=ro.data,model=b.effect)
		if(b.effect=="status") {
			f.permute<-f1$fitted[,1]-f1$fitted[,2]
		}
		if(b.effect=="year") {
			f.permute1<-f1$fitted[,1]-f1$fitted[,2]
			f.permute2<-f1$fitted[,1]-f1$fitted[,3]
			f.permute3<-f1$fitted[,2]-f1$fitted[,3]
			f.permute<-as.numeric(apply(cbind(f.permute1,f.permute2,f.permute3),1,max))
		}
		if(b.effect=="year.and.status") {
			f.permute1.1<-f1$fitted[,1]-f1$fitted[,3]
			f.permute2.1<-f1$fitted[,1]-f1$fitted[,5]
			f.permute3.1<-f1$fitted[,3]-f1$fitted[,5]
			f.permute1<-as.numeric(apply(cbind(f.permute1.1,f.permute2.1,f.permute3.1),1,max))
			
			f.permute1.1<-f1$fitted[,2]-f1$fitted[,2]
			f.permute2.1<-f1$fitted[,2]-f1$fitted[,4]
			f.permute3.1<-f1$fitted[,4]-f1$fitted[,6]
			f.permute2<-as.numeric(apply(cbind(f.permute1.1,f.permute2.1,f.permute3.1),1,max))
			f.permute<-cbind(f.permute1,f.permute2)
		}
		if(b.effect=="year.by.status") {
			f.permute1.1<-f1$fitted[,1]-f1$fitted[,3]
			f.permute2.1<-f1$fitted[,1]-f1$fitted[,5]
			f.permute3.1<-f1$fitted[,3]-f1$fitted[,5]
			f.permute1<-as.numeric(apply(cbind(f.permute1.1,f.permute2.1,f.permute3.1),1,max))
			
			f.permute1.1<-f1$fitted[,2]-f1$fitted[,2]
			f.permute2.1<-f1$fitted[,2]-f1$fitted[,4]
			f.permute3.1<-f1$fitted[,4]-f1$fitted[,6]
			f.permute2<-as.numeric(apply(cbind(f.permute1.1,f.permute2.1,f.permute3.1),1,max))
			f.permute<-cbind(f.permute1,f.permute2)
		}
		
		return(f.permute)      
	}
	

	calc.model<-function(j) {
		f1<-fmodel.estimate(resp.dat=ro.data,model=b.effect)
		
		if(b.effect=="status") {
			#f1.noe<-fmodel.estimate(resp.dat=ro.data,model="noe")
			f.permute<-f1$fitted[,1]-f1$fitted[,2]
			#f.permute<-as.numeric(apply(f.permute,1,max))
		}
#		if(b.effect=="year") {
#			f.permute1<-f1$fitted[,1]-f1$fitted[,2]
#			f.permute2<-f1$fitted[,1]-f1$fitted[,3]
#			f.permute3<-f1$fitted[,2]-f1$fitted[,3]
#			f.permute<-as.numeric(apply(cbind(f.permute1,f.permute2,f.permute3),1,max))
#		}
#		if(b.effect=="year.and.status") {
#			f.permute1.1<-f1$fitted[,1]-f1$fitted[,3]
#			f.permute2.1<-f1$fitted[,1]-f1$fitted[,5]
#			f.permute3.1<-f1$fitted[,3]-f1$fitted[,5]
#			f.permute1<-as.numeric(apply(cbind(f.permute1.1,f.permute2.1,f.permute3.1),1,max))
#			
#			f.permute1.1<-f1$fitted[,2]-f1$fitted[,2]
#			f.permute2.1<-f1$fitted[,2]-f1$fitted[,4]
#			f.permute3.1<-f1$fitted[,4]-f1$fitted[,6]
#			f.permute2<-as.numeric(apply(cbind(f.permute1.1,f.permute2.1,f.permute3.1),1,max))
#			f.permute<-cbind(f.permute1,f.permute2)
#		}
		if(b.effect=="year.by.status") {
			f.permute1.1<-f1$fitted[,1]-f1$fitted[,2]
			f.permute2.1<-f1$fitted[,3]-f1$fitted[,4]
			f.permute3.1<-f1$fitted[,5]-f1$fitted[,6]
			f.permute<-cbind(f.permute1.1,f.permute2.1,f.permute3.1)
		#	f.permute1<-as.numeric(apply(cbind(f.permute1.1,f.permute2.1,f.permute3.1),1,max))
			
		#	f.permute1.1<-f1$fitted[,2]-f1$fitted[,4]
		#	f.permute2.1<-f1$fitted[,2]-f1$fitted[,4]
		#	f.permute3.1<-f1$fitted[,4]-f1$fitted[,6]
		#	f.permute2<-as.numeric(apply(cbind(f.permute1.1,f.permute2.1,f.permute3.1),1,max))
		#	f.permute<-cbind(f.permute1,f.permute2)
		}
		
		return(f.permute)      
	}
	
    boot.generate.fishsize<- function(i) {
		rand.boot<-          runif(xx.agg$Length[i])
		rand.boot<-   approx(c(0,boot.lookup[,i]),c(20,EvPts),xout=rand.boot)$y
		rand.apply<-  sm.density(rand.boot,display="none",positive=TRUE,
				eval.points=EvPts,h=ro.mean.pars)$estimate
		
		rand.apply<- rand.apply/sum(rand.apply)
		#  cat(dim(rand.apply),"\n")
		#boot.dump[(k-1)*length(EvPts)+1:length(EvPts),]<-rand.apply
		return(rand.apply)
	}
	
	data.length<-nrow(xx.agg)  
	
	if(b.effect=="year.by.status" | b.effect=="year.and.status") {
		f.2008<-matrix(0,ncol=length(EvPts),nrow=n.boot)
		f.2009<-matrix(0,ncol=length(EvPts),nrow=n.boot)
		f.2010<-matrix(0,ncol=length(EvPts),nrow=n.boot)
		
		for(k in 1:n.boot){
			ro.data<-as.numeric(sapply(1:18,boot.generate.fishsize))
			f.stuff<-calc.model(k)
			f.2008[k,]<-f.stuff[,1]
			f.2009[k,]<-f.stuff[,2]
			f.2010[k,]<-f.stuff[,3]
		}
		
		return(list(f2008=f.2008,f2009=f.2009,f2010=f.2010))
	}else{
		f.out<-matrix(0,ncol=length(EvPts),nrow=n.boot)
		
		for(k in 1:n.boot){
			ro.data<-as.numeric(sapply(1:18,boot.generate.fishsize))
			f.stuff<-calc.model(k)
			f.out[k,]<-f.stuff
		}
		return(f.out)
	}
}


#rm(f.permute,f.permute1.1,f.permute2.1,f.premute3.1,boot.generate.fishsize,ro.data,
#		rand.boot,rand.apply,b.effect,f1,data.length,f.2008,f.2009,f.2010,f.out,f.stuff)

alpha<-0.015

                                                                                                      
                                                                                                      
#####Are all these plots tooo smooth - main plot                                                                                                 
#  need to plot out model
system.time(boot.ci.status<-boot.interval.model(n.boot=999,b.effect="status",raw.data=ro.density.data)) 
boot.ci.status.sort<-t(apply(boot.ci.status,2,sort))
boot.ci.status.median<-as.vector(t(apply(boot.ci.status,2,median)))
par(mfrow=c(1,1))
# plot((f.status$fitted[,1]-f.status$fitted[,2])~EvPts,type="l")
plot(boot.ci.status.median~EvPts,type="l")
abline(h=0,lty=2)
lines( (boot.ci.status.sort[,ceiling(alpha/2*ncol(boot.ci.status.sort))])~EvPts,lty=2,col=2)
lines((boot.ci.status.sort[,ncol(boot.ci.status.sort)-floor(alpha/2*ncol(boot.ci.status.sort))])~EvPts,lty=2,col=3)


# Report status ----
boot.ci.status.bound<-numeric(length(EvPts))
boot.ci.status.bound<-  ifelse((f.status$fitted[,1]-f.status$fitted[,2])>(boot.ci.status.median-boot.ci.status.sort[,ceiling(alpha/2*ncol(boot.ci.status.sort))]), 1,
                                                                                                                                         ifelse((f.status$fitted[,1]-f.status$fitted[,2])<(boot.ci.status.median-boot.ci.status.sort[,ncol(boot.ci.status.sort)-floor(alpha/2*ncol(boot.ci.status.sort))]),-1,0))
                                                                                                        diff<-diff(boot.ci.status.bound)
                                                                                                         diff.which<-which(diff==1 | diff== -1)
                                                                                                        sign<-sign(boot.ci.status.bound[c(diff.which)+1])
                                                                                                        boot.report.status<-matrix(nrow=length(sign)+1,ncol=3)
                                                                                                        boot.report.status[1,]<-c(min(EvPts),EvPts[diff.which[1]],sign(boot.ci.status.bound[1]))
                                                                                                        for(i in 1:(length(sign)-1)) {
                                                                                                          boot.report.status[i+1,]<-c(EvPts[diff.which[i]+1],EvPts[diff.which[i+1]],sign[i])
                                                                                                        }
                                                                                                        boot.report.status[nrow(boot.report.status),]<-c(EvPts[diff.which[length(diff.which)]+1],
                                                                                                                                                     max(EvPts),sign(boot.ci.status.bound[length(EvPts)]))
                                                                                                        colnames(boot.report.status)<-c("start.length","end.length","out.of.bound")
                                                                                                        #  -1 means below, 1 above and 0 between
                                                                                                        boot.report.status                                                                                                                                                           
#lines( (boot.ci.status.sort[,ceiling(alpha/2*ncol(boot.ci.status.sort))]-boot.ci.status.median)~EvPts,lty=2,col=2)
#lines(  (boot.ci.status.sort[,ncol(boot.ci.status.sort)-floor(alpha/2*ncol(boot.ci.status.sort))]-boot.ci.status.median )~EvPts,lty=2,col=3)
#lines(  (f.status$fitted[,1]-f.status$fitted[,2] - boot.ci.status.sort[,ncol(boot.ci.status.2)-floor(alpha/2*ncol(boot.ci.status.2))])~EvPts,lty=2,col=3)
                                                                                                        
#                                                                                                         
# Report Status                                                                                                        start.length end.length out.of.bound
#                                                                                                         [1,]           21         33            0
#                                                                                                         [2,]           34         67           -1
#                                                                                                         [3,]           68        104            0
#                                                                                                         [4,]          105        299            1
#                                                                                                         [5,]          300        359            0
#                                                                                                         [6,]          360       1000           -1
#                                                                                                         
                                                                                                    
                                                                                                        
system.time(boot.ci.ybys<-boot.interval.model(n.boot=999,b.effect="year.by.status",raw.data=ro.density.data)) 
boot.ci.ybys.2008.sort<-t(apply(boot.ci.ybys$f2008,2,sort))
boot.ci.ybys.2008.median<-as.vector(t(apply(boot.ci.ybys$f2008,2,median)))
boot.ci.ybys.2009.sort<-t(apply(boot.ci.ybys$f2009,2,sort))
boot.ci.ybys.2009.median<-as.vector(t(apply(boot.ci.ybys$f2009,2,median)))
boot.ci.ybys.2010.sort<-t(apply(boot.ci.ybys$f2010,2,sort))
boot.ci.ybys.2010.median<-as.vector(t(apply(boot.ci.ybys$f2010,2,median)))



boot.ci.ybys.bound<-matrix(ncol=3,nrow=length(EvPts))
boot.ci.ybys.bound[,1]<-  ifelse((f.ybys$fitted[,1]-f.ybys$fitted[,2])>(boot.ci.ybys.2008.median-boot.ci.ybys.2008.sort[,ceiling(alpha/2*ncol(boot.ci.ybys.2008.sort))]), 1,
				ifelse((f.ybys$fitted[,1]-f.ybys$fitted[,2])<(boot.ci.ybys.2008.median-boot.ci.ybys.2008.sort[,ncol(boot.ci.ybys.2008.sort)-floor(alpha/2*ncol(boot.ci.ybys.2008.sort))]),-1,0))
boot.ci.ybys.bound[,2]<-   ifelse((f.ybys$fitted[,3]-f.ybys$fitted[,4])>(boot.ci.ybys.2009.median-boot.ci.ybys.2009.sort[,ceiling(alpha/2*ncol(boot.ci.ybys.2009.sort))]), 1,
		ifelse((f.ybys$fitted[,3]-f.ybys$fitted[,4])<(boot.ci.ybys.2009.median-boot.ci.ybys.2009.sort[,ncol(boot.ci.ybys.2009.sort)-floor(alpha/2*ncol(boot.ci.ybys.2009.sort))]),-1,0))
boot.ci.ybys.bound[,3]<-  ifelse((f.ybys$fitted[,5]-f.ybys$fitted[,6])>(boot.ci.ybys.2010.median-boot.ci.ybys.2010.sort[,ceiling(alpha/2*ncol(boot.ci.ybys.2010.sort))]), 1,
		ifelse((f.ybys$fitted[,5]-f.ybys$fitted[,6])<(boot.ci.ybys.2010.median-boot.ci.ybys.2010.sort[,ncol(boot.ci.ybys.2010.sort)-floor(alpha/2*ncol(boot.ci.ybys.2010.sort))]),-1,0))

diff.2008<-diff(boot.ci.ybys.bound[,1])
diff.2009<-diff(boot.ci.ybys.bound[,2])
diff.2010<-diff(boot.ci.ybys.bound[,3])
diff.which.2008<-which(diff.2008==1 | diff.2008== -1)
sign.2008<-sign(boot.ci.ybys.bound[c(diff.which.2008)+1,1])
boot.report.2008<-matrix(nrow=length(sign.2008)+1,ncol=3)
boot.report.2008[1,]<-c(min(EvPts),EvPts[diff.which.2008[1]],sign(boot.ci.ybys.bound[1,1]))
for(i in 1:(length(sign.2008)-1)) {
	boot.report.2008[i+1,]<-c(EvPts[diff.which.2008[i]+1],EvPts[diff.which.2008[i+1]],sign.2008[i])
}
boot.report.2008[nrow(boot.report.2008),]<-c(EvPts[diff.which.2008[length(diff.which.2008)]+1],
		          max(EvPts),sign(boot.ci.ybys.bound[length(EvPts),1]))
colnames(boot.report.2008)<-c("start.length","end.length","out.of.bound")
#  -1 means below, 1 above and 0 between
boot.report.2008

#start.length end.length out.of.bound
#[1,]           21         27            0
#[2,]           28         78           -1
#[3,]           79         96            0
#[4,]           97        331            1
#[5,]          332        366            0
#[6,]          367       1000           -1
diff.which.2009<-which(diff.2009==1 | diff.2009== -1)
sign.2009<-sign(boot.ci.ybys.bound[c(diff.which.2009)+1,2])
boot.report.2009<-matrix(nrow=length(sign.2009)+1,ncol=3)
boot.report.2009[1,]<-c(min(EvPts),EvPts[diff.which.2009[1]],sign(boot.ci.ybys.bound[1,2]))
for(i in 1:(length(sign.2009)-1)) {
	boot.report.2009[i+1,]<-c(EvPts[diff.which.2009[i]+1],EvPts[diff.which.2009[i+1]],sign.2009[i])
}
boot.report.2009[nrow(boot.report.2009),]<-c(EvPts[diff.which.2009[length(diff.which.2009)]+1],
		max(EvPts),sign(boot.ci.ybys.bound[length(EvPts),2]))
colnames(boot.report.2009)<-c("start.length","end.length","out.of.bound")
#  -1 means below, 1 above and 0 between
boot.report.2009
#start.length end.length out.of.bound
#[1,]           21         39           -1
#[2,]           40         96            0
#[3,]           97        281            1
#[4,]          282        324            0
#[5,]          325       1000           -1


diff.which.2010<-which(diff.2010==1 | diff.2010== -1)
sign.2010<-sign(boot.ci.ybys.bound[c(diff.which.2010)+1,3])
boot.report.2010<-matrix(nrow=length(sign.2010)+1,ncol=3)
boot.report.2010[1,]<-c(min(EvPts),EvPts[diff.which.2010[1]],sign(boot.ci.ybys.bound[1,3]))
for(i in 1:(length(sign.2010)-1)) {
	boot.report.2010[i+1,]<-c(EvPts[diff.which.2010[i]+1],EvPts[diff.which.2010[i+1]],sign.2010[i])
}
boot.report.2010[nrow(boot.report.2010),]<-c(EvPts[diff.which.2010[length(diff.which.2010)]+1],
		max(EvPts),sign(boot.ci.ybys.bound[length(EvPts),3]))
colnames(boot.report.2010)<-c("start.length","end.length","out.of.bound")
#  -1 means below, 1 above and 0 between
boot.report.2010
#
#start.length end.length out.of.bound
#[1,]           21         41           -1
#[2,]           42        122            0
#[3,]          123        268            1
#[4,]          269        303            0
#[5,]          304        606           -1
#[6,]          607       1000            0
                                                                                                        par(mfrow=c(1,3))
                                                                                                                                                                                                            plot(boot.ci.ybys.2008.median~EvPts,type="l",main="2008 Year*Status",ylim=c(-1e-3,2e-3))
                                                                                                         abline(h=0,lty=2)
                                                                                                         lines( boot.ci.ybys.2008.sort[,ceiling(alpha/2*ncol(boot.ci.ybys.2008.sort))]~EvPts,lty=2,col=2)
                                                                                                         lines(  boot.ci.ybys.2008.sort[,ncol(boot.ci.ybys.2008.sort)-floor(alpha/2*ncol(boot.ci.ybys.2008.sort))]~EvPts,lty=2,col=3)
                                                                                                         #abline(v=EvPts[which(diff.2008==1 | diff.2008== -1)])
                                                                                                           
                                                                                                      
# par(mfrow=c(1,1)) - I have mucked up below - can fix it
                                                                                                      plot(boot.ci.ybys.2009.median~EvPts,type="l",main="2009 Year*Status",ylim=c(-1e-3,2e-3))
#                                                                                                       plot(boot.ci.ybys.2009.median~EvPts,type="l",main="Status",ylim=c(-1e-3,2e-3))                                                                   
                                                                                                      abline(h=0,lty=2)
                                                                                                         lines( boot.ci.ybys.2009.sort[,ceiling(alpha/2*ncol(boot.ci.ybys.2009.sort))]~EvPts,lty=2,col=2)
                                                                                                         lines(  boot.ci.ybys.2009.sort[,ncol(boot.ci.ybys.2009.sort)-floor(alpha/2*ncol(boot.ci.ybys.2008.sort))]~EvPts,lty=2,col=3)
                                                                                                         
                                                                                                       plot(boot.ci.ybys.2010.median~EvPts,type="l",main="2010 Year*Status",ylim=c(-1e-3,2e-3))
                                                                                                         abline(h=0,lty=2)
                                                                                                         lines( boot.ci.ybys.2010.sort[,ceiling(alpha/2*ncol(boot.ci.ybys.2010.sort))]~EvPts,lty=2,col=2)
                                                                                                         lines(  boot.ci.ybys.2010.sort[,ncol(boot.ci.ybys.2010.sort)-floor(alpha/2*ncol(boot.ci.ybys.2008.sort))]~EvPts,lty=2,col=3)
                                                                                                        

#
save.image("fish_image_20160516.RData")
#                                                                                                         par(mfrow=c(1,3))
#                                                                                                         plot((f.ybys$fitted[,1]-f.ybys$fitted[,2])~EvPts,type="l",main="2008 Year*Status",ylim=c(-1e-3,2e-3))
#                                                                                                         abline(h=0,lty=2)
#                                                                                                         lines( (boot.ci.ybys.2008.sort[,ceiling(alpha/2*ncol(boot.ci.ybys.2008.sort))]-boot.ci.ybys.2008.median)~EvPts,lty=2,col=2)
#                                                                                                         lines(  (boot.ci.ybys.2008.sort[,ncol(boot.ci.ybys.2008.sort)-floor(alpha/2*ncol(boot.ci.ybys.2008.sort))]-boot.ci.ybys.2008.median)~EvPts,lty=2,col=3)
#                                                                                                         #abline(v=EvPts[which(diff.2008==1 | diff.2008== -1)])
#                                                                                                         
#                                                                                                         
#                                                                                                         plot((f.ybys$fitted[,3]-f.ybys$fitted[,4])~EvPts,type="l",main="2009 Year*Status",ylim=c(-1e-3,2e-3))
#                                                                                                         abline(h=0,lty=2)
#                                                                                                         lines( (boot.ci.ybys.2009.sort[,ceiling(alpha/2*ncol(boot.ci.ybys.2009.sort))]-boot.ci.ybys.2009.median)~EvPts,lty=2,col=2)
#                                                                                                         lines(  (boot.ci.ybys.2009.sort[,ncol(boot.ci.ybys.2009.sort)-floor(alpha/2*ncol(boot.ci.ybys.2008.sort))]-boot.ci.ybys.2009.median)~EvPts,lty=2,col=3)
#                                                                                                         
#                                                                                                         plot((f.ybys$fitted[,5]-f.ybys$fitted[,6])~EvPts,type="l",main="2010 Year*Status",ylim=c(-1e-3,2e-3))
#                                                                                                         abline(h=0,lty=2)
#                                                                                                         lines( (boot.ci.ybys.2010.sort[,ceiling(alpha/2*ncol(boot.ci.ybys.2010.sort))]-boot.ci.ybys.2010.median)~EvPts,lty=2,col=2)
#                                                                                                         lines(  (boot.ci.ybys.2010.sort[,ncol(boot.ci.ybys.2010.sort)-floor(alpha/2*ncol(boot.ci.ybys.2008.sort))])-boot.ci.ybys.2010.median~EvPts,lty=2,col=3)
#                                                                                                         


                                                                                                        
###   DEBUGGING
boot.ci.status<-boot.interval.model(n.boot=999,b.effect="status",raw.data=ro.density.data) 
system.time(boot.ci.year<-boot.interval.model(n.boot=999,b.effect="year",raw.data=ro.density.data) )
boot.ci.yands<-boot.interval.model(n.boot=999,b.effect="year.and.status",raw.data=ro.density.data) 

plot(f.noe$fitted~EvPts,type="l",xlab="Fish Size Length (mm)",ylab="Estimated Probability",ylim=c(0,0.004),lwd=2)
lines(f.status$fitted[,2]~EvPts,col=2)
lines(f.status$fitted[,1]~EvPts,col=2)

boot.ci.status.2<-matrix(boot.ci.status,ncol=999)
boot.ci.status.2<-t(apply(cbind(f.status$fitted[,1]-f.status$fitted[,2],boot.ci.status.2),1,sort))

lines((f.noe$fitted+boot.ci.status.2[,51])~EvPts,lty=2)
lines((f.noe$fitted+boot.ci.status.2[,950])~EvPts,lty=2)

 
 