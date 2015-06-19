# plots
source("mod/calculating.r")
library(mixtools)

# calculate regular x values over range of sample
dom <- function(sample){
	seq(min(sample),max(sample),length.out=100)
}

#plot turnout
plot_turnout_cdf <- function(V,N,main){
	a<-calculate_a(V,N)
	pop_mean<-calculate_a(sum(V),sum(N))
	pop_sd<-custom_sd(a,pop_mean)
	pop_intercept<-cdf_intercept(a,pop_mean)
	sub<-sprintf("Population mean (magenta) intercepts cdf @ %f",
		pop_intercept)
	plot(ecdf(a),verticals=T,do.points=F,main=main,sub=sub)
	abline(v=pop_mean,col="magenta")
	abline(h=cdf_intercept(a,pop_mean),col="magenta")
	x<-dom(a)
	lines(x=x,y=pnorm(q=x,mean=pop_mean,sd=pop_sd),col="green")	
}

plot_turnout_density <- function(V,N,main="Add a title",do.model=F){
	a<-calculate_a(V,N)
	d<-density(a)
	model_mean<-mean(first_local_maximum(d))
	model_sd<-custom_sd(a[a<=model_mean],model_mean)
	pop_mean<-calculate_a(sum(V),sum(N))
	pop_sd<-custom_sd(a,pop_mean)
	sub<-sprintf("Population mean (magenta) %f",
		pop_mean)
	plot(d,main=main,sub=sub)
	if(do.model==T){
		lines(x=d$x,y=dnorm(x=d$x,mean=model_mean,sd=model_sd),col="magenta")
	}
	lines(x=d$x,y=dnorm(x=d$x,mean=pop_mean,sd=pop_sd),col="green")
	abline(v=pop_mean,col="magenta")
}

#plot mix
plot_mix <- function(V,N,k=2,main="Add a title"){
	a<-calculate_a(V,N)
	mixmdl<-normalmixEM(a,k=k)
	plot(mixmdl,which=2)	
}

plot_triple <- function(file,main=file){
	b<-read_ballot(file,do.cook=F)
	V<-b$V
	N<-b$N
	plot_turnout_density(V=V,N=N,main=paste(main,"total"))
	plot_turnout_cdf(V=V,N=N,main=paste(main,"total"))
	VP<-b$VP[!is.na(b$NP)]
	NP<-b$NP[!is.na(b$NP)]
	plot_turnout_density(V=VP,N=NP,main=paste(main,"postal"))
	plot_turnout_cdf(V=VP,N=NP,main=paste(main,"postal"))
	VNP<-(b$V-b$VP)[!is.na(b$NP)]
	NNP<-(b$N-b$NP)[!is.na(b$NP)]
	plot_turnout_density(V=VNP,N=NNP,main=paste(main,"non-postal"))
	plot_turnout_cdf(V=VNP,N=NNP,main=paste(main,"non-postal"))
}

do_triple_plots <- function(file="triple_plots.pdf"){
	pdf(file)
	foreach(file=paste(sep="","csv/",list.files(path="csv/")))%do%{plot_triple(file)}
	dev.off()
}