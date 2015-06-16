# plots
source("mod/calculating.r")

# calculate regular x values over range of sample
dom <- function(sample){
	seq(min(sample),max(sample),length.out=100)
}

#plot turnout
plot_turnout <- function(V,N,main){
	a<-calculate_a(V,N)
	pop_mean<-calculate_a(sum(V),sum(N))
	pop_sd<-custom_sd(a,pop_mean)
	pop_intercept<-cdf_intercept(a,pop_mean)
	sub<-sprintf("Population mean (magenta) intercepts cdf @ %f",
		pop_intercept)
	plot(ecdf(a),verticals=T,do.points=F,main=main,sub=sub)
	abline(v=pop_mean,col="magenta")
	abline(h=cdf_intercept(a,pop_mean),col="magenta")
}

#plot mix
plot_mix <- function(V,N,main="Add a title"){
	a<-calculate_a(V,N)
	mixmdl<-normalmixEM(a)
	plot(mixmdl,which=2)	
}

