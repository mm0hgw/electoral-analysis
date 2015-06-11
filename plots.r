# plots
source("mod/calculating.r")

# calculate regular x values over range of sample
dom <- function(sample){
	seq(min(sample),max(sample),length.out=100)
}

# calculate cdf intercept
cdf_intercept <- function(sample,mean){
	if(length(sample)>0){
		return(sum(sample<mean)/length(sample))
	}
	return()
}

#plot turnout
plot_turnout <- function(V,N,main){
	a<-calculate_a(V,N)
	pop_mean<-calculate_a(sum(V),sum(N))
	pop_sd<-custom_sd(a,pop_mean)
	pop_intercept<-cdf_intercept(a,pop_mean)
#	sample_intercept<-cdf_intercept(a,mean(a))
	sub<-sprintf("Population mean (magenta) intercepts cdf @ %f",
		pop_intercept)
	plot(ecdf(a),verticals=T,do.points=F,main=main,sub=sub)
#	lines(x=dom(a),pnorm(dom(a),mean=pop_mean,sd=pop_sd),col="magenta")
#	lines(x=dom(a),pnorm(dom(a),mean=mean(a),sd=sd(a)),col="blue")
	abline(h=0.5)
	abline(v=pop_mean,col="magenta")
#	abline(v=mean(a),col="blue")
	abline(h=cdf_intercept(a,pop_mean),col="magenta")
#	abline(h=cdf_intercept(a,mean(a)),col="blue")
}
