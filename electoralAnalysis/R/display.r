#
#       Display functions
#

#
#	display setup functions
#
 
pdf_target<-function(name="turnout_analysis"){
        pdf(file=paste(name,".pdf",sep=""),paper="a4",width=7,height=10)
}

png_iphone6 <- function(name="turnout_analysis"){
	png(	file=paste(name,".png",sep=""),
		height=1334,
		width=750
	)
}

svg_iphone6 <- function(name="turnout_analysis"){
	svg(	file=paste(name,".svg",sep=""),
		height=1334,
		width=750
	)
}

#
#	display component functions
#

#plot mean/sd lines on a graph
mean_sd_lines <- function(sample,p_mean=mean(sample),p_sd=sd(sample)){
        abline(v=p_mean,col="green")
        abline(v=p_mean+p_sd*seq(1,4),col="blue")
        abline(v=p_mean-p_sd*seq(1,4),col="blue")
}
 
#calculate separate sds for lhs / rhs of peak density and display
peak_separate_sd_lines<-function(i,i_peak,separate_sd=F){
	if(separate_sd==T){
        	i_l_sd <- custom_sd(i[i<i_peak],i_peak)
        	i_r_sd <- custom_sd(i[i>i_peak],i_peak)
	}else{
		i_sd <- custom_sd(i,i_peak)
		i_l_sd <- i_sd
		i_r_sd <- i_sd
	}
        abline(v=i_peak,col="green")
        abline(v=i_peak+i_r_sd*seq(1,4),col="blue")
        abline(v=i_peak-i_l_sd*seq(1,4),col="blue")
}

#
#	display functions
#

# custom plot function
custom_plot_ecdf<-function(sample,match_pattern,do.density=T,do.cdf=T){
        names<-names(sample)
        intercept<-sample
        matched_points <- grep(pattern=match_pattern,names)
        i_match <-intercept[matched_points]
        i_unmatch <- intercept[-matched_points]
        density_obj <- density(intercept)
        i_x<-density_obj$x
        ecdf_obj <- ecdf(intercept)
        i_peak <- density_obj$x[which.max(density_obj$y)]
        i_mean <- mean(intercept)
        i_sd <- custom_sd(intercept,i_mean)
        i_dev <- deviation_in_SDs(i_match,i_mean,i_sd)
	prob <- sigma_to_probability(i_dev)
        main <- paste("Matching \"",match_pattern,"\"",sep="")
        sub <- paste("Matched sample range ",sprintf("%.2f",min(i_dev)),
                " to ",sprintf("%.2f",max(i_dev)),
                " sigma. Probability range 1/",sprintf("%.2f",1/max(prob)),
		" to 1/",sprintf("%.2f",1/min(prob)),sep="")   
                # do density plot
        if(do.density==T){
                plot(density_obj,main=paste(main,"population mean / cdf intercept error density"),sub=sub)
                lines(x=i_x,dnorm(x=i_x,mean=i_mean,sd=i_sd),col="magenta") 
                peak_separate_sd_lines(intercept,i_mean)
                points(x=i_match,y=dnorm(x=i_match,mean=i_mean,sd=i_sd),pch=1,col="red")
        }
                # do c.d.f. plot
        if(do.cdf==T){
                plot(ecdf_obj,verticals=T,do.points=F,main=paste(main,"population mean / cdf intercept error c.d.f."),sub=sub)
                lines(x=i_x,pnorm(q=i_x,mean=i_mean,sd=i_sd),col="magenta")
                points(x=i_match,y=pnorm(q=i_match,mean=i_mean,sd=i_sd),pch=1,col="red")
                peak_separate_sd_lines(intercept,i_mean)
                abline(h=0.5)
        }
	names(prob)<-names(i_match)
        return(i_dev)
}

single_display<-function(sample){
	pm<-sum(sample["V",])/sum(sample["N",])
        sub<-paste("population mean intercepted by c.d.f. at",intercept)
        plot(ecdf(sample),main=main,sub=sub,verticals=T,do.points=F)
        abline(v=pm,col="blue")
        abline(h=intercept,col="red")
}


#
#	normalised a display functions
#

add_a_line <- function(a,...){
	d<-density(a)
	lines(x=d$x,y=d$y,...)
}

plot_with_normal <- function(d,...){
	plot(d,...)
	lines(x=d$x,y=dnorm(x=d$x))
}

plot_two_part_sample <- function(V,N,...){
	sample <- V/N
	d<-density(sample)
	plot(d,...)
	pmean<-sum(V)/sum(N)
	psd<-sqrt(mean((sample-pmean)^2))
	lines(x=d$x,y=dnorm(d$x,mean=pmean,sd=psd),col="blue")
}

custom_chisq <- function(d,...){
	sum((d$y-dnorm(d$x))^2)
}

# plot a set of shpaefile object regions
# polygon_list is a 
region_plot <- function(shape_obj,sample,...){
	require(rgdal)
	require(foreach)
	color_vector <- sample_to_color(sample)
	lim<-max(abs(sample))
	leg<-seq(-lim,lim,length.out=256)
	
	layout(t(1:2),widths=c(6,1))
	par(mar=c(.5,.5,.5,.5),oma=rep(3,4),las=1)
	plot(shape_obj,...)
	foreach(p=shape_obj@polygons,
		q=color_vector
	)%do%{
		lapply(p@Polygons,
			function(r){
				polygon(r@coords,col=q)
			}
		)
	}	
	image(1,
		leg,
		t(seq_along(leg)),
		col=sample_to_color(leg),
		axes=FALSE,
		ylab="standard deviations from mean"
	)
	axis(4)
}

cdf_display<-function(
	V,
	N,
	normalise=FALSE,
	do.plot=TRUE,
	...
){
	if(normalise==FALSE){
		p_mean<-sum(V)/sum(N)
		sample<-V/N
		p_sd<-custom_sd(sample,center=p_mean)
	}else{
		sample<-calculate_normalised_a(V,N)
		p_mean<-0
		p_sd<-1
	}
	l<-limits(sample)
	hw<-(l[2]-l[1])/5
	x<-seq(l[1]-hw,l[2]+hw,length.out=200)
	cdf_obj<-ecdf(sample)
	if(do.plot==TRUE){
		plot(cdf_obj,do.points=FALSE,verticals=TRUE,...)
		lines(x,pnorm(x,mean=p_mean,sd=p_sd),col="blue")
		lines(x=c(rep(p_mean,2),min(x)),y=c(0.5,rep(cdf_mean_intercept(V,N),2)),col="red")
	}
	return(cdf_obj)
}

density_display<-function(
	V=0,
	N=0,
	normalise=FALSE,
	do.plot=TRUE,
	...
){
	if(normalise==FALSE){
		p_mean<-sum(V)/sum(N)
		sample<-V/N
		p_sd<-custom_sd(sample,center=p_mean)
	}else{
		sample<-calculate_normalised_a(V,N)
		p_mean<-0
		p_sd<-1
	}
	l<-limits(sample)
	hw<-(l[2]-l[1])/5
	x<-seq(l[1]-hw,l[2]+hw,length.out=200)
	d_obj<-density(sample)
	if(do.plot==TRUE){
		plot(d_obj,...)
		lines(
			x,
			dnorm(x,mean=p_mean,sd=p_sd),
			col="blue"
		)
	}
	return(d_obj)
}
