#
#       Display functions
#

#
#	display setup functions
#
 
pdf_target<-function(name="turnout_analysis"){
        pdf(file=paste(name,".pdf",sep=""),paper="a4",width=7,height=10)
}

#
#	display component functions
#

#plot mean/sd lines on a graph
mean_sd_lines <- function(sample,p_mean=mean(sample),p_sd=sd(sample)){
        abline(v=p_mean,col="green")
        abline(v=p_mean+p_sd,col="blue")
        abline(v=p_mean-p_sd,col="blue")
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
        abline(v=i_peak+i_r_sd,col="blue")
        abline(v=i_peak-i_l_sd,col="blue")
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
        main <- paste("Matching \"",match_pattern,"\"",sep="")
        sub <- paste("Matched sample ranges from ",sprintf("%.2f",min(i_dev)),
                " to ",sprintf("%.2f",max(i_dev)),
                " in SDs of deviation.",sep="")   
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
        return(i_dev)
}

single_display<-function(sample){
	pm<-sum(sample["V",])/sum(sample["N",])
        sub<-paste("population mean intercepted by c.d.f. at",intercept)
        plot(ecdf(sample),main=main,sub=sub,verticals=T,do.points=F)
        abline(v=pm,col="blue")
        abline(h=intercept,col="red")
}
