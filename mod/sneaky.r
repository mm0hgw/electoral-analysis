# the sneaky module

# NO BIG FEET!

require(parallel)
require(chron)
require(doMC)
registerDoMC(cores=detectCores(),nocompile=TRUE)

#
#	Calculating 
#

#custom sd calculator based upon "Eq. S1" and "Eq. S2"
#in Klimek et al. (2009)
custom_sd <- function(x,center=mean(x)){
        sqrt(mean((x-center)^2))  
}

#given a matrix with N,V,a rows, compute population mean, compare turnout vector to mean
#count occurances and divide by vector length
cdf_mean_intercept<-function(bal){
        out<-(sum(bal["a",]<sum(bal["V",])/sum(bal["N",])) / length(bal["N",]))
	if(out>1){
		return(1)
	}
	return(out)
}

#calculate deviation in SDs
deviation_in_SDs <- function(x,x_mean=mean(x),x_sd=sd(x)){
        abs((x-x_mean)/x_sd)
}

#
#	Display
#

#plot mean/sd lines on a graph
mean_sd_lines <- function(sample,p_mean=mean(sample),p_sd=sd(sample)){
        abline(v=p_mean,col="green")
        abline(v=p_mean+p_sd,col="blue")
        abline(v=p_mean-p_sd,col="blue")
}
 
#calculate separate sds for lhs / rhs of peak density and display
peak_separate_sd_lines<-function(i,i_peak){
        i_l_sd <- custom_sd(i[i<i_peak],i_peak)
        i_r_sd <- custom_sd(i[i>i_peak],i_peak)
        abline(v=i_peak,col="green")
        abline(v=i_peak+i_r_sd,col="blue")
        abline(v=i_peak-i_l_sd,col="blue")
}
 
pdf_target<-function(name="turnout_analysis"){
        pdf(file=paste(name,".pdf",sep=""),paper="a4",width=7,height=10)
}

