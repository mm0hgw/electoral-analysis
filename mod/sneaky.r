# the sneaky module

# NO BIG FEET!

require(parallel)
require(foreach)
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

# specialised division that doesn't break on edge cases
calculate_a <- function(V,N){
		# create edge case masks
        mask0 <- N==0 # These will become 0 instead of NaN
	mask1 <- V>N  # These will become 1 instead of >1
        a<-V/N
        a[mask0]<-0
	a[mask1]<-1
        return(a)
}

#given a matrix with N,V,a rows, compute population mean, compare turnout vector to mean
#count occurances and divide by vector length
cdf_mean_intercept<-function(bal){
	if(length(bal["N",])==0){ # trap zero division
		return()
	}
        out<-sum(bal["a",]< calculate_a(sum(bal["V",]),sum(bal["N",]))) / length(bal["N",])
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

# open a pdf display
pdf_target<-function(name="turnout_analysis"){
        pdf(file=paste(name,".pdf",sep=""),paper="a4",width=7,height=10)
}

