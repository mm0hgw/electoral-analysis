#
#	Calculating 
#
require(foreach)
require(NORMT3)

# Real number only probability computation
sigma_to_probability <- function(x){
        Re(erfc(x/sqrt(2)))
}

#sort sample by decreasing deviation
sort_sample <- function(sample){
	o<-as.numeric(sample)
	p<-abs(o-mean(o))
	sample[order(p,decreasing=T)]
}

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

# specialised subtraction handling edge cases
calculate_np <- function(total,postal){
	out <- total - postal
	out[postal==0] <- 0
	out
}

# calculate cdf intercept  
cdf_intercept <- function(sample,mean){
        if(length(sample)>0){
                return(sum(sample<mean)/length(sample))
        }
        return()
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

# calculate cdf/mean intercepts for ballot and subregions
results_by_region <- function(bal,tag,title){
		# identify any subregions
        regions<-levels(as.factor(bal$Region))
	if(sum(regions=="")==0){
		regions<-c("",regions)
	}
		# for each sub region, run cdf_mean_intercept, seed results with whole set
	FUN<-function(r){
		if(r==""){
			a<-bal$ballots
		}else{
			a<-bal$ballots[,bal$Region==r]
		}
		mask<-is.na(a["N",])
		b<-a[,!mask]
		if(length(b["N",b["N",]!=0])<=2|sum(b["N",])==0){
			return(-1)
		}
		out<-cdf_mean_intercept(b)
		if(is.na(out)){
			return(-1)
		}
                out
	}
	out_names<-paste(title,regions,tag)
	out<-foreach(r=regions,
		.inorder=F,
		.combine=c,
		.multicombine=T,
		.options.multicore=mcoptions
	)%dopar%{
		FUN(r)
	}
	names(out)=out_names
	out<-out[out!=-1]
        return(out)
}

# how we like our ballots cooked for easy grepping
cook_ballot <- function(ballot,title){
        ballot$NNP<-calculate_np(ballot$N,ballot$NP)
        ballot$VNP<-calculate_np(ballot$V,ballot$VP)
        ballot$a<-calculate_a(ballot$V,ballot$N)
        ballot$ap<-calculate_a(ballot$VP,ballot$NP)
        ballot$anp<-calculate_a(ballot$VNP,ballot$NNP)

        bal=rbind(N=ballot$N,
                        V=ballot$V,
                        a=ballot$a)
        balp=rbind(N=ballot$NP,
                        V=ballot$VP,
                        a=ballot$ap)
        balnp=rbind(N=ballot$NNP,   
                        V=ballot$VNP,
                        a=ballot$anp)

        objt<-list(
                name=ballot$name,
                Region=ballot$Region,
                ballots=bal)
        objp<-list(
                name=ballot$name,
                Region=ballot$Region,
                ballots=balp)
        objnp<-list(
                name=ballot$name,
                Region=ballot$Region,
                ballots=balnp)
	out<-results_by_region(objt,"total",title)
	outp<-results_by_region(objp,"postal",title)
	outnp<-results_by_region(objnp,"non-p",title)

        return(c(out,outp,outnp))
}
