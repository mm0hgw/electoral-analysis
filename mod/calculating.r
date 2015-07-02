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

calculate_normalised_a <- function(V,N){
	a<-calculate_a(V,N)
	pop_mean <- calculate_a(sum(V),sum(N))
	pop_sd <- custom_sd(a,pop_mean)
	(a-pop_mean)/pop_sd
}

normalised_a <- function(b){
	total<- calculate_normalised_a(b$V,b$N)
	mask <- !is.na(b$NP)
	postal<- calculate_normalised_a(b$VP[mask],b$NP[mask])
	nonp <- calculate_normalised_a(
		calculate_np(b$V[mask],b$VP[mask]),
		calculate_np(b$N[mask],b$NP[mask]))
	list(total,postal,nonp)
}

# cook a list of files using the normalised turnout technique
cook_files <- function(files){
	foreach(file=files,
		.combine=c,
		.inorder=F,
		.options.multicore=mcoptions,
		.export=c("normalised_a","custom_chisq")
	) %dopar% {
		b<-read_ballot(file,do.cook=F)
		a<-normalised_a(b)
		names(a)<-paste(file,c("total","postal","non-postal"))
		unlist(lapply(lapply(a,density),custom_chisq))
	}
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

# find a local maximum by iteration.
first_local_maximum <- function(density_obj,from=min(density_obj$x)){
                # identify where we're iterating from
        trail <- which.min(abs(density_obj$x-from))
                # step forward forward to some positive gradient
        while(trail<length(density_obj$y)&density_obj$y[trail+1]<=density_obj$y[trail]){
                trail <- trail + 1
        }
                # step up the positive gradient to the trailing edge of the maximum
        while(trail<length(density_obj$y)&density_obj$y[trail+1]>=density_obj$y[trail]){
                trail <- trail + 1
        }
                # if we've reached the end, we haven't found a maximum
        if(trail>=length(density_obj$x)){
                return(NA)      # reached the end of the vector
        }
                # now look for the leading edge
        lead <- trail
        while(lead<length(density_obj$y)&density_obj$y[lead-1]>=density_obj$y[lead]){
                lead <- lead - 1
        }
                # if we we're asked the next maximum from here, return it.
        if(density_obj$x[lead] <= from){
                first_local_maximum(density_obj,from=trail)
        }
                # return our answer
        return(density_obj$x[lead:trail])
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
