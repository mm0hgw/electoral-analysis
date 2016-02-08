#
#	Calculating 
#
require(foreach)
require(NORMT3)

# Real number only probability computation
sigma_to_probability <- function(x){
        Re(erfc(x/sqrt(2)))
}

# cdf / mean intercept
cdf_mean_intercept<-function(V,N){
	sum((V/N)<(sum(V)/sum(N)))/length(N)
}

#sort sample by decreasing deviation
sort_sample <- function(sample){
	o<-as.numeric(sample)
	p<-abs(o-mean(o))
	sample[order(p,decreasing=TRUE)]
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

# calculate normalised turnout
calculate_normalised_a <- function(V,N){
	a<-calculate_a(V,N)
	pop_mean <- calculate_a(sum(V),sum(N))
	pop_sd <- custom_sd(a,pop_mean)
	(a-pop_mean)/pop_sd
}

# turnout function generator 
a_fn_fn <- function(N){
  function(V) calculate_normalised_a(V,N)
}

# cook a list of files using the normalised turnout technique
cook_files <- function(files){
	foreach(file=files,
		.combine=c,
		.inorder=FALSE,
		.options.multicore=mcoptions,
		.export=c("normalised_a","custom_chisq")
	) %dopar% {
		b<-read_ballot(file,do.cook=FALSE)
		a<-normalised_a(b)
		names(a)<-paste(file,c("total","postal","non-postal"))
		unlist(lapply(lapply(a,density),custom_chisq))
	}
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
