#
#	Calculating 
#
require(foreach)
require(NORMT3)

# condition for switch functions
# 1 if x has length 0
# 2 if x has length 1
# 3 otherwise
sw_cond <- function(x){
  min(length(x)+1,3)
}

noop<-function(...){invisible(...)}


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

# convert sample to colour
sample_to_color <- function(sample){
        a<-sample/max(abs(sample))
        z<-rep(0,length(a))
        lo<-hi<-z
        lo[a>0]<-a[a>0]
        hi[a<0]<- -a[a<0]
        r<-1-hi-hi
        r[r<0]<-0
        g<-1-hi-lo
        b<-hi
        rgb(r,g,b)

}

# turnout function generator 
a_fn_fn <- function(N,normalise=TRUE){
	if(normalise==TRUE){
		function(V) calculate_normalised_a(V,N)
	}else{
		function(V) calculate_a(V,N)
	}
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

# chisq test function
ballot_chisq_to_normal <- function(ballot,tag="",
  W_list=c("V", quorate_names(ballot))
){
  a_fn<-a_fn_fn(ballot[,"N"])
  c_fn <-function(n){
    W<-ballot[,n]
    if(sum(W)==0){
    	NA
    }else{
    	w<-density(a_fn(W))
   	 sum((w$y-dnorm(w$x))^2)
  	}
  }
  out<-ballot_sapply(ballot,W_list,c_fn)
  names(out)<-paste(tag,names(out))
  out
}

# sapply a function across a ballot
ballot_sapply<-function(ballot,v_list,r_fn){
  switch(sw_cond(v_list),{},{
    out<-r_fn(v_list[1])
    names(out)<-v_list
    out
  },{
    out<-sapply(v_list,r_fn)
    names(out)<-v_list
    out
  })
}

	# https://stat.ethz.ch/pipermail/r-help/2006-March/101023.html
interleave <- function(v1,v2){
  ord1<-2*1:length(v1)-1
  ord2<-2*1:length(v2)
  c(v1,v2)[order(c(ord1,ord2))]
}

# sum vote columns to provide W columns
compute_W<-function(ballot){
  cn <- colnames(ballot)
  id <- ballot[,name_column(cn)]
  if(length(id)==0|sum(cn=="V")==0|sum(cn=="N")==0){return()}
  V <- ballot[,"V"]
  if(sum(ballot[,"N"])==0){
    t<-grep("Turnout",colnames(ballot))
    N<-round(N<-V/ballot[,t])
  }else{
    N<-ballot[,"N"]
  }
  a_fn <- a_fn_fn(N)
  a <-a_fn(V)
  polled<-polled_names(ballot)
  out<-cbind(N,V,a)
  rownames(out)<-id
  colnames(out)<-c("N","V","a")
  switch(sw_cond(polled),{out},{
    W <- compute_W_single(a_fn,ballot[,grep(polled,cn)])
    if(length(dim(W))==2){
      colnames(W) <- interleave(paste("W_",polled,sep=""),paste("w_",polled,sep=""))
      out<-cbind(out,W)
    }
    out
  },{
    W <- foreach(p=polled,.combine=cbind)%do% {
      x<-compute_W_single(a_fn,ballot[,grep(p,cn)])
      if(length(dim(x))==2){
	colnames(x)<-interleave(paste("W_",p,sep=""),paste("w_",p,sep=""))
	x
      }
    }
    out<-cbind(out,W)
    out
  })
}

# helper function for above
compute_W_single <- function(a_fn,W){
  i <- min(3,1+length(unlist(W))%/%length(a_fn(0)))
  switch(i,{},{
    v <- a_fn(W)
    out <- cbind(W,v)
    out
  },{
    w<-rowSums(W)
    v<-a_fn(w)
    out<-cbind(w,v)
    out
  })
}

# find raw vote columns
polled_names <- function(ballot){
	all_n <- gsub("W_","",names(col_map_table))
	find_fn <- function(n){
	  i<-grep(n,colnames(ballot))
	  switch(sw_cond(i),{FALSE},{
	    if(sum(ballot[,i])!=0){TRUE}else{FALSE}
	  },{
	    if(sum(colSums(ballot[,i])!=0)>0){TRUE}else{FALSE}
	  })
	}
	mask <- sapply(all_n,find_fn)
	names(mask)[mask]
}

# return indices of suitable participants
# must poll non zero in at least half of reporting areas
quorate_index <- function(ballot){
  n <- paste("W_",polled_names(ballot),sep="")
  i <- foreach(ni=n,.combine=c)%do%{grep(ni,colnames(ballot))}
  switch(min(length(i)+1,3),{},{
    if(sum(ballot[,i]!=0)>=(dim(ballot)[1])/2){i}
  },{
    i[(colSums(ballot[,i]!=0)>=(dim(ballot)[1])/2)]
  })
}

# return names of suitable participants
quorate_names <- function(ballot){
  colnames(ballot)[quorate_index(ballot)]
}
ballot_totals <- function(b){
	i<-grep("(^numeric$|^integer$)",
		sapply(b[1,],class)
	)
	colSums(b[,i])
}

ballot_turnouts <- function(b){
	b2<-ballot_totals(b)
	i<-grep("^N$",names(b2))
	b2[-i]/b[i]
}