# BFI :p

require(parallel)
require(doParallel)
require(foreach)

# setup parallelisation parameters
no_cores <- detectCores()-1
if(no_cores==0){no_cores<-1}
mcoptions <- list(preschedule=F,
	set.seed=F,
	silent=T,
	cores=no_cores)


# make custom fork cluster
makeCustomCluster <- function(){
	cl<-makeForkCluster(nnodes=no_cores,nocompile=T)
	registerDoParallel(cl=cl,cores=no_cores)
	cl
}

reset_cluster<-function(){
	if(exists("cl")){
		stopCluster(cl)
		rm("cl")
	}
	cl<<-makeCustomCluster()
}

# algorithm control constants
sieve_limit<-75037
chunk_size<-1e08
max_job_size<-chunk_size
single_thread_limit<-chunk_size

chunker <- function(from,to){		
	if(no_cores==1){
		return(rbind(from,to))
	}
	n<-((to-from)/no_cores)
	f1<-round(c(from+n*seq(0,no_cores-1)))
	t1<-round(c(from+n*seq(1,no_cores-1),to))
	rbind(f1[f1!=t1],t1[f1!=t1])
}

domain_divider<-function(d,s=no_cores){	#divide set d into n roughly equal parts
	key<-seq_along(d)%%s
		#ceiling(seq_along(d)/s)
	split(d,key)	
}


primes_cache_reset<-function(){  		# cache reset function
	primes_cache <<- vector()
	primes_cache_top <<-1
}

if(!exists("primes_cache")){			# assert primes_cache in environment
	primes_cache_reset()
}

primes_cache_stats<-function(){			# basic cache display
	c(top_prime=max(primes_cache), 
		no_primes=length(primes_cache),
		top=primes_cache_top)
}

time_display<-function(s){
	sprintf("%.0f:%2.0f:%2.2f",s%/%3600,s%/%60%%60,s%%60)
}

#' primes_list
#' @export
primes_list<-function(to){		# non prime finder
	if(to-primes_cache_top>max_job_size){
		j<-seq(primes_cache_top,to,by=max_job_size)
		lapply(j,primes_list)
	}
	if(primes_cache_top<to){
		r<-setdiff(generator_controller(primes_cache_top,to),primes_cache)
		primes_cache<<-c(primes_cache,r)
		cat(paste("Extended cache from",primes_cache_top,
			"to",to,"and found",length(r),"new primes ",
			length(primes_cache),"in cache\n"))
		primes_cache_top<<-to
	}
	primes_cache[primes_cache<=to]	# assemble and return all 
	if(length(primes_cache)!=0){
		i<-1
		l<-length(primes_cache)
		while(i<l&&primes_cache[i+1]<=to){
			i <- i+1
		}
		return(head(primes_cache,n=i))
	}
	vector()
}

generator_controller<-function(from,to){		# domain extender
	pl<-primes_list(floor(sqrt(to)))
	r<-chunker(from,to)
	a<-to-from
	cat(paste("from",from,"to",to,":",a,"candidates... Running",length(r[1,]),"jobs\n"))
	out<-foreach(x=r,
		.combine=c,
		.multicombine=TRUE
	)%dopar%{
		generator_worker(x[1],x[2],pl[pl<=floor(sqrt(x[2]))])
	}
	b<-length(out)
	cat(paste(b,"found in",a,"candidates",sprintf("%0.2f%%",b/a*100),"\n"))
	return(out)
}


non_prime_factory<-function(from,to){
	function(n){
		if(n^2+n>to){
			return(n^2)
		}
		fn<-from+n-from%%n
		if(fn<n^2){
			fn <- n^2
		}
		tn<-to-to%%n
		if(tn<fn){
			return(vector())
		}
		seq(fn,tn,by=n)
	}
}

generator_worker <- function(from,to,p=primes_list(floor(sqrt(to)))){
	if(to<=from){return(vector())}
	fun<-non_prime_factory(from,to)
	np<-foreach(f=p,
		.combine=multi_union,			# turn off prescheduler
		.multicombine=TRUE,		# keep data recovery simple
		.inorder=FALSE,
		.options.multicore=mcoptions
	)%do%{
		fun(f)
	}
	setdiff(seq(from+1,to),np)
}

multi_union<-function(...){
	x<-c(...)
	x[!duplicated(x)]
}

#' factorise
#' @export
factorise<-function(x){				# factoriser
	xsqrt<-floor(sqrt(x))			# find sqrt(x) (max possible prime factor)
	primes<-primes_list(xsqrt) 		# pull all required primes
	factors<-primes[(x%%primes)==0]		# discard all that don't factorise
	factors					# return
}

extend_list<-function(){
	primes_list(primes_cache_top+max_job_size)
}

if(!exists("cl")){
	reset_cluster()
}
