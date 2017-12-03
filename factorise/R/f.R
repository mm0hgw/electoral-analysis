
parallelEnv <- new.env()

get.sensible.threads <- function(){
	get('sensible.threads',env=parallelEnv)
}

#' @importfrom parallel detectCores
set.sensible.threads <- function(x=min(1,parallel::detectCores()-1)){
	assign('sensible.threads',x)
	if(file.exists(x)){
		load(x,env=primesEnv)
	}
}

if(!exists('sensible.threads',env=parallelEnv)) set.sensible.threads()

primes_list <- function(x,cacheFile='~/primes.rda'){
	if(file.exists(cacheFile)) load(cacheFile)
	if(!exists(cap)) cap <- 1
	capreq <- floor(sqrt(x))
	if(cap<capreq){
		primes_list(capreq,cacheFile)
		load(cacheFile)
	}
	
}

chunker <- function(from,to){
	no_cores <- min(get.sensible.threads(),to-from)
	if(no_cores==1){
		return(list(c(from,to)))
	}
	n<-((to-from)/no_cores)
	f1<-round(c(from+n*seq(0,no_cores-1)))
	t1<-round(c(from+n*seq(1,no_cores-1),to))
	list(f1[f1!=t1],t1[f1!=t1])
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

#' @importFrom get.lapply get.lapply
generator_worker <- function(fromto,p=primes_list(floor(sqrt(fromto[2])))){
	from <- fromto[1]
	to <- fromto[2]
	if(to<=from){return(vector())}
	fun <- non_prime_factory(from,to)
	LAPPLYFUN <- get.lapply::get.lapply()
	np <- do.call(multi_union,LAPPLYFUN(p[p<=floor(sqrt(fromto[2]))],fun))
	setdiff(seq(from+1,to),np)
}

#' @importFrom get.lapply get.lapply
generator_controller<-function(from,to){		# domain extender
	pl<-primes_list(floor(sqrt(to)))
	r<-chunker(from,to)
	a<-to-from
	cat(paste("from",from,"to",to,":",a,"candidates... Running",length(r[1,]),"jobs\n"))
	LAPPLYFUN <- get.lapply::get.lapply()
	out <- do.call(c,LAPPLYFUN(r,generator_worker,pl))
	b<-length(out)
	cat(paste(b,"found in",a,"candidates",sprintf("%0.2f%%",b/a*100),"\n"))
	return(out)
}

