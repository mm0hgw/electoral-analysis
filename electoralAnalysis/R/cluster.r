#
#	Cluster setup
#

require(parallel)
require(doParallel)

# setup parallelisation parameters
no_cores <- max(1,detectCores()-1)
mcoptions <- list(preschedule=TRUE,
	set.seed=FALSE,
	silent=TRUE,
	cores=no_cores)

# make custom fork cluster
makeCustomCluster <- function(){
	cl<-makeForkCluster(nnodes=no_cores,nocompile=T)
	registerDoParallel(cl=cl,cores=no_cores)
	cl
}

# switch to sequential processing
set_sequential <- function(){
	registerDoSEQ()
}

# switch to parallel processing
set_parallel <- function(){
	if(exists("cl")){
		registerDoParallel(cl=cl,cores=no_cores)
	}else{
		print("no cluster detected")
	}
}
