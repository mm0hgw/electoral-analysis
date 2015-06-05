#
#	Cluster setup
#

require(parallel)
require(doParallel)

#registerDoMC(cores=detectCores()/2,nocompile=TRUE)

makeCustomCluster <- function(){
	cl<-makeForkCluster(nnodes=detectCores(),nocompile=T)
	registerDoParallel(cl=cl,cores=detectCores())
	cl
}

if(!exists("cl")){
	cl<-makeCustomCluster()
}
