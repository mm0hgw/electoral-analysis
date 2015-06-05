#
#	Cluster setup
#

require(parallel)
require(doParallel)

makeCustomCluster <- function(){
	cl<-makeForkCluster(nnodes=detectCores(),nocompile=T)
	registerDoParallel(cl=cl,cores=detectCores())
	cl
}

#if(!exists("cl”)){
#	cl<-makeCustomCluster()
#}
registerDoSEQ()