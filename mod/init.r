#
#	Cluster setup
#

require(parallel)
require(doParallel)

no_cores <- detectCores()
mcoptions <- list(preschedule=F,set.seed=F,silent=T)

makeCustomCluster <- function(){
	cl<-makeForkCluster(nnodes=no_cores,nocompile=T)
	registerDoParallel(cl=cl,cores=no_cores)
	cl
}



#if(!exists("cl")){
#	cl<-makeCustomCluster()
#}
#registerDoSEQ()