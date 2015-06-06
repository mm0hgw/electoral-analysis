#
#	Cluster setup
#

require(parallel)
require(doParallel)

no_cores <- detectCores()
mcoptions <- list(preschedule=F,set.seed=F,silent=T,cores=no_cores)

makeCustomCluster <- function(){
	cl<-makeForkCluster(nnodes=no_cores,nocompile=T)
	registerDoParallel(cl=cl,cores=no_cores)
	cl
}
