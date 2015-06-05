#
#	Cluster setup
#

require(parallel)
require(doMC)

registerDoMC(cores=detectCores(),nocompile=TRUE)
