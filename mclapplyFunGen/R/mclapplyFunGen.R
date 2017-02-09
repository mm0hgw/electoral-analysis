#'mclapplyFunGen
#'@importFrom parallel detectCores mclapply
#'@export
mclapplyFunGen <- function(
	mc.preschedule=TRUE,
	mc.set.seed=FALSE,
	mc.silent=TRUE,
	mc.cores=max(1,detectCores()-1)
){
	function(...){
		mclapply(...,
			mc.preschedule=mc.preschedule,
			mc.set.seed=mc.set.seed,
			mc.silent=mc.silent,
			mc.cores=mc.cores
		)
	}
}

#'mclapplyFunGen
#'@aliases NULL
"_PACKAGE"
