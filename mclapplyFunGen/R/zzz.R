#'@importFrom get.lapply set.lapply
.onLoad <- function(libname, pkgname){
	get.lapply::set.lapply(mclapplyFunGen())
	get.lapply::set.seeded.lapply(mclapplyFunGen(mc.set.seed=TRUE))
}
