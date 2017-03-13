#'@importFrom get.lapply set.lapply
.onLoad <- function(libname, pkgname){
	get.lapply::set.lapply(mclapplyFunGen())
}
