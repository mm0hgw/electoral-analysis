#'@importFrom get.lapply set.lapply
.onLoad <- function(libname, pkgname){
	set.lapply(mclapplyFunGen())
}
