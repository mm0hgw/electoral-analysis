#'@importFrom get.lapply set.lapply
.onLoad <- function(){
	set.lapply(mclapplyFunGen())
}
