#'@importFrom get.lapply set.lapply
.onLoad <- function(libname, pkgname) {
    getLapply::setLapply(mclapplyFunGen())
    getLapply::setSeededLapply(mclapplyFunGen(mc.set.seed = TRUE))
}
