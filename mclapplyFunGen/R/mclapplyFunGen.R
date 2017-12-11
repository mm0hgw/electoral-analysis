#'mclapplyFunGen
#'@importFrom parallel mclapply
#'@importFrom get.lapply get.sensible.threads
#'@export
mclapplyFunGen <- function(mc.preschedule = TRUE, mc.set.seed = FALSE, mc.silent = TRUE, 
    mc.cores = get.lapply::get.sensible.threads() {
    function(...) {
        parallel::mclapply(..., mc.preschedule = mc.preschedule, mc.set.seed = mc.set.seed, 
            mc.silent = mc.silent, mc.cores = mc.cores)
    }
}

#'mclapplyFunGen
#'@aliases NULL
"_PACKAGE"
