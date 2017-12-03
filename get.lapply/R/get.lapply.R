#'get.lapply-package
#'@aliases NULL
#'@description
#'The get.lapply package.
"_PACKAGE"

get.lapply.env <- new.env()

#'get.lapply
#'@description
#'Get a lapply
#'@export
get.lapply <- function() {
    get("lapply", envir = get.lapply.env)
}

#'set.lapply
#'@description
#'Set the served lapply.
#'@param x a 'function'
#'@export
set.lapply <- function(x) {
    stopifnot(is.function(x))
    assign("lapply", x, envir = get.lapply.env)
}

#'get.seeded.lapply
#'@description
#'Get a lapply
#'@export
get.seeded.lapply <- function() {
    get("seeded.lapply", envir = get.lapply.env)
}

#'set.seeded.lapply
#'@description
#'Set the served lapply.
#'@param x a 'function'
#'@export
set.seeded.lapply <- function(x) {
    stopifnot(is.function(x))
    assign("seeded.lapply", x, envir = get.lapply.env)
}

#'get.chunkSize
#'@description
#'Get a chunkSize
#'@export
get.chunkSize <- function() {
    get("chunkSize", envir = get.lapply.env)
}

#'set.chunkSize
#'@description
#'Set the served chunkSize
#'@param x an 'integer'
#'@export
set.chunkSize <- function(x) {
    stopifnot(length(x) == 1)
    x <- as.integer(x)
    stopifnot(is.integer(x))
    assign("chunkSize", x, envir = get.lapply.env)
}

set.chunkSize(.Machine$integer.max)
set.lapply(lapply)
set.seeded.lapply(lapply)

#'get.sensible.threads
#'@export
get.sensible.threads <- function() {
    get("sensible.threads", envir = get.lapply.env)
}

#'set.sensible.threads
#'@param x a 'numeric' defining a sensible number of threads to run. Default : min(1,parallel::detectCores()-1)
#'@importfrom parallel detectCores
#'@export
set.sensible.threads <- function(x = min(1, parallel::detectCores() - 1)) {
    assign("sensible.threads", x)
}

if (!exists("sensible.threads", envir = get.lapply.env)) set.sensible.threads()
