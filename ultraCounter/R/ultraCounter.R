integerPrecisionLimit <- 2^.Machine$double.digits - 1

#'ultraCounter
#'@description
#' Define a multidimensional parameters space, according to the sequence of vectors supplied as arguments.
#' These are treated as containing categorical variables.
#'@return
#' The ultraCounter object has features:
#' out$elems stores the original arguments,
#' out$colvals stores the value change by a +1 change in each column,
#' out$len caches the overall length, 
#' out$index(x) maps an integer in the index range to a corresponding list of parameters,
#' out$revIndex(...) maps a parameter set to an integer in the index range.
#' out$sample(k=1e3) select a uniformly random sample of k points.
#'@param ... vectors containing categorical variables
#'@export
ultraCounter <- function(...) {
    out <- list()
    class(out) <- c("ultraCounter", class(out))
    out$elems <- list(...)
    stopifnot(length(out$elems) > 0)
    out$lens <- sapply(out$elems, length)
    stopifnot(all(sapply(seq(length(out$elems)), function(x) {
        length(unique(out$elems[[x]])) == out$lens[x]
    })))
    out$len <- prod(out$lens)
    stopifnot(length(out) < integerPrecisionLimit)
    out$colvals <- sapply(seq(length(out$lens), 0), function(x) {
        prod(tail(out$lens, n = x))
    })
    out$index <- function(x, LAPPLYFUN = lapply) {
        if (length(x) > 1) 
            return(lapply(x, out$index, LAPPLYFUN = LAPPLYFUN))
        stopifnot(x%%1 == 0)
        stopifnot(x > 0 && x <= out$len)
        key <- ((x - 1)%%out$colvals[-length(out$colvals)])%/%out$colvals[-1] + 1
        LAPPLYFUN(seq_along(key), function(x) {
            out$elems[[x]][key[x]]
        })
    }
    out$revIndex <- function(...) {
        args <- list(...)
        stopifnot(length(args) == length(out$elems))
        stopifnot(all(sapply(seq_along(args), function(x) {
            args[[x]] %in% out$elems[[x]]
        })))
        sum(sapply(seq_along(args), function(x) {
            which.max(args[[x]] == out$elems[[x]]) - 1
        }) * out$colvals[-1]) + 1
    }
    out$sample <- function(k = 1000) {
        stopifnot(out$len <= .Machine$integer.max)
        out$index(sample.int(out$len, k))
    }
    out
}

#'@method format ultraCounter
#'@export
format.ultraCounter <- function(x, ...) {
    paste("ultraCounter object Length:", x$len)
}

#'@method print ultraCounter
#'@export
print.ultraCounter <- function(x, ...) {
    cat((format(x, ...)), "\n")
}

#'@method length ultraCounter
#'@export
length.ultraCounter <- function(x) {
    x$len
}


a <- rep(9, 5)
b <- lapply(a, seq, from = 0)
uc <- do.call(ultraCounter, b)
lapply(seq(5), uc$index, sapply)
lapply(seq(5) * 10, uc$index, sapply)
lapply(seq(5) * 100, uc$index, sapply)

a1 <- c("zero", "one", "two")
a2 <- seq(5) + 2

uc1 <- ultraCounter(a1, a2)
uc1$index(1, sapply)
sapply(uc1$index(1), typeof)

bit <- c(FALSE, TRUE)
uc2 <- do.call(ultraCounter, lapply(seq(.Machine$double.digits - 1), function(x) return(bit)))
uc2$index(seq(3), sapply)
uc2$index(length(uc2) + 1 - seq(3), sapply)
length(uc2)

uc3 <- ultraCounter(seq(0, 9), a1, bit, a1)
length(uc3)
uc3$index(1, sapply)
uc3$index(length(uc3), sapply)

a3 <- seq(0, 1, 0.05)
a4 <- seq(0, 5, 0.1)
uc4 <- ultraCounter(a3, a3, a4)
uc4
do.call(rbind,uc4$sample(10))

z1 <- uc4$index(1)
z2 <- uc4$index(length(uc4))
do.call(uc4$revIndex, z1)
do.call(uc4$revIndex, z2)

hexchar <- strsplit("0123456789ABCDEF", "")[[1]]
octchar <- seq(0, 7)

uc5 <- ultraCounter(hexchar, hexchar, hexchar, hexchar)
uc5$index(1)
uc5$index(uc5$len)
