# setup cache
primesEnv <- new.env()
assign("cap", 1, envir = primesEnv)
assign("primes", vector(), envir = primesEnv)

precisionLimit <- 2^.Machine$double.digits - 1

#' getPrimes
#'@param x a 'numeric' integer describing the maximum desired prime.
#'@importFrom utils tail
#'@importFrom getLapply getChunkSize
#' @export
getPrimes <- function(x) {
    if (length(x) > 1) 
        return(lapply(x, getPrimes))
    stopifnot(x <= precisionLimit)
    stopifnot(x%%1 == 0)
    stopifnot(x > 0)
    cap <- get("cap", envir = primesEnv)
    primes <- get("primes", envir = primesEnv)
    capreq <- floor(sqrt(x))
    if (cap < capreq) {
        primes <- getPrimes(capreq)
        cap <- capreq
    }
    ch <- getLapply::getChunkSize()
    if (x - cap > ch) {
        j <- seq(cap, x, by = ch)
        primes <- lapply(j, getPrimes)[[length(j)]]
    }
    if (cap < x) {
        r <- setdiff(primeGen(cap, x), primes)
        primes <- c(primes, r)
        cat(paste("Extended cache from", cap, "to", x, "and found", length(r), "new primes ", 
            length(primes), "in cache\n"))
        cap <- x
        assign("primes", primes, envir = primesEnv)
        assign("cap", cap, envir = primesEnv)
    }
    primes[primes <= x]
}

#'primesN
#'@param x a 'numeric' indexing the primes cache
#'@export
primesN <- function(x) {
    if (length(x) > 1) 
        return(lapply(x, primesN))
    stopifnot(x%%1 == 0)
    stopifnot(x > 0)
    primes <- get("primes", envir = primesEnv)
    while (length(primes) < x) {
        d <- x - length(primes)
        primes <- getPrimes(get("cap", envir = primesEnv) + 20 * d)
    }
    primes[x]
}

#' @importFrom getLapply getSensibleThreads
chunker <- function(from, to) {
    no_cores <- getLapply::getSensibleThreads()
    if (no_cores == 1) {
        return(list(c(from, to)))
    }
    n <- ((to - from)/no_cores)
    f1 <- round(c(from + n * seq(0, no_cores - 1)))
    t1 <- round(c(from + n * seq(1, no_cores - 1), to))
    o <- cbind(f1[f1 != t1], t1[f1 != t1])
    lapply(seq(nrow(o)), function(x) o[x, ])
}

nonPrimeGen <- function(from, to) {
    function(n) {
        if (n^2 + n > to) {
            return(n^2)
        }
        fn <- from + n - from%%n
        if (fn < n^2) {
            fn <- n^2
        }
        tn <- to - to%%n
        if (tn < fn) {
            return(vector())
        }
        seq(fn, tn, by = n)
    }
}

#' @importFrom getLapply getLapply
#' @importFrom ultraCombo multiUnion
primeGenThread <- function(fromto) {
    from <- fromto[1]
    to <- fromto[2]
    if (to <= from) {
        return(vector())
    }
    fun <- nonPrimeGen(from, to)
    p <- getPrimes(floor(sqrt(to)))
    LAPPLYFUN <- getLapply::getLapply()
    np <- do.call(ultraCombo::multiUnion, LAPPLYFUN(p, fun))
    setdiff(seq(from + 1, to), np)
}

#' @importFrom getLapply getLapply
primeGen <- function(from, to) {
    # domain extender
    r <- chunker(from, to)
    a <- to - from
    cat(paste("from", from, "to", to, ":", a, "candidates... Running", length(r), 
        "jobs\n"))
    LAPPLYFUN <- getLapply::getLapply()
    out <- do.call(c, LAPPLYFUN(r, primeGenThread))
    b <- length(out)
    cat(paste(b, "found in", a, "candidates", sprintf("%0.2f%%", b/a * 100), "\n"))
    return(out)
}

#' factorise
#'@param x a 'numeric' integer describing a number to factorise.
#'@export
factorise <- function(x) {
    if (length(x) > 1) 
        return(lapply(x, factorise))
    stopifnot(x <= precisionLimit)
    stopifnot(x%%1 == 0)
    stopifnot(x > 0)
    p <- getPrimes(floor(sqrt(x)))
    p[(x%%p) == 0]
}
