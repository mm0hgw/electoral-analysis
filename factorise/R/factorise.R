
if (!exists("primesEnv")) primesEnv <- new.env()

precisionLimit <- 2^.Machine$double.digits - 1

#' getPrimes
#'@param x a 'numeric' integer describing the maximum desired prime.
#'@importFrom utils tail
#'@importFrom getLapply getChunkSize
#' @export
getPrimes <- function(x) {
    stopifnot(x <= precisionLimit)
    stopifnot(length(x) == 1)
    stopifnot(x%%1 == 0)
    stopifnot(x > 0)
    if (exists("cap", envir = primesEnv)) {
        cap <- get("cap", envir = primesEnv)
    } else {
        cap <- 1
    }
    if (exists("primes", envir = primesEnv)) {
        primes <- get("primes", envir = primesEnv)
    } else {
        primes <- vector("numeric")
    }
    capreq <- floor(sqrt(x))
    if (cap < capreq) {
        primes <- getPrimes(capreq)
        cap <- capreq
    }
    ch <- getLapply::getChunkSize()
    if (x - cap > ch) {
        j <- seq(cap, x, by = ch)
        lapply(j, getPrimes)
        cap <- tail(j, n = 1)
    }
    if (cap < x) {
        r <- setdiff(generator_controller(cap, x), primes)
        primes <- c(primes, r)
        cat(paste("Extended cache from", cap, "to", x, "and found", length(r), "new primes ", 
            length(primes), "in cache\n"))
        cap <- x
        assign("primes", primes, envir = primesEnv)
        assign("cap", cap, envir = primesEnv)
    }
    primes[primes <= x]  # assemble and return all 
}

primesN <- function(x) {
    if (!exists("primes", envir = primesEnv)) 
        stop("no primes cache")
    primes <- get("primes", envir = primesEnv)
    stopifnot(all(x <= length(primes)))
    stopifnot(all(x%%1 == 0))
    stopifnot(all(x > 0))
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

non_prime_factory <- function(from, to) {
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
generator_worker <- function(fromto, p = getPrimes(floor(sqrt(fromto[2])))) {
    from <- fromto[1]
    to <- fromto[2]
    if (to <= from) {
        return(vector())
    }
    fun <- non_prime_factory(from, to)
    p <- p[p <= floor(sqrt(to))]
    LAPPLYFUN <- getLapply::getLapply()
    np <- do.call(ultraCombo::multiUnion, LAPPLYFUN(p, fun))
    setdiff(seq(from + 1, to), np)
}

#' @importFrom getLapply getLapply
generator_controller <- function(from, to) {
    # domain extender
    pl <- getPrimes(floor(sqrt(to)))
    r <- chunker(from, to)
    a <- to - from
    cat(paste("from", from, "to", to, ":", a, "candidates... Running", length(r), 
        "jobs\n"))
    LAPPLYFUN <- getLapply::getLapply()
    out <- do.call(c, LAPPLYFUN(r, generator_worker, pl))
    b <- length(out)
    cat(paste(b, "found in", a, "candidates", sprintf("%0.2f%%", b/a * 100), "\n"))
    return(out)
}

#' factorise
#'@param x a 'numeric' integer describing a number to factorise.
#'@export
factorise <- function(x) {
    stopifnot(length(x) == 1)
    stopifnot(x <= precisionLimit)
    stopifnot(x%%1 == 0)
    stopifnot(x > 0)
    p <- getPrimes(floor(sqrt(x)))
    p[(x%%p) == 0]
}
