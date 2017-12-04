
if (!exists("primesEnv")) primesEnv <- new.env()

precisionLimit <- 2^.Machine$double.digits - 1

#' primes_list
#' @export
primes_list <- function(x) {
    stopifnot(x <= precisionLimit)
    stopifnot(length(x) == 1)
    stopifnot(x%%1 == 0)
    stopifnot(x > 0)
    if (!exists("cap", envir = primesEnv)) 
        cap <- 1 else cap <- get("cap", envir = primesEnv)
    if (!exists("primes", envir = primesEnv)) 
        primes <- vector("numeric") else primes <- get("primes", envir = primesEnv)
    capreq <- floor(sqrt(x))
    if (cap < capreq) {
        primes <- primes_list(capreq)
        cap <- capreq
    }
    ch <- get.lapply::get.chunkSize()
    if (x - cap > ch) {
        j <- seq(cap, x, by = ch)
        lapply(j, primes_list)
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

#' @importFrom get.lapply get.sensible.threads
chunker <- function(from, to) {
    no_cores <- get.lapply::get.sensible.threads()
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

#' @importFrom get.lapply get.lapply
#' @importFrom ultraCombo multiUnion
generator_worker <- function(fromto, p = primes_list(floor(sqrt(fromto[2])))) {
    from <- fromto[1]
    to <- fromto[2]
    if (to <= from) {
        return(vector())
    }
    fun <- non_prime_factory(from, to)
    p <- p[p <= floor(sqrt(to))]
    LAPPLYFUN <- get.lapply::get.lapply()
    np <- do.call(ultraCombo::multiUnion, LAPPLYFUN(p, fun))
    setdiff(seq(from + 1, to), np)
}

#' @importFrom get.lapply get.lapply
generator_controller <- function(from, to) {
    # domain extender
    pl <- primes_list(floor(sqrt(to)))
    r <- chunker(from, to)
    a <- to - from
    cat(paste("from", from, "to", to, ":", a, "candidates... Running", length(r), 
        "jobs\n"))
    LAPPLYFUN <- get.lapply::get.lapply()
    out <- do.call(c, LAPPLYFUN(r, generator_worker, pl))
    b <- length(out)
    cat(paste(b, "found in", a, "candidates", sprintf("%0.2f%%", b/a * 100), "\n"))
    return(out)
}

#' factorise
#'@export
factorise <- function(x) {
    stopifnot(length(x) == 1)
    stopifnot(x <= precisionLimit)
    stopifnot(x%%1 == 0)
    stopifnot(x > 0)
    p <- primes_list(floor(sqrt(x)))
    p[(x%%p) == 0]
}
