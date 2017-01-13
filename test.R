function (s, k = 7) 
{
    n <- length(s)
    combnGen <- combnGG(n, k)
    chunks <- chunker(0, choose(n, k))
    lo <- chunks[1, ]
    ln <- chunks[2, ]
    chisq_table <- foreach(offset = lo, n = ln, .inorder = TRUE, 
        .packages = "stats", .combine = c) %do% {
        if (n == 0) {
            return(vector())
        }
        print(c(offset, n))
        out <- foreach(x = icount(n), .combine = c) %do% {
            si <- s[combnGen(offset + x)]
            sn <- (si - mean(si))/sd(si)
            d <- density(sn)
            chisq <- sum((d$y - dnorm(d$x))^2)
            chisq
        }
        out
    }
    print(order(chisq_table))
    indices <- head(n = 20, order(chisq_table, decreasing = TRUE))
    print(indices)
    out2 <- foreach(i = indices, .combine = rbind) %do% {
        si <- s[combnGen(i)]
        sn <- (si - mean(si))/sd(si)
        d <- density(sn)
        c(chisq = chisq_table[i], mean = mean(sn), sd = sd(sn), 
            skew = skewness(sn), kurt = kurtosis(sn), peak_x = d$x[which.max(d$y)], 
            peak_y = max(d$y))
    }
    rownames(out2) <- indices
    print(out2)
    out3 <- do.call(rbind, lapply(indices, function(i) {
        si <- s[combnGen(i)]
        (si - mean(si))/sd(si)
    }))
    rownames(out3) <- indices
    print(out3)
    out4 <- combnGen(indices)
    rownames(out4) <- indices
    print(out4)
    list(sample = s, raw_combinations = out4, combinations = out3, 
        report = out2)
}
function (from, to) 
{
    if (no_cores == 1) {
        return(rbind(from, to))
    }
    n <- ((to - from)/no_cores)
    f1 <- round(c(from + n * seq(0, no_cores - 1)))
    t1 <- round(c(from + n * seq(1, no_cores - 1), to))
    out <- matrix(data = c(f1, t1 - f1), nrow = 2, byrow = TRUE)
    out
}
