require(moments)
require(iterators)
require(foreach)

analyse_sample<-function (s, k = 7) 
{
    n <- length(s)
    combnGen <- combnGG(n, k)
    chunks <- chunker(0, choose(n, k))
    lo <- chunks[1, ]
    ln <- chunks[2, ]
    chisq_table <- foreach(offset = lo, 
    	n = ln, 
    	.inorder = TRUE, 
    	.packages = "stats",
    	.combine = c
    ) %do% {
        if (n == 0) {
            return(vector())
        }
        print(c(offset, n))
        out <- foreach(x = icount(n),
        	.combine = c
        ) %do% {
            si <- s[combnGen(offset + x)]
            sn <- (si - mean(si))/sd(si)
            d <- density(sn)
            chisq <- sum((d$y - dnorm(d$x))^2)
            chisq
        }
        out
    }
    indices <- head(n = 20, order(chisq_table, decreasing = TRUE))
    out2 <- foreach(i = indices, .combine = rbind) %do% {
        si <- s[combnGen(i)]
        sn <- (si - mean(si))/sd(si)
        d <- density(sn)
        c(chisq = chisq_table[i], mean = mean(sn), sd = sd(sn), 
            skew = skewness(sn), kurt = kurtosis(sn), peak_x = d$x[which.max(d$y)], 
            peak_y = max(d$y))
    }
    rownames(out2) <- indices
    out3 <- do.call(rbind, 
    	lapply(indices, 
    		function(i) {
      	si <- s[combnGen(i)]
       (si - mean(si))/sd(si)
    		}
    	)
    )
    rownames(out3) <- indices
    out4 <- combnGen(indices)
    rownames(out4) <- indices
    list(sample = s, 
    	raw_combinations = out4, 
    	combinations = out3, 
    	report = out2
    )
}
chunker<-function (from, to, chunkSize=1e5) 
{
    d<-to-from
    if (d<3*chunkSize) {
        return(rbind(from, to))
    }
    
    n <- d%/%chunkSize
    if(d%%chunkSize==0){
    	n<-n-1
    }
    f1 <- round(c(from + chunkSize * seq(0, n)))
    t1 <- round(c(from + chunkSize * seq(1, n), to))
    out <- list(offset = f1, n=t1 - f1)
    out
}
