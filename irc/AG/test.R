require(moments)
require(iterators)
require(foreach)
require(combnGen)
require(doParallel)
require(plyr)
require(bit)
require(superChoose)

# setup parallelisation parameters
no_cores <- max(1,detectCores()-1)
mcoptions <- list(preschedule=FALSE,
	set.seed=FALSE,
	silent=TRUE,
	cores=no_cores)

# make custom fork cluster
makeCustomCluster <- function(){
	cl<-makeForkCluster(nnodes=no_cores,nocompile=T)
	registerDoParallel(cl=cl,cores=no_cores)
	cl
}


analyse_sample<-function (s = rnorm(15), k = 7) 
{
    n <- length(s)
    combnGen <- combnGG(n, k)
    ch <- chunk(1,superChoose(n,k))
    cl<-makeCustomCluster()
    chisq_table <- foreach(
    	chunk<-ch, 
    	.inorder = TRUE, 
    	.packages = "stats",
    	.combine = c,
    	.options.multicore = mcoptions
    ) %dopar% {
    	n <- chunk[2]-chunk[1]+1
    	offset <- chunk[1]-1
        if (n == 0) {
            return(vector())
        }
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
    stopCluster(cl)
    registerDoSEQ()
    indices <- head(n = 20, 
    	order(chisq_table, 
    	decreasing = TRUE)
    )
    out2 <- foreach(i = indices, 
    	.combine = rbind
    ) %do% {
        si <- s[combnGen(i)]
        sn <- (si - mean(si))/sd(si)
        d <- density(sn,n=64)
        c(chisq = chisq_table[i], 
        	mean = mean(sn), 
        	sd = sd(sn), 
         skew = skewness(sn), 
         kurt = kurtosis(sn), 
         peak_x = d$x[which.max(d$y)], 
         peak_y = max(d$y)
        )
    }
    rownames(out2) <- indices
    out3 <- do.call(rbind, 
    	lapply(indices, 
    		function(i) {
      	si <- s[combnGen(i)]
      	si<-si[order(si)]
       (si - mean(si))/sd(si)
    		}
    	)
    )
    rownames(out3) <- indices
    out4 <- combnGen(indices)
    rownames(out4) <- indices
    out5<-count(as.vector(out4))
    out5<-out5[order(out5$freq,decreasing=TRUE),]
    list(sample = s, 
    	raw_combinations = out4,
    	raw_count = out5,
    	combinations = out3, 
    	report = out2
    )
}
