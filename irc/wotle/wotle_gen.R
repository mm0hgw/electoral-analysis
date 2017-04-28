
wotle_gen <- function(
	readingN = 1e2
	rowN = 2e4
	readingSet = c('d','r')
){
	colN <- length(readingSet)*readingN
	colnames <- sapply(seq(readingN),function(x)paste(sep='','hero_',readingSet,'_',x))
	out1 <- as.data.frame(matrix(sample(c(TRUE,FALSE),rowN*colN,replace=TRUE),
			nrow=rowN,
			ncol=colN,
			dimnames=list(seq(rowN),colnames)
		)
	)
	out2 <- sample(c(0,1),rowN,replace=TRUE)
	out <- cbind(radiant_win=out2,out1)
	out
}
