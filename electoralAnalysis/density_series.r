# setup data read options
require(moments)
require(foreach)

	# takes a single ballot
	# returns a list of FUN(SAMPLE_FUN()) results applied to the ballot

ballot_funk<-function(b,
	SAMPLE_FUN=calculate_normalised_a,
	FUN=density,
		# pull names from ballot
	i_names=colnames(b)[(colnames(b)!="N")&(sapply(b[1,],is.numeric))],
		# shift names to human readable
	o_names=gsub("^V$","overall",i_names)	
){
		# apply FUN to SAMPLE_FUN over named elements
	out<-lapply(i_names,
		function(n){
			FUN(SAMPLE_FUN(b[,n],b$N))
		}
	)
		# tag results
	names(out)<-paste(o_names,".turnout",sep="")
		# return
	out
}

ballot_list_funk<-function(b_list,
	SAMPLE_FUN=calculate_normalised_a,
	FUN=density
){
	foreach(b=b_list,
		n=names(b_list),
		.combine=c
	)%do%{
		out<-ballot_funk(b,
			SAMPLE_FUN=SAMPLE_FUN,
			FUN=FUN
		)
		names(out)<-paste(n,".",names(out),sep="")
		out
	}
}

metadata <- function(
	l
){
	foreach(b=lapply(l,metadata_b),
		n=names(l),
		.combine=rbind
	)%do%{
		rownames(b)<-paste(n,".",rownames(b),sep="")
		b
	}
}

metadata_b <- function(
	b
){
	out_d<-ballot_funk(b)
	
	out_logchisq <- unlist(
		lapply(
			out_d,
			logchisq_fn
		)
	)

	out_skewness <- unlist(
		ballot_funk(
			b,
			FUN=skewness
		)
	)

	out_sum<-unlist(ballot_funk(
		b,
		FUN=return,
		SAMPLE_FUN=function(V,N)sum(V)
	))

	out_a<-unlist(ballot_funk(
		b,
		FUN=return,
		SAMPLE_FUN=function(V,N)sum(V)/sum(N)
	))
	cbind(
		logchisq=out_logchisq,
		skewness=out_skewness,
		sum=out_sum,
		a=out_a
	)
}

logchisq_fn <- function(d){
	log(sum((d$y-dnorm(d$x))^2))
}

chisq_chart <- function(d=ballot_list_funk(SC_list),n=6,decreasing=TRUE){
	chart(d[head(order(sapply(d,logchisq_fn),decreasing=decreasing),n)],lwd=2)
	x<-seq(-4,4,0.05)
	lines(x,dnorm(x),lwd=2,col=8,lty=2)
}

scatter_chart <- function(m,...){
	a<-m[,"a"]
	lc<-m[,"logchisq"]
	na<-(a-mean(a))/sd(a)
	nlc<-(lc-mean(lc))/sd(lc)
	k<-head(order(na+nlc,decreasing=TRUE))
	plot(	x=lc[-k],
		y=a[-k],
		xlab="log chisq deviation from Gaussian",
		ylab="turnout",
		xlim=limits(lc),
		ylim=limits(a),
		col=8,
		pch=8,
		...
	)
	points(	x=lc[k],
		y=a[k],
		col=seq(length(k)),
		pch=seq(length(k)),
		...
	)
	legend(	"bottomleft",
		legend=rownames(m)[k],
		col=seq(length(k)),
		pch=seq(length(k))
	)
}
