
library(foreach)

independence_test_list <- function(l,...){
	foreach(
		b=l,
		n=names(l)
	)%do%{
		independence_test(b,name=n,...)
	}
}

independence_test <- function(
	b,
	col="",
	n=100,
	name=""
){
	if(length(col)!=2){
		mask<-sapply(b[1,],is.numeric)
		mask2<-grep("(^N$|^V$)",colnames(b)[mask])
		mask3<-order(colSums(b[,mask][,-mask2]),decreasing=TRUE)
		col<-colnames(b)[mask][-mask2][head(mask3,n=2)]
	}
	a1<-calculate_a(b[,col[1]],b$N)
	a1m<-sum(b[,col[1]])/sum(b$N)
	a1sd<-custom_sd(a1,a1m)
	a2<-calculate_a(b[,col[2]],b$N)
	a2m<-sum(b[,col[2]])/sum(b$N)
	a2sd<-custom_sd(a2,a2m)
	a3<-calculate_a(b[,col[1]]+b[,col[2]],b$N)
	a4<-rnorm(length(a1),a1m,a1sd)+a2
	a5<-rnorm(length(a2),a2m,a2sd)+a1
	l<-list(a3,a4,a5,a1,a2)
	names(l)<-c(
		paste(col,collapse=" + "),
		paste("simulated",col[1],"+",col[2]),
		paste(col[1],"+","simulated",col[2]),
		col[1],
		col[2]
	)
	out<-cor(a1,a2)
	if(name!=""){
		main<-paste(
			"Independence test on",
			name,
			"over",
			n,
			"iterations correlation:",
			sprintf("%.3f",out)
		)
		names(l)<-paste(name,names(l))
		png_iphone6(paste(sep="","Ind",name))
	}else{
		main<-paste("Independence test over",
			n,
			"iterations correlation:",
			sprintf("%.3f",out)
		)
	}
	d<-lapply(l,density)
	chart(d,main=main)
	sapply(	FUN=function(x){
			lines(density(rnorm(length(a1),mean(a1),sd(a1))+a2),col=2)
			lines(density(rnorm(length(a2),mean(a2),sd(a2))+a1),col=3)
		},
		seq(n-1)
	)
	lines(d[[1]],lwd=3)		
	lines(d[[4]],lwd=3,col=4)
	lines(d[[5]],lwd=3,col=5)
	if(name!=""){
		dev.off()
	}
	return(out)
}
