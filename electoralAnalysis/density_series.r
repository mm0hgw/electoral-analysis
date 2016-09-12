# setup data read options
require(moments)

SC_GE_fn <- function(b){
	list(overall.turnout=b$V,
		SNP.turnout=b$SNP,
		nonSNP.turnout=b$V-b$SNP
	)
}

SC_GE_tag_fn <- function(GE,name,FUN){
	f<-a_fn_fn(GE$N)
	out<-lapply(
		SC_GE_fn(GE),
		function(b){FUN(f(b))}
	)
	names(out) <- gsub("^",paste(name,".",sep=""),names(out))
	out
}

SC_GE_list_fn <- function(SC_GE_list,FUN=density){
	foreach(GE=SC_GE_list,n=names(SC_GE_list),.combine=c)%do%{
		out<-SC_GE_tag_fn(GE,n,FUN)
	}
}

SC_GE_list <- list(GE2005S=GE2005S,
	GE2010S=GE2010S,
	GE2015S=GE2015S,
	SP2007=SP2007,
	SP2011=SP2011,
	SP2016=SP2016
)

SC_GE_skewness <- unlist(SC_GE_list_fn(SC_GE_list,FUN=skewness))

S_fn <- a_fn_fn(SIR2014$N)

SIR_fn <-function(FUN){ list(SIR2014.overall.turnout=FUN(S_fn(SIR2014$V)),
	SIR2014.Yes.turnout=FUN(S_fn(SIR2014$Yes)),
	SIR2014.No.turnout=FUN(S_fn(SIR2014$No))
)}

SIR_sum<-c(SIR2014.overall.turnout=sum(SIR2014$V),
	SIR2014.Yes.turnout=sum(SIR2014$Yes),
	SIR2014.No.turnout=sum(SIR2014$No)
)

SIR_a<-c(SIR2014.overall.turnout=sum(SIR2014$V)/sum(SIR2014$N),
	SIR2014.Yes.turnout=sum(SIR2014$Yes)/sum(SIR2014$N),
	SIR2014.No.turnout=sum(SIR2014$No)/sum(SIR2014$N)
)


SIR<-SIR_fn(density)
SIR_skewness<-SIR_fn(skewness)

SC_GE<-SC_GE_list_fn(SC_GE_list)

SC_d_list<-c(SIR,SC_GE)
SC_d_skewness<-unlist(c(SIR_skewness,SC_GE_skewness))

chisq_fn <- function(d){
	sum((d$y-dnorm(d$x))^2)
}

SC_d_chisq<-unlist(lapply(SC_d_list,chisq_fn))

SC_sum_fn<-function(GE){
	out<-colSums(GE[,c("V","SNP")])
	out<-c(out,out["V"]-out["SNP"])
	names(out)[3]<-"nonSNP"
	out
}

SC_GE_sum<-foreach(d=lapply(SC_GE_list,SC_sum_fn),n=names(SC_GE_list),.combine=c)%do%{ 
	out<-d
	names(out)<-gsub("^",paste(n,".",sep=""),names(out))
	out
}

SC_GE_a_fn<-function(GE){
	sum_fn(GE)/sum(GE[,"N"])
}

SC_GE_a<-foreach(d=lapply(SC_GE_list,SC_GE_a_fn),n=names(SC_GE_list),.combine=c)%do%{ 
	out<-d
	names(out)<-gsub("^",paste(n,".",sep=""),names(out))
	out
}

SC_d_sum<-c(SIR_sum,SC_GE_sum)
SC_d_a<-c(SIR_a,SC_GE_a)

chisq_chart <- function(d=SC_d_list,n=6,decreasing=TRUE){
	chart(d[head(order(sapply(d,chisq_fn),decreasing=decreasing),n)],lwd=5)
}

# UK level density
UK_GE_list <- list(GE2005=GE2005,
	GE2010=GE2010,
	GE2015=GE2015
)

