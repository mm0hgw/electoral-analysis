require(foreach)
require(doParallel)
require(beepr)

# convert a binary vector to a binary string
bin_str <- function(x){
	out<-rep("0",length(x))
	out[x==TRUE]<-"1"
	paste(paste(out,collapse=""),"\n")
}

# check region set for contiguity
contiguity_check  <- function(
	x,
	table_file="ScottishCouncilBorders.tab"
){
	t <- read.table(table_file)[x,x]
	n <- ncol(t)
	rt <- rowSums(t)
	if(max(rt)==n){
		return(TRUE)
	}
	r<-as.vector(t[which.max(rt),])
	while(TRUE){
		et<-(t[r==FALSE,r==TRUE])
		if(length(dim(et))==0){ 
			if(sum(et)==0){
				return(FALSE)
			}else{
				return(TRUE)
			}
		}else{
			ret<-(rowSums(et)!=0)
			sret<-sum(ret)
			if(sret==0){
				return(FALSE)
			}
			if(sret==length(ret)){
				return(TRUE)
			}
			r[r==FALSE]<-ret
		}
	}
}

# recursive region check
recursive_region_check <- function(
	ballot=compute_W(read.csv("SIR2014.csv")),
	border_table=read.table("ScottishCouncilBorders.tab"),
	k=ncol(border_table)
){
	n<-ncol(border_table)
	r<-combn(n,k)
	out<-foreach(
		i=icount(ncol(r)),
		.combine=rbind,
		.inorder=FALSE,
		.options.multicore=mcoptions
	)%dopar%{
		if(contiguity_check(r[,i])){
			ballot_chisq_to_normal(ballot[r[,i],])
		}else{
			vector()
		}
	}
	beep(9)
	out
}
