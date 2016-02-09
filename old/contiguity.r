require(foreach)
require(doParallel)
require(beepr)

# convert a binary vector to a binary string
bin_str <- function(x){
	out<-rep("0",length(x))
	out[x==TRUE]<-"1"
	paste(paste(out,collapse=""),"\n")
}

#logical or columns
col_or <- function(x){
	if(length(nrow(x))==0){
		return(x)
	}
	foreach(i=icount(ncol(x)),.combine=c)%do%{
		y<-x[,i]
		cat(bin_str(y))
		while(length(y)>1){
			y<-c(y[1]|y[2],y[c(-1,-2)])
		}
		y
	}
}

# check region set for contiguity
contiguity_check  <- function(
	x,
	table_file="ScottishCouncilBorders.tab"
){
	t <- read.table(table_file)[x,x]
	n <- ncol(t)
	rt<-rowSums(t)
	if(max(rt)==n){return(TRUE)}
	r<-as.vector(t[which.max(rt),])
	while(TRUE){
		et<-(t[r==FALSE,r==TRUE])
		if(length(dim(et))==0){
			if(sum(et)==0){return(FALSE)}else{return(TRUE)}
		}else{
			ret<-(rowSums(et)!=0)
			sret<-sum(ret)
			if(sret==0){return(FALSE)}
			if(sret==length(ret)){return(TRUE)}
			cat(bin_str(r))
			r[r==FALSE]<-ret
		}
	}
}

# recursive region check
recursive_region_check <- function(
	ballot,
	vname="W_No",
	border_table=read.table("ScottishCouncilBorders.tab"),
	k=ncol(border_table)
){
	n<-ncol(border_table)
	out<-foreach(
		i=combn(n,k),
		.combine=rbind,
		.inorder=FALSE,
		.options.multicore=mcoptions
	)%do%{
		if(contiguity_check(i)==FALSE){return(vector())}
		print(as.vector(i))
	}
	beep(9)
	out
}
