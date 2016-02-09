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
	print(t)
	n <- ncol(t)
	target <- rep(TRUE,n)
	p<-t[which.max(rowSums(t)),]
	cat(bin_str(target))
	cat(bin_str(p))
	
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
