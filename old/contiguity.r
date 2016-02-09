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
	foreach(i=icount(ncol(x)),.combine=c)%do%{
		y<-x[,i]
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
	t <- read.table(table_file)
	n <- ncol(t)
	target <- rep(FALSE,n)
	target[x] <- TRUE
	p<-t[which.max(rowSums(t[x,target])),]&target
	cat(bin_str(target))
	cat(bin_str(p))
	while(sum(col_or(t[p,])&target==target)<n){
		o<-(col_or(t[p,]))&target
		if(sum((o==p))==n){
			return(FALSE)
		}
		p<-o
		cat(bin_str(p))
	}
	return(sum(col_or(t[p,])&target==target)==n)
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
