require(foreach)
require(doParallel)

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
	target <- rep(FALSE,ncol(t))
	target[x] <- TRUE
	p<-t[which.max(rowSums(t[x,target])),]&target
	while(sum(col_or(t[p,])&target==target)>0){
		o<-(col_or(t[p,]))&target
		if(sum((o==p))==0){
			return(FALSE)
		}
		p<-o
	}
	return(sum(col_or(t[p,])&target!=target)==0)
}

# recursive region check
recursive_region_check <- function(
	ballot,
	vname="W_No",
	border_table=read.table("ScottishCouncilBorders.tab"),
	k=ncol(border_table)
){
	n<-ncol(border_table)
	foreach(
		i=combn(n,k),
		.combine=rbind,
		.inorder=FALSE,
		.options.multicore=mcoptions
	)%do%{
		if(contiguity_check(i)==FALSE){return(vector())}
		print(as.vector(i))
	}
}
