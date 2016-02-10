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
	t#a square table of logical values
){
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

#wrapper to check contiguity both of
#a region and its inverse
contiguty_check_wrapper <- function(
	 b,#ballot border table
	x#region vector
){
	 contiguity_check(b[x,x])&contiguity_check(b[-x,-x])
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
		.combine=cbind,
		.inorder=FALSE,
		.options.multicore=mcoptions
	)%dopar%{
		if(contiguity_check(border_table[r[,i],r[,i]])){
			ballot_chisq_to_normal(ballot[r[,i],])
		}else{
			vector()
		}
	}
	out
}

region_check <- function(
	ballot=compute_W(read.csv("SIR2014.csv")),
	border_table=read.table("ScottishCouncilBorders.tab")
){
	n<-ncol(border_table)
	a<-seq(5,n-1)
	a<-a[choose(n,a)*a<1e9]
	a<-a[order(choose(n,a)*a)]
	foreach(i=a,.combine=cbind)%do%{
		datafile<-paste("SIR2014_k",i,".tab",sep="")
		if(file.exists(datafile)){
			data<-read.table(datafile)
		}else{
			data<-(recursive_region_check(ballot,border_table,k=i))
			write.table(data,file=datafile)
		}
		out<-rowMeans(data)
		cat(file="contiguity.log",append=TRUE,
			paste(i,paste(out,collapse=" "),"\n"))
		beep(9)
		out
	}
}

parallel_combn <- function(
	x,#the integer uid of this combination
	n,#n choose k
	k
){
	if(k==1){return(x)}
	cnk<-choose(n-1,k-1)
	if(x<=cnk){
		out<-c(1,parallel_combn(x,n-1,k-1)+1)
	}else{
		out<-parallel_combn(x-cnk,n-1,k)+1
	}
	cat(paste(x,n,k,":",paste(out,collapse=" "),"\n"))
	out
}

test_parallel<-function(n,k){
	tgt<-combn(n,k)
	foreach(i=icount(ncol(tgt)))%do%{
		tgt[,i]==parallel_combn(i,n,k)
	}
}


