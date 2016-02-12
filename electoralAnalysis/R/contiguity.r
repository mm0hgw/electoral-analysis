require(foreach)
require(doParallel)
require(beepr)
require(combnGen)

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
contiguity_check_wrapper <- function(
	 b,#ballot border table
	x#region vector
){
	 contiguity_check(b[x,x])
}

chunk_size <-5e5

# recursive region check
recursive_region_check <- function(
	ballot=compute_W(read.csv("data/SIR2014.csv")),
	border_table=read.table("data/SIR2014_borders.tab"),
	k=ncol(border_table),
	W_list=c("V",quorate_names(ballot))
){
	n<-ncol(border_table)
	combnLutGen<-combnLutGenGen(n,k)
	cnk<-choose(n,k)
	out<-foreach(W=W_list,.combine=rbind)%do%{
		datafile<-paste("data/",name,"_k",i,"_",W,".tab",sep="")
		if(file.exists(datafile)){
			read.table(datafile)
		}else{
			out<-recursive_region_check_loop_fn(
				combnLutGen,
				0,
				cnk,
				ballot,
				border_table,
				W
			)
			write.table(out,datafile)
			cat(file="contiguity.log",append=TRUE,
				paste(datafile,"added to cache"))
			beep(9)
			out
		}
	}
	names(out)<-W_list
	out
}

recursive_region_check_loop_fn <- function(
	combnLutGen,
	from,
	length.out,
	ballot,
	border_table,
	W
){
	out<-foreach(
		j=icount(length.out),
		.combine=c,
		.inorder=FALSE,
		.maxcombine=500,
		.options.multicore=mcoptions
	)%dopar%{
		i<-combnLutGen(j+from)
		if(contiguity_check_wrapper(border_table,i)){
			ballot_chisq_to_normal(ballot[i,],W)
		}else{
			vector()
		}
	}
	out
}

# check region
region_check <- function(
	name="SIR2014",
	ballot=compute_W(read.csv(paste("data/",name,".csv",sep=""))),
	border_table=read.table(paste("data/",name,"_borders.tab",sep="")),
	W_list=c("V",quorate_names(ballot))
){
	n<-ncol(border_table)
	a<-seq(5,n-1)
	#a<-a[choose(n,a)*a<1e9]
	a<-a[order(choose(n,a))]
	foreach(i=a,.combine=c)%do%{
		data<-(recursive_region_check(ballot,border_table,k=i,W_list))
		out<-foreach(i=icount(ncol(data)),
			.combine=c
		)%do%{
			mean(data[i,])
		}
		names(out)<-rownames(data)
		out
	}
}

