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
	t #a square table of logical values
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

border_table <- function(csv_table){
	n<-nrow(csv_table)
	out<-matrix(FALSE,nrow=n,ncol=n)
	foreach(i=icount(n))%do%{
		out[i,i]<-TRUE
		out[csv_table[i,]==TRUE,i]<-TRUE
		out[i,csv_table[i,]==TRUE]<-TRUE
	}
	out
}

csv_table <- function(border_table){
	n<-nrow(border_table)
	ncol<-max(rowSums(border_table))
	out<-matrix(0,nrow=n,ncol=ncol)
	foreach(i=icount(n))%do%{
		j<-seq(n)[border_table[i,]==TRUE]
		out[i,seq(length(j))]<-j
	}
	out
}

chunk_size <-5e5

# recursive region check
recursive_region_check <- function(
	ballot=compute_W(read.csv("data/SIR2014.csv")),
	border_table=read.table("data/SIR2014_borders.tab"),
	k=ncol(border_table),
	W_list=c("V",quorate_names(ballot)),
	name="SIR2014"
){
	n<-ncol(border_table)
	combnLutGen<-combnLutGenGen(n,k)
	cnk<-choose(n,k)
	out<-foreach(W=W_list,.combine=c,.options.multicore=mcoptions)%dopar%{
		gc()
		datafile<-paste("data/",name,"_k",k,"_",W,".tab",sep="")
		if(file.exists(datafile)){
			data<-read.table(datafile)
			out<-mean(unlist(data))
			rm(data)
			cat(paste(datafile,": mean",out,"\n"))
			out
		}else{
			data<-recursive_region_check_loop_fn(
				combnLutGen,
				0,
				cnk,
				ballot,
				border_table,
				W
			)
			write.table(data,datafile)
			out<-mean(data)
			rm(data)
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
		.inorder=TRUE,
		.maxcombine=500,
		.options.multicore=mcoptions
	)%dopar%{
		if(contiguity_check_wrapper(
			border_table,
			combnLutGen(j+from)
		)==TRUE){
			ballot_chisq_to_normal(
				ballot[combnLutGen(j+from),],W_list=W)
		}else{vector()}
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
	a<-seq(2,n-1)
	a<-a[choose(n,a)*a<2^.Machine$double.digits-1]
	a<-a[order(choose(n,a))]
	foreach(i=a,.combine=c,.options.multicore=mcoptions)%dopar%{
		recursive_region_check(ballot,border_table,k=i,W_list,name)
	}
}

