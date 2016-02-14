require(foreach)
require(doParallel)
require(beepr)
require(combnGen)
require(contiguity)

# convert a binary vector to a binary string
bin_str <- function(x){
	out<-rep("0",length(x))
	out[x==TRUE]<-"1"
	paste(paste(out,collapse=""),"\n")
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
		if(contiguityCheck(
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

