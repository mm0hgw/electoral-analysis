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

#http://stackoverflow.com/questions/5596107/reading-the-last-n-lines-from-a-huge-text-file
ReadLastLines <- function(x,n,...){    
  con <- file(x)
  open(con)
  out <- scan(con,n,what="char(0)",sep="\n",quiet=TRUE,...)

  while(TRUE){
    tmp <- scan(con,1,what="char(0)",sep="\n",quiet=TRUE)
    if(length(tmp)==0) {close(con) ; break }
    out <- c(out[-1],tmp)
  }
  out
}

getTime <- function(){
	s <- Sys.time()
	H <- as.numeric(format(s,"%H"))
	M <- as.numeric(format(s,"%M"))
	S <- as.numeric(format(s,"%S"))

	((H*60)+M)*60+S
}

# recursive region check
recursive_region_check <- function(
	ballot=compute_W(read.csv("data/SIR2014.csv")),
	border_table=read.table("data/SIR2014_borders.tab"),
	k=ncol(border_table),
	W_list=c("V",quorate_names(ballot)),
	name="SIR2014"
){
	n<-ncol(border_table)
	combnGen<-combnGG(n,k)
	cnk<-choose(n,k)
	mcoptions <- list(preschedule=FALSE,
		set.seed=FALSE,
		silent=TRUE,
		cores=min(no_cores,length(W_list))
	)
	out<-foreach(W=W_list,.combine=c,.options.multicore=mcoptions)%dopar%{
		datafile<-paste("data/",name,"_k",k,"_",W,".tab",sep="")
		i<-1
		if(file.exists(datafile)){
			d<-do.call(rbind,(strsplit(ReadLastLines(datafile,2*no_cores)," ")))
			i<-max(as.numeric(gsub("\"","",d[,1])))+1
		}else{
			cat(paste("\"",W,"\"\n",sep=""),file=datafile)
		}
		from<-i-1
		while(i<=cnk){
			j<-combnGen(i+from)
			if(contiguityCheck(
				border_table,
				j
			)==TRUE){
				l<-paste("\"",i+from,"\" ",
					ballot_chisq_to_normal(
						ballot[j,],W_list=W
					),"\n",sep=""
				)
				cat(file=datafile,append=TRUE,l)
			}
			vector()
		}
		mean(unlist(read.table(datafile)))
	}
	names(out)<-W_list
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
	a<-a[order(choose(n,a),decreasing=TRUE)]
	mcoptions <- list(preschedule=TRUE,
		set.seed=FALSE,
		silent=TRUE,
		cores=min(max(no_cores%/%length(W_list)+1,1),no_cores)
	)
	foreach(i=a,.combine=c,.options.multicore=mcoptions)%do%{
		recursive_region_check(ballot,border_table,k=i,W_list,name)
	}
}

