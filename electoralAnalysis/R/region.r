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

list.tableFiles<-function(name="SIR2014"){
	out<-list.files(path="data/",pattern=paste(name,"_k",sep=""))
	out[-grep("_index",out)]
}

fastColFoo<- function(t,foo){
	out<-do.call(c,lapply(seq(ncol(t)),function(i)foo(t[,i])))
	names(out)<-colnames(t)
	out
}

fastRowFoo<- function(t,foo,.combine=c){
	out<-do.call(.combine,lapply(seq(nrow(t)),function(i)foo(t[,i])))
	names(out)<-colnames(t)
	out
}

mean_table<-function(name="SIR2014",fileList=paste("data/",list.tableFiles(name),sep="")){
	out<-foreach(l=fileList,.combine=rbind)%do%{
		logcat(paste("Reading",l),file="io.log")
		d<-read.table(l)
		logcat(paste("Read",l),file="io.log")
		n<-nrow(d)
		k<-as.numeric(gsub(".tab","",gsub(paste("data/",name,"_k",sep=""),"",l)))
		kn<-32
		i<-as.numeric(rownames(d)[n])
		p<-round(100*i/choose(kn,k),digits=3)
		o<-c(p=p,k=k,n=n,i=i,fastColFoo(d,mean))#,fastColFoo(d,sd))
		rm(d)
		gc()
		o
	}
	p<-out[,1]
	out<-out[,-1]
	rownames(out)<-p
	out
}

logcat<-function(obj,file){
	if(length(dim(obj))>1){
		lapply(seq(nrow(obj)),function(x)logcat(obj[x,],file=file))
	}else{
		if(is.numeric(obj)){
			obj<-round(digits=3,obj)
		}
		objstr<-paste(obj,collapse=" ")
		objstrn<-paste(Sys.time(),objstr,"\n")
		cat(objstrn,file=file,append=TRUE)
	}
}

plot_trend <- function(m=mean_table()){
	phone_png()
	x<-m[,1]
	ylim<-(range(m[,c(-1,-2,-3)]))
	plot(type="b",pch=4,x=x,y=(m[,4]),ylim=ylim,
		xlab="k",ylab="mean chisq to Gaussian")
	counter<-5
	while(counter<=ncol(m)){
		lines(type="b",pch=counter,x=x,y=(m[,counter]))
		counter<-counter+1
	}
	l<-colnames(m)[c(-1,-2,-3)]
	legend("topright",legend=l,pch=seq(4,length.out=length(l)))
	n<-32
	foreach(k=m[,1],x=m[,3])%do%{
		logcat(combnG(x,n,k),file="region.log")
	}
	logcat(100*as.vector(m[,2])/as.vector(m[,3]),file="region.log")
	logcat(rownames(m),file="region.log")
	dev.off()
	system2(stdout=NULL,"git","pull")
	system2(stdout=NULL,"git",c("add","Rplot001.png"))
	system2(stdout=NULL,"git",c("commit","-m","plot"))
	system2(stdout=NULL,wait=FALSE,"git","push")
	m
}

plot_trend_repeat <- function(){
	m<-mean_table()
	while(TRUE){
		plot_trend(m)
		logcat(m,file="region.log")
		oldtime<-getTime()
		while(nrow(n<-mean_table())==nrow(m)){
			plot_trend(n)
			write.table(n,file="readout.tab")
			logcat(n,file="region.log")
			
			newtime<-getTime()
			duration <- round( newtime - oldtime )
			logcat(((n-m)/duration)[,c(2,3)],file="region.log")
			oldtime<-newtime
			logcat(paste(duration,"seconds"),file="region.log")
			Sys.sleep(3600-getTime()%%3600)
			
		}
		m<-n
		beep(9)
	}
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
	datafile<-paste("data/",name,"_k",k,".tab",sep="")
	i<-1
	if(file.exists(datafile)){
		d<-do.call(rbind,(strsplit(ReadLastLines(datafile,1)," ")))
		i<-max(as.numeric(gsub("\"","",d[,1])))+1
	}else{
		cat(paste(gsub(",","",toString(W_list)),"\n",sep=""),file=datafile)
	}
	while(i<=cnk){
		j<-combnGen(i)
		if(contiguityCheck(
			border_table,
			j
		)==TRUE){
			l<-paste("\"",i,"\" ",
				gsub(",","",toString(
					ballot_chisq_to_normal(
						ballot[j,]
					))
				),"\n",sep=""
			)
			cat(file=datafile,append=TRUE,l)
		}
		i<-i+1
	}
	beep(9)
	vector()
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
		cores=no_cores
	)
	cl<-makeCustomCluster()
	foreach(i=a,.combine=c,.options.multicore=mcoptions)%dopar%{
		recursive_region_check(ballot,border_table,k=i,W_list,name)
	}
	stopCluster(cl)
}

fn001 <- function(b,x,k){
	if(length(x)>1){
		cl<-makeCustomCluster()
		out<-foreach(
			y=x,
			.combine=multi_union,
			.inorder=FALSE,
			.options.multicore=mcoptions
		)%dopar%{
			fn001(b,y,k)
		}
		stopCluster(cl)
		out<-as.vector(out[!duplicated(out)])
		out<-out[order(out)]
		names(out)<-NULL
		out
	}else{
		n<-dim(b)[1]
		com<-combnG(x,n,k)
		if(k==1){
			newElem<-setdiff(seq(n)[b[x,]==TRUE],x)
		}else{
			invcom<-setdiff(seq(n),com)
			newElem<-seq(n)[invcom][rowSums(b[invcom,com])!=0]
		}
		out<-do.call(rbind,lapply(seq(length(newElem)),function(x)com))
		out<-cbind(out,newElem)
		out<-revCombnG(out,n)
		names(out)<-NULL
		out
	}
}

multi_union<-function(...){
	out<-c(...)
	out[!duplicated(out)]
}

index_files <- function(name="SIR2014"){
	foo<-list.files(path="data/",pattern=name)
	foo<-foo[grep(name,foo)]
	foo<-foo[grep("index",foo)]
	paste("data/",foo,sep="")
}

fn002 <- function(name="SIR2014"){
	b<-read.table(paste("data/",name,"_borders.tab",sep=""))
	n<-ncol(b)
	l<-index_files(name)
	if((length(l)==1)&sum(l=="")==1){
		i<-2
		indices<-seq(n)
	}else{
		foo<-as.numeric(gsub("_index.tab","",gsub(paste("data/",name,"_k",sep=""),"",l)))
		i<-max(foo)+1
		indices<-read.table(l[which.max(foo)])[,1]
	}
	if(i==n)return()
	for(k in seq(i,n)){
		indices<-fn001(b,indices,k-1)
		indexfile<-paste("data/",name,"_k",k,"_index.tab",sep="")
		write.table(indices,file=indexfile)
	}
}

fn003 <- function(name="SIR2014"){
	l<-index_files(name)
	z<-compute_W(read.csv(paste("data/",name,".csv",sep="")))
	W_list<-quorate_index(z)
	n<-nrow(z)
	foreach(kfile=l)%dopar%{
		outfile<-gsub("_index","",ifile)
		k<-as.numeric(gsub("_index.tab","",gsub(paste("data/",name,"_k",sep=""),"",kfile)))
		combnGen<-combnGG(n,k)
		if(file.exists(outfile)){
			foo<-as.numeric(colnames(read.table(outfile)))
			bar<-read.table(ifile)[,1]
			x<-setdiff(bar,foo)
			rm(foo)
			rm(bar)
		}else{
			cat(paste(gsub(",","",toString(W_list)),"\n",sep=""),file=datafile)
			x<-read.table(ifile)[,1]
		}
		foreach(i=x)%do%{
			j<-
			out<-paste(
				i,
				" ",
				gsub(
					",",
					"",
					toString(
						ballot_chisq_to_normal(
							ballot[j,]
						)
					)
				),
				"\n",
				sep=""
			)
			cat(
				file=outfile,
				append=TRUE,
				out)
		}
	}
}

write_k_index<-function(name,k){
	n<-nrow(read.csv(paste("data/",name,".csv",sep="")))
	b<-read.table(paste("data/",name,"_borders.tab",sep=""))
	combnGen<-combnGG(n,k)
	outfile<-paste("data/",name,"_k",k,"_index.tab",sep="")
	tmpfile<-gsub("data","/tmp",outfile)
	lastfile<-paste("data/",name,"_k",k-1,"_index.tab",sep="")
	if(file.exists(outfile))return()
	if(!file.exists(lastfile)){
		return()
	}
	ii<-read.table(lastfile)[,1]
	cat("\"x\"\n",file=tmpfile)
	p<-1
	for(i in ii){
		j<-combnGen(i)
		if(k==1){
			newElem<-setdiff(seq(n)[b[j,]==TRUE],j)
		}else{
			newElem<-seq(n)[-j][rowSums(b[-j,j])!=0]
		}
		for(elem in newElem){
			out<-revCombnG(c(j,elem),n)
			cat(paste("\"",p,"\" ",out,"\n",sep=""),file=tmpfile,append=TRUE)
			p<-p+1
		}
	}
	foo<-read.table(tmpfile)[,1]
	foo<-foo[!duplicated(foo)]
	write.table(foo,file=outfile)
}

write_chisq<-function(name,k){
	ballot<-read.csv(paste("data/",name,".csv",sep=""))
	n<-nrow()
	combnGen<-combnGG(n,k)
	indexfile<-paste("data/",name,"_k",k,"_index.tab",sep="")
	outfile<-gsub("_index","",indexfile)
	i<-read.table(indexfile)[,1]
	if(file.exists(outfile)){
		j<-as.numeric(rownames(read.table(outfile)))
		i<-i[!(i %in% j)]
	}
	for(x in i){
		y<-combnGen(x)
		out<-paste(
			y,
			" ",
			gsub(
				",",
				"",
				toString(
					ballot_chisq_to_normal(
						ballot[y,]
					)
				)
			),
			"\n",
			sep=""
		)
		cat(
			file=outfile,
			append=TRUE,
			out
		)
	}
	return(TRUE)
}

handle_ballot <- function(name){
	n<-nrow(read.csv(paste("data/",name,".csv",sep="")))
	indexfile <- paste("data/",name,"_k1_index.tab",sep="")
	write.table(seq(n),file=indexfile)
	joblist<-vector()
	for(k in seq(n)){
		indexfile <- paste("data/",name,"_k",k,"_index.tab",sep="")
		if(!file.exists(indexfile)){
			write_k_index(name,k)
		}
		joblist<-c(joblist,mcparallel(write_chisq(name,k)))
	}
	lapply(joblist,mccollect)
}
	