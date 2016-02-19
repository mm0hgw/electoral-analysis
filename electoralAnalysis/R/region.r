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
	list.files(path="data/",pattern=paste(name,"_k",sep=""))
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
		logcat(Sys.time(),file="io.log")
		logcat(paste("Reading",l),file="io.log")
		d<-read.table(l)
		logcat(Sys.time(),file="io.log")
		logcat(paste("Read",l),file="io.log")
		n<-nrow(d)
		k<-as.numeric(gsub(".tab","",gsub(paste("data/",name,"_k",sep=""),"",l)))
		kn<-32
		i<-as.numeric(rownames(d)[n])
		p<-round(i/choose(kn,k)*100000)/1000
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
		objstr<-paste(obj,collapse=" ")
		objstrn<-paste(objstr,"\n")
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
	logcat(Sys.time(),file="region.log")
	foreach(k=m[,1],x=m[,3])%do%{
		logcat(combnG(x,n,k),file="region.log")
	}
	logcat(as.vector(m[,2])/as.vector(m[,3]),file="region.log")
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
		oldtime<-Sys.time()
		while(nrow(n<-mean_table())==nrow(m)){
			plot_trend(n)
			logcat(n,file="region.log")
			logcat(n-m,file="region.log")
			newtime<-Sys.time()
			duration <- round( newtime - oldtime )
			oldtime<-newtime
			logcat(paste(duration,"seconds"),file="region.log")
			system2(stdout=NULL,"sleep",paste(duration*10))
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
	foreach(i=a,.combine=c,.options.multicore=mcoptions)%dopar%{
		recursive_region_check(ballot,border_table,k=i,W_list,name)
	}
}

