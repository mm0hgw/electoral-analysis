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
}

fastColFoo<- function(t,foo){
	out<-do.call(c,lapply(seq(ncol(t)),function(i)foo(t[,i])))
	names(out)<-colnames(t)
	out
}

fastRowFoo<- function(t,foo,.combine=c){
	out<-do.call(.combine,lapply(seq(nrow(t)),function(i)foo(t[i,])))
	names(out)<-colnames(t)
	out
}

mean_table<-function(name="SIR2014",fileList=paste("data/",list.tableFiles(name),sep="")){
	ballot<-compute_W(
		read.csv(
			paste("data/",
				name,
				".csv",
				sep=""
	)	)	)
	kn<-nrow(ballot)
	out<-foreach(l=fileList,.combine=rbind)%do%{
		logcat(paste("Reading",l),file="io.log")
		d<-read.table.smart(l)
		logcat(paste("Read",l),file="io.log")
		n<-nrow(d)
		k<-as.numeric(gsub(".tab","",gsub(paste("data/",name,"_k",sep=""),"",l)))
		i<-as.numeric(rownames(d)[n])
		p<-round(100*i/choose(kn,k),digits=3)
		o<-c(p=p,k=k,n=n,i=i,fastColFoo(d,mean))#,fastColFoo(d,sd))
		rm(d)
		gc()
		o
	}
	p<-out[,1]
	names(p)<-sapply(l,fn003)
	print(p[p!=100.000])
	out<-out[,-1]
	out<-rbind(out,
		c(nrow(ballot),
			1,
			1,
			ballot_chisq_to_normal(ballot)
	)	)
	rownames(out)<-out[,1]
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
	dev.off()
	m
}

plot_trend_repeat <- function(name="SIR2014"){
	report_period<-900
	m<-mean_table(name)
	while(TRUE){
		plot_trend(m)
		logcat(m,file="region.log")
		oldtime<-getTime()
		while(nrow(n<-mean_table(name))==nrow(m)){
			starttime<-getTime()
			plot_trend(n)
			write.table(n,file="readout.tab")
			newtime<-getTime()
			duration <- round( newtime - oldtime )
			readtime <- round( newtime - starttime )
			o<-((n-m)/duration)[,c(2,3)]
			p<-cbind(n[,1],round(n[,3]*100/choose(32,n[,1]),digits=3),n[,c(-1,-2,-3)],o,o[,1]/o[,2])
			logcat(p,file="region.log")
			oldtime<-newtime
			logcat(paste(duration,"seconds"),file="region.log")
			m<-n
			Sys.sleep(report_period-(getTime()-readtime)%%report_period)
		}
		m<-n
		oldtime<-getTime()
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
	datafile<-paste("data/",name,"_k",sprintf("%2.0f",k),".tab",sep="")
	i<-1
	if(file.exists(datafile)){
		d<-system2("tail",c("-n1",datafile),stdout=TRUE)
		i<-max(as.numeric(sub("\".*","",sub("\"","",d))))+1
	}else{
		cat(paste(gsub(",","",toString(W_list)),"\n",sep=""),file=datafile)
	}
	buffer_limit<-160
	buffer<-vector()
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
			buffer<-c(buffer,l)
			if(length(buffer)>=buffer_limit){
				cat(paste(buffer,collapse=""),file=datafile,append=TRUE)
				buffer<-vector()
			}
		}
		i<-i+1
	}
	if(length(buffer)>0){
		cat(paste(buffer,collapse=""),file=datafile,append=TRUE)
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

fn001 <- function(datafile){
	d<-system2("tail",c("-n1",datafile),stdout=TRUE)
	max(as.numeric(sub("\".*","",sub("\"","",d))))
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
	n<-nrow(read.csv(paste("data/",name,".csv",sep="")))
	l<-paste("data/",list.tableFiles(name),sep="")
	p<-sapply(l,function(x)choose(n,fn003(x)))
	i<-sapply(l,fn001)
	out<-cbind(i,p,p-i,i/p)
	rownames(out)<- l
	out
}

fn003 <- function(datafile){
	as.numeric(sub(".tab","",sub(paste("data/.*_k",sep=""),"",datafile)))
}

fn004 <- function(name="SIR2014"){
	m<-fn002(name)
	startTime <- getTime()
	period_time <- 15
	while(TRUE){
		Sys.sleep(period_time-getTime()%%period_time)
		n<-fn002(name)
		mask<-m[,1]!=n[,1]
		endTime<-getTime()
		duration<-endTime-startTime
		out<-(duration*(1-n)/(n-m))[,4]
		names(out)<-list.tableFiles(name)
		print(Sys.time()+out[mask])
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

nlines <- function(file){
	as.numeric(sub(" .*","",system2("wc",c("-l",file),stdout=TRUE)))
}

max_thread_size<-1e5

read.table.smart<-function(file,nrow=nlines(file)){
	sample<-read.table(file,nrow=5)
	cc<-c("character",sapply(sample,class))
	cn<-colnames(sample)
	offset<-6
	out<-sample
	if(nrow-offset>max_thread_size){
		cl<-makeCustomCluster()
		nthreads<-(nrow-offset)%/%max_thread_size
		rnl<-rep((nrow-offset)%/%nthreads,nthreads)
		rnl[1]<-rnl[1]+(nrow-offset)%%nthreads
		rsl<-sapply(seq(nthreads)-1,
			function(x)sum(offset,head(rnl,n=x))
		)
		o<-foreach(rn=rnl,
			rs=rsl,
			.combine=rbind,
			.multicombine=TRUE,
			.options.multicore=mcoptions
		)%dopar%{
			o<-read.table(file,
				skip=rs,
				nrow=rn,
				comment.char="",
				colClasses=cc
			)
			rownames(o)<-o[,1]
			o<-o[,-1]
			colnames(o)<-cn
			o
		}
		out<-rbind(out,o)
		stopCluster(cl)
		rm(o)
		gc()
		out
	}else{
		o<-read.table(file,
			skip=offset,
			nrow=nrow-offset,
			comment.char="",
			colClasses=cc
		)
		rownames(o)<-o[,1]
		o<-o[,-1]
		colnames(o)<-cn
		out<-rbind(out,o)
		rm(o)
		gc()
		out
	}
}