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
	out<-do.call(c,lapply(seq(ncol(t)),function(i)foo(t[!is.na(t[,i]),i])))
	names(out)<-colnames(t)
	out
}

fastRowFoo<- function(t,foo,.combine=c){
	out<-do.call(.combine,lapply(seq(nrow(t)),function(i)foo(t[i,!is.na(t[i,])])))
	names(out)<-colnames(t)
	out
}

mean_table<-function(
	name="SIR2014",
	fileList=system2(
		"ls",
		c("-S",
			paste("data/",
				name,
				"_k*.tab",
				sep=""
			)
		),
		stdout=TRUE
	),
	ballot=compute_W(
		read.csv(
			paste("data/",
				name,
				".csv",
				sep=""
			)
		)
	)
){
	cl<-makeCustomCluster()
	cn<-strsplit(gsub("\"","",readline(fileList[1],1))," ")[[1]]
	out<-foreach(l=fileList,.combine=rbind)%dopar%{
		logcat(paste("Reading",l),file="io.log")
		con<-file(l)
		scan(con,1,what="char(0)",quiet=TRUE)
		o<-0
		counter<-0
		while(
			length(
				line<-scan(con,
					1,
					what="char(0)",
					quiet=TRUE
				)
			)>0
		){
			counter<-counter+1
			o<-o+log(
				as.numeric(
					strsplit(
						line,
						" "
					)[[1]][-1]
				)
			)
		}
		close(con)
		logcat(paste("Read",l),file="io.log")
		beep(11)
		o/counter
	}
	out<-rbind(out,
		log(ballot_chisq_to_normal(ballot))
	)
	k<-fn003(fileList)
	rownames(out)<-c(k,nrow(ballot))
	colnames(out)<-cn
	stopCluster(cl)
	beep(3)
	out[order(k),]
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

plot_trend <- function(name="SIR2014",m=mean_table(name)){
	file<-paste(name,".png",sep="")
	phone_png(file)
	x<-as.numeric(rownames(m))
	ylim<-(range(m))
	plot(type="b",pch=1,x=x,y=(m[,1]),ylim=ylim,
		xlab="k",ylab="mean log chisq to Gaussian")
	counter<-2
	while(counter<=ncol(m)){
		lines(type="b",pch=counter,x=x,y=(m[,counter]))
		counter<-counter+1
	}
	l<-colnames(m)
	legend("topright",legend=l,pch=seq(from=1,length.out=length(l)))
	require(buildPackage)
	dev.off()
	gitPush(file,paste(name,"plot",sep="_"))
	m
}

plot_trend_repeat <- function(name="SIR2014"){
	report_period<-3600
	while(TRUE){
		plot_trend(name)
		logcat(m,file="region.log")
		oldtime<-getTime()
		while(nrow(n<-mean_table(name))==nrow(m)){
			starttime<-getTime()
			plot_trend(name)
			write.table(n,file=paste(name,"_mean_table.tab",sep=""))
			newtime<-getTime()
			duration <- round( newtime - oldtime )
			readtime <- round( newtime - starttime )
			o<-((n-m)/duration)[,c(2,3)]
			p<-cbind(n[,1],round(n[,3]*100/choose(32,n[,1]),digits=3),n[,c(-1,-2,-3)],o,o[,1]/o[,2])
			logcat(p,file="region.log")
			oldtime<-newtime
			logcat(paste(duration,"seconds"),file="region.log")
			m<-n
			system2("./pushplot.sh",wait=FALSE)
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
	datafile<-paste("data/",name,"_k",sprintf("%02.0f",k),".tab",sep="")
	i<-1
	if(file.exists(datafile)){
		d<-system2("tail",c("-n1",datafile),stdout=TRUE)
		i<-max(as.numeric(sub("\".*","",sub("\"","",d))))+1
	}else{
		cat(paste(gsub(",","",toString(W_list)),"\n",sep=""),file=datafile)
	}
	buffer_limit<-1e3
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
						ballot[j,],
						W_list=W_list
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
	beep(11)
	vector()
}

# check region
region_check <- function(
	name="SIR2014",
	ballot=compute_W(read.csv(paste("data/",name,".csv",sep=""))),
	border_table=borderTable(read.csv(paste("data/",name,"_borders.csv",sep=""))),
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
	plot_trend(name)
	beep(3)
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
	as.numeric(gsub(".tab","",gsub(paste("data/.*_k",sep=""),"",datafile)))
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
		print(fn002())
		print(Sys.time()+out[mask])
	}
}

nlines <- function(file){
	as.numeric(sub(" .*","",system2("wc",c("-l",file),stdout=TRUE)))
}

readline<-function(file,n){
	system2("head",
		c(
			paste("-n",
				n,
				sep=""
			),
			file,
			"|",
			"tail",
			"-n1"
		),
		stdout=TRUE
	)
}

max_thread_size<-1e4

read.table.smart<-function(file,nrow=nlines(file)){
	sample<-read.table(file,nrow=5)
	cc<-c("character",sapply(sample,class))
	cn<-colnames(sample)
	offset<-6
	out<-sample
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