require(moments)
require(foreach)
require(rgl)
require(beepr)
require(iterators)

# condition for switch functions
# 1 if x has length 0
# 2 if x has length 1
# 3 otherwise
sw_cond <- function(x){
  min(length(x)+1,3)
}

W2w<-function(x){gsub("W_","w_",x)}

cdf_mean_intercept<-function(V,N){sum((V/N)<(sum(V)/sum(N)))/length(N)}

a_fn_fn <- function(N){
  function(W) calculate_normalised_a(W,N)
}

grep_fn_fn <- function(x){
  function(pattern){
    grep(pattern,x)
  }
}

ballot_gen_fn <- function(n,mean,sd){
  function(){
    rnorm(n,mean,sd)
  }
}

pull_source <- function(){
	system2("git","pull")
	source("density_scatter.r")
}

col_or <- function(x){
	foreach(i=icount(ncol(x)),.combine=c)%do%{
		y<-x[,i]
		while(length(y)>1){
			y<-c(y[1]|y[2],y[c(-1,-2)])
		}
		y
	}
}

contiguous_check  <- function(
	x,
	table_file="ScottishCouncilBorders.tab"
){
	t <- read.table(table_file)
	target <- rep(FALSE,ncol(t))
	target[x] <- TRUE
	p<-t[x[1],]&target
	while(sum(xor(col_or(t[p,])&target,target))>0){
		o<-(p|col_or(t[p,]))&target
		print(cbind(o,p))
		if(sum(xor(o,p))==0){
			return(FALSE)
		}
		p<-o
	}
	print(p)
	print (target)
	return(sum(col_or(t[p,])&target!=target)==0)
}

ballot_unstuffer_2 <- function(table){
  yes <- 1617989
  N <- (table[,2])
  a_fn <- a_fn_fn(N)
  Np <- (table[,4])
  Nnp <- (table[,6])
  no <- table[,1]
  p <- table[,3]
  np <- table[,5]
  org<-cbind(no,p,np)
  Ntab<-cbind(N,Np,Nnp)
  profile_fn<-function(no,p,np){
    sum(quick_chisq(calculate_normalised_a(no,N))*sum(N),
    quick_chisq(calculate_normalised_a(p,Np))*sum(Np),
    quick_chisq(calculate_normalised_a(np,Nnp))*sum(Nnp))
   }
  flag <- 1
  while(flag){
    a<-foreach(i=c(1,2),.combine=cbind,.options.multicore=mcoptions)%:%
    foreach(j=seq(1,nrow(table)),.combine=c,.options.multicore=mcoptions)%dopar%{
      switch(i,{
	if(p[j]==0|no[j]==0){1e06}else{
	  d_no <- no
	  d_no[j] <- d_no[j] - 1
	  d_p <- p
	  d_p[j] <- d_p[j] - 1
	  profile_fn(d_no,d_p,np)
	}
      },{
	if(p[j]==0|no[j]==0){1e06}else{
	  d_no <- no
	  d_no[j] <- d_no[j] - 1
	  d_np <- np
	  d_np[j] <- d_np[j] - 1
	  profile_fn(d_no,p,d_np)
	}
      })
    }
    b<-(a==min(range(a)))
    p<-p-b[,1]
    np<-np-b[,2]
    no<-no-rowSums(b)
    out<-cbind(no,p,np)
    rownames(out)<-rownames(table)
    V<-np+p
    if(runif(1)<0.1){
      d1<-density(calculate_normalised_a(no,N))
      d2<-density(calculate_normalised_a(p,Np))
      d3<-density(calculate_normalised_a(np,Nnp))
      print(rbind(100*(org-out)/Ntab,
	sum=colSums(org-out)
      ))
      cat(paste(" mean",sprintf("%.4f",mean(a_fn(V))),"\n",
	sum(no),"no",sprintf("%.2f",100*sum(no)/(sum(no)+yes)),
	"% ",yes,"yes",sprintf("%.2f",100*yes/(sum(no)+yes)),"%\n"))
	if(runif(1)<0.1){
      plot(d1,col="blue",xlim=range(d1$x,d2$x,d3$x),
      ylim=range(d1$y,d2$y,d3$y))
      lines(d2,col="red")
      lines(d3,col="green")
      x<-seq(min(d1$x,d2$x,d3$x),max(d1$x,d2$x,d3$x),by=0.01)
      lines(x,dnorm(x))
	  write.table(out,"running.tab")
	}
      }
     if((mean(a_fn(V)))<0){flag<-0} 
  }
  write.table(out,"done.tab")
  beep(9)
  out
}

quick_chisq <- function(sample){
  d<-density(sample)
  sum((d$y-dnorm(d$x))^2)
}

poop<-function(ballot){
  i <- quorate_index(ballot)
  alpha <- sqrt(0.5)^length(i)
  d_fn <- function(x){density(n=64,calculate_normalised_a(x,ballot[,"N"]))}
  a <- d_fn(ballot[,"V"])
  lapply(i,function(W){
    v<- d_fn(ballot[,W])
    x<-a$x
    y<-v$x
    z<-outer(a$y,v$y)
    col<-(col_map(colnames(ballot)[W]))
    rgl.surface(x,y,z*10,alpha=0.1,col=col)
  })
}

ballot_matrix <- function(ballot,winner=""){
  winner <- winner[winner %in% quorate_names(ballot)]
  if(length(winner)>0){cook_matrix(ballot[,"a"],ballot[,W2v(winner)])}
}

ballot_ranges<-function(ballot){
  w_list <- W2w(quorate_names(ballot))
  r_fn <-function(w){
    max(density(ballot[,w])$y)
  }
  ballot_sapply(ballot,w_list,r_fn)
}

ballot_cor<-function(ballot){
  v_list <- W2w(quorate_names(ballot))
  r_fn <-function(v){
    i<-grep(v,colnames(ballot))
    cor(ballot[,"a"],ballot[,i])
  }
  ballot_sapply(ballot,v_list,r_fn)
}

ballot_cdfn<-function(ballot,tag=""){
  W_list <- quorate_names(ballot)
  V<-ballot[,"V"]
  N<-ballot[,"N"]
  
  r_fn <-function(n){
    W<-ballot[,n]
    sum(W/N<sum(W)/sum(N))/length(W)
  }
  out<-c(a=r_fn(V),ballot_sapply(ballot,W_list,r_fn))
  names(out)<-paste(tag,names(out))
  out
}

ballot_chisq_to_normal <- function(ballot,tag=""){
  W_list <- quorate_names(ballot)
  a_fn<-a_fn_fn(ballot[,"N"])
  
  c_fn <-function(n){
    W<-ballot[,n]
    w<-density(a_fn(W))
    sum((w$y-dnorm(w$x))^2)
  }
  out<-c(a=c_fn("V"),ballot_sapply(ballot,W_list,c_fn))
  names(out)<-paste(tag,names(out))
  out
}

ballot_report <- function(ballot,tag=""){
  report_fn <- function(x){
    c(mean(x),sd(x),skewness(x),kurtosis(x))
  }
  cn<-c("mean","sd","skewness","kurtosis")
  i<-quorate_names(ballot)
  a_fn<-a_fn_fn(ballot[,"N"])
  switch(sw_cond(i),{
    out<-report_fn(a_fn(ballot[,"V"]))
    names(out)<-cn
    out
  },{
    out<-rbind(report_fn(a_fn(ballot[,"V"])),report_fn(a_fn(ballot[,i])))
    colnames(out)<-cn
    rownames(out)<-paste(tag,c("a",i))
    out
  },{
    out<-rbind(report_fn(a_fn(ballot[,"V"])),
      foreach(j=i,.combine=rbind)%do%{report_fn(a_fn(ballot[,j]))})
    colnames(out)<-cn
    rownames(out)<-paste(tag,c("a",i))
    out
  })
}

ballot_sapply<-function(ballot,v_list,r_fn){
  switch(sw_cond(v_list),{},{
    out<-r_fn(v_list[1])
    names(out)<-v_list
    out
  },{
    out<-sapply(v_list,r_fn)
    names(out)<-v_list
    out
  })
}

cook_matrix <- function(x,y,n=128){
  a<-density(x,n=n)
  b<-density(y,n=n)
  ay<-(a$y)
  by<-(b$y)
  outer(ay,by)
}


plot_both<-function(ballot,main="Sample Data"){
	plot_density(ballot,main)
	plot_scatter(ballot,main)
}

limits<-function(...){
	c(min(c(...)),max(c(...)))
}

col_map_table <- c(
	W_Lab="red",
	W_SNP="green",
	W_Con="blue",
	W_LD="cyan",
	W_Yes="cyan",
	W_No="blue"
)

col_map <- function(x){
  n<-names(col_map_table)
  foreach(z=names(col_map_table),y=col_map_table)%do%{
    x<-gsub(z,y,x)
  }
  x
}

plot_density<-function(ballot,main="Sample data"){
        a_fn<- a_fn_fn(ballot[,"N"])
	a_d <- density(a_fn(ballot[,"V"]))
	i <- quorate_index(ballot)
	col<-c("black","purple",col_map(quorate_names(ballot)))
	leg<-c("Gaussian","turnout",gsub("W_","",quorate_names(ballot)))
	print(i)
	switch(min(length(i)+1,3),{
	  plot(a_d,main=main,ylim=limits(a_d$y,dnorm(0)),col="purple")
	  lines(a_d$x,dnorm(a_d$x))
	},{
	  v_d<-density(a_fn(ballot[,i]))
	  xlim<-limits(v_d$x,a_d$x)
	  ylim<-limits(v_d$y,a_d$y,dnorm(0))
	  plot(a_d,xlim=xlim,ylim=ylim,main=main,col="purple")
	  lines(a_d$x,dnorm(a_d$x))
          lines(v_d,col=col[3])
	},{
	  v_list<-foreach(j=i)%do%{density(a_fn(ballot[,j]))}
	  xlim<-foreach(v=v_list,.combine=limits,.multicombine=TRUE,.init=a_d$x)%do%{v$x}
	  ylim<-foreach(v=v_list,.combine=limits,.multicombine=TRUE,.init=c(dnorm(0),a_d$y))%do%{v$y}
	  plot(a_d,xlim=xlim,ylim=ylim,type="l",col="purple",main=main)
	  lines(a_d$x,dnorm(a_d$x))
	  foreach(v=v_list,c=col[c(-1,-2)])%do%{
	    lines(v,col=c)
	  }
	})
	legend(x="topleft",pch=2,col=col,legend=leg)
	return()
}

plot_scatter<-function(ballot,main="Sample data"){
  a_fn <- a_fn_fn(ballot[,"N"])
  a <- a_fn(ballot[,"V"])
  a_d <- density(a)
  n <- colnames(ballot)
  i <- quorate_index(ballot)
  switch(min(length(i)+1,3),{},{
    mask<-ballot[,i]!=0
    v<-a_fn(ballot[,i])
    leg<-c(gsub("W_","",n[i]))
    plot(x=a[mask],y=v[mask],pch=3,main=paste(main,leg),xlab="Turnout",ylab=paste(leg,"vote"),
    sub=paste("Covariance:",cor(a,v)))
    abline(v=mean(a),h=mean(v),col="red")
    abline(v=0,h=0,col="black")
    legend(x="topleft",pch=2,col=c("black","red"),legend=c("Population Mean","Sample Mean"))
  },{
    W_list<-ballot[,i]
    leg<-c(gsub("W_","",n[i]))
    foreach(W=W_list,l=leg)%do%{
      mask<-W!=0
      v<-a_fn(W)
      plot(x=a[mask],y=v[mask],pch=3,main=paste(main,l),xlab="Turnout",ylab=paste(l,"vote"),
      sub=paste("Covariance:",cor(a,v)))
      abline(v=mean(a),h=mean(a_fn(W)),col="red")
      abline(v=0,h=0,col="black")
      legend(x="topleft",pch=2,col=c("black","red"),legend=c("Population Mean","Sample Mean"))
    }
  })
}

polled_names <- function(ballot){
	all_n <- gsub("W_","",names(col_map_table))
	find_fn <- function(n){
	  i<-grep(n,colnames(ballot))
	  switch(sw_cond(i),{FALSE},{
	    if(sum(ballot[,i])!=0){TRUE}else{FALSE}
	  },{
	    if(sum(colSums(ballot[,i])!=0)>0){TRUE}else{FALSE}
	  })
	}
	mask <- sapply(all_n,find_fn)
	names(mask)[mask]
}

quorate_index <- function(ballot){
  n <- paste("W_",polled_names(ballot),sep="")
  i <- foreach(ni=n,.combine=c)%do%{grep(ni,colnames(ballot))}
  switch(min(length(i)+1,3),{},{
    if(sum(ballot[,i]!=0)>2){i}
  },{
    i[(colSums(ballot[,i]!=0)>(dim(ballot)[1])/2)]
  })
}

quorate_names <- function(ballot){
  colnames(ballot)[quorate_index(ballot)]
}

compute_W_single <- function(a_fn,W){
  i <- min(3,1+length(unlist(W))%/%length(a_fn(0)))
  switch(i,{},{
    v <- a_fn(W)
    out <- cbind(W,v)
    out
  },{
    w<-rowSums(W)
    v<-a_fn(w)
    out<-cbind(w,v)
    out
  })
}

name_labels <- c("Ward","name")

name_column <- function(x){
  foreach(n=name_labels,.combine=c)%do%{
    grep(n,x)
  }
}

# https://stat.ethz.ch/pipermail/r-help/2006-March/101023.html
interleave <- function(v1,v2){
  ord1<-2*1:length(v1)-1
  ord2<-2*1:length(v2)
  c(v1,v2)[order(c(ord1,ord2))]
}

compute_W<-function(ballot){
  cn <- colnames(ballot)
  id <- ballot[,name_column(cn)]
  if(length(id)==0|sum(cn=="V")==0|sum(cn=="N")==0){return()}
  V <- ballot[,"V"]
  if(sum(ballot[,"N"])==0){
    t<-grep("Turnout",colnames(ballot))
    N<-round(N<-V/ballot[,t])
  }else{
    N<-ballot[,"N"]
  }
  a_fn <- a_fn_fn(N)
  a <-a_fn(V)
  polled<-polled_names(ballot)
  out<-cbind(N,V,a)
  rownames(out)<-id
  colnames(out)<-c("N","V","a")
  switch(sw_cond(polled),{out},{
    W <- compute_W_single(a_fn,ballot[,grep(polled,cn)])
    if(length(dim(W))==2){
      colnames(W) <- interleave(paste("W_",polled,sep=""),paste("w_",polled,sep=""))
      out<-cbind(out,W)
    }
    out
  },{
    W <- foreach(p=polled,.combine=cbind)%do% {
      x<-compute_W_single(a_fn,ballot[,grep(p,cn)])
      if(length(dim(x))==2){
	colnames(x)<-interleave(paste("W_",p,sep=""),paste("w_",p,sep=""))
	x
      }
    }
    out<-cbind(out,W)
    out
  })
}

clean_columns <- function(ballot){
	ballot[,colSums(ballot)!=0]
}

phone_png<-function(...){
  png(...,width=720,height=1280)
}

pdf_a4 <- function(...){
  pdf(paper="a4",width=18,height=24,...)
}

add_av<-function(ballot){
	cbind(ballot,a=ballot$V/ballot$N,v=ballot$W/ballot$V)
}

delta_limit <- 1.5

hunt_outliers <- function(ballot){
	i<-quorate_index(ballot)
	a_mask<-abs(calculate_normalised_a(ballot$V,ballot$N))>delta_limit
	switch(min(length(i)+1,3),{return()},{
	  return()
	 },{
	    x<-foreach(x=i,.combine=cbind)%do%{abs((calculate_normalised_a(ballot[,x],ballot$V))>delta_limit)&a_mask}
	    colnames(x)<-quorate_names(ballot)
	    ballot[rowSums(x)!=0,]
	 })
}

load_list <- function(list){
  lapply(lapply(list,read.csv),compute_W)
}
display_list <- function(list){
  foreach(n=list,b=load_list(list))%do%{
    plot_both(b,n)
  }
}
