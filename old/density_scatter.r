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
	col_map_table[grep(x,names(col_map_table))]
}

plot_density<-function(ballot,main="Sample data"){
        a <- calculate_normalised_a(ballot$V,ballot$N)
	a_d <- density(a)
	i <- quorate_index(ballot)
	switch(min(length(i)+1,3),{
		col<-c("black","purple")
		leg<-c("Gaussian","turnout")
		plot(a_d,type="l",col="purple",main=main)
		legend(x="topleft",pch=2,ylim=limits(a_d$y,dnorm(0)),col=col,legend=leg)
		lines(a_d$x,dnorm(a_d$x))
		s<-paste("Turnout sample mean:",sprintf("%.2f",mean(a)))
		title(sub=s)
	},{
		v <- calculate_normalised_a(ballot[,i],ballot$V)
		v_d<-density(v)
		xlim<-limits(v_d$x,a_d$x)
		ylim<-limits(v_d$y,a_d$y,dnorm(0))
		x<-seq(min(xlim),max(xlim),by=0.01)
		col<-c("black","purple",sapply(n[i],col_map))
		leg<-c("Gaussian","turnout",gsub("W_","",n[i]))
		plot(a_d,xlim=xlim,ylim=ylim,type="l",col="purple",main=main)
		legend(x="topleft",pch=2,col=col,legend=leg)
		lines(x,dnorm(x))
		s<-paste("Sample means: Turnout",sprintf("%.2f",mean(a)))
                lines(density(v),col=col_map(n[i]))
                s<-paste(s,leg[3],sprintf("%.2f",mean(v)))
		title(sub=s)
	},{
		v_fn <- function(W){calculate_normalised_a(W,ballot$V)}
		v_list<-lapply(ballot[,i],v_fn)
		xlim<-foreach(v=v_list,.combine=limits,.multicombine=TRUE,.init=a_d$x)%do%{
			density(v)$x
		}
		ylim<-foreach(v=v_list,.combine=limits,.multicombine=TRUE,.init=c(0,dnorm(0),a_d$y))%do%{
			density(v)$y
		}
		x<-seq(min(xlim),max(xlim),by=0.01)
		col<-c("black","purple",sapply(n[i],col_map))
		leg<-c("Gaussian","turnout",gsub("W_","",n[i]))
		plot(a_d,xlim=xlim,ylim=ylim,type="l",col="purple",main=main)
		legend(x="topleft",pch=2,col=col,legend=leg)
		lines(x,dnorm(x))
		s<-paste("Sample means: Turnout",sprintf("%.2f",mean(a)))
		foreach(v=v_list,ni=gsub("W_","",n[i]),c=col[c(-1,-2)])%do%{
			lines(density(v),col=c)
			s<-paste(s,ni,sprintf("%.2f",mean(v)))
		}
		title(sub=s)
	})
}

plot_scatter<-function(ballot,main="Sample data"){
        a <- ballot$V/ballot$N
	a_d <- density(a)
	n <- names(ballot)
	i <- grep("W_",n)
	i <- i[sapply(ballot[,i],sum)!=0]
	if(length(i)!=0){
		v_fn <- function(W){W/ballot$V}
		W_list<-ballot[,i]
		leg<-c(gsub("W_","",n[i]))
		foreach(W=W_list,l=leg)%do%{
			plot(x=a,y=v_fn(W),pch=3,main=paste(main,l),xlab="Turnout",ylab=paste(l,"vote"))
                        abline(v=mean(a),h=mean(v_fn(W)),col="red")
                        abline(v=sum(ballot$V)/sum(ballot$N),h=sum(W)/sum(ballot$V),col="black")
                        legend(x="topleft",pch=2,col=c("black","red"),legend=c("Population Mean","Sample Mean"))
		}
	}
	return()
}

present_names <- function(ballot){
	all_n <- gsub("W_","",names(col_map_table))
	find_fn <- function(n){grep(n,colnames(ballot))}
	mask <- sapply(sapply(all_n,find_fn),length)!=0
	names(mask)[mask]
}

quorate_index <- function(ballot){
  n <- paste("W_",present_names(ballot),sep="")
  i <- foreach(ni=n,.combine=c)%do%{grep(ni,colnames(ballot))}
  i[foreach(x=i,.combine=c)%do%{sum(ballot[,i])!=0}]
}
quorate_names <- function(ballot){
  n <- paste("W_",present_names(ballot),sep="")
  i <- foreach(ni=n,.combine=c)%do%{grep(ni,colnames(ballot))}
  n[foreach(x=i,.combine=c)%do%{sum(ballot[,i])!=0}]
}

compute_W<-function(ballot){
	
	if(sum(ballot$N)==0){
	  t<-grep("Turnout",colnames(ballot))
	  ballot$N<-ballot$V/ballot[,t]
	}
	z<-vector()
	present_n<-present_names(ballot)
	W<-foreach(n=present_n,.combine=cbind)%do%{
		index<-grep(n,colnames(ballot))
		ballot_adder<-function(a,b){a+b}
		z<-c(z,index)
		foreach(i=index,.combine=ballot_adder)%do%{
			ballot[,i]
		}
	}
	colnames(W)<-paste("W_",present_n,sep="")
	mask<-colSums(W)!=0
	out<-cbind(ballot[,-z],W)
	q_n <- quorate_names(out)
 
	a<-calculate_normalised_a(out$V,out$N)
	v<-foreach(x=q_n,.combine=cbind)%do%{
	  calculate_normalised_a(W[,x],ballot$V)
	}
	colnames(v)<-gsub("W_","v_",q_n)
	out<-cbind(ballot,W,a=a,v)
	out[,-z]
}

clean_columns <- function(ballot){
	ballot[,colSums(ballot)!=0]
}

phone_png<-function(filename){
  png(file=filename,width=720,height=1280)
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