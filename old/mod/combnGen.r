
# choose look up table gen
chooseLutGen <- function(n,k){
	out<-matrix(1,nrow=n,ncol=k)
	foreach(i=icount(n))%:%
	foreach(j=icount(k))%do%{
		if(i>j){
			out[i,j]<-choose(i,j)
		}
	}
}

#combination generator generator
combnGenGen <- function(n,k){
	chooseLut <- chooseLutGen(n-1,k-1)
	combnGen <- function(x,ng=n,kg=k){
		if(kg==1){return(x)}
		cnk<-chooseLut[ng-1,kg-1]
		if(x<=cnk){
			out<-c(1,combnGen(x,ng-1,kg-1)+1)
		}else{
			out<-combnGen(x-cnk,ng-1,kg)+1
		}
		out
	}
	return(combnGen)
}

# unit test for combination generator
testCombnGen<-function(n,k){
	tgt<-combn(n,k)
	combnGen<-combnGenGen(n,k)
	foreach(
		i=icount(choose(n,k)),
		.combine=sum
	)%dopar%{
		tgt[,i]!=combnGen(i)
	}
}


