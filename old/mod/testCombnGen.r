require(foreach)
require(iterators)

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

