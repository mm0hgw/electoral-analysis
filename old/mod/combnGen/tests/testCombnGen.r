require(foreach)
require(iterators)
require(combnGen)

# unit test for combination generator
testCombnGen<-function(n,k){
	target<-combn(n,k)
	combnGen<-combnGenGen(n,k)
	foreach(
		i=icount(choose(n,k)),
		.combine=sum
	)%do%{
		target[,i]!=combnGen(i)
	}
}
