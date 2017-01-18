
z<-cbind(seq(7),8,10)

z2<-function(x,n=10){
	cat("z2",n,":",paste(collapse=",",x),"\n")
	k<-length(x)
	Lut<-combnGen:::LutGen(n,length(x))
	x<-c(0,x,n+1)
	v<-vector()
	y<-length(x)

	for(i in seq(2,y)){
		v<-c(x[i]-x[i-1]-1,v)
	}
	
	cat("z2",paste(collapse=",",v),"\n")
	out<-1
	
	for(j in seq(2,length(v))){
		cat("z2","j",j,"\n")
		if(j==2){
			out<-out+v[2]
		}
		if(j>2){
			if(v[j]>0){
				offset<-j-2+sum(head(v+1,n=j-1)-1)
				cat("z2","offset:",offset,"\n")
				i<-seq(v[j])+offset
				cat("z2","i",paste(collapse=",",i),"\n")
				out<-out+sum(Lut[i,j-2])
			}
		}
		cat("z2","j:",j,"out:",out,"\n")
	}
	cat("z2","out:",out,"\n")
	cat("z2","test:",paste(collapse=",",combnGen::combnG(out,n,k)),"\n")
	out
}

lapply(seq(nrow(z)),function(i){z2(z[i,])})
