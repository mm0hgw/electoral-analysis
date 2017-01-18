
z<-c(1,9,10)
z2<-function(x,n=10){
	k<-length(x)
	Lut<-combnGen:::LutGen(n,length(x))
	x<-c(0,x,n+1)
	v<-vector()
	y<-length(x)
	if(y>1){
		for(i in seq(2,y)){
			v<-c(x[i]-x[i-1]-1,v)
		}
	}
	out<-0
	for(j in seq(2,length(v))){
		if(j==2){
			out<-out+v[1]
		}else{
			if(v[j]>0){
				offset<-j-2+sum(v[seq(j-2)])
				i<-seq(v[j])+offset
				out<-out+sum(Lut[i,j])
			}
		}
	}
}
z2(z)
