density_plot<-function(ballot,filename,main,winner){
  png(file=filename,width=480,height=640)
  a_d<-density(calculate_normalised_a(ballot$V,ballot$N))
  v_d<-density(calculate_normalised_a(ballot$W,ballot$V))
  xlim<-c(min(c(a_d$x,v_d$x)),max(c(a_d$x,v_d$x)))
  x<-seq(xlim[1],xlim[2],by=0.01)
  ylim<-c(0,max(a_d$y,v_d$y,dnorm(0)))
  plot(a_d,col="blue",main=main,xlim=xlim,ylim=ylim)
  legend(x="topleft",pch=1,col=c("red","blue","black"),legend=c(paste("%",winner,"vote"),"% turnout","Gaussian"))
  lines(v_d,col="red")
  lines(x,y=dnorm(x))
  dev.off()
}

scatter_plot<-function(ballot,filename,main,winner){
  png(file=filename,width=480,height=640)
  plot(pch=1,x=ballot$V/ballot$N,y=ballot$W/ballot$V,main=main,xlab="Turnout",ylab=paste(winner,"vote"))
  abline(v=mean(ballot$V/ballot$N),h=mean(ballot$W/ballot$V),col="red")
  abline(v=sum(ballot$V)/sum(ballot$N),h=sum(ballot$W)/sum(ballot$V),col="black")
  legend(x="topleft",pch=2,col=c("black","red"),legend=c("Population Mean","Sample Mean"))
  dev.off()
}
