# the sneaky module

# NO BIG FEET!

#custom sd calculator based upon "Eq. S1" and "Eq. S2"
#in Kilmek et al. 
custom_sd <- function(x,center=mean(x)){
        sqrt(mean((x-center)^2))  
}

#given a matrix with N,V,a rows, compute population mean, compare turnout vector to mean
#count occurances and divide by vector length
cdf_mean_intersect<-function(bal){                                       
        return(sum(bal["a",]<sum(bal["V",])/sum(bal["N",])) / length(bal["N",]))
}

# how we like our ballots cooked for easy grepping
cook_ballot <- function(ballot){
        ballot$NNP<-ballot$N-ballot$NP
        ballot$VNP<-ballot$V-ballot$VP
        ballot$a<-ballot$V/ballot$N
        ballot$ap<-ballot$VP/ballot$NP
        ballot$anp<-ballot$VNP/ballot$NNP


        bal=rbind(N=c(ballot$N,ballot$NP,ballot$NNP),
                        V=c(ballot$V,ballot$VP,ballot$VNP),
                        a=c(ballot$a,ballot$ap,ballot$anp))
        out<-list(
                name=c(paste(ballot$name,"total"),
                        paste(ballot$name,"postal"),
                        paste(ballot$name,"non-p")),
                Region=rep(ballot$Region,times=3),
                ballots=bal)
        return(out)
}
