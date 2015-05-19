results_by_region <- function(bal,tag,title){
        lev<-levels(as.factor(bal$Region))   
        lev<-lev[lev!=""]
        out<-foreach(region=lev,
                        .combine=rbind,
                        .init=c(paste(title,tag),
				cdf_mean_intercept(bal$ballots)),
                        .inorder=FALSE,
                        .multicombine=TRUE) %dopar%{
                        c(paste(title,region,tag),
				cdf_mean_intercept(bal$ballots[,bal$Region==region]))
        }
        return(out)
}

# how we like our ballots cooked for easy grepping
cook_ballot <- function(ballot,title){
        ballot$NNP<-ballot$N-ballot$NP
        ballot$VNP<-ballot$V-ballot$VP
        ballot$a<-ballot$V/ballot$N   
        ballot$ap<-ballot$VP/ballot$NP
        ballot$anp<-ballot$VNP/ballot$NNP

        bal=rbind(N=ballot$N,
                        V=ballot$V,
                        a=ballot$a)
        balp=rbind(N=ballot$NP,
                        V=ballot$VP,
                        a=ballot$ap)
        balnp=rbind(N=ballot$NNP,   
                        V=ballot$VNP,
                        a=ballot$anp)

        objt<-list(
                name=ballot$name,
                Region=ballot$Region,
                ballots=bal)
        objp<-list(
                name=ballot$name,
                Region=ballot$Region,
                ballots=balp)
        objnp<-list(
                name=ballot$name,
                Region=ballot$Region,
                ballots=balnp)
	out<-results_by_region(objt,"total",title)
	outp<-results_by_region(objp,"postal",title)
	outnp<-results_by_region(objnp,"non-p",title)

        return(rbind(out,outp,outnp))
}

read_custom_csv <- function(file){
        bal<-read.csv(file)
        if(is.null(bal$V)||is.null(bal$N)||is.null(bal$VP)||is.null(bal$NP)||is.null(bal$name)){
                print(paste("Crisis! file",file,"makes crisis!"))
                return(NULL)
        }
        if(is.null(bal$Region)){
                Region <- rep("",length(bal$name))
        }else{
                Region <- bal$Region
        }
        ballot<-list(
                name=as.character(bal$name),
                Region=as.factor(Region),
                N=as.numeric(as.character(bal$N)),
                V=as.numeric(as.character(bal$V)),
                NP=as.numeric(as.character(bal$NP)),
                VP=as.numeric(as.character(bal$VP)))
        return(cook_ballot(ballot,file))
}




read_unitaries_ballot <- function(){
	unitaries_key <- c(2,3,4,5,17,34)
        p<-read.csv("csv/Local elections 2014 - Electoral data - UNITARIES.csv")[,unitaries_key]
        p2<-p[p[,1]!=""&p[,2]!=""&p[,3]!=""&p[,4]!=""&p[,5]!=""&p[,6]!="",]
        title<-"LE2014 UNITARIES"
        ballot<-list(
		title=title,
                name=as.character(p2[,2]),
                Region=as.factor(p2[,1]),
                N=as.numeric(as.character(p2[,3])),
                V=as.numeric(as.character(p2[,4])),
                NP=as.numeric(as.character(p2[,5])),
                VP=as.numeric(as.character(p2[,6])))
        return(cook_ballot(ballot,title))
}


read_all_custom_csv<-function(){
	p<-paste("csv/",list.files(path="csv/",pattern="--.csv$"),sep="")
	
	print(grep(p,pattern=".csv$",value=T))
	q<-foreach(n=grep(p,pattern=".csv$",value=T),
		.combine=rbind,
		.inorder=F,
		.multicombine=T)%dopar%{
		read_custom_csv(n)
	}	
}


custom_plot_ecdf<-function(bucket,match_pattern){
	names<-as.character(bucket[,1])
	intercept<-as.numeric(bucket[,2])
        matched_points <- grep(pattern=match_pattern,names)
        i_match <-intercept[matched_points]
        i_unmatch <- intercept[-matched_points]
	density_obj <- density(intercept)
        i_x<-density_obj$x
        ecdf_obj <- ecdf(intercept)
        i_peak <- density_obj$x[which.max(density_obj$y)]
	i_mean <- mean(intercept)
        i_sd <- custom_sd(intercept,i_mean)
	i_dev <- deviation_in_SDs(i_match,i_mean,i_sd)
        main <- paste("Matching \"",match_pattern,"\"",sep="")
        sub <- paste("Matched sample ranges from ",sprintf("%.2f",min(i_dev)),
                " to ",sprintf("%.2f",max(i_dev)),
                " in SDs of deviation.",sep="")
                # do density plot  
        plot(density_obj,main=paste(main,"population mean / cdf intercept error density"),sub=sub)
        lines(x=i_x,dnorm(x=i_x,mean=i_peak,sd=custom_sd(intercept,i_peak)),col="magenta")
	peak_separate_sd_lines(intercept,i_peak)
        points(x=i_match,y=dnorm(x=i_match,mean=i_peak,sd=i_sd),pch=1,col="red")
                # do c.d.f. plot
        plot(ecdf_obj,verticals=T,do.points=F,main=paste(main,"population mean / cdf intercept error c.d.f."),sub=sub)
        lines(x=i_x,pnorm(q=i_x,mean=i_mean,sd=sd(intercept)),col="magenta")
        points(x=i_match,y=pnorm(q=i_match,mean=i_mean,sd=custom_sd(intercept,i_mean)),pch=1,col="red")
	peak_separate_sd_lines(intercept,i_mean)
        abline(h=0.5)
	return(i_dev)
}


