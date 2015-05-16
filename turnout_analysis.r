require("foreach")
source("mod/sneaky.r")
# calculate non-postal totals by removing postal totals from overall totals
# calculate turnout fractions of all 3 categories by division
cook_election <- function(ballot){
	ballot<-cbind(ballot,NNP=ballot$N-ballot$NP,VNP=ballot$V-ballot$VP)
	ballot<-cbind(ballot,a=ballot$V/ballot$N,ap=ballot$VP/ballot$NP,anp=ballot$VNP/ballot$NNP)
	return(ballot)
}

# plot a single cdf display with population mean and intersection data
plot_single_display<-function(a,pm,main,grepping_pattern){
		# switch for detailed output on match
	if(any(grep(main,pattern=grepping_pattern))){
		output<-TRUE
	}else{
		output<-FALSE
	}
	intersect<-(sum(a<pm)/length(a))
	
	if(output==TRUE){
                sub<-paste("population mean intersected by c.d.f. at",intersect)
                plot(ecdf(a),main=main,sub=sub)
                abline(v=pm)
                abline(h=0.5)
        }
	return(c(name=main,intersect=intersect))
}

# read file, analyse and schedule plots
multi_plot_election <- function(name,grepping_pattern){
		# remove any csv suffix from data and setup output
	name<-sub(".csv$","",name)
		# load data
	ballot <- read.csv(paste(name,".csv",sep=""))
		# sanity check for cookability
	if(is.null(ballot$V)||is.null(ballot$N)||is.null(ballot$VP)||is.null(ballot$NP))
		{return()} # back away slowly if uncookable
		#check for regions
	out<-if(!is.null(ballot$Region)){
			# iterate over regions, rbind, order not important
                foreach(region=levels(ballot$Region),
			.combine=rbind,
			.inorder=FALSE,
			.multicombine=TRUE)%dopar%{
                        rballot <- ballot[ballot$Region == region,]
                        rname <- paste(name,region) 
                        plot_multi_display(rballot,rname,grepping_pattern)
                }
	}
	out<-rbind(out,plot_multi_display(ballot,name,grepping_pattern))
	return(out)
}

# cdf tri plot of election with population mean
plot_multi_display<-function(ballot,name,grepping_pattern){
		# cook election
	ballot<-cook_election(ballot)
		# pull turnout vectors
	a <- ballot$a
	ap <- ballot$ap
	anp <- ballot$anp
		# calculate population mediums
	pm_a <- sum(ballot$V)/sum(ballot$N)
	pm_ap <- sum(ballot$VP)/sum(ballot$NP)
	pm_anp <- sum(ballot$VNP)/sum(ballot$NNP)
		# process titles
	main_a <- paste(name,"total turnout cdf")
	main_ap <- paste(name,"postal turnout cdf")
	main_anp <- paste(name,"non-postal turnout cdf")
		# plot displays
	out<-rbind(
                plot_single_display(a,pm_a,main_a,grepping_pattern),
                plot_single_display(ap,pm_ap,main_ap,grepping_pattern),
                plot_single_display(anp,pm_anp,main_anp,grepping_pattern)
	)
	return(out)
}

# make displays from all csvs in current directory
plot_all <- function(grepping_pattern="SIR") {
		# use all .csv in current directory to make plots
	out<-foreach(name=list.files(pattern=".csv$"),
		.combine=rbind,
                .inorder=FALSE,
                .multicombine=TRUE) %dopar% {
		multi_plot_election(name,grepping_pattern)
	}
		# sort collected error data
	out<-out[order(as.numeric(out[,"intersect"])),]
	return(out)
}


analyse_plot <- function(grepping_pattern="EPE2014 Scotland"){
		# setup output
	pdf(paste("turnout_analysis_",grepping_pattern,".pdf",sep=""),paper="a4",width=7,height=10)
		# pull & compute data for display
	bucket<-plot_all(grepping_pattern)
	intersect <- as.numeric(bucket[,"intersect"])
	density_obj <- density(intersect)
	ecdf_obj <- ecdf(intersect)
	i_peak <- density_obj$x[which.max(density_obj$y)]
	i_sd <- custom_sd(intersect,i_peak)
	matched_points <- grep(pattern=grepping_pattern,bucket[,"name"])
	matched_sd <- custom_sd(intersect[matched_points],center=i_peak)
	unmatched_sd <- custom_sd(intersect[-matched_points],center=i_peak)
	sd_ratio <- matched_sd/unmatched_sd
	main <- paste("Matching \"",grepping_pattern,"\"",sep="")
	sub <- paste("matched SD:",sprintf("%.2f",matched_sd),
		" unmatched SD:",sprintf("%.2f",unmatched_sd),
		" ratio:",sprintf("%.1f",sd_ratio),sep="")
		# do density plot
	plot(density_obj,main=paste(main,"Overall error density"),sub=sub)
	abline(v=i_peak,col="green")
	abline(v=i_peak+i_sd,col="blue")
	abline(v=i_peak-i_sd,col="blue")
	for(point in intersect[matched_points]){
		abline(v=point,col="red")
	}
		# do c.d.f. plot
	plot(ecdf_obj,main=paste(main,"Overall error c.d.f."),sub=sub)
	abline(v=i_peak,col="green")
	abline(v=i_peak+i_sd,col="blue")
	abline(v=i_peak-i_sd,col="blue")
	abline(h=0.5)

	for(point in intersect[matched_points]){
		abline(v=point,col="red")
	}
		# close output
	dev.off()
		# write intercept data to .csv
	
	write.csv(bucket,file="turnout_analysis.csv")
	write.csv(bucket[grep(bucket[,"name"],pattern=grepping_pattern),],file=paste("turnout_analysis_",grepping_pattern,".csv",sep=""))
		# and toss it out in case someone wants it
	return(bucket)
}