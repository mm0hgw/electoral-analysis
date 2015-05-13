require("foreach")

# calculate non-postal totals by removing postal totals from overall totals
# calculate turnout fractions of all 3 categories by division
cook_election <- function(ballot){
	ballot<-cbind(ballot,NNP=ballot$N-ballot$NP,VNP=ballot$V-ballot$VP)
	ballot<-cbind(ballot,a=ballot$V/ballot$N,ap=ballot$VP/ballot$NP,anp=ballot$VNP/ballot$NNP)
	return(ballot)
}

# plot a single cdf display with population mean
plot_single_display<-function(a,pm,main){
	intersect<-(sum(a<pm)/length(a))
	sub<-paste("population mean intersected by c.d.f. at",intersect)
	plot(ecdf(a),main=main,sub=sub)
	abline(v=pm)
	abline(h=0.5)
	return(c(name=main,intersect=intersect))
}

# read file, analyse and schedule plots
multi_plot_election <- function(name){
		# remove any csv suffix from data and setup output
	name<-sub(".csv$","",name)
		# load data
	ballot <- read.csv(paste(name,".csv",sep=""))
		#check for regions
	out<-if(!is.null(ballot$Region)){
			# iterate over regions, rbind, order not important
                foreach(region=levels(ballot$Region),
			.combine=rbind,
			.inorder=FALSE,
			.multicombine=TRUE)%dopar%{
                        rballot <- ballot[ballot$Region == region,]
                        rname <- paste(name,region) 
                        plot_multi_display(rballot,rname)
                }
	}
	out<-rbind(out,plot_multi_display(ballot,name))
	return(out)
}

# cdf tri plot of election with population mean
plot_multi_display<-function(ballot,name){
		# setup output
	pdf(paste(name,".pdf",sep=""),paper="a4",width=7,height=10)
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
                plot_single_display(a,pm_a,main_a),
                plot_single_display(ap,pm_ap,main_ap),
                plot_single_display(anp,pm_anp,main_anp)
	)
		# close output
	dev.off()
	return(out)
}

# make displays from all csvs in current directory
plot_all <- function() {
	out<-foreach(name=list.files(pattern=".csv$"),
		.combine=rbind,
                .inorder=FALSE,
                .multicombine=TRUE) %dopar% {
		multi_plot_election(name)
	}
	out<-out[order(as.numeric(out[,"intersect"])),]
	
	return(out)
}

analyse_plot <- function(table=plot_all()){
		# setup output
	pdf("analysis.pdf",paper="a4",width=7,height=10)
	data <- as.numeric(table[,"intersect"])
	density_obj <- density(data)
	ecdf_obj <- ecdf(data)
	SIR_points <- data[grep(pattern="SIR",table[,"name"])]
	sub <- "Scottish Independence Referendum points marked with red intersections"
	plot(density_obj,main="Overall error density",sub=sub)
	abline(v=0.5)
	for(point in SIR_points){
		abline(v=point,col="red")
	}
	plot(ecdf_obj,main="Overall error c.d.f.",sub=sub)
	for(point in SIR_points){
		abline(v=point,col="red")
	}
	abline(h=0.5)
	abline(v=0.5)
	dev.off()
	return(table)
}