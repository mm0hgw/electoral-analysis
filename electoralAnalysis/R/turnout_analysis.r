#
#	Main loop
#

# import components
source("R/cluster.r")
source("R/calculating.r")
source("R/display.r")
source("R/import.r")

if(!exists("cl")){
	cl<-makeCustomCluster()
}

if(!exists("total_sample")){
	total_sample <- assemble_sample()
}

# basic display
do_display<-function(match_pattern="SIR2014",sample=total_sample){
	pdf(file=paste("turnout_analysis_",match_pattern,".pdf",sep=""))
	out<-custom_plot_ecdf(sample,match_pattern)
	dev.off()
	return(out)
}

