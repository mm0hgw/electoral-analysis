#
#	Main loop
#

# import components
source("mod/cluster.r")
source("mod/calculating.r")
source("mod/display.r")
source("mod/import.r")

if(!exists("cl")){
	cl<-makeCustomCluster()
}

if(!exists("total_sample")){
	total_sample <- assemble_sample()
}

# basic display
do_display<-function(output_fn,match_pattern="SIR2014",sample=total_sample){
	output_fn(filename=paste("turnout_analysis_",match_pattern,sep=""))
	out<-custom_plot_ecdf(sample,match_pattern)
	dev.off()
	return(out)
}

