#
#	Main loop
#

# import components
source("mod/init.r")
source("mod/calculating.r")
source("mod/display.r")
source("mod/import.r")

# build sample
total_sample <- assemble_sample()

# basic display
do_display<-function(match_pattern="SIR2014",sample=total_sample){
	pdf_target(paste("turnout_analysis_",match_pattern,sep=""))
	out<-custom_plot_ecdf(sample,match_pattern)
	dev.off()
	return(out)
}

