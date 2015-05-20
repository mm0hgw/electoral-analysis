source ("mod/sneaky.r")
source ("mod/unsneaky.r")

total_sample <- rbind(read_all_custom_csv())


do_display<-function(match_pattern="SIR2014",sample=total_sample){
	pdf_target(paste("turnout_analysis_",match_pattern,sep=""))
	out<-custom_plot_ecdf(sample,match_pattern)
	dev.off()
	return(out)
}