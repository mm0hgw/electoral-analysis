
ballot_<-function(b,
	b_name,
	SAMPLE_FUN=calculate_normalised_a,
	FUN=density,
		# pull names from ballot
	i_names=colnames(b)[(colnames(b)!="N")&(sapply(b[1,],is.numeric))],
		# shift names to human readable
	o_names=gsub("^V$","overall",i_names)
	
){
		# apply FUN to SAMPLE_FUN over named elements
	out<-lapply(i_names,
		function(n){
			FUN(SAMPLE_FUN(b[,n],b$N))
		}
	)
		# tag results
	names(out)<-paste(b_name,".",o_names,".turnout",sep="")
		# return
	out
}
