pull_source <- function(){
	system2("git","pull")

	source("mod/cluster.r")
	source("mod/calculating.r")
	source("density_scatter.r")
	source("git.r")
	source("contiguity.r")
	
	if(exists("cl")){
		rm("cl")
	}
	cl<-makeCustomCluster()
	
}
