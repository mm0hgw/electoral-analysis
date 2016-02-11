pull_source <- function(){
	system2("git","pull")

	source("mod/cluster.r")
	source("mod/calculating.r")
	source("mod/combnGen/R/combnGen.r")
	source("density_scatter.r")
	source("git.r")
	source("contiguity.r")
	
	
	source("mod/cluster_launch.r")
	
}
