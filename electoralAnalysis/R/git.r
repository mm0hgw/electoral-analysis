pull_source <- function(){
	system2("git","pull")
	source("R/cluster.r")
	source("R/calculating.r")
	source("R/density_scatter.r")
	source("R/git.r")
	source("R/region.r")
	source("R/cluster_launch.r")
}
