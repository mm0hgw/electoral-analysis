# reset cluster

if(exists("cl")){
	stopCluster(cl)
	if(exists("cl")){
		rm("cl")
	}
}

cl<-makeCustomCluster()