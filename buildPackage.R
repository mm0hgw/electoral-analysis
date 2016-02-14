require(devtools)

gitPull<- function(){
	system2("git",c("pull"))
}

findPackages <- function(path="."){
	l<-gsub("./","",list.dirs(path=path))
	print(lapply(paste(l,"/NAMESPACE",sep=""),file.exists))
	
}

buildPackage <- function(package){
	document(package)
	system2("git",c("add", paste(package,"/man/*",sep="")))
	system2("R",c("CMD","check",package))
	system2("R",c("CMD","build",package))
	install.packages(paste(package,"_1.0.tar.gz",sep=""))
}

buildPackages<-function(packages=findPackages()){
	lapply(packages,buildPackage)
}
