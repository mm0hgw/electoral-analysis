#'@name buildPackage
#'@import devtools
"_PACKAGE"

require(devtools)

#'	@export
gitPull<- function(){
	system2("git",c("pull"))
}

#'	@export
gitPush<-function(filelist,comment){
	system2("git",c("pull"))
	system2("git",c("add", filelist))
	system2("git",c("commit","-m",comment))
	system2("git",c("push"))
}

#'	@export
findPackages <- function(path="."){
	l<-gsub("./","",list.dirs(path=path))
	l[unlist(lapply(paste(l,"/DESCRIPTION",sep=""),file.exists))]
}

#'	@import devtools
#'	@export
buildPackage <- function(package){
	gitPull()
	system2("rm",paste(package,"/NAMESPACE",sep=""))
	system2("rm",c("-r",paste(package,"/man/",sep="")))
	document(package)
	system2("R",c("CMD","check",package))
	system2("R",c("CMD","build",package))
	gitPush(list.files(pattern=package,include.dirs=TRUE),"build")
	install.packages(paste(package,"*.tar.gz",sep=""))
}

#'@export
buildPackages<-function(packages=findPackages()){
	lapply(packages,buildPackage)
}
