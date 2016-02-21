#'@name buildPackage
#'@import devtools
"_PACKAGE"

require(devtools)

#'	@name gitPull
#'	@description Execute a git pull
#'	@export
gitPull<- function(){
	system2("git",c("pull"))
}

#'	@name gitPush
#'	@description Git add, commit and push
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
	p<-list.files(pattern=".tar.gz")
	p<-p[grep(package,p)]
	
	install.packages(p)
}

#'@export
buildPackages<-function(packages=findPackages()){
	lapply(packages,buildPackage)
}
