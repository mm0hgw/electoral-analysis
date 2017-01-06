#'buildPackage
#'@import devtools
"_PACKAGE"

require(devtools)

#'	 gitPull
#'	@description Execute a git pull
#'	@export
gitPull<- function(){
	system2("git",c("pull"))
}

#'	 gitPush
#'	@description Git add, commit and push
#'	@export
gitPush<-function(filelist,comment){
	system2("git",c("pull"))
	system2("git",c("add", filelist))
	system2("git",c("commit","-m",comment))
	system2("git",c("push"))
}

#'	 findPackages
#'	@description Provide package directories
#'	@export
findPackages <- function(path="."){
	l<-list.dirs(path=path)
	l[unlist(lapply(paste(l,"/DESCRIPTION",sep=""),file.exists))]
}

#'	 buildPackage
#'	@description Build a package.
#'	@import devtools
#'	@export
buildPackage <- function(package){
	gitPull()
	document(package)
	system2("R",c("CMD","build","--as-cran",package))
		p<-list.files(pattern=".tar.gz")
	print(p)
	p<-p[grep(package,p)]
	print(p)
	system2("R",c("CMD","check",p))
	gitPush(
		list.files(pattern=package,include.dirs=TRUE),
		paste("build",p,sep=":")
	)
	install.packages(p)
}

#'	 buildPackages
#'	@description Build a list of packages.
#'	@import devtools
#'	@export
buildPackages<-function(packages=findPackages()){
	lapply(packages,buildPackage)
}

#' rebuild
#' @export
rebuild<-function(p){
	detach(paste("package:",p,sep=""),character.only=TRUE)
	buildPackage(p)
	require(p,character.only=TRUE)
}
