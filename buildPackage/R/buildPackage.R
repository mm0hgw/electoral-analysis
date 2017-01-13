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
buildPackage <- function(package,
	pull=TRUE,
	check=TRUE,
	as.cran=FALSE,
	push=FALSE,
	install=TRUE
){
	if(pull)gitPull()
	devtools::document(package)
	system2("R",c("CMD","build",package))
	if(check)checkPackage(package,as.cran)
	if(push)gitPushBuild(package)
	if(install)install.packages(
			tail(n=1,
				list.files(
					pattern=paste(sep="",package,"_*.tar.gz")
				)
			)
		)
}

gitPushBuild<-function(package){
	files<-paste(collapse=" ",
		paste(sep="",
			package,
			c(
				"*.tar.gz",
				"/NAMESPACE",
				"/man/*",
				".Rcheck/"
			)
		)
	)
	system(paste("git add",files))
	system(paste(sep="","git commit -m build:",	package))
	system("git push")
}

#' checkPackage
#' @export
checkPackage<-function(package,as.cran=FALSE){
	p<-list.files(pattern=".tar.gz")
	p<-p[tail(n=1,grep(paste(package,"_",sep=""),p))]
	if(as.cran==TRUE){
		system2("R",c("CMD","check","--as-cran",p))
	}else{
		system2("R",c("CMD","check",p))
	}
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
		buildPackage(p)
	require(p,character.only=TRUE)
}
