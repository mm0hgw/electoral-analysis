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

cleanPackage <- function(package){
	system(paste(sep="","rm -r ",package,".Rcheck/ ",package,"_*.tar.gz"))
	system(paste(sep="","git rm ",package,".Rcheck/ ",package,"_*.tar.gz"))
}

#'	 buildPackage
#'	@description Build a package.
#'	@import devtools
#'	@export
buildPackage <- function(package,
	pull=TRUE,
	check=TRUE,
	clean=TRUE,
	as.cran=FALSE,
	push=FALSE,
	install=TRUE
){
	if(pull)gitPull()
	devtools::document(package)
	if(clean)cleanPackage(package)
	system2("R",c("CMD","build",package))
	if(check)checkPackage(package,as.cran)
	if(push)gitPushBuild(package)
	if(install&&0<length(p<-tail(n=1,
				list.files(
					pattern=paste(sep="",package,"_*.tar.gz")
				)
			)
		)
	)install.packages(
		repos=NULL,
		pkgs=p
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
				paste(sep="",
					".Rcheck/",
					c("00check.log",
					"00install.out",
						paste(sep="",
							package,
							c("-manual.pdf",
								"-Ex.Rout",
								"-Ex.timings"
							)
						)
					)
				)
			)
		)
	)
	print(files)
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
