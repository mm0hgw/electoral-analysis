#'build-package
#'@importFrom utils install.packages tail
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
gitPush<-function(filelist,comment=""){
	system2("git",c("pull"))
	system2("git",c("add", filelist))
	system2("git",c("commit","-m",paste(sep="","\"",comment,"\"")))
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
	system(paste(sep="","rm -r ",package,".Rcheck/ ",
		package,"/man/* ",package,"*.tar.gz")
	)
	system(paste(sep="","git rm -r ",package,"/man/*"))
	system(paste(sep="","mkdir /dev/shm/",package,".Rcheck"))
	system(paste(sep="","ln -s /dev/shm/",package,".Rcheck ."))
}

installPackage<-function(package){
	files<-grep(paste(sep="",package,"_"),
		list.files(),
		value=TRUE
	)
	#print(files)
	if(length(files)>0){
		install.packages(
			repos=NULL,
			pkgs=files
		)
	}
}

#'	 buildPackage
#'	@description Build a package.
#'	@import devtools
#'	@export
buildPackage <- function(package,
	pull=build,
	build=check,
	check=push,
	clean=push,
	as.cran=FALSE,
	push=as.cran,
	install=TRUE
){
	if(pull)gitPull()
	if(clean)cleanPackage(package)
	if(build){
		devtools::document(package)
		Rcpp::compileAttributes(package)
		devtools::build(package)
	}
	if(check)checkPackage(package,as.cran)
	if(push)pushPackage(package)
	if(install)installPackage(package)
}

pushPackage<-function(package){
		# list roxygen generated files
	roxygenfiles<-paste(sep="",
		package,
		c(
			"/DESCRIPTION",
			"/NAMESPACE",
			"/R/RcppExports.R",
			"/src/RcppExports.cpp",
			paste(sep="",
				"/R/",
				list.files(paste(sep="",package,"/man/"),
					pattern="*.R"
				)
			),
			paste(sep="",
				"/man/",
				list.files(paste(sep="",package,"/man/"),
					pattern="*.Rd"
				)
			),
			paste(sep="",
				"/src/",
				list.files(paste(sep="",package,"/man/"),
					pattern="(*.c)|(*.cpp)|(*.h)|(*.hpp)|"
				)
			)
		)
	)
		# pull check log
	system2("mv",
		c(
			paste(sep="",
				package,
				".Rcheck/00check.log"
			),
			paste(sep="",
				package,
				"-00Rcheck.log"
			)
		)
	)
		# pull manual, examples and example timings
	system2("mv",
		c(
			paste(sep="",
				package,
				".Rcheck/",
				package,
				c("-manual.pdf",
					"-Ex.Rout",
					"-Ex.timings"
				)
			),
			"."
		)
	)
		# list build feedback
	Rcheckfiles<-list.files(pattern=paste(sep="",package,"-"))
	files<-c(roxygenfiles,Rcheckfiles)
	#print(files)
	files<-c(files[sapply(files,file.exists)]	)
	print(files)
	gitPush(files,
		paste(sep="","build:",	package)
	)
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
