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
gitPush<-function(comment){
	system2("git",c("pull"))
	system2("git",c("commit","-a","-m",paste(sep="","\"",comment,"\"")))
	system2("git",c("push"))
}

#'	 gitAdd
#'	@description Git add, commit and push
#'	@export
gitAdd<-function(filelist){
	system2("git",c("add", filelist))
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
	system(paste(sep="","mkdir -p /dev/shm/",package,".Rcheck"))
	system(paste(sep="","ln -s /dev/shm/",package,".Rcheck ."))
}

installPackage<-function(package){
	x<-read.xcf(paste(sep="",package,"/DESCRIPTION"))
	
	files<-grep(paste(sep="",package,"_",x[1,"Version"],".tar.gz"),
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
#'	@import Rcpp
#'	@export
buildPackage <- function(package,
	pull=build,
	build=check,
	check=cran,
	clean=push,
	cran=FALSE,
	add=build,
	push=cran,
	install=build
){
	detachPackage(package, TRUE)
	if(pull)gitPull()
	if(clean)cleanPackage(package)
	if(build){
		Rcpp::compileAttributes(package)
		devtools::build(package)
	}
	if(check)devtools::check(package,cran=cran)
	if(add)addPackage(package)
	if(push)pushPackage(package)
	if(install)installPackage(package)
}

installPackage <- function(package){
	DFile <- 	paste(sep="",
		package,
		"/DESCRIPTION"
	)
	x <- read.dcf(DFile)
	pkgGz <- paste(sep="",
		package,
		"_",
		x[1,"Version"],
		".tar.gz"
	)
	if(file.exists(pkgGz)){
		install.packages(pkgGz)
		requireNamespace(package)
	}
}

addPackage<-function(package){
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
	gitAdd(files)
}

pushPackage<-function(package){
	gitPush(paste(sep="","build:", package))
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

#'detach_package
#'@param pkg a 'character' describing a package to unload.
#'@param character.only a 'logical' flag, whether to skip deparsing/
#'@export
detach_package <- function(pkg, character.only = FALSE)
{
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}

#' rebuild
#' @export
rebuild<-function(p){
		buildPackage(p)
	require(p,character.only=TRUE)
}
