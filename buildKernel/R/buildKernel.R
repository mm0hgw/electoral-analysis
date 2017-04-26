
#' cloneKernel
#' @export
cloneKernel <- function(
	kernelCloneUrl='https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git',
	branch='master'
){
	system('mkdir -p ~/git')
	setwd('~/git')
	system2('git',c('clone','-b',branch,'--single-branch',kernelCloneUrl))
	setwd('~/git/linux/')
}

#' pullBuildDir
#' @export
pullBuildDir <- function(
	HDDDir='~/git/linux',
	buildDir='/tmp/linux'
){
	setwd(HDDDir)
	buildPackage::gitPull()
	system(paste('mkdir -p',buildDir))
	system(paste(sep='','cp -uR ',HDDDir,'/* ',buildDir))
	system(paste(sep='','cp ',HDDDir,'/.config ',buildDir,'/.config'))
	setwd(buildDir)
}

#' buildKernel
#' @export
buildKernel <- function(
	HDDDir='~/git/linux',
	buildDir='/tmp/linux',
	local='brain',
	rev=4.11,
	jobs=max(1,parallel::detectCores()-1),
	job='bindeb-pkg'
){
	pullBuildDir(HDDDir,buildDir)
	system(paste(sep='','nice -19 distcc-pump make ',job,' -j',jobs+1,
			' -l',jobs,' LOCALVERSION=',local,' KDEB_PKGVERSION=',rev
		)
	)
}

testKernel <- function(
){
}

#system.time(buildKernel())

