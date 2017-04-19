
cloneKernel <- function(
	kernelCloneUrl='https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git',
	branch='master'
){
	system('mkdir -p ~/git')
	setwd('~/git')
	system2('git',c('clone','-b',branch,'--single-branch',kernelCloneUrl))
	setwd('~/git/linux/')
}

buildKernel <- function(
	HDDDir='~/git/linux',
	buildDir='/tmp/linux',
	local='brain',
	rev=4.11,
	jobs=max(1,parallel::detectCores()-1),
	job='bindeb-pkg'
){
	setwd(HDDDir)
	buildPackage::gitPull()
	system(paste('rm -rf',buildDir))
	system(paste('mkdir -p',buildDir))
	system(paste(sep='','cp -uR ',HDDDir,'/* ',buildDir))
	system(paste(sep='','cp ',HDDDir,'/.config ',buildDir,'/.config'))
	setwd(buildDir)
	
	system(paste(sep='','make ',job,' -j',jobs,' LOCALVERSION=',
			local,' KDEB_PKGVERSION=',rev
		)
	)
	
}

#system.time(buildKernel())

