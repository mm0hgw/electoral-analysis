
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
	buildDir='/tmp/linux',
	local='brain',
	rev=0.1,
	jobs=max(1,parallel::detectCores()-1)
){
	system(paste('rm -rf',buildDir))
	system(paste('mkdir -p',buildDir))
	system(paste(sep='','cp -R ~/git/linux/* ',buildDir))
	system(paste(sep='','cp ~/git/linux/.config ',buildDir,'/.config'))
	setwd(buildDir)
	system(paste(sep='','make deb-pkg -j',jobs,' LOCALVERSION=',
			local,' KDEB_PKGVERSION=',rev
		)
	)
	
}

buildKernel()

