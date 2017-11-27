
#' cloneKernel
#' @export
cloneKernel <- function(kernelCloneUrl = "https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git", 
    branch = "master") {
    system("mkdir -p ~/git")
    setwd("~/git")
    system2("git", c("clone", "-b", branch, "--single-branch", kernelCloneUrl))
    setwd("~/git/linux/")
}

#' pullBuildDir
#' @export
pullBuildDir <- function(HDDDir = "~/git/linux", buildDir = "/tmp/linux") {
    setwd(HDDDir)
    buildPackage::gitPull()
    system(paste("mkdir -p", buildDir))
    system(paste(sep = "", "cp -uR ", HDDDir, "/* ", buildDir))
    system(paste(sep = "", "cp ", HDDDir, "/.config ", buildDir, "/.config"))
    setwd(buildDir)
}

#' buildKernel
#' @export
buildKernel <- function(HDDDir = "~/git/linux", buildDir = "/tmp/linux", local = system(intern = TRUE, 
    "uname -n"), jobs = max(1, parallel::detectCores() - 1), job = "bindeb-pkg") {
    rev_ <- system(paste("cat ", HDDDir, "/Makefile|grep -E '(^VERSION|^PATCHLEVEL|^SUBLEVEL|^EXTRAVERSION)'|awk '{print $3;}'", 
        sep = ""), intern = TRUE)
    rev <- paste(paste(rev_[1:3], collapse = "."), rev_[4], sep = "")
    pullBuildDir(HDDDir, buildDir)
    system(paste(sep = "", "nice -19 distcc-pump make ", job, " -j", jobs + 1, " -l", 
        jobs, " LOCALVERSION=", paste('-',local,sep=''), " KDEB_PKGVERSION=", rev))
    system(paste(sep = "", "cp -u ", buildDir, "/.config ", HDDDir, "/.config"))
    deb <- list.files(paste(sep='',buildDir,'/..'),pattern='.deb$')
    bindeb <- deb[-grep('-dbg',deb)]
    system(paste('sudo dpkg -i',paste(collapse=' ',bindeb)))
}

testKernel <- function() {
}

# system.time(buildKernel())

