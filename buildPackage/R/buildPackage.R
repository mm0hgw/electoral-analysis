#'build-package
#'@aliases NULL
#'@importFrom utils install.packages tail
#'@import devtools
"_PACKAGE"

#'detachPackage
#'@param package a 'character' describing a package to unload.
#'@param character.only a 'logical' flag, whether to skip deparsing/
#'@export
detachPackage <- function(package, character.only = FALSE) {
    if (!character.only) {
        package <- deparse(substitute(package))
    }
    x <- paste("package", package, sep = ":")
    while (x %in% search()) {
        detach(x, unload = TRUE, character.only = TRUE)
    }
}

#' gitClone
#'@param url the url to clone
#'@export
gitClone <- function(url) {
    system(paste("git", "clone", "--progress", url))
}

#' gitPull
#'@description Execute a git pull
#'@export
gitPull <- function() {
    system2("git", c("pull"))
}

#' gitPush
#'@param comment 'character'
#'@description Git add, commit and push
#'@export
gitPush <- function(comment) {
    system2("git", c("pull"))
    system2("git", c("commit", "-a", "-m", paste(sep = "", "\"", comment, "\"")))
    system2("git", c("push"))
}

#' gitAdd
#'@param filelist 'character'
#'@description Git add, commit and push
#'@export
gitAdd <- function(filelist) {
    system2("git", c("add", filelist))
}

#' findPackages
#'@description Provide package directories
#'@param path 'character'
#'@export
findPackages <- function(path = ".") {
    l <- list.dirs(path = path)
    l[unlist(lapply(paste(l, "/DESCRIPTION", sep = ""), file.exists))]
}

#' buildPackage
#'@param package 'character'
#'@param pull 'logical'
#'@param build 'logical'
#'@param check 'logical'
#'@param cran 'logical'
#'@param add 'logical'
#'@param push 'logical'
#'@param install 'logical'
#'@description Build a package.
#'@import devtools
#'@import Rcpp
#' @importFrom formatR tidy_dir
#'@export
buildPackage <- function(package, pull = build, build = check, check = cran, cran = FALSE, 
    add = build, push = cran, install = build) {
    Rdir <- paste(sep = "", package, "/R")
    detachPackage(package, TRUE)
    if (pull) 
        gitPull()
    if (build) {
        # RcppExports gets b0rked by formatR, so remove and regenerate  
    RcppFiles <- paste(sep='',package,'/',c('R','src'),'/RcppExports.',c('R','cpp'))
    lapply(RcppFiles[sapply(RcppFiles,file.exists)],file.remove)
        formatR::tidy_dir(Rdir)
        devtools::document(package)
        Rcpp::compileAttributes(package)
        devtools::build(package)
    }
    if (check) 
        devtools::check(package, cran = cran)
    if (add) 
        addPackage(package)
    if (push) 
        pushPackage(package)
    if (install) 
        installPackage(package)
}

pushPackage <- function(package) {
    DFile <- paste(sep = "", package, "/DESCRIPTION")
    x <- read.dcf(DFile)
    gitPush(paste("buildPackage", package, x[1, "Version"]))
}

installPackage <- function(package) {
    DFile <- paste(sep = "", package, "/DESCRIPTION")
    x <- read.dcf(DFile)
    pkgGz <- paste(sep = "", package, "_", x[1, "Version"], ".tar.gz")
    if (file.exists(pkgGz)) {
        install.packages(pkgGz)
        pkg <- strsplit(package, "/")[[1]]
        requireNamespace(pkg[length(pkg)])
    }
}

addPackage <- function(package) {
    # list roxygen generated files
    roxygenfiles <- paste(sep = "", package, c("/DESCRIPTION", "/NAMESPACE", "/R/RcppExports.R", 
        "/src/RcppExports.cpp", paste(sep = "", "/R/", list.files(paste(sep = "", 
            package, "/R/"), pattern = "*.R")), paste(sep = "", "/man/", list.files(paste(sep = "", 
            package, "/man/"), pattern = "*.Rd")), paste(sep = "", "/src/", list.files(paste(sep = "", 
            package, "/src/"), pattern = "(*.c)|(*.cpp)|(*.h)|(*.hpp)|"))))
    files <- roxygenfiles
    # print(files)
    files <- c(files[sapply(files, file.exists)])
    gitAdd(files)
}
