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
    system2("git", c("pull", "--rebase=preserve"))
}

getBranch <- function(branch=NULL){
    if (is.null(branch)) 
      	branch <- gitCurrentBranch()
    if (length(branch) == 1) 
        branch <- strsplit(branch, "/")[[1]]
    stopifnot(length(branch) == 2)
    branch
}

#'gitFetch
#'@param branch 'character' remote branch to fetch
#'@param merge 'logical'
#'@export
gitFetch <- function(branch = NULL, merge = TRUE) {
branch <- getBranch(branch)
    system2("git", c("fetch", branch, "--prune"))
    if(merge)gitMerge(branch)
   }

#'gitCheckout
#'@param branch remote branch to checkout
#'@export   
   gitCheckout <- function(branch){
    system2("git", c("checkout", paste(branch, collapse = "/")))
}

gitMerge <- function(branch=NULL){
    gitCheckout(branch[2])
branch <- getBranch(branch)
    flag <- system2("git", c("merge", "--ff-only", paste(branch, collapse = "/")))
  #  cat("done merge attempt\n")
    
    if (flag != 0) {
        system2("git", c("rebase", "--preserve-merges", paste(branch, collapse = "/")))
    gitRebaseMerge()
    }
}


#' gitPush
#'@export
gitPush <- function() {
    system2("git", c("push"))
}

#' gitCommit
#'@param comment 'character'
#'@export
gitCommit <- function(comment) {
    system2("git", c("commit", "-a", "-m", paste(sep = "", "\"", comment, "\"")))
}

#' gitAdd
#'@param filelist 'character'
#'@description Git add, commit and push
#'@export
gitAdd <- function(filelist) {
    system2("git", c("add", filelist))
}

hashFinder <- function(line) if (nchar(line[1]) == 40) {
    TRUE
} else {
    FALSE
}
sizeSorter <- function(line) as.integer(line[5])

#'ls.git.hashes
#'@param pattern 'character' a regexp to filter output
#'@export
ls.git.hashes <- function(pattern = ".*") {
    z1 <- system("git verify-pack -v .git/objects/pack/pack-*.idx", intern = TRUE)
    z2 <- strsplit(z1, " ")
    z3 <- z2[sapply(z2, function(x) nchar(x[1]) == 40)]
    z4 <- as.integer(sapply(z3, "[", 5))
    names(z4) <- sapply(z3, "[", 1)
    z5 <- z4[grep(pattern, names(z4))]
    sort(z5, decreasing = TRUE)
}

#'ls.git.files
#'@param pattern 'character' a regexp to filter output
#'@export
ls.git.files <- function(pattern = ".*") {
    z1 <- system(intern = TRUE, "git rev-list --all --objects")
    z2 <- strsplit(z1, " ")
    z3 <- z2[sapply(z2, length) == 2]
    z4 <- sapply(z3, "[", 2)
    sort(grep(pattern, value = TRUE, unique(z4)))
}

#'hashLookup
#'@export
hashLookup <- function() {
    z1 <- system(intern = TRUE, "git rev-list --all --objects")
    z2 <- strsplit(z1, " ")
    z3 <- z2[sapply(z2, function(x) length(x) == 2 && x[2] != "")]
    names(z3) <- sapply(z3, "[", 1)
    z3
}

#'gitForcePush
#'@param branch a 'character' label of the branch to use
#'@export
gitForcePush <- function(branch = NULL) {
    if (is.null(branch)) {
        branch <- gitCurrentBranch()
    }
    
    if (length(branch) == 1) {
        branch <- strsplit(branch, "/")[[1]]
    }
    system(paste("git push --force", branch))
}

#'gitExpunge
#'@param fileName a 'character' vector of filenames to expunge from repo
#'@param n max number to handle
#'@importFrom utils head
#'@export
gitExpunge <- function(fileName, n = 200) {
    while (length(fileName) > n) {
        gitExpunge(head(fileName, n = n))
        fileName <- fileName[-seq(n)]
    }
    system(paste(sep = "", "git filter-branch --tag-name-filter cat ", "--index-filter 'git rm -r --cached --ignore-unmatch ", 
        paste(collapse = " ", fileName), "' --prune-empty -f -- --all"))
}

#'gitPurge
#'@export
gitPurge <- function() {
    system("rm -rf .git/refs/original")
    system("git reflog expire --expire=now --all")
    system("git gc --prune=now")
    system("git gc --aggressive --prune=now")
}

#'gitRebase
#'@param from identifier to rebase upon.
#'@param tool mergetool to use
#'@export
gitRebase <- function(from) {
    system(paste("git rebase -i", from))
    gitRebaseMerge()
}



gitRebaseMerge <- function( ) {
        while (!(system2("git",c( "rebase", "--continue"))==0)) {
    while (!(system2('git','mergetool',stdout=TRUE) ==0)) 0
    }
}

gitStatus <- function(...) {
    system2("git", c("status", ...), stdout = TRUE)
}

gitCurrentBranch <- function(){
        line <- gitStatus("-sb")[1]
        branch <- strsplit(line, "\\.\\.\\.")[[1]][2]
        strsplit(branch,' ')[[1]][1]
}
