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

#'gitFetch
#'@param branch remote branch to fetch
#'@export
gitFetch <- function(branch = NULL) {
    if (is.null(branch)) {
        line <- gitStatus("-sb")[1]
        branch <- strsplit(line, "\\.\\.\\.")[[1]][2]
    }
    if (length(branch) == 1) {
        branch <- strsplit(branch, "/")[[1]]}
    stopifnot(length(branch) == 2)
    system2("git", c("fetch", branch[1], "--prune"))
    cat("done fetch\n")
    system2("git", c("checkout", branch[2]))
    cat("done checkout\n")
    flag <- system2("git", c("merge", "--ff-only", paste(branch, collapse = "/")))
    cat("done merge attempt\n")
    
    if (flag != 0) {
        cat("git", c("rebase", '--preserve-merges',paste(branch, collapse = "/")))
    }
}

gitStatus <- function(...) {
    system2("git", c("status", ...), stdout = TRUE)
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
#'@export
gitForcePush <- function(dest = "origin") {
    system(paste("git push --force --all", dest))
}

#'gitExpunge
#'@param fileName a 'character' vector of filenames to expunge from repo
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
gitRebase <- function(from, tool = "kdiff3") {
    system(paste("git rebase -i", from))
    while (system(paste(sep = "", "git mergetool --tool=", tool)) == 0) {
        while (system("git rebase --continue") == 0) 0
    }
}
