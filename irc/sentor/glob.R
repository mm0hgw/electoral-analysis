showGlob <- function(...){
files <- Sys.glob(...)
lapply(files,return)
}

loadGlob <- function(...){
files <- Sys.glob(...)
lapply(files,open,sep='\n',what='character')
}