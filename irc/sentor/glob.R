showGlob <- function(...){
Sys.glob(...)
}

loadGlob <- function(...){
files <- Sys.glob(...)
lapply(files,open,sep='\n',what='character')
}
