showGlob <- function(...){
Sys.glob(...)
}

loadGlob <- function(...){
files <- Sys.glob(...)
lapply(files,scan,sep='\n',what='character')
}
