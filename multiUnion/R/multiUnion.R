#'multiUnion-package
#'@aliases NULL
'_PACKAGE'

#'multiUnion
#'@description A union function 
#' which takes multiple arguments.
#'@param ... vectors of values
#'@export
multiUnion <- function(...){
	unique(c(...))
}

#'multiIntersect
#'@param ... vectors of values
#'@export
multiIntersect <- function(...){
	l1<-list(...)
	u<-do.call(multiUnion,l1)
	l2 <- lapply(l1,setdiff,x=u)
	l3 <- do.call(multiUnion,l2)
	setdiff(u,l3)
}
