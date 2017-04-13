library(foreach)

library(data.table)

library(dplyr)


mat<-matrix(c(rnorm(100)),nrow=10)

rownames(mat) <- letters[1:10]

colnames(mat) <- letters[1:10]

dt <- data.frame(name =c("bob","bob","bob","john","john","greg","greg"),

group = c("a","b","c","d","f","e","c"), year1 = c(0,2,2,0,0,2,2),

year2 = c(1,2,3,4,5,3,3), stringsAsFactors = FALSE)

dt <- as.data.table(dt)

CID <- sort(unique(dt$name))  # unique people
names(CID) <- CID
group <- dt$group
names(group) <- group
temps <-  colSums(mat)

do.year <- function(year){
	do.call(rbind,
		lapply(CID,
			function(noob){
				groupMask <- dt[['name']] %in% noob & dt[[year]]!=0
				print(group[groupMask])
				if(any(groupMask)){
					colSums(mat[group[groupMask],,drop=FALSE]) / temps
				}else{
					vector()
				}
			}
		)
	)
}

print(system.time(uh2<-lapply(c(year1='year1',year2='year2'),do.year))
print(uh2)
