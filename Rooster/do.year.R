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

CID <- sort(unique(dt$name))# unique people
names(CID) <- CID
group <- dt$group
names(group) <- group
temps <- colSums(mat)

do.dt <- function(dt){
	CID <- sort(unique(dt$name))# unique people
	names(CID) <- CID
	group <- dt$group
	names(group) <- group
	n <- length(group)
	temps <- colSums(mat)
	years <- grep('^year',names(dt))
	names(years) <- names(dt)[years]
	notYears <- setdiff(seq_along(names(dt)),years)
	names(notYears) <- names(dt)[notYears]
	
	procGroups <- do.call(rbind, 
		lapply(group,
			function(x){
				mat[x,,drop=FALSE] / temps
			}
		)
	)
	do.call(rbind,
		lapply(seq_along(years),
			function(yearI){
				if(dt[[years[yearI]]]==0)
					return(vector())
				do.call(rbind,
					lapply(CID,
						function(noob){
							groupMask <- dt[['name']] %in% noob
							if(any(groupMask)){
								print(names(year))
								data.frame(noob=as.factor(rep(noob,n)),
									year=as.factor(rep(names(years)[yearI],n)),
									group=as.factor(group),
									as.numeric(colSums(procGroups[groupMask,,drop=FALSE]))
								)
								
							}else{
								vector()
							}
						}
					)
				)
			}
		)
	)
}

print(system.time(uh2<-do.dt(dt)))
print(uh2)
