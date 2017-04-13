library(foreach)

library(data.table)

library(dplyr)



mat<-matrix(c(rnorm(100)),nrow=10)

rownames(mat) <- letters[1:10]

colnames(mat) <- letters[1:10]

dt <- data.frame(name =c("bob","bob","bob","john","john","greg","greg"),

group = c("a","b","c","d","f","e","c"), year1 = c(0,2,2,0,1,2,2),

year2 = c(1,2,3,4,5,3,3), stringsAsFactors = FALSE)

dt <- as.data.table(dt)



CID <- sort(unique(dt$name))# unique people



# I need to filter for every year and get only those names of people and group that values in years arent 0 so i #jam this result into a list

EachYearList <- foreach( years = c("year1","year2")) %do% {

tmp <- dt[, c("name", "group", years), with = FALSE][get(years) != 0][, unique(group), by = name]

colnames(tmp)[2] <- "group"

setkey(tmp, name)

}

# Make 2 level list so I can actually use it later

List <- lapply(1:length(EachYearList), function(y)

lapply(1:length(CID), function(x) as.character(EachYearList[[y]][J(CID[x]), nomatch = 0L][, group])))







final <- function(mat = "", list = "") {

CID <- sort(unique(dt$name))

temps <- apply(mat,2, sum)

avg <-foreach(years = 1 : length(list)) %:%

foreach(names = c(1:length(CID)), .combine = cbind) %do%  {  

apply(mat[list[[years]][[names]], , drop = FALSE],2, sum)/ temps

}

years <- c("year1","year2")

for (i in 1:length(years)){
colnames(avg[[i]]) <- CID
avg[[i]] <- melt(avg[[i]])
avg[[i]] <- subset(avg[[i]], value != 0)
colnames(avg[[i]])[1:3] <- c("group", "name", paste0(years[i]))
}
avgall <- Reduce(full_join, avg)

}

print(system.time(uh <-final(mat=mat,list=List)))

print(uh)

