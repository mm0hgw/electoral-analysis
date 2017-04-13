library(foreach)

library(data.table)

 

mat<-matrix(c(rnorm(100)),nrow=10)

rownames(mat) <- letters[1:10]

colnames(mat) <- letters[1:10]

dt <- data.frame(name =c("bob","bob","bob","john","john","greg","greg"),

group = c("a","b","c","d","f","e","c"), year1 = c(0,2,2,0,1,2,2),

year2 = c(1,2,3,4,5,3,3), stringsAsFactors = FALSE)

dt <- as.data.table(dt)

 

 

CID <- sort(unique(dt$name))  # unique people

 

# I need to filter for every year and get only those names of people and group that values in years arent 0 so i #jam this result into a list

EachYearList <- foreach( years = c("year1","year2")) %do% {

  tmp <- dt[, c("name", "group", years), with = FALSE][get(years) != 0][, unique(group), by = name]

  colnames(tmp)[2] <- "group"

  setkey(tmp, name)

}

# Make 2 level list so I can actually use it later

  List <- lapply(1:length(EachYearList), function(y)

    lapply(1:length(CID), function(x) as.character(EachYearList[[y]][J(CID[x]), nomatch = 0L][, group])))

 

#Now I basically need to to apply this formula  

temps <-  apply(mat  ,2, sum)

#And then I used very complex sheenanigans here, but basically i run through each element of list this function

#and spit out full table of

yaiks <- apply(mat[List[[1]][[2]], , drop = FALSE],2, sum)/ temps  

# anyways some more sheenigans,

#to spit out data.frame/data.table. Since List[[1]][[2]] represents year1 and "john"

#we would end up with result data.frame

#So we run it through all and rbind them and you get the picture I hope.

 

result <- as.data.frame(yaiks)

result$year <- rep("year1", 10)

result$group <- rownames(result)

result$name <- rep("john",10)

result

