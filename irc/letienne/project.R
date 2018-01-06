#cleanup

closeAllConnections()

 

 

#Projects

ToPs <- c("rest_TableOfProjects.Rda",

"failmefilename.Rda")

 

getToP <- function(TableOfProjectsFile=ToPs[2]){

if(length(TableOfProjectsFile)!=1 || typeof(TableOfProjectsFile)!='character' || !file.exists(TableOfProjectsFile)){

 stop(paste("ERROR: Project',TableOfProjectsFile,'is not good, breaking!")) # more useful error message

}

readRDS(file=TableOfProjectsFile)

}

 

ProjectNames<-c("DevRunTest" ,

"sdhsdsddssd",

"AllCityCapture")

 

doProject <-function(ToP,ProjectName=ProjectNames[2]){ # make ProjectName a parameter

if (typeof(ProjectName) != 'character'){

 stop(paste("ERROR: Bad Project Name:",ProjectName))

 }

TableOfProjects <- getToP(ToP)

  if(sum(ProjectName==rownames(TableOfProjects))!=1){ # rownames are unique

   stop(paste("ERROR:',ProjectName,'is not present, breaking!") # not present is the only possible error

   }

projectData <- TableOfProjects[ProjectName,]

 # checks done, thing is doable

}

 

lapply(ToPs,function(ToP){

    lapply(ProjectNames,function(ProjectName){

doProject(Top,ProjectName)

})

})

