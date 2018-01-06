# cleanup
closeAllConnections()

# Projects
ToPs <- c("rest_TableOfProjects.Rda", "failmefilename.Rda")

getToP <- function(TableOfProjectsFile) {
    if (length(TableOfProjectsFile) != 1 || typeof(TableOfProjectsFile) != "character" || 
        !file.exists(TableOfProjectsFile)) {
        stop(paste("ERROR: Project",TableOfProjectsFile,"is not good, breaking!"))  # more useful error message
    }
    readRDS(file = TableOfProjectsFile)
}

ProjectNames <- c("DevRunTest", "sdhsdsddssd", "AllCityCapture")

doProject <- function(TableOfProjects, ProjectName) {
    # make ProjectName a parameter
    if (typeof(ProjectName) != "character") 
        stop(paste("ERROR: Bad Project Name:", ProjectName))
    if (sum(ProjectName == rownames(TableOfProjects)) != 1) # rownames are unique
        stop(paste("ERROR:",ProjectName,"is not present, breaking!"))  # not present is the only possible error
    projectData <- TableOfProjects[ProjectName, ]
    # checks done, thing is doable
}

lapply(ToPs, function(ToP) {
    lapply(ProjectNames, function(ProjectName) {
        try(doProject(getToP(Top), ProjectName))
    })
})
