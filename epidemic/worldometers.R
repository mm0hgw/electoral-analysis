
source("R/worldometers.R")


countries <- c("IT", "KR", "UK", "US")

processCountry <- function(country) {
    
    rawFile <- paste0("data/worldometers_", country, ".csv")
    rawTab <- read.csv(rawFile)
    
    tab <- calculateInactiveRecoveriesAndNew(rawTab)
    write.csv(tab, file = paste0(country, ".csv"))
    
    key <- (tab$Active.Cases != 0) & (tab$New.Cases != 0)
    newVSActive <- tab[, c("Active.Cases", "New.Cases")]
    
    png(paste0(country, ".png"), 1024, 768)
    plot(newVSActive[key, ], log = "xy", type = "b", main = paste("New cases vs active cases on logarithmic scales in", 
        country))
    text(newVSActive[key, ], labels = tab$Infection.Day[key], pos = 2, cex = 0.75)
    dev.off()
    
    png(paste0(country, "2.png"), 1024, 768)
    plot(tab[, c("Infection.Day", "Infection.Factor")], type = "l", main = paste("Infection factor in", 
        country))
    dev.off()

tab
}

tabList <- lapply(countries, processCountry)
names(tabList)<-countries