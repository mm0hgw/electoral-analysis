
source("R/worldometers.R")


countries <- c("UK", "KR", "IT")

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
}

lapply(countries, processCountry)
