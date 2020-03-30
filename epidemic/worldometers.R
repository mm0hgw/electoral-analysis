
source("R/worldometers.R")


countries <- c(UK = 16, KR = 27, IT = 17)

processCountry <- function(country, delta) {
    
    rawFile <- paste0("data/worldometers_", country, ".csv")
    rawTab <- read.csv(rawFile)
    
    tab <- calculateInactiveRecoveriesAndNew(rawTab)
    write.csv(tab, file = paste0(country, ".csv"))
    
    key <- (tab$Active.Cases != 0) & (tab$New.Cases != 0)
    newVSActive <- tab[, c("Active.Cases", "New.Cases")]
    days <- delta + seq_along(tab$Total.Cases)
    
    png(paste0(country, ".png"), 1024, 768)
    plot(newVSActive[key, ], log = "xy", type = "b", main = country)
    text(newVSActive[key, ], labels = days[key], pos = 2, cex = 0.75)
    dev.off()
    
    key2 <- (tab$Active.Cases != 0)
    
    png(paste0(country, "2.png"), 1024, 768)
    plot(days[key2], tab$New.Cases[key2]/tab$Active.Cases[key2], type = "b", main = country)
    dev.off()
}

lapply(seq_along(countries), function(x) processCountry(names(countries)[x], countries[x]))
