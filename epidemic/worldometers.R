
source("R/worldometers.R")


countries <- c(UK = 16, KR = 27, IT = 17)

processCountry <- function(country, delta) {
    
    rawFile <- paste0("data/worldometers_", country, ".csv")
    rawTab <- read.csv(rawFile)

    tab <- calculateInactiveRecoveriesAndNew(rawTab)
    write.csv(tab, file = paste0(country, ".csv"))
    
    key <- (tab$Active.Cases != 0) & (tab$New.Cases != 0)
    newVSActive <- tab[key, c("Active.Cases", "New.Cases")]
    days <- delta + seq_along(tab$Total.Cases)[key]
    
    png(paste0(country, ".png"), 1024, 768)
    plot(newVSActive, log = "xy", type = "b")
    text(newVSActive, labels = days, pos = 2, cex = 0.75)
    dev.off()
}

lapply(seq_along(countries), function(x) processCountry(names(countries)[x], countries[x]))
