
source("R/worldometers.R")


countries <- c("ES", "IT", "KR",'NL', "UK", "US")

processCountry <- function(country) {
    
    rawFile <- paste0("data/worldometers_", country, ".csv")
    rawTab <- read.csv(rawFile, stringsAsFactors = FALSE)
    rawTab$Date <- as.Date(rawTab$Date)
write.csv(rawTab[,initColIDs],rawFile)
    
    tab <- calculateInactiveRecoveriesAndNew(rawTab)
    write.csv(tab, file = paste0(country, ".csv"))
    
    key <- (tab$Active.Cases != 0) & (tab$New.Cases != 0)
    newVSActive <- tab[, c("Active.Cases", "New.Cases")]
dateKeyStarts <- key & !c(FALSE,key[-length(key)])
dateKeyMids <- as.numeric(format(tab$Date,'%d')) %in% c(1,15)
dateKey <- (dateKeyStarts | dateKeyMids) & key
    
    png(paste0(country, ".png"), 1024, 768)
    plot(newVSActive[key, ], log = "xy", type = "l", main = paste("New cases vs active cases on logarithmic scales in", 
        country))
    text(newVSActive[dateKey, ], labels = format(tab$Date[dateKey],'%b %d'), pos =
3)
    dev.off()
    
    
    tab
}

tabList <- lapply(countries, processCountry)
names(tabList) <- countries

png("Infection.Factor.png", 1024, 768)
plotTabList(tabList, c("Date", "Infection.Factor"), main = "Infection Factor vs Date")
dev.off()
