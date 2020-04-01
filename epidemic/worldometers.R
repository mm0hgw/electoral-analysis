
source("R/worldometers.R")


countries <- c("CN", "DE", "ES", "IT", "KR", "NL", "UK", "US")

processCountry <- function(country) {
    
    rawFile <- paste0("data/worldometers_", country, ".csv")
    rawTab <- read.csv(rawFile, stringsAsFactors = FALSE)
    rawTab$Date <- as.Date(rawTab$Date)
    write.csv(rawTab[, initColIDs], rawFile)
    
    
    tab <- calculateInactiveRecoveriesAndNew(rawTab)
    write.csv(tab, file = paste0(country, ".csv"))
    
    # clip rows with <500 cases
    
    tab <- tab[rawTab$Total.Cases >= 500, ]
    
    key <- (tab$Active.Cases != 0) & (tab$New.Cases != 0)
    newVSActive <- tab[, c("Active.Cases", "New.Cases")]
    dateKeyStarts <- key & !c(FALSE, key[-length(key)])
    dateKeyMids <- as.numeric(format(tab$Date, "%d")) %in% c(1, 15)
    dateKey <- (dateKeyStarts | dateKeyMids) & key
    
    # png(paste0(country, '.png'), 1024, 768) plot(newVSActive[key, ], log = 'xy',
    # type = 'l', main = paste('New cases vs active cases on logarithmic scales in',
    # country)) text(newVSActive[dateKey, ], labels = format(tab$Date[dateKey], '%b
    # %d'), pos = 3) dev.off()
    
    tab
}

tabList <- lapply(countries, processCountry)
names(tabList) <- countries


tabListSplit <- split(tabList, ceiling(seq_along(tabList)/4))

lapply(seq_along(tabListSplit), function(x) {
    filename1 <- paste0("Infection.Factor.", x, ".png")
    png(filename1, 1024, 768)
    plotTabList(tabListSplit[[x]], c("Date", "Infection.Factor"), main = "Infection Factor vs Date")
    dev.off()
    filename2 <- paste0("New.vs.Active.", x, ".png")
    png(filename2, 1024, 768)
    New.vs.Active <- lapply(tabListSplit[[x]], function(x) {
        x[x$New.Cases != 0, ]
    })
    plotTabList(New.vs.Active, c("Active.Cases", "New.Cases"), main = "New Cases vs Active Cases", 
        log = "xy")
    lapply(seq_along(New.vs.Active), function(x) {
        key <- as.numeric(format(New.vs.Active[[x]]$Date, "%d")) %in% c(1, 15)
        text(New.vs.Active[[x]][key, c("Active.Cases", "New.Cases")], format(New.vs.Active[[x]][key, 
            "Date"], "%b %d"), col = x)
    })
    dev.off()
})



countries <- c('IT','UK','ES','DE','NL','US')

    png("New.vs.Active.png", 1024, 768)
    New.vs.Active <- lapply(tabList[countries], function(x) {
        x[x$New.Cases != 0, ]
    })
    plotTabList(New.vs.Active, c("Active.Cases", "New.Cases"), main = "New Cases vs Active Cases", 
        log = "xy")
    lapply(seq_along(New.vs.Active), function(x) {
        key <- as.numeric(format(New.vs.Active[[x]]$Date, "%d")) %in% c(1, 15)
        text(New.vs.Active[[x]][key, c("Active.Cases", "New.Cases")], format(New.vs.Active[[x]][key, 
            "Date"], "%b %d"), col = x)
    })
    dev.off()

