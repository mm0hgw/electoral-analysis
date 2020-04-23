
source("R/worldometers.R")
if (!dir.exists("out/charts")) dir.create("out/charts", recursive = TRUE)
if (!dir.exists("out/tables")) dir.create("out/tables", recursive = TRUE)

countries <- c("CN", "DE", "ES", "FR", "IT", "KR", "NL", "SE", "UK", "US")

processCountry <- function(country) {
    
    rawFile <- paste0("data/worldometers_", country, ".csv")
    rawTab <- read.csv(rawFile, stringsAsFactors = FALSE)
    rawTab$Date <- as.Date(rawTab$Date)
    write.csv(rawTab[, initColIDs], rawFile)
    
    
    tab <- calculateInactiveRecoveriesAndNew(rawTab)
    write.csv(tab, file = paste0("out/tables/", country, ".csv"))
    
    warnKey <- sapply(seq(nrow(tab)), function(x) {
        any(tab[x, -8] < 0)
    })
    
    if (any(warnKey == TRUE)) {
        warnKey2 <- warnKey | c(warnKey[-1], 0) | c(warnKey[c(-1, -2)], 0, 0)
        write.csv(tab[warnKey2, ], file = paste0("out/warnings.", country, ".csv"))
    }
    
    # clip rows with <500 cases
    
    tab <- tab[rawTab$Total.Cases >= 500, ]
    
    tab
}

tabList <- lapply(countries, processCountry)
names(tabList) <- countries


tabListSplit <- split(tabList, ceiling(seq_along(tabList)/6))

lapply(seq_along(tabListSplit), function(x) {
    filename1 <- paste0("out/charts/Infection.Factor.", x, ".png")
    png(filename1, 800, 600)
    plotTabList(tabListSplit[[x]], c("Date", "Infection.Factor"), main = "Infection Factor", 
        ylim = c(0, 1))
    dev.off()
    filename2 <- paste0("out/charts/New.vs.Active.", x, ".png")
    png(filename2, 800, 600)
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
    filename3 <- paste0("out/charts/Mortality.", x, ".png")
    png(filename3, 800, 600)
    plotTabList(tabListSplit[[x]], c("Date", "Mortality.Rate"), main = "Mortality Rate", 
        ylim = c(0, 1))
    dev.off()
    filename3 <- paste0("out/charts/Activity.Rate.", x, ".png")
    png(filename3, 800, 600)
    plotTabList(tabListSplit[[x]], c("Date", "Activity.Rate"), main = "Active / Total Cases")
    dev.off()
})

countries <- c("IT", "UK", "SE", "KR", "FR", "ES")

png("out/charts/New.vs.Active.png", 800, 600)
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

lapply(seq_along(tabList), function(x) {
    filename1 <- paste0("out/charts/Active.Cases.and.New.Deaths.", names(tabList)[x], 
        ".png")
    png(filename1, 800, 600)
    x.Date <- tabList[[x]]$Date
    y.Active.Cases <- tabList[[x]]$Active.Cases
    y.New.Deaths <- tabList[[x]]$New.Deaths
    plot(x = x.Date, y = y.Active.Cases/max(y.Active.Cases), main = paste("Active Cases and New Deaths in", 
        names(tabList)[x]), type = "l", ylab = "Proportion of peak")
    lines(x = x.Date, y = y.New.Deaths/max(y.New.Deaths), col = 2)
    legend("right", col = seq(2), pch = 1, legend = c(paste("Active Cases", max(y.Active.Cases)), 
        paste("New Deaths", max(y.New.Deaths))))
    dev.off()
})
