

initColIDs <- c("Date", "Infection.Day", "Total.Cases", "Active.Cases", "Deaths")
colIDs <- c("Date", "Infection.Day", "Total.Cases", "Active.Cases", "Deaths", "Inactive.Cases", 
    "Recoveries", "New.Cases", "New.Inactive.Cases", "New.Deaths", "New.Recoveries")

calculateRateOfChange <- function(x) {
    n <- length(x)
    c(0, x[-1] - x[-n])
}

calculateInactiveRecoveriesAndNew <- function(tab) {
    stopifnot(all(initColIDs %in% colnames(tab)))
    x <- tab[, initColIDs]
    xCols <- nrow(x)
    x$Inactive.Cases <- x$Total.Cases - x$Active.Cases
    x$Recoveries <- x$Inactive.Cases - x$Deaths
    x$New.Cases <- calculateRateOfChange(x$Total.Cases)
    x$New.Active.Cases <- calculateRateOfChange(x$Active.Cases)
    x$New.Inactive.Cases <- calculateRateOfChange(x$Inactive.Cases)
    x$New.Deaths <- calculateRateOfChange(x$Deaths)
    x$New.Recoveries <- calculateRateOfChange(x$Recoveries)
    x$Infection.Factor <- x$New.Cases/x$Active.Cases
    x$Infection.Factor[x$Active.Cases==0] <- 0
    x
}

logFilter <- function(x) {
    sapply(seq(nrow(x)), function(n) all(x[n, ] > 0))
}

graphRange <- function(tabList) {
    stopifnot(all(sapply(tabList, ncol) == 2))
    xy <- do.call(rbind, tabList)
    list(xlim = range(xy[, 1]), ylim = range(xy[, 2]))
}

plotTabList <- function(rawTabList, fields, ...) {
    tabList <- lapply(rawTabList, "[", TRUE, fields)
    args <- c(list(...), graphRange(tabList), type = "l")
    args$x <- tabList[[1]]
    do.call(plot, args)
    lapply(seq_along(tabList)[-1], function(x) {
        lines(tabList[[x]], col = x)
    })
    legend("right", legend = names(tabList), col = seq_along(tabList), pch = 1)
}
