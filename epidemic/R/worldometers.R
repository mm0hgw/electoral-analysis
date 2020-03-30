

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
    x
}

logFilter <- function(x) {
    sapply(seq(nrow(x)), function(n) all(x[n, ] > 0))
}
