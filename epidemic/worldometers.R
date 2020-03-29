
source("R/worldometers.R")


UKRaw <- read.csv("data/worldometers_UK.csv")
UK <- calculateInactiveRecoveriesAndNew(UKRaw)

write.csv(UK, file = "UK.csv")

UKNewVSActive <- UK[, c("Active.Cases", "New.Cases")]

UKDays <- 16 +seq_along(UK$Total.Cases)

png("UK.png", 1024, 768)
plot(UKNewVSActive, log = "xy", type = "b")
text(UKNewVSActive, labels = UKDays, pos = 2, cex = 0.75)
dev.off()

KRRaw <- read.csv("data/worldometers_KR.csv")
KR <- calculateInactiveRecoveriesAndNew(KRRaw)

write.csv(KR, file = "KR.csv")

KRNewVSActive <- KR[, c("Active.Cases", "New.Cases")]

KRDays <- 27 +seq_along(KR$Total.Cases)

png("KR.png", 1024, 768)
plot(KRNewVSActive, log = "xy", type = "b")
text(KRNewVSActive, labels = KRDays, pos = 2, cex = 0.75)
dev.off()
