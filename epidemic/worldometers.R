
source('R/worldometers.R')

UKRaw <- read.csv('data/worldometers_UK.csv')
UK <- calculateInactiveRecoveriesAndNew(UKRaw)

write.csv(UK,file='UK.csv')

UKNewVSActive <- UK[,c('Active.Cases','New.Cases')]

key <- logFilter(UKNewVSActive)

png('UK.png',1024,768)
plot(UKNewVSActive[key,],log='xy',type='b')
text(UKNewVSActive[key,],labels=gsub('2020-','',UK$Date[key]),pos=2,cex=0.75)
dev.off()
