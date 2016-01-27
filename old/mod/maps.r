# maps and fun stuff
require(maptools)


OSGB1936 <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs")

councils<-readShapePoly(
	"../boundary-line/Data/GB/district_borough_unitary_region",
	proj4string=OSGB1936)

scot_councils<-councils[grep("^S",councils$CODE),]

