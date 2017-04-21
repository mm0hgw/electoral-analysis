library(lattice)
library(buildPackage)

pngFile <- 'test.png'

png(pngFile)
ngroups <- length(unique(barley$site)) + 1 
bwplot(yield ~ variety, data = barley, box.width = 1/ ngroups, 
	groups = year, scales=(x=list(rot=45)), 
	auto.key = list(points = FALSE, rectangles = TRUE, space = "right"), 
	par.settings=list(box.rectangle = list(col=c("red", "green"), lwd=3), 
	superpose.polygon=list(col=c("green", "red"), pch=c(15,15)) ), 
	panel.groups = function(x, y, ..., group.number) 
	{ 
		panel.bwplot(x + (group.number-1.5)/ngroups, y, ...) 
	}, 
	panel=function(...) 
	{
	 panel.grid(h = -1, v=0) 
	 panel.superpose(par.settings=list(box.rectangle=list(col=c("green", "red"))), ...) 
	} 
)
dev.off()
gitAdd(pngFile)
gitPush(pngFile,pngFile)
