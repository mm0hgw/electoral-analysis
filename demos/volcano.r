# http://stackoverflow.com/questions/1896419/plotting-a-3d-surface-plot-with-contour-map-overlay-using-r

require(rgl)

volcano_rgl_plot <- function(){
  data(volcano)
  z <- 2 * volcano # Exaggerate the relief
  x <- 10 * (1:nrow(z)) # 10 meter spacing (S to N)
  y <- 10 * (1:ncol(z)) # 10 meter spacing (E to W)
  zlim <- range(z)
  zlen <- zlim[2] - zlim[1] + 1
  colorlut <- terrain.colors(zlen,alpha=0) # height color lookup table
  col <- colorlut[ z-zlim[1]+1 ] # assign colors to heights for each point
  open3d()
  rgl.surface(x, y, z, color=col, alpha=0.75, back="lines")
  colorlut <- heat.colors(zlen,alpha=1) # use different colors for the contour map
  col <- colorlut[ z-zlim[1]+1 ] 
  rgl.surface(x, y, matrix(1, nrow(z), ncol(z)),color=col, back="fill")
}

trig_plot <- function(){
  temp <- seq(-pi, 0, length = 50)
  x <- c(rep(1, 50) %*% t(cos(temp)))
  y <- c(cos(temp) %*% t(sin(temp)))
  z <- c(sin(temp) %*% t(sin(temp)))
  scatterplot3d(x, y, z, highlight.3d=TRUE,
   col.axis="blue", col.grid="lightblue",
   main="scatterplot3d - 2", pch=20)
}


require(akima)
require(rgl)

#
build_points <- function(p,V,N,scale=1,...){
	x<-p$Long
	y<-p$Lat
	a<-calculate_a(V,N)
	pop_mean <- calculate_a(sum(V),sum(N))
	pop_sd <- custom_sd(a,pop_mean)
	z <- (a - pop_mean) * scale / pop_sd
	s<-interp(x,y,z,xo=dom(x),yo=dom(y),...)
	s
}

contours <- 8192
heatlut <- heat.colors(contours,alpha=1)
terrainlut <- terrain.colors(contours,alpha=1)
plot_terrain_color <- function(s,colorlut=terrainlut,...){
	zlim <- range(s$z[!is.na(s$z)])
	zlen <- zlim[2] - zlim[1]
	zmin <- zlim[1]
	contour <- zlen / contours
	col<-colorlut[ceiling((s$z-zmin)/contour)]
	surface3d(s$x,s$y,s$z,col=col,...)
}
