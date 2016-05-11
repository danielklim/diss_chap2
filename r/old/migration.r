migrations <- subset(person.events, event == "migrate")
migration.table <- table(migrations$meta1, migrations$meta2, dnn=c("from", "to"))
#f_city_name <- function(x){city.agents$city_name[city.agents$id==x]}
#rownames(migration.table) <- sapply(rownames(migration.table), f_city_name)
#colnames(migration.table) <- sapply(colnames(migration.table), f_city_name)
print(migration.table)

# river plot data
migration.edges <- matrix(NA, nrow = prod(dim(migration.table)), ncol = 7, dimnames = list(NULL, c('N1', 'N2', 'Value', 'x', 'y', 'xend', 'yend')))
i = 1
for(rn in rownames(migration.table)){
	for(cn in colnames(migration.table)){
		## riverplot
		migration.edges[i, 'N1'] = rn
		migration.edges[i, 'N2'] = cn
		migration.edges[i, 'Value'] = migration.table[rn, cn]

		## gmap
		c_fr <-  city.agents[city.agents$id == rn, ]
		migration.edges[i, 'x'] = c_fr$long
		migration.edges[i, 'y'] = c_fr$lat

		c_to <-  city.agents[city.agents$id == cn, ]
		migration.edges[i, 'xend'] = c_to$long
		migration.edges[i, 'yend'] = c_to$lat

		i = i + 1
	}
}
migration.edges <- data.frame(
	N1 = migration.edges[,"N1"], 
	N2 = migration.edges[,"N2"], 
	Value = as.integer(migration.edges[,"Value"]),
	x = as.numeric(migration.edges[,"x"]),
	y = as.numeric(migration.edges[,"y"]),
	xend = as.numeric(migration.edges[,"xend"]),
	yend = as.numeric(migration.edges[,"yend"])
)
migration.edges$color = ifelse(migration.edges$yend > migration.edges$y, "green", "blue")
migration.edges$curvature  = ifelse(migration.edges$yend > migration.edges$y, 0.5, -0.5)

## riverplot
m.r <- makeRiver(
	nodes = data.frame( 
		ID = city.agents$id, 
		labels = city.agents$city_name, 
		x = -city.agents$long, 
		y = city.agents$lat
	), 
	edges = migration.edges)
plot(m.r, srt = 0)

## gmap
map_data <- get_map(location = make_bbox(long, lat, city.agents, f = 0.05), filename="socal")
map.migration <- 
ggmap(map_data, extent="normal") + geom_segment(aes(x=x, y=y, yend=yend, xend=xend, colour=color, lwd=Value), data=migration.edges, arrow = arrow())
#ggmap(map_data, extent="normal") + geom_curve(aes(x=x, y=y, yend=yend, xend=xend, color=color), data=migration.edges, arrow = arrow())

edges <- list()
for(i in 1:nrow(migration.edges)){
	r <- migration.edges[i,]
	x1 <- r$x
	y1 <- r$y
	x2 <- r$xend
	y2 <- r$yend
	l <- r$curvature * sqrt((x2-x1)^2 + (y2-y1)^2)/2
	sign <- r$curvature/abs(r$curvature)

	xmean <- mean(c(x1, x2))
	ymean <- mean(c(y1, y2))
	m = (y2 - y1)/(x2 - x1)
	m0 = -1/m
	b0 = -(ymean - m0 * xmean)
	theta <- atan2(y2 - y1, x2 - x1)
	theta0 <- theta + sign*pi/2

	x <- sign * sqrt(l^2/(1+tan(theta0)^2)) + xmean
	y <- m0 * x - b0

	s <- spline(c(x1, x, x2), c(y1, y, y2), n=50)
	s$color = r$color
	s$lwd = r$Value
	if(s$x[1] != x1){
		s$x = rev(s$x)
		s$y = rev(s$y)
	}
	edges[[i]] <- s
}

mm = ggmap(map_data, extent="normal")
for(i in edges) 
	mm = mm + geom_path(aes(x=x, y=y, lwd=lwd, color=color), data=data.frame(i), arrow = arrow())
mm

#ggmap(map_data, extent="normal") + geom_path(aes(x=x, y=y), data=data.frame(edges[[1]]), arrow = arrow())