#http://stackoverflow.com/questions/20216179/plot-curved-lines-between-two-locations-in-ggplot2

myCurve<-with(migration.edges[1,], {curveGrob(x, y, xend, yend, default.units = "npc",
               curvature = 0.3, angle = 90, ncp = 20, shape = 1,
               square = FALSE, squareShape = 1,
               inflect = FALSE, arrow = arrow(), open = TRUE,
               debug = FALSE,
               name = NULL, gp = gpar(), vp = NULL)})


map.migration <- ggmap(map_data, extent="normal")+
  coord_cartesian()  + annotation_custom(grob=myCurve,migration.edges[1,'x'],migration.edges[1,'y'],migration.edges[1,'xend'],migration.edges[1,'yend'])


myCurve2<-curveGrob(0, 0, 1, 1, default.units = "npc",
               curvature = -0.3, angle = 60, ncp = 10, shape = 1,
               square = FALSE, squareShape = 1,
               inflect = FALSE, arrow = arrow(), open = TRUE,
               debug = FALSE,
               name = NULL, gp = gpar(), vp = NULL)

g + 
  annotation_custom(grob=myCurve,0,10,0,10) + # plot from 0,0 to 10,10
  annotation_custom(grob=myCurve2,2.5,6,2.5,6)   # plot from 2.5,2.5 to 6,6