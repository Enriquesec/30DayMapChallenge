library(rgdal)
library(stringr)
setwd("Desktop/RNR/")
ogrListLayers(dsn="history-2019-10-04.kml")

points <- readOGR("history-2019-10-04.kml", "Location history from 2019-10-04 to 2019-10-04 ", require_geomType="wkbPoint")

lines <- readOGR("history-2019-10-04.kml", "Location history from 2019-10-04 to 2019-10-04 ", require_geomType="wkbLineString")

polygons <- readOGR("history-2019-10-04.kml", "Location history from 2019-10-04 to 2019-10-04 ", require_geomType="wkbPolygon")

lines.df<-fortify(lines)


  ggplot(lines.df,aes(long,lat))+geom_polygon(color="red")+
  theme_void()+
  theme(panel.background = element_rect(fill = "black"))

  
gsub()
