library(rgdal)
library(stringr)
library(ggplot2)
library(lubridate)
library(gganimate)
library(gifski)
devtools::install_github("thomasp85/transformr")
library(transformr)
setwd("Desktop/RNR/")

archivos<-dir()
archivos<-archivos[-1]
archivos<-substring(archivos,9,18)
con<-0
data<-data.frame()
for (i in archivos){
  con <- con+1
  lines <- readOGR(paste0("history-",i,".kml"),paste0("Location history from ",i," to ",i," "), require_geomType="wkbLineString")
  fechasllegada <-str_extract(lines@data$Description,"2.*T.*to")
  fechasfinal   <-str_extract(lines@data$Description,"to.*D")
  lines@data$llegada <-as.Date(hms(substring(fechasllegada,12,19)),origin=today())
  lines@data$final   <-as.Date(hms(substring(fechasfinal,15,22)),origin=today())
  lines@data$id <-rownames(lines@data)
  lines.df <-fortify(lines)
  lines.df$dia <-rep(con,nrow(lines.df))
  tab <-table(lines.df$id)
    for (i in names(tab)){
      lines.df$horas[lines.df$id==i]=seq(lines@data$llegada[lines@data$id==i],lines@data$final[lines@data$id==i],length.out = tab[i])
    }  
  data<-rbind(data,lines.df)
  print(con)
}

lines.df<-data
bbox<-matrix(c(min(data$long),max(data$long),min(data$lat),max(data$lat)),2,byrow = T)
library(ggmap)
image<-get_map(location =bbox, source="osm")
ggmap(image)
ggplot(data=lines.df,aes(long,lat,colour=factor(dia)))+
  geom_line(linetype = "dashed",size=2)+
  scale_color_viridis_d() +
  theme_void()+
  theme(panel.background = element_rect(fill = "black"))+
  transition_reveal(horas)

ggmap(image)+
geom_line(data=lines.df,aes(long,lat,colour=factor(dia)),linetype = "dashed",size=1.6)+
  geom_point(data=lines.df,aes(long,lat,colour=factor(dia)),size=1.6)+
  theme_void()+
  theme(panel.background = element_rect(fill = "black"),legend.position="none")+
  transition_reveal(horas)

anim_save("google_viajes.gif")
