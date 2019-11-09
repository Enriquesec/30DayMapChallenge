library(tidyverse)
library(data.table)
library(hexbin)
setwd("Escritorio/30DayMapChallenge-master/Día 4 Hexagon: Accidentes de coches en la CDMX/")

accidentes <- fread("incidentes-viales-c5.csv")

accidentes <-accidentes %>% 
  select(mes,latitud,longitud)
accidentes<-drop_na(accidentes)

avion <- function(mes){
ggplot(accidentes[accidentes$mes==mes],aes(latitud,longitud))+
  stat_binhex(bins = 20,show.legend = F,colour="blue")+
  scale_fill_gradientn(colours=c("blue","red"))+
  theme_void()+
  theme(panel.background = element_rect(fill = "black"))+
    title("Acidentes en la Ciudad de México (2018)")
  
  daprint(paste0("saving plot ", mes))
  ggsave(filename = paste0("Accidentes_",mes,".png"),
         width = 8,height=8,dpi = 150)
}


fecha<-accidentes%>%select(mes)%>%distinct()
fecha$Fecha%>%
  map_df(avion)

library(magick)
list.files(pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% # lee cada archivo
  image_join() %>% # junta cada imagen
  image_animate(fps=5) %>% # animacion
  image_write("final.gif") # escribe el gif final
