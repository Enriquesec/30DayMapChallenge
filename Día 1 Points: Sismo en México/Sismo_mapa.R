library(tidyverse)
library(lubridate)
library(purrr)

sismos2<-read.csv("SSNMX_catalogo_20170101_20191105.csv",skip = 4)
sismos2<-sismos2%>%select(Fecha, Magnitud,Latitud,Longitud)
sismos2$Fecha<-ymd(sismos2$Fecha)
sismos2<-drop_na(sismos2)
summary(sismos2)

gg<-ggplot(cord)
avion <- function(dat){
  ggplot(sismos2[sismos2$Fecha==dat,],aes(Latitud,Longitud))+
    geom_point(size=sismos2[sismos2$Fecha==dat,]$Magnitud/5,color="red")+
    xlim(cord$lat)+
    ylim(cord$lon)+
    theme_void()+
    theme(panel.background = element_rect(fill = "black"))
  
  print(paste0("saving plot ", dat))
  ggsave(filename = paste0("hgm_ndwi_",dat,".png"),
         width = 8,height=8,dpi = 150)
}

fecha<-sismos2%>%select(Fecha)%>%distinct()%>%filter(Fecha>date("2019-08-01"))
fecha$Fecha%>%
  map_df(avion)

library(magick)
list.files(pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% # lee cada archivo
  image_join() %>% # junta cada imagen
  image_animate(fps=5) %>% # animacion
  image_write("final.gif") # escribe el gif final
