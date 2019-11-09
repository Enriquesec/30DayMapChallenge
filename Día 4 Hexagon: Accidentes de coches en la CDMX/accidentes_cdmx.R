library(tidyverse)
library(data.table)
library(hexbin)
library(purrr)
setwd("~/Desktop/accidentes_trafico/")

accidentes <- fread("incidentes-viales-c5.csv")

accidentes <-accidentes %>% 
  select(mes,latitud,longitud)
accidentes<-drop_na(accidentes)

avion <- function(nuevo){
  ggplot(accidentes[accidentes$mes==nuevo],aes(latitud,longitud))+
    stat_binhex(bins = 17,show.legend = F,colour="blue")+
    xlim(c(19.10 ,19.58))+
    ylim(c(-99.37 ,-98.95))+
    scale_fill_gradientn(colours=c("blue","red"))+
    theme_void()+
    theme(panel.background = element_rect(fill = "black"))
  
  print(paste0("saving plot ", nuevo))
  ggsave(filename = paste0("Accidentes_",nuevo,".png"),
         width = 8,height=8,dpi = 150)
}


fecha<-accidentes%>%select(mes)%>%distinct()
fecha$mes%>%
  map_df(avion)

library(magick)
list.files(pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% # lee cada archivo
  image_join() %>% # junta cada imagen
  image_animate(fps=2) %>% # animacion
  image_write("final.gif") # escribe el gif final
