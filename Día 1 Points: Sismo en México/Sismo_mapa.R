library(tidyverse) # Manejo de los data frame.
library(lubridate) # Manejo de fechas.
library(purrr)     

# Cargamos los datos, seleccionamos las columnas a ocupar y eliminamos las columnas con datos faltantes.
sismos2<-read.csv("SSNMX_catalogo_20170101_20191105.csv",skip = 4) 
sismos2<-sismos2%>%select(Fecha, Magnitud,Latitud,Longitud)
sismos2$Fecha<-ymd(sismos2$Fecha) # Formato de fecha.
sismos2<-drop_na(sismos2)
summary(sismos2)

# Creemos la función para generar los mapas por fechas.
avion <- function(dat){
  ggplot(sismos2[sismos2$Fecha==dat,],aes(Latitud,Longitud))+   # Seleccionamos los datos con la fecha indicada.
    geom_point(size=sismos2[sismos2$Fecha==dat,]$Magnitud/5,color="red")+ # Graficamos los puntos.
    xlim(cord$lat)+   # Ajustamos los límites de x & y, para que todas las imagenes tengan el mismo margen. 
    ylim(cord$lon)+
    theme_void()+     # Personalización del temple.
    theme(panel.background = element_rect(fill = "black"))
  
  print(paste0("saving plot ", dat)) 
  ggsave(filename = paste0("hgm_ndwi_",dat,".png"), # Guardamos la imagen.
         width = 8,height=8,dpi = 150)
}

# Creamos las imagenes en las distintas fechas de los datos.
fecha<-sismos2%>%select(Fecha)%>%distinct()%>%filter(Fecha>date("2019-08-01"))
fecha$Fecha%>%
  map_df(avion)

# Creamos el gif.
library(magick)
list.files(pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% # lee cada archivo
  image_join() %>% # junta cada imagen
  image_animate(fps=5) %>% # animacion
  image_write("final.gif") # escribe el gif final
