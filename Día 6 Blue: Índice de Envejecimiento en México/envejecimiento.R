library(tidyverse)
library(readxl)
library(rgdal)
setwd("Desktop/Poblacion/")

# Leemos los datos.
envejecimiento <- read_xlsx("Población_05.xlsx",skip = 4,n_max = 33
                            ,col_names = c("estado","uno","dos","tres","cuatro","cinco","seis") )
# Leemos el shapile por estado de México.
mexico <- readOGR(dsn="destdv250k_2gw",layer="destdv250k_2gw") 


# Quitamos el índice a nivel nacional.
envejecimiento <- envejecimiento[-1,]
envejecimiento$estado[envejecimiento$estado=="Ciudad de México"] <- "Distrito Federal"
envejecimiento <- envejecimiento %>% arrange(estado) 

# Cargamos los paquetes para perzonalizar el mapa.
library(broom)
install.packages("gpclib", type="source")
library(rgeos)
library(maptools)

# ggplot2 necesita un formato espacial para graficar poligonos.
mexico_envejecimiento.df <- tidy(mexico_envejecimiento, region = "NUM_EDO")
envejecimiento$estado<-names(table(mexico_envejecimiento.df$id))

# Unidmos el mapa y las tasas.
df <- merge(mexico_envejecimiento.df,envejecimiento,by.x="id",by.y="estado") 

# Creamos el mapa
ggplot() +
  geom_polygon(data = df, 
               aes(fill=seis, x = long, y = lat, group = group)) +
  theme_void()+     # Personalización del temple.
  theme(plot.title = element_text(colour = "white",hjust = 0.5,face="bold.italic",size = 17),
        panel.background = element_rect(fill = "black"),
        legend.background = element_rect(fill = "black"),
        legend.key = element_rect(fill = "black", color = NA),
        legend.title = element_text(color = "white", size = 10),
        legend.text = element_text(color = "white"),
        plot.background = element_rect(fill = "black")) +
  labs(title = "Tasa de envejecimiento en México 2015",fill = "") +
  scale_fill_gradient(limits=c(0,75))

# `1990` `1995` `2000` `2005` `2010` `2015`

# Guardamos las imagenes.
ggsave(filename = paste0("tasa_seis",".png"), # Guardamos la imagen.
       width = 8,height=8,dpi = 150)  
  
# Cargamos la librerìa para el gif.
library(magick)
# Cleamos el git con las imagenes.
list.files(pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% # lee cada archivo
  image_join() %>% # junta cada imagen
  image_animate(fps=2) %>% # animacion
  image_write("final.gif") # escribe el gif final
