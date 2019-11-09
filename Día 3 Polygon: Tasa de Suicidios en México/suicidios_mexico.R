library(tidyverse) # manejo de data.frame
library(rgdal) # manejo de shp.

setwd("~/Desktop/RNR/") 

suicidios <- read.csv("Suicidios.csv",header = F)
suicidios <- suicidios[1:2]
names(suicidios) <- c("estado","tasa_suicidio")
suicidios$estado<-as.character(suicidios$estado)
suicidios$estado[24]<-"Distrito Federal" # Renombramos a la Ciudad de México.

suicidios <- suicidios %>%
  filter(estado!="Nacional")%>%
  arrange(estado)

suicidios$estado_new <- names(table(mexico$ENTIDAD)) # Pegamos los nombres que tiene el shp para poder hacer
suicidios<-suicidios[,2:3]                           # el merge.

mexico <- readOGR(dsn="destdv250k_2gw",layer="destdv250k_2gw") # leemos el shp.

mexico_suicidios <- merge(mexico,suicidios,by.x="ENTIDAD",by.y="estado_new") # Unimos la tasa de suicidio con
                                                                             # el mapa de México.


levels <- seq(min(suicidios$tasa_suicidio),max(suicidios$tasa_suicidio),length.out = 15)
col <- colorRampPalette(c("dark green","yellow","red"))(32)  
plot(mexico_suicidios,main="Tase de suicidios en México",col=col[suicidios$tasa_suicidio]) # Graficamos. 

library(RColorBrewer)
brewer.pal(suicidios$tasa_suicidio,name = "RdGy")
