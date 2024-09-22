########## Entregable 1 ############
######## Referncias a los paquetes utilizados ######################
#paquete ggplot2: https://ggplot2.tidyverse.org/
#paquete readr: https://readr.tidyverse.org/
#paquete forcats: https://forcats.tidyverse.org/
####################################################################

##### Visualizacion explicativa
#### lectura de los dataset ############

library(readr)
datos_privada <- read_delim("Ruta del archivo csv", 
                            delim = ";", escape_double = FALSE, col_names = FALSE, 
                            trim_ws = TRUE, skip = 2)

datos_publica <- read_delim("Ruta del archivo csv", 
                            delim = ";", escape_double = FALSE, col_names = FALSE, 
                            trim_ws = TRUE, skip = 2)
datos_finales<-data.frame(year=datos_privada$X1,total_privada=datos_privada$X17*1000,total_publica=datos_publica$X16[-(1:7)]*1000)
datos_finales$proporcion_privada<-round(datos_finales$total_privada/datos_finales$total_privada[1],2)
datos_finales$proporcion_publica<-round(datos_finales$total_publica/datos_finales$total_publica[1],2)
datos_finales$year<-as.numeric(substr(datos_finales$year,1,4))
datos_finales

library(ggplot2)
ggplot(datos_finales, aes(x = year)) + 
  geom_line(aes(y = proporcion_privada*100, color = "Privada"),size=1.5) +  # Primera línea
  geom_line(aes(y = proporcion_publica*100, color = "Pública"),size=1.5) +  # Segunda línea
  labs( x = "Años", 
       y = "Porcentaje de crecimiento") +
  ggtitle("LA UNIVERSIDAD PRIVADA RECORTA DISTANCIA A LA PÚBLICA EN CYL")+
  scale_color_manual(name = "", values = c("Privada" = "green", "Pública" = "red")) + 
  scale_x_continuous(breaks = seq(2002, 2022, by = 1),limits = c(2002,2023),expand = c(0,0))+
  geom_text(aes(label="292%"),x=2022,y=292,vjust=0,hjust=1.25,size=7,color="forestgreen")+
  geom_text(aes(label="66%"),x=2022,y=66,vjust=-1,size=7,color="firebrick4")+
  theme_minimal()+
  geom_text(aes(label = "Las universidades privadas\ncasi triplican el número de alumnos\nmatriculados desde 2002, mientras\nlas públicas cada vez tienen menos\nalumnos."), x = 2007, y = 250, 
            size = 3.5, color = "black", vjust = 1.5) +
theme(plot.title = element_text(face = "bold",hjust = 0.5),axis.text.x = element_text(angle = 60, vjust = 0.5, hjust = 1,margin = margin(b=10)),
      axis.line = element_line(color="black"))
datos_finales


###### Visualizacion exploratoria
library(forcats)
library(readr)
personas_con_discapacidad <- read_delim("Ruta del arcivo csv", 
                                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(personas_con_discapacidad)

personas_con_discapacidad$total<-rowSums(personas_con_discapacidad[,5:7])
personas_con_discapacidad$`Grupo de edad`<-fct_relevel(personas_con_discapacidad$`Grupo de edad`,"De 0 a 4 años","De 5 a 9 años","De 10 a 14 años","De 15 a 19 años","De 20 a 24 años","De 25 a 29 años","De 30 a 34 años","De 35 a 39 años","De 40 a 44 años","De 45 a 49 años","De 50 a 54 años","De 55 a 59 años","De 60 a 64 años","De 65 a 69 años","De 70 a 74 años","De 75 a 79 años","De 80 a 84 años","De 85 a 89 años","De 90 a 94 años","De 95 y más años")
library(ggplot2)
ggplot(personas_con_discapacidad, aes(x = `Grupo de edad`, y = total)) +
  geom_boxplot(fill="navajowhite") +
  labs(x = "Grupos de edad", y = "Personas con discapacidad", title = "DIAGRAMA DE CAJAS POR GRUPOS DE EDAD") +
  theme_minimal()+
  theme(plot.title = element_text(face = "bold",hjust = 0.5),axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1,margin = margin(b=18)),
        axis.line = element_line(color="black"))

######### Modelo ANOVA ##############
anova<-aov(total~`Grupo de edad`,data=personas_con_discapacidad)
summary(anova)
