
#Fecha de Creación: Febrero 1 2025
#Creado por: Isis Eliana Delgado Martínez
#Asignatura: Haciendo Economía
#Taller 1

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(haven)
library(dplyr)

setwd("C:/Users/Isis/Downloads")

Base_1 <- read_dta("TenderosFU03_Publica.dta")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#La pregunta a resolver: ¿Los micronegocios usan internet?

uso_internet <- as.numeric(Base_1$uso_internet)
frecuencia <- table(uso_internet)

diagrama <- barplot(frecuencia, 
            beside = TRUE, 
            col = c("lightblue", "lightgreen"),  
            names.arg = c("No usa Internet", "Usa Internet"), 
            main = "Distribución de Uso de Internet",
            ylab = "Frecuencia",
            ylim = c(0, max(frecuencia) + 100))

text(x = diagrama, 
     y = frecuencia, 
     labels = frecuencia, 
     pos = 3,            
     cex = 1,          
     font = 1)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#La pregunta a resolver: 
#Los negocios que utilizan internet, ¿Qué herramientas tecnológicas usan?

presencia_internet_1<- as.numeric(Base_1$presencia_internet__1)
presencia_internet_2<- as.numeric(Base_1$presencia_internet__2)
presencia_internet_3<- as.numeric(Base_1$presencia_internet__3)
presencia_internet_4<- as.numeric(Base_1$presencia_internet__4)
presencia_internet_5<- as.numeric(Base_1$presencia_internet__5)
presencia_internet_6<- as.numeric(Base_1$presencia_internet__6)

presencia_internet<-data.frame(presencia_internet_1,presencia_internet_2,
                               presencia_internet_3,presencia_internet_4,
                               presencia_internet_5,presencia_internet_6)

presencia_internet_<- presencia_internet %>% 
  filter(presencia_internet_1 == 1) %>% 
  filter(presencia_internet_2 == 1) %>%
  filter(presencia_internet_3 == 1) %>%
  filter(presencia_internet_4 == 1) %>%
  filter(presencia_internet_5 == 1) %>%
  filter(presencia_internet_6 == 1)

frecuencias <- apply(presencia_internet, 2, function(x) table(factor(x, levels = c(0, 1))))

diagrama_1 <- barplot(frecuencias, 
                    beside = TRUE, 
                    col = c("turquoise","magenta"),  
                    names.arg = c( "Página web propia\n(dominio/hosting propio)",
                                   "Página web con opción \n de carrito de compras",
                                   "Línea Whatsapp","Página Facebook",
                                   "Página Instragram","Registro en Google Maps"), 
                    main = "Uso de Herramientas de Internet",
                    ylab = "Frecuencia",
                    ylim = c(0, max(frecuencia) + 100),
                    cex.names = 0.75)

legend(x = 15, y = max(frecuencias) + 170,
       legend = c("No (0)", "Sí (1)"), 
       fill = c("turquoise", "magenta"), 
       cex = 0.9, 
       bty = "n")

text(x = diagrama_1, 
     y = frecuencias, 
     labels = frecuencias, 
     pos = 3,            
     cex = 1,          
     font = 0.5)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#La pregunta a resolver: ¿El uso de internet se debe al COVID-19?

internet_covid <- as.numeric(Base_1$internet_covid)
frecuencia_1 <- table(internet_covid)

diagrama_2 <- barplot(frecuencia_1, 
                    beside = TRUE, 
                    col = c("orange", "grey"),  
                    names.arg = c("No", "Si"), 
                    main = "Uso de internet a raíz de la pandemia",
                    ylab = "Frecuencia",
                    ylim = c(0, max(frecuencia) ))

text(x = diagrama_2, 
     y = frecuencia_1, 
     labels = frecuencia_1, 
     pos = 3,            
     cex = 1,          
     font = 1)
