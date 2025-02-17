
#Fecha de Creación: Febrero 9 2025
#Creado por: Isis Eliana Delgado Martínez
#Asignatura: Haciendo Economía
#Taller 1
#Segunda Entrega

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)


Base <- read_dta("TenderosFU03_Publica.dta")
poblacion <-read_excel("TerriData_Dim2_Sub4.xlsx")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Seleccionar variables relevantes de la base original
Base_1 <- Base %>% select(Munic_Dept,actG1,actG2,actG3,actG4,actG5,actG6,actG7,
                          actG8,actG9,actG10,actG11,uso_internet)


#Cambiar características necesarias
Base_1$Munic_Dept <- as.numeric(Base_1$Munic_Dept)


# Renombrar Variables (Left Join)
Base_1<-Base_1 %>% rename(`Código Entidad` = Munic_Dept)


#Organizar base población
poblacion_1 <- poblacion %>% 
  select(`Código Entidad`,Entidad,Indicador,`Dato Numérico`,Año) %>% 
  filter(Año == "2022") %>% 
  filter(!grepl("Porcentaje", Indicador)) %>% 
  mutate(`Dato Numérico` = as.numeric(gsub("\\.", "", 
                                           gsub(",", ".", `Dato Numérico`))))


poblacion_2 <- poblacion_1 %>% 
  group_by( `Código Entidad`,Entidad) %>%
  summarise(poblacion_total = sum(`Dato Numérico`, na.rm = TRUE)/100)

poblacion_2$`Código Entidad`<-as.numeric(poblacion_2$`Código Entidad`)


#Left Join
Base_2 <- left_join(Base_1,poblacion_2, by="Código Entidad")


#Renombrar variables
Base_2<-Base_2 %>% rename(Ciudad = Entidad) %>% 
  rename(Población = poblacion_total) %>% 
  rename(Código = `Código Entidad`)

#Ordenar Columnas
Base_2 <-  Base_2[, c("Población", "Código", "Ciudad","actG1","actG2",
                      "actG3","actG4","actG5","actG6","actG7","actG8","actG9",
                      "actG10","actG11","uso_internet" )]


#Agrupando (Base de las tiendas por actividad económica)
Base_3 <- Base_2 %>% 
  group_by(Población,Ciudad) %>% 
  summarise(across(starts_with("act"),~(sum(.))))

names(Base_3) <- c("Población","Ciudad","Tienda", "Comida preparada", 
                     "Peluquería y Belleza", "Ropa", "Otras variedades",
                     "Papelería y Comunicaciones", "Vida Nocturna", 
                     "Productos Bajo Inventario", "Salud","Servicios",
                     "Ferretería y afines")


#Base de los establecimientos que usan internet por actividad económica
#Agrupada por ciudad
Base_4 <- Base_2 %>%
  reframe(across(starts_with("act"),~(.*uso_internet))) %>%
  mutate(Ciudad =Base_2$Ciudad) %>% 
  mutate(Población=Base_2$Población) %>% 
  group_by(Población,Ciudad) %>% 
  summarise(across(starts_with("act"),~(sum(.))))

names(Base_4) <- c("Población","Ciudad","Tienda", "Comida preparada", 
                   "Peluquería y Belleza", "Ropa", "Otras variedades",
                   "Papelería y Comunicaciones", "Vida Nocturna", 
                   "Productos Bajo Inventario", "Salud","Servicios",
                   "Ferretería y afines")


#Base final
Base_fin <- round(Base_4[, 3:ncol(Base_4)]/Base_3[, 3:ncol(Base_3)],3)
Base_fin <- cbind(Base_3[, 1:2], Base_fin)
names(Base_fin) <- c("Población","Ciudad","Tienda", "Comida preparada", 
                     "Peluquería y Belleza", "Ropa", "Otras variedades",
                     "Papelería y Comunicaciones", "Vida Nocturna", 
                     "Productos Bajo Inventario", "Salud","Servicios",
                     "Ferretería y afines") 

Base_fin <- Base_fin %>% rename(`Ciudad o Municipio`= `Ciudad`)


#Archivo excel (presentación)

writexl::write_xlsx(
  list(
    "Proporciones de Uso Internet" = Base_fin,
    "Total Negocios" = Base_3,
    "Negocios con Internet" = Base_4
  ),
  "Tablas Excel Presentación.xlsx"
)


# Transformar las bases a formato largo (Power BI)
Base_long <- Base_fin %>%
  pivot_longer(cols = -c(Población, `Ciudad o Municipio`),
               names_to = "Tipo de negocio",
               values_to = "Proporción de uso de Internet")


Base_3_long <- Base_3 %>%
  pivot_longer(cols = -c(Población, Ciudad), 
               names_to = "Tipo de negocio", 
               values_to = "Cantidad de negocios")


Base_4_long <- Base_4 %>%
  pivot_longer(cols = -c(Población, Ciudad), 
               names_to = "Tipo de negocio", 
               values_to = "Negocios con Internet")


#Archivo para cargar a Power BI
writexl::write_xlsx(list(
  "Negocios"=Base_3long,"Uso Internet" =Base_4_long,"Proporciones"=Base_long), 
  "Tablas Excel Power BI.xlsx"
)
