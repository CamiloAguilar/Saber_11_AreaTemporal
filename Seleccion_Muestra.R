#***********************************************************************************************************************
#******************************************* Diseno Muestral ESTMAS ****************************************************
#***********************************************************************************************************************
# Seleccion de Colegios 2015 - 2016 - 2017 
library(sampling)
library(dplyr)
library(stratification)
rm(list = ls())

#setwd("D:/Documents/MEA/SAE/Estudio_Caso_II/Modelo_Area_Temporal_Yu_Rao_Saber_Antioquia/Base_Datos")
saber2015 <- readRDS("./RDS/Colegios_2015.rds")

set.seed(12345)
# Estratificacion - Seleccion de Estratos y tama?o  
indica_estrato <- strata.LH(saber2015$Estudiantes,Ls = 5, CV = 0.03)
indica_estrato

cortes <- c(min(saber2015$Estudiantes),indica_estrato$bh,max(saber2015$Estudiantes))
saber2015$Estrato_colegios <- cut(saber2015$Estudiantes, breaks = cortes, include.lowest = T, right = F, 
                                  labels = paste0("estrato_col", 1:5))
table(saber2015$Estrato_colegios)



# Organizaci?n Base de Datos y Tama?os
saber2015 <- arrange(saber2015, Estrato_colegios)
tamano_estrato <- data.frame(Estrato_colegios = paste0("estrato_col", 1:5), nh =
                               indica_estrato$nh)

tamanoPoblaciOnal_estrato <- data.frame(Estrato_colegios = paste0("estrato_col", 1:5), Nh =
                                          indica_estrato$Nh)
set.seed(12345)
# MAS - Selecci?n de la Muestra
indica_mue <- sampling::strata(saber2015, stratanames = "Estrato_colegios", size = tamano_estrato$nh,
                               method = "srswr", description = T)
muestra <- getdata(saber2015, indica_mue)
muestra <- merge(muestra, tamanoPoblaciOnal_estrato)
muestra <- merge(muestra, tamano_estrato)

#setwd("D:/Documents/MEA/SAE/Estudio_Caso_II/Modelo_Area_Temporal_Yu_Rao_Saber_Antioquia/Muestra")
saveRDS(muestra, "./RDS/muestraESTMAS_2015.RDS")

library(survey)
# Dise?o Muestral 
diseno_muestral <- svydesign(ids = ~1,
                             strata = ~Estrato_colegios,
                             fpc = ~ Nh, data = muestra,
                             nest = T)

svymean(~PUNT_GLOBAL, diseno_muestral)
100 * cv(svymean(~PUNT_GLOBAL, diseno_muestral))

max(muestra$PUNT_GLOBAL)
mean(muestra$PUNT_GLOBAL)
mean(saber2015$PUNT_GLOBAL)           
max(saber2015$PUNT_GLOBAL)

svytotal(~Estudiantes, diseno_muestral)
100 * cv(svytotal(~Estudiantes, diseno_muestral))

sum(saber2015$Estudiantes)
sum(muestra$Estudiantes)

sum(weights(diseno_muestral))
nrow(saber2015)


#***********************
## Muestra 2016 ####
#***********************
estratos <- muestra %>% dplyr::select(Cod_colegio, Stratum, Nh)
head(estratos)

saber2016 <- readRDS("./RDS/Colegios_2016.rds")
muestra_2016 <- saber2016 %>%
                filter(Cod_colegio %in% muestra$Cod_colegio)
muestra_2016 <- merge(muestra_2016, estratos, by="Cod_colegio")
saveRDS(muestra_2016, "./RDS/muestraESTMAS_2016.RDS")

#***********************
## Muestra 2017 ####
#***********************
saber2017 <- readRDS("./RDS/Colegios_2017.rds")
## Re calculamos Nh para año 2017, pues el número de colegios aumentó
Nh_2017 <- estratos %>% group_by(Stratum) %>% summarise(Nh_ant=first(Nh))
tam <- nrow(saber2017)
Nh_2017$Nh <- round(Nh_2017$Nh_ant / sum(Nh_2017$Nh_ant) * tam)
Nh_2017 <- Nh_2017 %>% dplyr::select(-(Nh_ant))
head(Nh_2017)
sum(Nh_2017$Nh)


## Seleccionamos colegios para el año 2017 (los colegios que coinciden en la muestra 2015)
muestra_2017 <- saber2017 %>%
                filter(Cod_colegio %in% muestra$Cod_colegio)
muestra_2017 <- merge(muestra_2017, estratos, by="Cod_colegio") %>%
                dplyr::select(-(Nh))

muestra_2017 <- merge(muestra_2017, Nh_2017, by="Stratum")
saveRDS(muestra_2017, "./RDS/muestraESTMAS_2017.RDS")






