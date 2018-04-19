############## Diseño Muestral ###################
################## ESTMAS ######################## 
########## Selección de Colegios 2016 ############


setwd("D:/Documents/MEA/SAE/Estudio_Caso_II/Modelo_Area_Temporal_Yu_Rao_Saber_Antioquia/Base_Datos")
saber2017 <- readRDS("Colegios_2017.rds")

library(stratification)
set.seed(12345)
# Estratificación - Selección de Estratos y tamaño  
indica_estrato <- strata.LH(saber2017$Estudiantes,Ls = 5, CV = 0.03)
indica_estrato

cortes <- c(min(saber2017$Estudiantes),indica_estrato$bh,max(saber2017$Estudiantes))
saber2017$Estrato_colegios <- cut(saber2017$Estudiantes, breaks = cortes, include.lowest = T, right = F, 
                                  labels = paste0("estrato_col", 1:5))
table(saber2017$Estrato_colegios)

library(sampling)
library(dplyr)

# Organización Base de Datos y Tamaños
saber2017 <- arrange(saber2017, Estrato_colegios)
tamano_estrato <- data.frame(Estrato_colegios = paste0("estrato_col", 1:5), nh =
                               indica_estrato$nh)

tamanoPoblaciOnal_estrato <- data.frame(Estrato_colegios = paste0("estrato_col", 1:5), Nh =
                                          indica_estrato$Nh)
set.seed(12345)
# MAS - Selección de la Muestra
indica_mue <- sampling::strata(saber2017, stratanames = "Estrato_colegios", size = tamano_estrato$nh,
                               method = "srswr", description = T)
muestra <- getdata(saber2017, indica_mue)
muestra <- merge(muestra, tamanoPoblaciOnal_estrato)
muestra <- merge(muestra, tamano_estrato)

setwd("D:/Documents/MEA/SAE/Estudio_Caso_II/Modelo_Area_Temporal_Yu_Rao_Saber_Antioquia/Muestra")
saveRDS(muestra, "muestraESTMAS_2017.RDS")

library(survey)
# Diseño Muestral 
diseno_muestral <- svydesign(ids = ~1,
                             strata = ~Estrato_colegios,
                             fpc = ~ Nh, data = muestra,
                             nest = T)

svymean(~PUNT_GLOBAL, diseno_muestral)
100 * cv(svymean(~PUNT_GLOBAL, diseno_muestral))

max(muestra$PUNT_GLOBAL)
mean(muestra$PUNT_GLOBAL)
mean(saber2017$PUNT_GLOBAL)           
max(saber2017$PUNT_GLOBAL)

svytotal(~Estudiantes, diseno_muestral)
100 * cv(svytotal(~Estudiantes, diseno_muestral))

sum(saber2017$Estudiantes)
sum(muestra$Estudiantes)

sum(weights(diseno_muestral))
nrow(saber2017)

