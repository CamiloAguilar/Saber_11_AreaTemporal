########### Diseño Muestral Bietapico ###################
################## ESTMAS ######################## 
#2016
#UPM ESTMAS: Colegios
#USM MAS: Estudiantes 

library(dplyr)
setwd("D:/Documents/MEA/SAE/Estudio_Caso_II/Modelo_Area_Temporal_Yu_Rao_Saber_Antioquia/Base_Datos_2")
Marcoest_saber2016_Aquia <- readRDS("saber2016_Aquia.rds")

length(unique(Marcoest_saber2016_Aquia$COLE_COD_MCPIO_UBICACION))
length(unique(Marcoest_saber2016_Aquia$COLE_COD_DANE_INSTITUCION))
length(unique(Marcoest_saber2016_Aquia$ESTU_AREA_RESIDE ))

cole <- Marcoest_saber2016_Aquia %>% group_by(COLE_COD_ICFES) %>% summarise(tx = n())
sum(cole$tx)


library(stratification)
set.seed(12345)
# Estratificación - Selección de Estratos y tamaño  
indica_estrato <- strata.LH(cole$tx,Ls = 5, CV = 0.03)
indica_estrato

cortes <- c(min(cole$tx), indica_estrato$bh, max(cole$tx))

cole$estrato_cole <- cut(cole$tx, breaks = cortes, include.lowest = T, right = F,
                         label = paste0("Estrato", 1:5))

cole<- arrange(cole, tx)


library(sampling)
library(dplyr)

# Organización Base de Datos y Tamaños
set.seed(12345)
# MAS - Selección de la Muestra

tamano_estrato <- data.frame(estrato_cole = paste0("estrato_cole", 1:5), nh =
                               indica_estrato$nh)

set.seed(12345)
indica_mue <- sampling::strata(cole, stratanames = "estrato_cole", size = tamano_estrato$nh,
                               method = "srswr", description = T)



tamanoPoblaciOnal_estrato <- data.frame(estrato_cole = paste0("estrato_col", 1:5), Nh =
                                          indica_estrato$Nh)
set.seed(12345)
mue_cole <- sampling::getdata(cole, indica_mue)
mue_cole <- mue_cole[c("estrato_cole", "COLE_COD_ICFES")]

#muestra <- getdata(Marcoest_saber2015_Aquia, indica_mue)
#muestra <- merge(muestra, tamanoPoblaciOnal_estrato)
#muestra <- merge(muestra, tamano_estrato)

Tamanos_cole <- data.frame(estrato_cole = paste0("Estrato",1:5), Nh = indica_estrato$Nh, nh = indica_estrato$nh)

marco_cole <- merge(Marcoest_saber2016_Aquia, mue_cole, all.y = T, by = "COLE_COD_ICFES") 
marco_cole <- merge(marco_cole, Tamanos_cole, by = "estrato_cole" )
names(marco_cole)


# Seleccionar estudiantes (MAS)
names(marco_cole)
length(unique(marco_cole$COLE_COD_ICFES))
length(unique(marco_cole$COLE_COD_DANE_INSTITUCION))

length(marco_cole$COLE_COD_DANE_INSTITUCION)
length(marco_cole$ESTU_COD_RESIDE_MCPIO)
length(marco_cole$COLE_COD_ICFES)

names(marco_cole)
consulta_estud <- marco_cole %>% group_by(COLE_COD_MCPIO_UBICACION, COLE_COD_ICFES) %>% 
  summarise(Num_est = n()) %>% arrange(-Num_est)

sum(consulta_estud$Num_est)
summary(consulta_estud$Num_est)
set.seed(12345)
consulta_estud$n_i <- ceiling(consulta_estud$Num_est * 0.6)
sum(consulta_estud$n_i)

## Estudiantes por Colegios () 
consulta_estud <- consulta_estud %>% arrange(COLE_COD_ICFES)

names(consulta_estud)[3] <- c("N_i")
muestra <- marco_cole %>% arrange(COLE_COD_ICFES)
names(marco_cole)

### estduaientes y Colegio al que pertenecen
set.seed(12345)
indica_mueestu <- sampling::strata(muestra, stratanames = "COLE_COD_ICFES",
                                   size = consulta_estud$n_i,
                                   method = "srswor", description = T)


EC2muestraXest2016 <- muestra[indica_mueestu$ID_unit,]  

EC2muestraXest2016 <- merge(EC2muestraXest2016,  consulta_estud)
length(unique(EC2muestraXest2016$COLE_COD_ICFES))
length(unique(EC2muestraXest2016$COLE_COD_MCPIO_UBICACION))

setwd("D:/Documents/MEA/SAE/Estudio_Caso_II/Modelo_Area_Temporal_Yu_Rao_Saber_Antioquia/Muestra_2")
saveRDS(EC2muestraXest2016, "muestra2Etapas_2016.RDS")

library(survey)
# Diseño Muestral 
names(EC2muestraXest2016)
diseno_muestral <- svydesign(ids = ~ COLE_COD_ICFES + ESTU_CONSECUTIVO,
                             strata = ~ estrato_cole,
                             fpc = ~ Nh + N_i, data = EC2muestraXest2016,
                             nest = T)

svymean(~PUNT_GLOBAL, diseno_muestral)
100 * cv(svymean(~PUNT_GLOBAL, diseno_muestral))

max(EC2muestraXest2016$PUNT_GLOBAL)
mean(EC2muestraXest2016$PUNT_GLOBAL)
mean(Marcoest_saber2016_Aquia$PUNT_GLOBAL)           
max(Marcoest_saber2016_Aquia$PUNT_GLOBAL)

svytotal(~PUNT_GLOBAL, diseno_muestral)
100 * cv(svytotal(~PUNT_GLOBAL, diseno_muestral))

sum(weights(diseno_muestral))
