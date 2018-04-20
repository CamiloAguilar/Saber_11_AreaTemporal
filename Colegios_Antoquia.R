#***********************************************************************************************************************
#*******************************************   Colegios Antioquia   ****************************************************
#***********************************************************************************************************************
rm(list = ls())
library(dplyr); library(readr); library(reshape2)
options(scipen=999)


saber2015 <- read_delim("./data/SB11-201502.txt", delim = "|")
saber2016 <- read_delim("./data/SB11-201602.txt", delim = "|")
saber2017 <- read_delim("./data/SB11-201702.txt", delim = "|")

## Datos Antioquia
saber2015_Aquia <- saber2015 %>% filter(ESTU_RESIDE_DEPTO=="ANTIOQUIA")
saber2016_Aquia <- saber2016 %>% filter(ESTU_RESIDE_DEPTO=="ANTIOQUIA")
saber2017_Aquia <- saber2017 %>% filter(ESTU_DEPTO_RESIDE=="ANTIOQUIA")
saber2017_Aquia$ESTU_FECHANACIMIENTO <- as.Date.character(saber2017_Aquia$ESTU_FECHANACIMIENTO, format="%d/%m/%Y") 
FExamen <- as.Date.character("2017-10-31")
saber2017_Aquia$ESTU_EDAD <- floor((FExamen - saber2017_Aquia$ESTU_FECHANACIMIENTO)/365.25)
saber2017_Aquia$FAMI_ESTRATO_VIVIENDA <- ifelse(saber2017_Aquia$FAMI_ESTRATOVIVIENDA =="Estrato 1", 1,
                                                ifelse(saber2017_Aquia$FAMI_ESTRATOVIVIENDA =="Estrato 2", 2,
                                                       ifelse(saber2017_Aquia$FAMI_ESTRATOVIVIENDA =="Estrato 3", 3,
                                                              ifelse(saber2017_Aquia$FAMI_ESTRATOVIVIENDA =="Estrato 4", 4,
                                                                     ifelse(saber2017_Aquia$FAMI_ESTRATOVIVIENDA =="Estrato 5",5,
                                                                            ifelse(saber2017_Aquia$FAMI_ESTRATOVIVIENDA =="Estrato 6", 6,
                                                                                   0))))))
saber2017_Aquia$FAMI_ESTRATO_VIVIENDA[1:10]
saber2017_Aquia$FAMI_ESTRATOVIVIENDA[1:10]

saber2017_Aquia$FAMI_PERSONAS_HOGAR<-ifelse(saber2017_Aquia$FAMI_PERSONASHOGAR=="1 a 2", 1.5,
                                            ifelse(saber2017_Aquia$FAMI_PERSONASHOGAR=="3 a 4", 3.5,
                                                   ifelse(saber2017_Aquia$FAMI_PERSONASHOGAR=="5 a 6", 5.5,
                                                          ifelse(saber2017_Aquia$FAMI_PERSONASHOGAR=="7 a 8", 7.5,
                                                                 10.5))))
mean(saber2017_Aquia$FAMI_PERSONAS_HOGAR, na.rm=T)

saber2017_Aquia$FAMI_EDUCA_MADRE<-ifelse(saber2017_Aquia$FAMI_EDUCACIONMADRE%in%c("Ninguno"), 1,
                                         ifelse(saber2017_Aquia$FAMI_EDUCACIONMADRE=="Primaria incompleta", 2,
                                                ifelse(saber2017_Aquia$FAMI_EDUCACIONMADRE=="Primaria completa", 3,
                                                       ifelse(saber2017_Aquia$FAMI_EDUCACIONMADRE=="Secundaria (Bachillerato) incompleta",4,
                                                              ifelse(saber2017_Aquia$FAMI_EDUCACIONMADRE=="Secundaria (Bachillerato) completa", 5,
                                                                     ifelse(saber2017_Aquia$FAMI_EDUCACIONMADRE=="Técnica o tecnológica incompleta", 6,
                                                                            ifelse(saber2017_Aquia$FAMI_EDUCACIONMADRE=="Técnica o tecnológica completa", 7,
                                                                                   ifelse(saber2017_Aquia$FAMI_EDUCACIONMADRE=="Educación profesional incompleta", 8,
                                                                                          ifelse(saber2017_Aquia$FAMI_EDUCACIONMADRE=="Educación profesional completa", 9,
                                                                                                 ifelse(saber2017_Aquia$FAMI_EDUCACIONMADRE=="Postgrado", 10,
                                                                                                        0))))))))))
mean(saber2017_Aquia$FAMI_EDUCA_MADRE, na.rm = T)

saber2017_Aquia$FAMI_EDUCA_PADRE<-ifelse(saber2017_Aquia$FAMI_EDUCACIONPADRE%in%c("Ninguno"), 1,
                                         ifelse(saber2017_Aquia$FAMI_EDUCACIONPADRE=="Primaria incompleta", 2,
                                                ifelse(saber2017_Aquia$FAMI_EDUCACIONPADRE=="Primaria completa", 3,
                                                       ifelse(saber2017_Aquia$FAMI_EDUCACIONPADRE=="Secundaria (Bachillerato) incompleta",4,
                                                              ifelse(saber2017_Aquia$FAMI_EDUCACIONPADRE=="Secundaria (Bachillerato) completa", 5,
                                                                     ifelse(saber2017_Aquia$FAMI_EDUCACIONPADRE=="Técnica o tecnológica incompleta", 6,
                                                                            ifelse(saber2017_Aquia$FAMI_EDUCACIONPADRE=="Técnica o tecnológica completa", 7,
                                                                                   ifelse(saber2017_Aquia$FAMI_EDUCACIONPADRE=="Educación profesional incompleta", 8,
                                                                                          ifelse(saber2017_Aquia$FAMI_EDUCACIONPADRE=="Educación profesional completa", 9,
                                                                                                 ifelse(saber2017_Aquia$FAMI_EDUCACIONPADRE=="Postgrado", 10,
                                                                                                        0))))))))))
mean(saber2017_Aquia$FAMI_EDUCA_PADRE, na.rm = T)

unique(saber2017_Aquia$COLE_AREA_UBICACION)
saber2017_Aquia$COLE_AREA_UBICACION<-ifelse(saber2017_Aquia$COLE_AREA_UBICACION=="URBANO", "U", "R")

rm(saber2017, saber2016, saber2015); gc()

saveRDS(saber2015_Aquia, "./RDS/saber2015_Aquia.rds")
saveRDS(saber2015_Aquia, "./RDS/saber2016_Aquia.rds")
saveRDS(saber2017_Aquia, "./RDS/saber2017_Aquia.rds")

#*******************************************************************
## 1. Agregación BD por colegios ####
#*******************************************************************

Colegios_2015 <- saber2015_Aquia %>%
                 group_by(Cod_colegio = as.character(COLE_COD_DANE_INSTITUCION)) %>%
                 summarise(Cod_mcipio=first(as.character(COLE_COD_MCPIO_UBICACION)), Estudiantes=n(), 
                           PUNT_LC = mean(PUNT_LECTURA_CRITICA, na.rm=T), 
                           PUNT_MA = mean(PUNT_MATEMATICAS), PUNT_CN = mean(PUNT_C_NATURALES, na.rm=T), 
                           PUNT_SC= mean(PUNT_SOCIALES_CIUDADANAS), PUNT_IN = mean(PUNT_INGLES, na.rm=T),
                           PUNT_GLOBAL = mean(PUNT_GLOBAL, na.rm = T), Edad_media = round(mean(ESTU_EDAD, na.rm=T)),
                           Jornada = first(COLE_JORNADA), Periodo=first(PERIODO)) %>%
                 filter(!is.na(Cod_colegio))

Colegios_2016 <- saber2016_Aquia %>%
     group_by(Cod_colegio = as.character(COLE_COD_DANE_INSTITUCION)) %>%
     summarise(Cod_mcipio=first(as.character(COLE_COD_MCPIO_UBICACION)), Estudiantes=n(), 
               PUNT_LC = mean(PUNT_LECTURA_CRITICA, na.rm=T), 
               PUNT_MA = mean(PUNT_MATEMATICAS), PUNT_CN = mean(PUNT_C_NATURALES, na.rm=T), 
               PUNT_SC= mean(PUNT_SOCIALES_CIUDADANAS), PUNT_IN = mean(PUNT_INGLES, na.rm=T),
               PUNT_GLOBAL = mean(PUNT_GLOBAL, na.rm = T), Edad_media = round(mean(ESTU_EDAD, na.rm=T)),
               Jornada = first(COLE_JORNADA), Periodo=first(PERIODO)) %>%
     filter(!is.na(Cod_colegio))

Colegios_2017 <- saber2017_Aquia %>%
     group_by(Cod_colegio = as.character(COLE_COD_DANE_SEDE)) %>%
     summarise(Cod_mcipio=first(as.character(COLE_COD_MCPIO_UBICACION)), Estudiantes=n(), 
               PUNT_LC = mean(PUNT_LECTURA_CRITICA, na.rm=T), 
               PUNT_MA = mean(PUNT_MATEMATICAS), PUNT_CN = mean(PUNT_C_NATURALES, na.rm=T), 
               PUNT_SC= mean(PUNT_SOCIALES_CIUDADANAS), PUNT_IN = mean(PUNT_INGLES, na.rm=T),
               PUNT_GLOBAL = mean(PUNT_GLOBAL, na.rm = T), Edad_media = round(mean(ESTU_EDAD, na.rm=T)),
               Jornada = first(COLE_JORNADA), Periodo=first(PERIODO)) %>%
     filter(!is.na(Cod_colegio))


saveRDS(Colegios_2015, "./RDS/Colegios_2015.rds")
saveRDS(Colegios_2016, "./RDS/Colegios_2016.rds")
saveRDS(Colegios_2017, "./RDS/Colegios_2017.rds")


#*******************************************************************
## 1. Agregación BD por estudiante ####
#*******************************************************************

Colegios_2015 <- saber2015_Aquia %>%
     group_by(Cod_colegio = as.character(COLE_COD_DANE_INSTITUCION)) %>%
     summarise(Cod_mcipio=first(as.character(COLE_COD_MCPIO_UBICACION)), Estudiantes=n(), 
               PUNT_LC = mean(PUNT_LECTURA_CRITICA, na.rm=T), 
               PUNT_MA = mean(PUNT_MATEMATICAS), PUNT_CN = mean(PUNT_C_NATURALES, na.rm=T), 
               PUNT_SC= mean(PUNT_SOCIALES_CIUDADANAS), PUNT_IN = mean(PUNT_INGLES, na.rm=T),
               PUNT_GLOBAL = mean(PUNT_GLOBAL, na.rm = T), Edad_media = round(mean(ESTU_EDAD, na.rm=T)),
               Jornada = first(COLE_JORNADA), Periodo=first(PERIODO)) %>%
     filter(!is.na(Cod_colegio))

Colegios_2016 <- saber2016_Aquia %>%
     group_by(Cod_colegio = as.character(COLE_COD_DANE_INSTITUCION)) %>%
     summarise(Cod_mcipio=first(as.character(COLE_COD_MCPIO_UBICACION)), Estudiantes=n(), 
               PUNT_LC = mean(PUNT_LECTURA_CRITICA, na.rm=T), 
               PUNT_MA = mean(PUNT_MATEMATICAS), PUNT_CN = mean(PUNT_C_NATURALES, na.rm=T), 
               PUNT_SC= mean(PUNT_SOCIALES_CIUDADANAS), PUNT_IN = mean(PUNT_INGLES, na.rm=T),
               PUNT_GLOBAL = mean(PUNT_GLOBAL, na.rm = T), Edad_media = round(mean(ESTU_EDAD, na.rm=T)),
               Jornada = first(COLE_JORNADA), Periodo=first(PERIODO)) %>%
     filter(!is.na(Cod_colegio))

Colegios_2017 <- saber2017_Aquia %>%
     group_by(Cod_colegio = as.character(COLE_COD_DANE_SEDE)) %>%
     summarise(Cod_mcipio=first(as.character(COLE_COD_MCPIO_UBICACION)), Estudiantes=n(), 
               PUNT_LC = mean(PUNT_LECTURA_CRITICA, na.rm=T), 
               PUNT_MA = mean(PUNT_MATEMATICAS), PUNT_CN = mean(PUNT_C_NATURALES, na.rm=T), 
               PUNT_SC= mean(PUNT_SOCIALES_CIUDADANAS), PUNT_IN = mean(PUNT_INGLES, na.rm=T),
               PUNT_GLOBAL = mean(PUNT_GLOBAL, na.rm = T), Edad_media = round(mean(ESTU_EDAD, na.rm=T)),
               Jornada = first(COLE_JORNADA), Periodo=first(PERIODO)) %>%
     filter(!is.na(Cod_colegio))


saveRDS(Colegios_2015, "./RDS/Colegios_2015.rds")
saveRDS(Colegios_2016, "./RDS/Colegios_2016.rds")
saveRDS(Colegios_2017, "./RDS/Colegios_2017.rds")


#*******************************************************************
## 1. Creación variables Auxiliares por Dominio: municipios ####
#*******************************************************************
## Departamento Antioquia

#********
# 2015
#********
aux_2015a <- saber2015_Aquia %>%
             filter(!is.na(COLE_COD_DANE_INSTITUCION)) %>%
             mutate(FAMI_INTERNET=ifelse(is.na(FAMI_INTERNET), "N", FAMI_INTERNET)) %>%
             group_by(Cod_colegio = as.character(COLE_COD_DANE_INSTITUCION), FAMI_INTERNET) %>%
             summarise(n=n()) %>%
             dcast(Cod_colegio ~ FAMI_INTERNET, value.var = "n") %>%
             mutate(freq_Hogar_internet = ifelse(is.na(N), 100, ifelse(is.na(S), 0, round(S / (S+N)*100, 2)))) %>%
             dplyr::select(Cod_colegio, freq_Hogar_internet)

aux_2015b <- saber2015_Aquia %>%
             filter(!is.na(COLE_COD_DANE_INSTITUCION)) %>%
             mutate(FAMI_COMPUTADOR=ifelse(is.na(FAMI_COMPUTADOR), "N", FAMI_COMPUTADOR)) %>%
             group_by(Cod_colegio = as.character(COLE_COD_DANE_INSTITUCION), FAMI_COMPUTADOR) %>%
             summarise(n=n()) %>%
             dcast(Cod_colegio ~ FAMI_COMPUTADOR, value.var = "n") %>%
             mutate(freq_Hogar_PC = ifelse(is.na(N), 100, ifelse(is.na(S), 0, round(S / (S+N)*100, 2)))) %>%
             dplyr::select(Cod_colegio, freq_Hogar_PC)
     
aux_2015z <- saber2015_Aquia %>%
             group_by(Cod_colegio = as.character(COLE_COD_DANE_INSTITUCION)) %>%
             mutate(COLE_BILINGUE=ifelse(is.na(COLE_BILINGUE), "N", COLE_BILINGUE)) %>%
             summarise(Municipio = first(ESTU_MCPIO_PRESENTACION),
                       Ubicacion = first(COLE_AREA_UBICACION), Caracter=first(COLE_CARACTER),
                       Naturaleza = first(COLE_NATURALEZA),  
                       M_Educa_padre=round(mean(FAMI_EDUCA_PADRE, na.rm = T)), 
                       M_Educa_madre=round(mean(FAMI_EDUCA_MADRE, na.rm = T)),
                       M_personas_hogar=round(mean(FAMI_PERSONAS_HOGAR, na.rm=T)),
                       M_estrato = round(mean(FAMI_ESTRATO_VIVIENDA, na.rm=T)))

#aux_2015 <- merge(aux_2015z, aux_2015a, by="Cod_colegio")
aux_2015 <- merge(aux_2015z, aux_2015b, by="Cod_colegio")
saveRDS(aux_2015, "./RDS/aux_2015.rds")

Colegios_2015 <- merge(Colegios_2015, aux_2015, by="Cod_colegio")
saveRDS(Colegios_2015, "./RDS/Colegios_2015.rds")

#********
# 2016
#********
aux_2016a <- saber2016_Aquia %>%
     filter(!is.na(COLE_COD_DANE_INSTITUCION)) %>%
     mutate(FAMI_INTERNET=ifelse(is.na(FAMI_INTERNET), "N", FAMI_INTERNET)) %>%
     group_by(Cod_colegio = as.character(COLE_COD_DANE_INSTITUCION), FAMI_INTERNET) %>%
     summarise(n=n()) %>%
     dcast(Cod_colegio ~ FAMI_INTERNET, value.var = "n") %>%
     mutate(freq_Hogar_internet = ifelse(is.na(N), 100, ifelse(is.na(S), 0, round(S / (S+N)*100, 2)))) %>%
     dplyr::select(Cod_colegio, freq_Hogar_internet)

aux_2016b <- saber2016_Aquia %>%
     filter(!is.na(COLE_COD_DANE_INSTITUCION)) %>%
     mutate(FAMI_COMPUTADOR=ifelse(is.na(FAMI_COMPUTADOR), "N", FAMI_COMPUTADOR)) %>%
     group_by(Cod_colegio = as.character(COLE_COD_DANE_INSTITUCION), FAMI_COMPUTADOR) %>%
     summarise(n=n()) %>%
     dcast(Cod_colegio ~ FAMI_COMPUTADOR, value.var = "n") %>%
     mutate(freq_Hogar_PC = ifelse(is.na(N), 100, ifelse(is.na(S), 0, round(S / (S+N)*100, 2)))) %>%
     dplyr::select(Cod_colegio, freq_Hogar_PC)

aux_2016z <- saber2016_Aquia %>%
     group_by(Cod_colegio = as.character(COLE_COD_DANE_INSTITUCION)) %>%
     summarise(Municipio = first(ESTU_MCPIO_PRESENTACION),
               Ubicacion = first(COLE_AREA_UBICACION), Caracter=first(COLE_CARACTER),
               Naturaleza = first(COLE_NATURALEZA),  
               M_Educa_padre=round(mean(FAMI_EDUCA_PADRE, na.rm = T)), 
               M_Educa_madre=round(mean(FAMI_EDUCA_MADRE, na.rm = T)),
               M_personas_hogar=round(mean(FAMI_PERSONAS_HOGAR, na.rm=T)),
               M_estrato = round(mean(FAMI_ESTRATO_VIVIENDA, na.rm=T))) 

#aux_2016 <- merge(aux_2016z, aux_2016a, by="Cod_colegio")
aux_2016 <- merge(aux_2016z, aux_2016b, by="Cod_colegio")
saveRDS(aux_2016, "./RDS/aux_2016.rds")

Colegios_2016 <- merge(Colegios_2016, aux_2016, by="Cod_colegio")
saveRDS(Colegios_2016, "./RDS/Colegios_2016.rds")


#********
# 2017
#********

aux_2017b <- saber2017_Aquia %>%
     filter(!is.na(COLE_COD_DANE_SEDE)) %>%
     mutate(FAMI_TIENECOMPUTADOR=ifelse(is.na(FAMI_TIENECOMPUTADOR), "N", FAMI_TIENECOMPUTADOR)) %>%
     mutate(FAMI_TIENECOMPUTADOR=ifelse(FAMI_TIENECOMPUTADOR=="Si", "Si", "No")) %>%
     group_by(Cod_colegio = as.character(COLE_COD_DANE_SEDE), FAMI_TIENECOMPUTADOR) %>%
     summarise(n=n()) %>%
     dcast(Cod_colegio ~ FAMI_TIENECOMPUTADOR, value.var = "n") %>%
     mutate(freq_Hogar_PC = ifelse(is.na(No), 100, ifelse(is.na(Si), 0, round(Si / (Si+No)*100, 2)))) %>%
     dplyr::select(Cod_colegio, freq_Hogar_PC)

aux_2017z <- saber2017_Aquia %>%
     group_by(Cod_colegio = as.character(COLE_COD_DANE_SEDE)) %>%
     summarise(Municipio = first(ESTU_MCPIO_RESIDE),
               Ubicacion = first(COLE_AREA_UBICACION), Caracter=first(COLE_CARACTER),
               Naturaleza = first(COLE_NATURALEZA),  
               M_Educa_padre=round(mean(FAMI_EDUCA_PADRE, na.rm = T)), 
               M_Educa_madre=round(mean(FAMI_EDUCA_MADRE, na.rm = T)),
               M_personas_hogar=round(mean(FAMI_PERSONAS_HOGAR, na.rm=T)),
               M_estrato = round(mean(FAMI_ESTRATO_VIVIENDA, na.rm=T))) 


aux_2017 <- merge(aux_2017z, aux_2017b, by="Cod_colegio")
saveRDS(aux_2017, "./RDS/aux_2017.rds")

Colegios_2017 <- merge(Colegios_2017, aux_2017, by="Cod_colegio")
names(Colegios_2017)
saveRDS(Colegios_2017, "./RDS/Colegios_2017.rds")


## FIN ##

