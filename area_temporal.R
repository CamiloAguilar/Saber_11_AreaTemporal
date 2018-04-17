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

rm(saber2017, saber2016, saber2015); gc()


#*******************************************************************
## 1. Agregación BD por colegios ####
#*******************************************************************

Colegios_2015 <- saber2015_Aquia %>%
                 group_by(Cod_colegio = as.character(COLE_COD_DANE_INSTITUCION)) %>%
                 summarise(Cod_mcipio=first(as.character(COLE_COD_MCPIO_UBICACION)), Estudiantes=n(), 
                           PUNT_LC = mean(PUNT_LECTURA_CRITICA, na.rm=T), 
                           PUNT_MA = mean(PUNT_MATEMATICAS), PUNT_CN = mean(PUNT_C_NATURALES, na.rm=T), 
                           PUNT_SC= mean(PUNT_SOCIALES_CIUDADANAS), PUNT_IN = mean(PUNT_INGLES, na.rm=T),
                           PUNT_GLOBAL = mean(PUNT_GLOBAL, na.rm = T), Edad_media = mean(ESTU_EDAD, na.rm=T),
                           Jornada = first(COLE_JORNADA), Periodo=first(PERIODO)) %>%
                 filter(!is.na(Cod_colegio))

Colegios_2016 <- saber2016_Aquia %>%
     group_by(Cod_colegio = as.character(COLE_COD_DANE_INSTITUCION)) %>%
     summarise(Cod_mcipio=first(as.character(COLE_COD_MCPIO_UBICACION)), Estudiantes=n(), 
               PUNT_LC = mean(PUNT_LECTURA_CRITICA, na.rm=T), 
               PUNT_MA = mean(PUNT_MATEMATICAS), PUNT_CN = mean(PUNT_C_NATURALES, na.rm=T), 
               PUNT_SC= mean(PUNT_SOCIALES_CIUDADANAS), PUNT_IN = mean(PUNT_INGLES, na.rm=T),
               PUNT_GLOBAL = mean(PUNT_GLOBAL, na.rm = T), Edad_media = mean(ESTU_EDAD, na.rm=T),
               Jornada = first(COLE_JORNADA), Periodo=first(PERIODO)) %>%
     filter(!is.na(Cod_colegio))

Colegios_2017 <- saber2017_Aquia %>%
     group_by(Cod_colegio = as.character(COLE_COD_DANE_SEDE)) %>%
     summarise(Cod_mcipio=first(as.character(COLE_COD_MCPIO_UBICACION)), Estudiantes=n(), 
               PUNT_LC = mean(PUNT_LECTURA_CRITICA, na.rm=T), 
               PUNT_MA = mean(PUNT_MATEMATICAS), PUNT_CN = mean(PUNT_C_NATURALES, na.rm=T), 
               PUNT_SC= mean(PUNT_SOCIALES_CIUDADANAS), PUNT_IN = mean(PUNT_INGLES, na.rm=T),
               PUNT_GLOBAL = mean(PUNT_GLOBAL, na.rm = T), Edad_media = mean(ESTU_EDAD, na.rm=T),
               Jornada = first(COLE_JORNADA), Periodo=first(PERIODO)) %>%
     filter(!is.na(Cod_colegio))


saveRDS(Colegios_2015, "./RDS/Colegios_2015.rds")
saveRDS(Colegios_2016, "./RDS/Colegios_2016.rds")
saveRDS(Colegios_2017, "./RDS/Colegios_2017.rds")



#*******************************************************************
## 1. Creación variables Auxiliares por Dominio: municipios ####
#*******************************************************************
## Departamento Antioquia

aux_2015 <- saber2015_Aquia %>%
            group_by(Cod_colegio = as.character(COLE_COD_DANE_SEDE)) %>%
            summarise(Cod_mcipio = first(as.character(COLE_COD_MCPIO_UBICACION)), Municipio = first(ESTU_RESIDE_MCPIO),
                      )











