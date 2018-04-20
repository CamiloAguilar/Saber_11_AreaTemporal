#***********************************************************************************************************************
#*******************************************       Fay Herriot      ****************************************************
#***********************************************************************************************************************
rm(list = ls())
library(sae); library(survey); library(readxl); library(dplyr); library(TeachingSampling); library(stringr)
options(survey.lonely.psu = "adjust")

#setwd("D:/Documents/MEA/SAE/Estudio_Caso_II/Modelo_Area_Temporal_Yu_Rao_Saber_Antioquia/Muestra")
muestra_2015 <- readRDS("./RDS/muestraESTMAS_2017.RDS")
muestra_2015$PUNT_GLOBAL <- round(muestra_2015$PUNT_GLOBAL)
muestra_2015$Edad_media <- as.numeric(muestra_2015$Edad_media)
muestra_2015$Cod_mcipio[muestra_2015$Cod_mcipio=="11001"]<-"05001"

## Info Auxiliar
InfoAux <- read.csv("./data/ESTADISTICAS_EN_EDUCACION_BASICA_POR_MUNICIPIO.csv", 
                    encoding="UTF-8")
InfoAux <- InfoAux %>% filter(DEPARTAMENTO=="Antioquia") %>%
           mutate(Cod_mcipio=paste0("0",CÓDIGO_MUNICIPIO)) %>%
           group_by(Cod_mcipio) %>%
           summarise(MUNICIPIO=first(MUNICIPIO), Poblacion_5_16=mean(POBLACIÓN_5_16, na.rm=T), 
                     Tasa_matricula_5_16=mean(TASA_MATRICULACIÓN_5_16, na.rm=T), 
                     Cobertura_neta=mean(COBERTURA_NETA, na.rm=T), Cobertura_Trans=mean(COBERTURA_NETA_TRANSICIÓN, na.rm=T),
                     Cobertura_neta_primaria = mean(COBERTURA_NETA_PRIMARIA, na.rm=T), 
                     Cobertura_neta_secundaria = mean(COBERTURA_NETA_SECUNDARIA, na.rm=T), 
                     Cobertura_neta_media = mean(COBERTURA_NETA_MEDIA, na.rm=T), 
                     Cobertura_bruta = mean(COBERTURA_BRUTA, na.rm=T), 
                     Cobertura_bruta_transi = mean(COBERTURA_BRUTA_TRANSICIÓN, na.rm=T), 
                     Cobertura_bruta_primaria = mean(COBERTURA_BRUTA_PRIMARIA, na.rm=T), 
                     Cobertura_bruta_secundaria = mean(COBERTURA_BRUTA_SECUNDARIA, na.rm=T), 
                     Cobertura_bruta_meadia = mean(COBERTURA_BRUTA_MEDIA, na.rm=T), 
                     Tamano_meadio_grupo = mean(TAMAÑO_PROMEDIO_DE_GRUPO, na.rm=T), 
                     Sedes_con_internet = mean(SEDES_CONECTADAS_A_INTERNET, na.rm=T), 
                     Tasa_Desercion = mean(DESERCIÓN, na.rm=T), Tasa_Desercion_trans = mean(DESERCIÓN_TRANSICIÓN, na.rm=T), 
                     Tasa_Desercion_primaria = mean(DESERCIÓN_PRIMARIA, na.rm=T), 
                     Tasa_Desercion_secundaria = mean(DESERCIÓN_SECUNDARIA, na.rm=T), 
                     Tasa_Desercion_meadia = mean(DESERCIÓN_MEDIA, na.rm=T), 
                     Tasa_aprobacion = mean(APROBACIÓN, na.rm=T), 
                     Tasa_Aprobacion_trans = mean(APROBACIÓN_TRANSICIÓN, na.rm=T), 
                     Tasa_Aprobacion_primaria = mean(APROBACIÓN_PRIMARIA, na.rm=T), 
                     Tasa_Aprobacion_secundaria = mean(APROBACIÓN_SECUNDARIA, na.rm=T), 
                     Tasa_Aprobacion_media = mean(APROBACIÓN_MEDIA, na.rm=T), 
                     Tasa_Reprobacion = mean(REPROBACIÓN, na.rm=T), 
                     Tasa_Reprobacion_trans = mean(REPROBACIÓN_TRANSICIÓN, na.rm=T), 
                     Tasa_Reprobacion_primaria = mean(REPROBACIÓN_PRIMARIA, na.rm=T), 
                     Tasa_Reprobacion_secundaria = mean(REPROBACIÓN_SECUNDARIA, na.rm=T), 
                     Tasa_Reprobacion_media = mean(REPROBACIÓN_MEDIA, na.rm=T), 
                     Tasa_Repitencia = mean(REPITENCIA, na.rm=T), 
                     Tasa_Repitencia_trans = mean(REPITENCIA_TRANSICIÓN, na.rm=T), 
                     Tasa_Repitencia_primaria = mean(REPITENCIA_PRIMARIA, na.rm=T), 
                     Tasa_Repitencia_secundaria = mean(REPITENCIA_SECUNDARIA, na.rm=T), 
                     Tasa_Repitencia_mean = mean(REPITENCIA_MEDIA, na.rm=T))

imputacion <- sapply(InfoAux[,3:ncol(InfoAux)], mean, na.rm=T)
for (i in 3:ncol(InfoAux)) {
     p <- unlist(InfoAux[,i])
     p[is.nan(p) | is.na(p)] <- imputacion[i]
     InfoAux[,i] <- p; rm(p)
}

saveRDS(InfoAux, "./RDS/InfoAux.rds")

#***************
## 2017 ####
#***************
diseno_muestral_2015 <- svydesign(ids = ~Cod_colegio,
                                  strata = ~Stratum,
                                  fpc = ~ Nh, data = muestra_2015,
                                  nest = T)

## Valores Poblacionales
Colegios_2015 <- readRDS("./RDS/Colegios_2015.rds")
pop_2015 <- Colegios_2015 %>%
     group_by(Municipio) %>%
     summarise(M_Punt_global = mean(PUNT_GLOBAL))
pop_2015

## Estimaciones directas
estmadirectas_2015 <- svyby(~PUNT_GLOBAL, ~Cod_mcipio, diseno_muestral_2015, FUN = svymean)
estmadirectas_2015$se <- round(estmadirectas_2015$se, 4)
estmadirectas_2015$V_estdir <- estmadirectas_2015$se ^ 2
estmadirectas_2015$cve <- estmadirectas_2015$se/estmadirectas_2015$PUNT_GLOBAL * 100
estmadirectas_2015 <- arrange(estmadirectas_2015, desc(PUNT_GLOBAL))
head(estmadirectas_2015)

## Cruce InfoAux con Estimaciones directas
table(estmadirectas_2015$Cod_mcipio %in% InfoAux$Cod_mcipio )

Datos <- merge(estmadirectas_2015, InfoAux, by="Cod_mcipio", all.x=T, all.y = F)


#*************************
# Exploración de datos
#*************************

var_candidates <- c("Tasa_matricula_5_16", "Cobertura_neta", "Cobertura_Trans", 
                    "Cobertura_neta_primaria", "Cobertura_neta_secundaria", "Cobertura_neta_media", 
                    "Tamano_meadio_grupo", 
                    "Sedes_con_internet", "Tasa_Desercion", "Tasa_Desercion_trans", 
                    "Tasa_Desercion_primaria", "Tasa_Desercion_secundaria",  
                    "Tasa_aprobacion", "Tasa_Aprobacion_trans", "Tasa_Aprobacion_primaria", 
                    "Tasa_Aprobacion_secundaria", "Tasa_Reprobacion", 
                    "Tasa_Reprobacion_trans", "Tasa_Reprobacion_primaria", "Tasa_Reprobacion_secundaria", 
                    "Tasa_Repitencia", "Tasa_Repitencia_trans", 
                    "Tasa_Repitencia_primaria", "Tasa_Repitencia_secundaria")
cor(Datos[, var_candidates])

modelocompleto <- paste0("PUNT_GLOBAL", " ~ ", paste(var_candidates, collapse = " + "))
modelocompleto <- as.formula(modelocompleto)

modelo_optimo <- step(lm(modelocompleto, data = Datos))
modelo_optimo$call

modelo_optimo <- names(modelo_optimo$coefficients)[2:length(names(modelo_optimo$coefficients))]
formula_var <- paste0("PUNT_GLOBAL", " ~ ", paste(modelo_optimo, collapse = " + "))
modelo_reducido <- lm(formula_var, data = Datos)
summary(modelo_reducido)

modelo_reducido2 <- lm(PUNT_GLOBAL ~ Tasa_Desercion + Tasa_aprobacion + Tasa_Aprobacion_primaria +
                            Tasa_Aprobacion_secundaria + Tasa_Reprobacion + Tasa_Reprobacion_primaria, 
                       data = Datos)
summary(modelo_reducido2)

#********************************************************************************************
FH_prommat <- mseFH(PUNT_GLOBAL ~ Tasa_Desercion + Tasa_aprobacion + Tasa_Aprobacion_primaria +
                         Tasa_Aprobacion_secundaria + Tasa_Reprobacion + Tasa_Reprobacion_primaria,  
                    V_estdir, data = Datos)









