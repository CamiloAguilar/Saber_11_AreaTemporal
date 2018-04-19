library(survey)
library(sae)
library(TeachingSampling)
library(dplyr)
library(readxl)

library(sae); library(survey); library(readxl);library(dplyr)
options(survey.lonely.psu = "adjust")

#setwd("D:/Documents/MEA/SAE/Estudio_Caso_II/Modelo_Area_Temporal_Yu_Rao_Saber_Antioquia/Muestra")
muestra_2017 <- readRDS("./RDS/muestraESTMAS_2017.RDS")

names(muestra_2017)
length(unique(muestra_2017$Municipio))
length(unique(muestra_2017$Cod_mcipio))

#***************
## 2017 ####
#***************
diseno_muestral_2017 <- svydesign(ids = ~1,
                                  strata = ~Estrato_colegios,
                                  fpc = ~ Nh, data = muestra_2017,
                                  nest = T)
names(muestra_2017)
estmadirectas_2017 <- svyby(~PUNT_GLOBAL, ~Municipio, 
                            diseno_muestral_2017, FUN = svymean)
estmadirectas_2017$V_estdir <- estmadirectas_2017$se ^ 2
estmadirectas_2017$cve <- estmadirectas_2017$se/estmadirectas_2017$PUNT_GLOBAL * 100


cons <- muestra_2017 %>% group_by(Municipio)  %>% 
  summarise(temp = n())
cons$temp <- NULL
estmadirectas_2017 <- merge(cons, estmadirectas_2017, by = "Municipio")
estmadirectas_2017 <- arrange(estmadirectas_2017, PUNT_GLOBAL)

#**************************************
## Estimador de Fay - Herriot ####
#**************************************
#Estimadirectas$MCPIO <- NULL

# Incorporar la informaci?n auxiliar
InfoAux_2017 <- muestra_2017 %>%  group_by(Municipio) %>% 
  summarise(Estudiantes = mean(Estudiantes),
            PUNT_LC = mean(PUNT_LC),
            PUNT_MA = mean(PUNT_MA), PUNT_CN = mean(PUNT_CN), PUNT_SC = mean(PUNT_SC),
            PUNT_IN=mean(PUNT_IN), Edad_media = mean(Edad_media), freq_Hogar_PC = mean(freq_Hogar_PC), 
            M_personas_hogar = mean(M_personas_hogar))
Datos <- merge(InfoAux_2017,estmadirectas_2017, by.x = "Municipio", by.y = "Municipio", all.y=T)
length(unique(Datos$Municipio))
head(Datos)
names(InfoAux_2017)
varcandidas <- c("PUNT_LC", 
                 "PUNT_MA", "PUNT_CN", 
                 "PUNT_SC", "PUNT_IN", "Edad_media", 
                 "freq_Hogar_PC", "M_personas_hogar")

names(Datos)
modelocompleto <- paste0("PUNT_GLOBAL", " ~ ", paste(varcandidas, collapse = " + "))
modelocompleto <- as.formula(modelocompleto)

modelo_optimo <- step(lm(modelocompleto, data = Datos))
modelo_optimo2 <- step(lm(modelocompleto, direction="forward", data = Datos))

modelo_reducido <- lm(PUNT_GLOBAL ~ PUNT_LC + PUNT_MA + PUNT_CN + PUNT_IN + freq_Hogar_PC, data = Datos)
summary(modelo_reducido)
cor(Datos[,c("PUNT_GLOBAL",names(modelo_reducido$coefficients)[2:6])])[1,]


FH_prommat <- mseFH(PUNT_MA ~ PUNT_LC + PUNT_CN + PUNT_IN + freq_Hogar_PC, V_estdir,
                    data = Datos)



