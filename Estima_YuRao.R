library(dplyr) #Depuración de bases
library(survey)# Estimaciones Directas
library(saery) # Implementación de Rao - Yule (Domingo Morales)
library(sae2)  # Modelo Dinamico (Temporal de área)
options(survey.lonely.psu = "adjust")
## contruir tablas con estimaciones y varianzas durexctas
# Estimaciones directa s a nivel de dominio en un periodo

setwd("D:/Documents/MEA/SAE/Estudio_Caso_II/Modelo_Area_Temporal_Yu_Rao_Saber_Antioquia/Muestra")
muestra_2015 <- readRDS("muestraESTMAS_2015.RDS")
muestra_2016 <- readRDS("muestraESTMAS_2016.RDS")
muestra_2017 <- readRDS("muestraESTMAS_2017.RDS")

# Variable de interés: Puntaje Global
# Covariables: demás asignaturas
# Dominio: Municipios 

################################################ 2015 #######################################
diseno_muestral_2015 <- svydesign(ids = ~1,
                                  strata = ~Estrato_colegios,
                                  fpc = ~ Nh, data = muestra_2015,
                                  nest = T)
names(muestra_2015)
estmadirectas_2015 <- svyby(~PUNT_GLOBAL, ~Municipio, 
                            diseno_muestral_2015, FUN = svymean)
estmadirectas_2015$V_est <- estmadirectas_2015$se ^ 2
estmadirectas_2015$Periodo <- 2015

# Traer las variables auxiliares a nivel de municipios (promedios de las materias) y los unos
##posibilidad Variables aux a nivel de mnicicpios 
setwd("D:/Documents/MEA/SAE/Estudio_Caso_II/Modelo_Area_Temporal_Yu_Rao_Saber_Antioquia/Base_Datos")
dir()
Colegios2015 <- readRDS("Colegios_2015.rds")
names(Colegios2015)
InfoAux_2015 <- Colegios2015 %>%  group_by(Municipio) %>% 
  summarise(Estudiantes = mean(Estudiantes),
            PUNT_LC = mean(PUNT_LC),
            PUNT_MA = mean(PUNT_MA), PUNT_CN = mean(PUNT_CN), PUNT_SC = mean(PUNT_SC),
            PUNT_IN=mean(PUNT_IN), Edad_media = mean(Edad_media), freq_Hogar_PC = mean(freq_Hogar_PC), 
            M_personas_hogar = mean(M_personas_hogar))


# Pegarle a la base de datos
estmadirectas_2015 <- merge(estmadirectas_2015, InfoAux_2015, by = "Municipio",
                            all.x = T)

################################################ 2016 #######################################
diseno_muestral_2016 <- svydesign(ids = ~1,
                                  strata = ~Estrato_colegios,
                                  fpc = ~ Nh, data = muestra_2016,
                                  nest = T)
names(muestra_2016)
estmadirectas_2016 <- svyby(~PUNT_GLOBAL, ~Municipio, 
                            diseno_muestral_2016, FUN = svymean)
estmadirectas_2016$V_est <- estmadirectas_2016$se ^ 2
estmadirectas_2016$Periodo <- 2016

# Traer las variables auxiliares a nivel de municipios (promedios de las materias) y los unos
##posibilidad Variables aux a nivel de mnicicpios 
setwd("D:/Documents/MEA/SAE/Estudio_Caso_II/Modelo_Area_Temporal_Yu_Rao_Saber_Antioquia/Base_Datos")
dir()
Colegios2016 <- readRDS("Colegios_2016.rds")
names(Colegios2016)
InfoAux_2016 <- Colegios2016 %>%  group_by(Municipio) %>% 
  summarise(Estudiantes = mean(Estudiantes),
            PUNT_LC = mean(PUNT_LC),
            PUNT_MA = mean(PUNT_MA), PUNT_CN = mean(PUNT_CN), PUNT_SC = mean(PUNT_SC),
            PUNT_IN=mean(PUNT_IN), Edad_media = mean(Edad_media), freq_Hogar_PC = mean(freq_Hogar_PC), 
            M_personas_hogar = mean(M_personas_hogar))


# Pegarle a la base de datos
estmadirectas_2016 <- merge(estmadirectas_2016, InfoAux_2016, by = "Municipio",
                            all.x = T)


################################################ 2017 #######################################
diseno_muestral_2017 <- svydesign(ids = ~1,
                                  strata = ~Estrato_colegios,
                                  fpc = ~ Nh, data = muestra_2017,
                                  nest = T)
names(muestra_2017)
estmadirectas_2017 <- svyby(~PUNT_GLOBAL, ~Municipio, 
                            diseno_muestral_2017, FUN = svymean)
estmadirectas_2017$V_est <- estmadirectas_2017$se ^ 2
estmadirectas_2017$Periodo <- 2017

# Traer las variables auxiliares a nivel de municipios (promedios de las materias) y los unos
##posibilidad Variables aux a nivel de mnicicpios 
setwd("D:/Documents/MEA/SAE/Estudio_Caso_II/Modelo_Area_Temporal_Yu_Rao_Saber_Antioquia/Base_Datos")
dir()
Colegios2017 <- readRDS("Colegios_2017.rds")
names(Colegios2017)
InfoAux_2017 <- Colegios2017 %>%  group_by(Municipio) %>% 
  summarise(Estudiantes = mean(Estudiantes),
            PUNT_LC = mean(PUNT_LC),
            PUNT_MA = mean(PUNT_MA), PUNT_CN = mean(PUNT_CN), PUNT_SC = mean(PUNT_SC),
            PUNT_IN=mean(PUNT_IN), Edad_media = mean(Edad_media), freq_Hogar_PC = mean(freq_Hogar_PC), 
            M_personas_hogar = mean(M_personas_hogar))


# Pegarle a la base de datos
estmadirectas_2017 <- merge(estmadirectas_2017, InfoAux_2017, by = "Municipio",
                            all.x = T)

####################### Integrar las fuentes de datos ##############
#Objetivo estima directas para cada dominio en cada unos de los 
estmadirectas <- bind_rows(estmadirectas_2015, estmadirectas_2016)
estmadirectas <- bind_rows(estmadirectas, estmadirectas_2017)
dim(estmadirectas)
# Dejar a los Municipios que aparecen en los tres período
names(estmadirectas)
num_enc_mpio = estmadirectas %>% group_by(Municipio) %>%
  summarise(md = n())# md numero de repeticiones que hay
View(num_enc_mpio)

estmadirectas_red <- merge(estmadirectas, num_enc_mpio, by = "Municipio", all.y = T)
estmadirectas_red <- filter(estmadirectas_red, md >= 3)
View(estmadirectas_red)
# Contar por dominio (Municipio) cuantas encuestas hay
md = estmadirectas_red %>% group_by(Municipio) %>%
  summarise(md = n())


estmadirectas_red <- arrange(estmadirectas_red, Municipio, Periodo)
names(estmadirectas_red)
library(saery)

X = estmadirectas_red[,c("PUNT_LC", "PUNT_MA", "PUNT_CN", "PUNT_SC", "PUNT_IN",
                         "freq_Hogar_PC")]
cor(estmadirectas_red[,"PUNT_GLOBAL"],X)
X$unos <- 1 

X <- X[c("unos", "PUNT_LC", "PUNT_MA", "PUNT_CN", "PUNT_SC", "PUNT_IN",
         "freq_Hogar_PC")]
det(X, logarithm =T)
names(X)
X <- as.matrix(X)
output.fit.indp <- fit.saery(X, ydi = estmadirectas_red$PUNT_GLOBAL,
                             D = length(unique(estmadirectas_red[,1])),
                             md = md$md, sigma2edi = estmadirectas_red$V_est,
                             model = "INDEP", conf.level = 0.95)
length(unique(estmadirectas_red[,1]))
output.fit.ar1 <- fit.saery(X, ydi = estmadirectas_red$PUNT_GLOBAL,
                            D = length(unique(estmadirectas_red[,1])),
                            md = md$md, sigma2edi = estmadirectas_red$V_est,
                            model = "AR1", conf.level = 0.95)

output.fit.ma1 <- fit.saery(X, ydi = estmadirectas_red$PUNT_GLOBAL,
                            D = length(unique(estmadirectas_red[,1])),
                            md = md$md, sigma2edi = estmadirectas_red$V_est,
                            model = "ma", conf.level = 0.95)

eblup.output.indep <- eblup.saery(X, ydi = estmadirectas_red$PUNT_GLOBAL,
                                  D = length(unique(estmadirectas_red[,1])),
                                  md = md$md, sigma2edi = estmadirectas_red$V_est,
                                  model = "INDEP", 
                                  plot = F, B = 1)

eblup.output.indep <- eblup.saery(X, ydi = estmadirectas_red$PUNT_GLOBAL,
                                  D = length(unique(estmadirectas_red[,1])),
                                  md = md$md, sigma2edi = estmadirectas_red$V_est,
                                  model = "INDEP", 
                                  plot = FALSE, type = "II", B = 2)
eblup.output.indep
?eblup.saery
e<-length(unique(estmadirectas_red[,1]))




# Modelo AR 1 
library(saery)
X = estmadirectas_red[,c("PUNT_LC", "PUNT_MA", "PUNT_CN", "PUNT_SC", "PUNT_IN",
                         "freq_Hogar_PC")]
cor(estmadirectas_red[,"PUNT_GLOBAL"],X)
X$unos <- 1 
X <- X[c("unos", "PUNT_LC", "PUNT_MA", "PUNT_CN", "PUNT_SC", "PUNT_IN",
         "freq_Hogar_PC")]
X <- as.matrix(X)

output.fit.ar1 <- fit.saery(X, ydi = estmadirectas_red$PUNT_GLOBAL,
                            D = length(unique(estmadirectas_red[,1])),
                            md = md$md, sigma2edi = estmadirectas_red$V_est,
                            model = "AR1", conf.level = 0.95)


eblup.output.ar1 <- eblup.saery(X, ydi = estmadirectas_red$PUNT_GLOBAL,
                                D = length(unique(estmadirectas_red[,1])),
                                md = md$md, sigma2edi = estmadirectas_red$V_est,
                                model = "AR1", 
                                plot = F, B = 1)
eblup.output.ar1


result.dyn <- eblupDyn(PUNT_GLOBAL ~ 
                         PUNT_LC + PUNT_MA + PUNT_CN + PUNT_SC + PUNT_IN +
                       freq_Hogar_PC,
                       D = length(unique(estmadirectas_red[,1])), T = 6,
                       vardir = diag(estmadirectas_red$V_est), 
                       data=estmadirectas_red)


resultados <- estmadirectas_red
resultados$eblup_temporal <- result.dyn$eblup

resultados$mse_eblup_temporal <- result.dyn$eblup.mse
