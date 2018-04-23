##############################################################################################
############################# Estimaciones para areas Peque?as ###############################
#################################  Modelo Rao-Yu ############################################
#############################################################################################

# Variable de inter?s: Puntaje Global en las Pruebas Saber
# Covariables: Algunas Estadisticas de Municipios con relaci?n a Educaci?n de los niveles preescolar, b?sica y media
# Dominio: Municipios 

library(dplyr) #Depuraci?n de bases
library(survey)# Estimaciones Directas
library(saery) # Implementaci?n de Rao-Yu (Domingo Morales)
options(survey.lonely.psu = "adjust")

########################## Muestra Pruebas saber: 2015 2016 2017 #################################

muestra_2015 <- readRDS("./Muestra_2/muestra2Etapas_2015.RDS")
muestra_2016 <- readRDS("./Muestra_2/muestra2Etapas_2016.RDS")
muestra_2017 <- readRDS("./Muestra_2/muestra2Etapas_2017.RDS")

################################################ 2015 #######################################
########################## Estimaciones Directas Calculadas en el FH ########################

estmadirectas_2015 <- readRDS("./Base_Datos_2/Estimaciones_Directas/estmadirectas_2015.rds")
estmadirectas_2015$Periodo <- 2015

############################################# 2016 ##########################################

InfoAux_2016 <- readRDS("./Base_Datos_2/Informacion_Auxiliar/InfoAux_2016.RDS")
InfoAux_2016 <- as.data.frame(InfoAux_2016)

mun <- InfoAux_2016$Cod_mcipio
mun2 <- unique(muestra_2016$COLE_COD_MCPIO_UBICACION)
table(mun2%in% mun)

Cod_mpios <- InfoAux_2016 %>% group_by(Cod_mcipio) %>% summarise (temp = n())
Cod_mpios$temp <- NULL
muestra_2016 <- merge(Cod_mpios, muestra_2016, all.y = T,
                 by.x = "Cod_mcipio", by.y = "COLE_COD_MCPIO_UBICACION")

diseno_muestral_2016 <- svydesign(ids = ~ COLE_COD_ICFES + ESTU_CONSECUTIVO,
                             strata = ~ estrato_cole,
                             fpc = ~ Nh + N_i, data = muestra_2016,
                             nest = T)

estmadirectas <- svyby(~PUNT_GLOBAL, ~ Cod_mcipio, 
                       diseno_muestral_2016, FUN = svymean)
estmadirectas$V_estdir <- estmadirectas$se ^ 2
estmadirectas$cve <- estmadirectas$se/estmadirectas$PUNT_GLOBAL * 100

cons <- muestra_2016 %>% group_by(Cod_mcipio)  %>% 
  summarise(temp = n())
cons$temp <- NULL

estmadirectas_2016 <- merge(cons, estmadirectas, by = "Cod_mcipio")
estmadirectas_2016 <- arrange(estmadirectas_2016, PUNT_GLOBAL)

# Incorporar la informaci?n auxiliar

estmadirectas_2016 <- merge(InfoAux_2016, estmadirectas_2016, by = "Cod_mcipio", all.y=T)
estmadirectas_2016$Periodo <- 2016

################################################ 2017 #######################################

InfoAux_2017 <- readRDS("./Base_Datos_2/Informacion_Auxiliar/InfoAux_2017.RDS")
InfoAux_2017 <- as.data.frame(InfoAux_2017)

mun <- InfoAux_2017$Cod_mcipio
mun2 <- unique(muestra_2017$COLE_COD_MCPIO_UBICACION)
table(mun2%in% mun)

Cod_mpios <- InfoAux_2017 %>% group_by(Cod_mcipio) %>% summarise (temp = n())
Cod_mpios$temp <- NULL

muestra_2017 <- merge(Cod_mpios, muestra_2017, all.y = T,
                      by.x = "Cod_mcipio", by.y = "COLE_COD_MCPIO_UBICACION")

diseno_muestral_2017 <- svydesign(ids = ~ COLE_CODIGO_ICFES + ESTU_CONSECUTIVO,
                                  strata = ~ estrato_cole,
                                  fpc = ~ Nh + N_i, data = muestra_2017,
                                  nest = T)

estmadirectas <- svyby(~PUNT_GLOBAL, ~ Cod_mcipio, 
                       diseno_muestral_2017, FUN = svymean)
estmadirectas$V_estdir <- estmadirectas$se ^ 2
estmadirectas$cve <- estmadirectas$se/estmadirectas$PUNT_GLOBAL * 100

cons <- muestra_2017 %>% group_by(Cod_mcipio, COLE_MCPIO_UBICACION)  %>% 
  summarise(temp = n())
cons$temp <- NULL

estmadirectas_2017 <- merge(cons, estmadirectas, by = "Cod_mcipio")
estmadirectas_2017 <- arrange(estmadirectas, PUNT_GLOBAL)

# Incorporar la informaci?n auxiliar por Cada a?o

estmadirectas_2017 <- merge(InfoAux_2017, estmadirectas_2017, by.x = "Cod_mcipio", by.y = "Cod_mcipio", all.y=T)
estmadirectas_2017$Periodo <- 2017

####################### Integrar las fuentes de datos ##############

# Estima directas para cada dominio 
estmadirectas <- bind_rows(estmadirectas_2015, estmadirectas_2016)
estmadirectas <- bind_rows(estmadirectas, estmadirectas_2017)
estmadirectas$COLE_MCPIO_UBICACION<-NULL

# Municipios que aparecen en los tres per?odo
num_enc_mpio = estmadirectas %>% group_by(MUNICIPIO) %>%
  summarise(md = n())

estmadirectas_red <- merge(estmadirectas, num_enc_mpio, by = "MUNICIPIO", all.y = T)
estmadirectas_red <- filter(estmadirectas_red, md >= 3)

View(estmadirectas_red)
# Contar por dominio (Municipio) cuantas encuestas hay
md = estmadirectas_red %>% group_by(MUNICIPIO) %>%
  summarise(md = n())


estmadirectas_red <- arrange(estmadirectas_red, MUNICIPIO, Periodo)

X = estmadirectas_red[,c("Poblacion_5_16", "Tasa_matricula_5_16", 
                         "Cobertura_neta", "Cobertura_Trans", "Cobertura_neta_primaria", 
                         "Cobertura_neta_secundaria", "Cobertura_neta_media", "Cobertura_bruta", 
                         "Cobertura_bruta_transi", "Cobertura_bruta_primaria", "Cobertura_bruta_secundaria", 
                         "Cobertura_bruta_meadia", "Tamano_meadio_grupo", "Sedes_con_internet", 
                         "Tasa_Desercion", "Tasa_Desercion_trans", "Tasa_Desercion_primaria", 
                         "Tasa_Desercion_secundaria", "Tasa_Desercion_meadia", "Tasa_aprobacion", 
                         "Tasa_Aprobacion_trans", "Tasa_Aprobacion_primaria", "Tasa_Aprobacion_secundaria", 
                         "Tasa_Aprobacion_media", "Tasa_Reprobacion", "Tasa_Reprobacion_trans", 
                         "Tasa_Reprobacion_primaria", "Tasa_Reprobacion_secundaria", "Tasa_Reprobacion_media", 
                         "Tasa_Repitencia", "Tasa_Repitencia_trans", "Tasa_Repitencia_primaria", 
                         "Tasa_Repitencia_secundaria", "Tasa_Repitencia_mean")]

cor(estmadirectas_red[,"PUNT_GLOBAL"],X)
estmadirectas_red<- estmadirectas_red[,c( "MUNICIPIO", "PUNT_GLOBAL", "se", "V_estdir", "cve" ,"Periodo",
                                          "Tamano_meadio_grupo", "Tasa_Reprobacion_media", 
                                         "Tasa_Repitencia_trans", "md")]

X$unos <- 1 
X <- X[c("unos", "Tamano_meadio_grupo", "Tasa_Reprobacion_media", 
         "Tasa_Repitencia_trans")]

X <- as.matrix(X)

# Ajuste del Modelo Correcto Rao-Yu (ind, Ar1 o Ma1)

output.fit.indp <- fit.saery(X, ydi = estmadirectas_red$PUNT_GLOBAL,
                             D = length(unique(estmadirectas_red[,1])),
                             md = md$md, sigma2edi = estmadirectas_red$V_estdir,
                             model = "INDEP", conf.level = 0.95)

output.fit.ar1 <- fit.saery(X, ydi = estmadirectas_red$PUNT_GLOBAL,
                            D = length(unique(estmadirectas_red[,1])),
                            md = md$md, sigma2edi = estmadirectas_red$V_estdir,
                            model = "AR1", conf.level = 0.95)


output.fit.ma1 <- fit.saery(X, ydi = estmadirectas_red$PUNT_GLOBAL,
                            D = length(unique(estmadirectas_red[,1])),
                              md = md$md, sigma2edi = estmadirectas_red$V_estdir,
                            model = "ma", conf.level = 0.95)

eblup.output.indep <- eblup.saery(X, ydi = estmadirectas_red$PUNT_GLOBAL,
                                  D = length(unique(estmadirectas_red[,1])),
                                  md = md$md, sigma2edi = estmadirectas_red$V_estdir,
                                  model = "INDEP", 
                                  plot = F, B = 1)

eblup.output.ma1 <- eblup.saery(X, ydi = estmadirectas_red$PUNT_GLOBAL,
                                  D = length(unique(estmadirectas_red[,1])),
                                  md = md$md, sigma2edi = estmadirectas_red$V_estdir,
                                  model = "MA1", 
                                  plot = F, B = 1)

eblup.output.ar1 <- eblup.saery(X, ydi = estmadirectas_red$PUNT_GLOBAL,
                                D = length(unique(estmadirectas_red[,1])),
                                md = md$md, sigma2edi = estmadirectas_red$V_estdir,
                                model = "ar1", 
                                plot = F, B = 1)
eblup.output.indep
eblup.output.ma1
eblup.output.ar1
mean(eblup.output.indep$mse.eblup)
mean(eblup.output.ma1$mse.eblup)
mean(eblup.output.ar1$mse.eblup)

mse <- data.frame("Modelo" = c("eblup_ind","eblup_ma1","eblup_ar1"), "Mean_mse" = c(35.4, 12.4, 15.3))
saveRDS(mse, "./rds/mse_YuRao.rds")


#Promedio del mse para el Modelo ma1 es mas bajo

resultados <- estmadirectas_red
resultados$eblup_Rao_Yu<-eblup.output.ma1$eblup
resultados$mse_eblup_Rao_Yu <- eblup.output.ma1$mse.eblup
resultados$Ydir<- eblup.output.ma1$direct

saveRDS(resultados, "./rds/resultados_YuRao.rds")

compara <- data.frame("Estimación" = c("Directo","Fay Harriot","Yu Rao"), "Mean_mse" = c(25.3, 10.4, 12.4))
saveRDS(compara, "./rds/compara.rds")


Antioquia$PG_FH <- ResultadosFinales$Y_dir
Antioquia$PG_dir <- ResultadosFinales$Y_FH
Antioquia$cve <- ResultadosFinales$cve_FH
Antioquia$nombre <- ResultadosFinales$Mpio

saveRDS(Antioquia, "./rds/Antioquia.rds")


