library(survey)
library(sae)
library(TeachingSampling)
library(dplyr)
library(readxl)

options(survey.lonely.psu = "adjust")

muestra <- readRDS("muestra2Etapas_2015.RDS")

InfoAux <- readRDS("InfoAux.RDS")
InfoAux <- as.data.frame(InfoAux)
head(InfoAux)
names(InfoAux)

mun <- InfoAux$Cod_mcipio
mun2 <- unique(muestra$COLE_COD_MCPIO_UBICACION)
table(mun2%in% mun)

Cod_mpios <- InfoAux %>% group_by(Cod_mcipio) %>% summarise (temp = n())
Cod_mpios$temp <- NULL

muestra <- merge(Cod_mpios, muestra, all.y = T,
                 by.x = "Cod_mcipio", by.y = "COLE_COD_MCPIO_UBICACION")

diseno_muestral <- svydesign(ids = ~ COLE_COD_ICFES + ESTU_CONSECUTIVO,
                             strata = ~ estrato_cole,
                             fpc = ~ Nh + N_i, data = muestra,
                             nest = T)

PG_total <- as.data.frame(svytotal(~PUNT_GLOBAL, diseno_muestral))
PG_total$cve <- (PG_total$PUNT_GLOBAL/PG_total$total) * 100
names(PG_total) <- c("Total", "se", "cve")
saveRDS(PG_total, "./rds/EstDirectoTotal_PG.rds")

PG_mean <- as.data.frame(svymean(~PUNT_GLOBAL, diseno_muestral))
PG_mean$cve <- (PG_mean$PUNT_GLOBAL/PG_mean$mean) * 100
names(PG_mean) <- c("Mean", "se", "cve")
saveRDS(PG_mean, "./rds/EstDirectomean_PG.rds")

estmadirectas <- svyby(~PUNT_GLOBAL, ~ Cod_mcipio, diseno_muestral, FUN = svymean)
estmadirectas$V_estdir <- estmadirectas$se ^ 2
estmadirectas$cve <- estmadirectas$se/estmadirectas$PUNT_GLOBAL * 100
cons <- muestra %>% group_by(Cod_mcipio, COLE_MCPIO_UBICACION)  %>% summarise(temp = n())
cons$temp <- NULL
estmadirectas <- merge(cons, estmadirectas, by = "Cod_mcipio")
estmadirectas <- arrange(estmadirectas, PUNT_GLOBAL)
saveRDS(estmadirectas, "./rds/EstDirectoDomMean_PG.rds")

#################################### Estimador de Fay - Herriot ####################
# Incorporar la informaci?n auxiliar

Datos <- merge(InfoAux, estmadirectas, by.x = "Cod_mcipio", by.y = "Cod_mcipio", all.y=T)
varcandidas <- c("Poblacion_5_16", "Tasa_matricula_5_16", 
                 "Cobertura_neta","Cobertura_bruta", 
                 "Cobertura_bruta_meadia", "Tamano_meadio_grupo", "Sedes_con_internet", 
                 "Tasa_Desercion", "Tasa_Desercion_meadia", "Tasa_aprobacion", 
                 "Tasa_Aprobacion_media", "Tasa_Reprobacion", 
                 "Tasa_Reprobacion_media", 
                 "Tasa_Repitencia", "Tasa_Repitencia_mean" )

modelocompleto <- paste0("PUNT_GLOBAL", " ~ ", paste(varcandidas, collapse = " + "))
modelocompleto <- as.formula(modelocompleto)

saveRDS(modelocompleto, "./rds/modelocompleto.rds")

modelo_optimo <- step(lm(modelocompleto, data = Datos))

saveRDS(modelo_optimo, "./rds/modelo_optimo.rds")
modelo_reducido <- lm(PUNT_GLOBAL ~  Poblacion_5_16 + Tasa_matricula_5_16 + Sedes_con_internet + 
                        Tasa_Reprobacion_media + Tasa_Repitencia_mean, data = Datos)
summary(modelo_reducido)
saveRDS(modelo_reducido, "./rds/modelo_reducido.rds")
cor(Datos[,c("PUNT_GLOBAL",names(modelo_reducido$coefficients)[2:6])])[1,]

FH_prommat <- mseFH(PUNT_GLOBAL ~ Tasa_matricula_5_16 + Sedes_con_internet + 
                      Tasa_Reprobacion_media + Tasa_Repitencia_mean, V_estdir,
                    data = Datos)
FH_prommat ## modelo  Mixto 

Datos$Y_FH <- FH_prommat$est$eblup ## mejor estimador lineal con minima varianza 
Datos$mseY_FH <- FH_prommat$mse
names(Datos)
Datos$cve_FH <- sqrt(FH_prommat$mse) / FH_prommat$est$eblup * 100
Datos$cve_dir <- sqrt(Datos$V_estdir) / Datos$PUNT_GLOBAL * 100
head(Datos)

# Dominios Observados 
Resultados_sd <- Datos[c("Cod_mcipio","COLE_MCPIO_UBICACION","PUNT_GLOBAL","cve_dir", "Y_FH", "mseY_FH","cve_FH")]
head(Resultados_sd)
names(Resultados_sd)[3] <- "Y_dir"
head(Resultados_sd)
View(Resultados_sd)

saveRDS(Resultados_sd, "./rds/resultados_sd.rds")
# Grafico Puntaje Global (Estimaci?n Directa Y_dir) Vs Estimaci?n Puntaje Global EBLUP Fay Herriot (Y_FH)
plot(Resultados_sd$Y_dir, type = "n", ylab = "Estimate", ylim = c(200, 400),
     xlab = "area (sorted by decreasing sample size)", cex.axis = 1.5,
     cex.lab = 1.5)
points(Resultados_sd$Y_dir, type = "b", col = 1, lwd = 2, pch = 1, lty = 1)
points(Resultados_sd$Y_FH, type = "b", col = 4, lwd = 2, pch = 4, lty = 2)
legend("top", legend = c("Direct", "EBLUP FH"), ncol = 2, col = c(1, 4), lwd = 2,
       pch = c(1, 4), lty = c(1, 2), cex = 1.3)


# Grafico cve Puntaje Global (cve_dir) Vs cve Estimaci?n Puntaje Global EBLUP Fay Herriot (cve_FH)
plot(Resultados_sd$cve_dir, type = "n", ylab = "CV", ylim = c(0, 10),
     xlab = "area (sorted by decreasing sample size)", cex.axis = 1.5,
     cex.lab = 1.5)
points(Resultados_sd$cve_dir, type = "b", col = 1, lwd = 2, pch = 1, lty = 1)
points(Resultados_sd$cve_FH, type = "b", col = 4, lwd = 2, pch = 4, lty = 2)
legend("top", legend = c("Direct", "EBLUP FH"), ncol = 2, col = c(1, 4), lwd = 2,
       pch = c(1, 4), lty = c(1, 2), cex = 1.3)

# Municipios Seleccionados (28)
mpiossel <- unique(muestra$Cod_mcipio)

# Municipio no seleccionados (101)
mpiosnosel <- InfoAux$Cod_mcipio[!(InfoAux$Cod_mcipio %in% mpiossel)]

############### Estimaciones del promedio ####################### 
####### para los dominios no observados en la muestra ###########
##################### X_rd #####################################

X_rd <- InfoAux[InfoAux$Cod_mcipio %in% mpiosnosel, 
                c("Tasa_matricula_5_16" , "Sedes_con_internet" , "Tasa_Reprobacion_media" , 
                      "Tasa_Repitencia_mean")]

# En la no muestra la informacion auxiliar de los 101 municipios 
X_rd$Unos <- 1
X_rd <- X_rd[c("Unos", c("Tasa_matricula_5_16" , "Sedes_con_internet" , "Tasa_Reprobacion_media" , 
                         "Tasa_Repitencia_mean"))]
X_rd <- as.matrix(X_rd)
View(X_rd)
row.names(X_rd) <- InfoAux[InfoAux$Cod_mcipio %in% mpiosnosel,]$Cod_mcipio
Beta <- as.matrix(FH_prommat$est$fit$estcoef$beta)# matriz de una sola columna

# Estimaci?n del puntaje Global FH para los dominios no observados (YFH_rd)
YFH_rd <- X_rd %*% Beta 
YFH_rd <- as.data.frame(YFH_rd)
YFH_rd$Cod_mcipio <- InfoAux[InfoAux$Cod_mcipio %in% mpiosnosel, ]$Cod_mcipio
colnames(YFH_rd)[1] <- "Y_FH_rd"                             

# Error cuadratico medio para los municipios no observados
# sigma_u ^2 
sigma2_u <- FH_prommat$est$fit$refvar

# Informaci?n auxiliar en la muestra (Organizaci?n de los Dominios Observados)
X_sd <- InfoAux[InfoAux$Cod_mcipio %in% mpiossel, 
                c("Tasa_matricula_5_16" , "Sedes_con_internet" , "Tasa_Reprobacion_media" , 
                  "Tasa_Repitencia_mean")]
X_sd$Unos <- 1
X_sd <- X_sd[c("Unos", "Tasa_matricula_5_16" , "Sedes_con_internet" , "Tasa_Reprobacion_media" , 
               "Tasa_Repitencia_mean")]
X_sd <- as.matrix(X_sd)
head(X_sd)
# \hat{V}(\hat{\boldsymbol{\beta}}): varianza de los par?metros
V_beta_est <- t(X_sd) %*% diag(sigma2_u + Datos$V_estdir) %*% X_sd  # 28 X 5
dim(X_sd)

# Error cuadr?tico para dominios no observados
MSE_Y.FH <- X_rd %*% solve(V_beta_est) %*% t(X_rd) + sigma2_u### solo me interesa los de la diagonal 
MSE_Y.FH <- diag(MSE_Y.FH)
MSE_Y.FH <- as.data.frame(as.table(MSE_Y.FH))
head(MSE_Y.FH)
colnames(MSE_Y.FH) <- c("Cod_mcipio", "MSE_Y_FH_rd")

Resultados_rd <- merge(YFH_rd, MSE_Y.FH, by = "Cod_mcipio")
head(Resultados_rd)

# Pegarle el municipio a Resultados_rd
Municipios <- InfoAux %>% group_by(Cod_mcipio, MUNICIPIO) %>% summarise(temp = n())
Municipios$temp <- NULL
Resultados_rd <- merge(Resultados_rd, Municipios, by = "Cod_mcipio")
head(Resultados_rd)

# Combinarla con las estimaciones de los municipios observados
names(Resultados_sd) # Dominios Obervados
names(Resultados_rd) # Dominios No obsevados
names(Resultados_sd)[c(5, 6, 7)] <- c("Y_FH_sd", "MSE_Y_FH_sd", "cve_FH_sd")

Resultados_sd$TipologiaDominio  <- "Observado"
Resultados_rd$TipologiaDominio  <- "No observado"
Resultados <- bind_rows(Resultados_sd, Resultados_rd )
Resultados$Y_FH <- ifelse(Resultados$TipologiaDominio == "Observado",
                          Resultados$Y_FH_sd, Resultados$Y_FH_rd)

Resultados$MSE_Y_FH <- ifelse(Resultados$TipologiaDominio == "Observado", 
                              Resultados$MSE_Y_FH_sd, Resultados$MSE_Y_FH_rd)
Resultados$cve_FH <- sqrt(Resultados$MSE_Y_FH) / Resultados$Y_FH * 100
ResultadosFinales <- Resultados[c("TipologiaDominio","Cod_mcipio", "COLE_MCPIO_UBICACION", "Y_dir", 
                                  "cve_dir", "Y_FH", "MSE_Y_FH", "cve_FH", "MUNICIPIO")]
ResultadosFinales<-arrange(ResultadosFinales, -Y_dir)
ResultadosFinales$Ranking<-1:nrow(ResultadosFinales)

head(ResultadosFinales)

names(ResultadosFinales) <- c("Tipologia_Dominio", "Cod_Mpio", "Mpio", "Y_dir", "cve_dir", "Y_FH", "MSE_Y_FH"
                              , "cve_FH", "MUNICIPIO", "Ranking")

ResultadosFinales <- ResultadosFinales[,c(1:8,10)]
head(ResultadosFinales)

saveRDS(ResultadosFinales, "./rds/ResultadosFinales.rds")






