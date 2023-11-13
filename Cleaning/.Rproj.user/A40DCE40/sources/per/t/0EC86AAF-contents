# CONCENTRADORA DATOS
conversor<- function(data_frame) {
  # Get the column names
  col_names <- names(data_frame)
  
  # Replace all uppercase letters with their lowercase counterparts in column names
  new_col_names <- chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz", col_names)
  
  # Set the modified column names back to the data frame
  names(data_frame) <- new_col_names
  
  # Return the modified data frame
  return(data_frame)
}

# FUNCIONES
dummy <- function(dataset, column, values, values_names) {
  # Create a factor variable with levels in the desired order
  column_factor <- factor(column, levels= values, labels = values_names, )
  
  # Create dummy variables from the factor variable
  dummy_matrix <- model.matrix(~ column_factor - 1, data = dataset)
  
  # Set the column names to match the values
  colnames(dummy_matrix) <- levels(column_factor)
  
  # Convert the dummy matrix into a data frame
  dummy_df <- as.data.frame(dummy_matrix)
  print(head(dummy_df))
  
  return(cbind(dataset, dummy_df))
}

# Por default  considera los ceros dentro de la media
reem_med<- function(columna,oldvalues, usezero=FALSE, zeros=c(0)) {
  
  if(usezero == FALSE){
    mean_value <- round(mean(as.numeric(columna[!columna %in% oldvalues & !columna %in% zeros]), na.rm = TRUE))
  }
  else{
    mean_value <- round(mean(as.numeric(columna[!columna %in% oldvalues]), na.rm = TRUE))
  }
  
  print(paste("Mean value:", mean_value))
  columna[columna %in% oldvalues] <- mean_value
  columna <- as.numeric(columna)
  return(columna)
}

reem_multiple <- function(columna, oldvalues, newvalues, tonum=FALSE, match = FALSE){
  if(match==TRUE){
    columna <- ifelse(columna %in% oldvalues, newvalues[match(columna,oldvalues)], columna) 
  }else{ 
    replaceval<- columna %in% oldvalues
    columna[replaceval] <- newvalues[1] 
  }
  # Convertir a numerico
  if (tonum ==TRUE){
    columna <- as.numeric(columna)
  }
  
  return(columna) 
}

reem_mode <- function(columna, oldvalues){
  # Calculate the mode of non-empty string values
  non_empty_values <- columna[!columna %in% oldvalues]
  
  mode_value <- names(sort(table(non_empty_values), decreasing = TRUE)[1])
  print(paste("Mode value:", mode_value))
  # Replace empty strings with the mode
  columna[columna %in% oldvalues] <- mode_value
  return(columna)
}

countv<- function(columna){
  cem <- sum(columna == "")
  print(paste("Empty_strings: ", cem))
  cem2 <- sum(columna != "")
  print(paste("Numeric_strings: ", cem2))
}

rm(list=ls(all=TRUE))
options(digits = 17)
#Establecer directorio de trabajo
setwd("Q:/Proyectos/ENFIH/datos/conjunto_de_datos_enfih_2019_csv/conjunto_de_datos_tmodulo_enfih_2019/conjunto_de_datos/Cleaning")


# Creo la tabla concentradora a partir de la tabla de hogares
tmp_con <- read.csv(file = "datos/TCONCENTRADORA.csv", header=TRUE, sep=",", colClasses="character")

countuni <- function(column){
  unique_count <- table(column)
  
  # Print the count of unique values
  print(unique_count)
}

#=tmp_con$ =tmp_con$ =tmp_con$ =tmp_con$  =tmp_con$ =tmp_con$ =tmp_con$ =tmp_con$ =tmp_con$ =tmp_con$ =tmp_con$ =tmp_con$ =tmp_con$ =tmp_con$ =tmp_con$    
  #--------------------#--------------------#--------------------#--------------------#--------------------
selcon <- data.frame(folio = tmp_con$FOLIO
                     ,NumPersHog=tmp_con$CAT_TAMHOG
                    ,NoED=tmp_con$NoED, EDBasic=tmp_con$EDBasic, EDMedia=tmp_con$EDMedia, EDLic=tmp_con$EdLic,EDPost=tmp_con$EDPost ,EDNoesp=tmp_con$EDNoesp
                     ,NoTienViv=tmp_con$NoTienHogar, VivPagada=tmp_con$VivPagada, VivPagan=tmp_con$VivPagan, VivRent=tmp_con$VivRent,VivOther=tmp_con$VivOther
                     ,NoOcup=tmp_con$NoOcup ,Empleado=tmp_con$Empleado ,Jornalero=tmp_con$Jornalero ,Patron=tmp_con$Patrón ,Emprend=tmp_con$Emprendedor, FamWorker=tmp_con$FamWorker
                     ,MontCuentNomi=tmp_con$V_NOMPEN, MontCuentAhorro=tmp_con$V_CTACHQS
                     ,TienActivFin=tmp_con$CACVO_FIN, MontCuentGob=tmp_con$V_CTAGOB 
                     ,MontCuentInv=tmp_con$V_PRVINVR, MontCuentAfore=tmp_con$V_AFORE 
                     ,MontCuentAhorroInf=tmp_con$V_OTROAF, TienActivNoFin=tmp_con$CACVO_NOFN
                     ,ValVentActivNoFin=tmp_con$VAL_NOFIN, ValVentViv=tmp_con$VAL_VPAL 
                     ,ValVentBienProd=tmp_con$VAL_BPROD, ValVentVehic=tmp_con$VAL_VEHIC
                     ,IngresoTotal=tmp_con$ING_TOTAL, RiquezaNeta=tmp_con$RIQ_NET
                     ,MontDeudCredViv=tmp_con$MNTO_VPAL, MontDeudCredNoHip=tmp_con$MTO_NOHIPO
                     ,MontDeudCredBanc_Depar=tmp_con$MTO_TCRD, MontDeudCredNom=tmp_con$MTO_NMPE 
                     ,MontDeudCredAutos=tmp_con$MTO_AUMO, inversiones=tmp_con$CINGXINVRS
                     ,MontDeudOtrCred=tmp_con$MTO_REST
                     ,NoAdeudos=tmp_con$SIN_DEUDA, RazDeudVivi = tmp_con$RAZ_5)

head(selcon)
# tamaño de hogar principal - CAT_TAMHOG
# Categoría de escolaridad	CAT_ESCOL
countuni(tmp_con$CAT_ESCOL)
tmp_con$CAT_ESCOL <- as.numeric(tmp_con$CAT_ESCOL) 
tmp_con <- dummy(tmp_con, tmp_con$CAT_ESCOL, c(0,1,2,3,4,9), c("NoED","EDBasic","EDMedia","EdLic","EDPost","EDNoesp"))
#Categoría de tenencia de la vivienda habitada por el hogar principal	- CAT_TNCIA
tmp_con$CAT_TNCIA<-as.numeric(tmp_con$CAT_TNCIA)
tmp_con <- dummy(tmp_con, tmp_con$CAT_TNCIA, c(0,1,2,3,4), c("NoTienHogar","VivPagada","VivPagan","VivRent","VivOther"))
#Categoría de posición en el trabajo	CAT_POS
countuni(tmp_con$CAT_POS)
tmp_con$CAT_POS<-as.numeric(tmp_con$CAT_TNCIA)
tmp_con <- dummy(tmp_con, tmp_con$CAT_POS, c(0,1,2,3,4,5), c("NoOcup","Empleado","Jornalero","Patrón","Emprendedor","FamWorker"))

head(tmp_con, n=12)
#Condición de tenencia de activos financieros	
#Monto de cuenta de ahorro y/o cheques	V_CTACHQS
#Monto de cuenta de nómina y/o pensión	V_NOMPEN
#Monto de cuenta para recibir apoyos de gobierno	V_CTAGOB
#Monto de cuenta de depósitos a plazo fijo y/o de fondos de inversión	V_PRVINVR
#Monto de cuenta de ahorro para el retiro o Afore	V_AFORE
#Monto de cuentas de ahorro informal	V_OTROAF
#Condición de tenencia de activos no financieros	CACVO_NOFN
#Valor de los activos no financieros	VAL_NOFIN
#Valor de venta de Vivienda principal	VAL_VPAL
#Valor de venta de negocios de dueño único	VAL_NGCIO
#Valor de venta de bienes productivos	VAL_BPROD
#Valor de venta de vehículos	VAL_VEHIC
#Ingreso corriente efectivo total de los hogares	ING_TOTAL
#Riqueza neta de los hogares       - RIQ_NET
#Monto de crédito de vivienda principal	MNTO_VPAL
#Monto de deuda de crédito no hipotecario	MTO_NOHIPO
#Monto de deuda de tarjeta departamental y/o bancaria	MTO_TCRD
#Monto de deuda de crédito de nómina y/o personal	MTO_NMPE
#Monto de deuda de crédito de automotores	MTO_AUMO 
#Condición de tenencia de ingresos por inversiones	CINGXINVRS
#Monto de deuda de otros créditos	MTO_REST
#Monto de deuda de crédito hipotecario y/o no hipotecario	MTO_CTOT
#Condición de no tenencia de adeudos	SIN_DEUDA
#Razones de la Deuda hipotecaria / Valor de la vivienda principal	RAZ_5
head(tmp_con$RAZ_5,n=20) 

                     #--------------------#--------------------#--------------------#--------------------#--------------------
# *************************************** TVIVIENDO **************************** ***************

#------------------------------------------------------------------------------
tmp_viv <- read.csv(file = "datos/TVivienda.csv", header=TRUE, sep=",", colClasses="character")
nrow(tmp_viv)
tmp_viv <- conversor(tmp_viv)
#--------------------NUMERO DE DORMITORIOS--------------------

mean_value <- round(mean(as.numeric(tmp_viv$p3_1[tmp_viv$p3_1 != "99"]), na.rm = TRUE))
#   Convertimos la columna en numerica y cambiamos los 99 en la media
tmp_viv$p3_1  <- as.integer(tmp_viv$p3_1)
tmp_viv$p3_1[tmp_viv$p3_1 == 99] <- mean_value
 

#--------------------NUMERO DE CUARTOS--------------------

mean_value_room <- round(mean(as.numeric(tmp_viv$p3_2[tmp_viv$p3_2 != "99"]), na.rm = TRUE))
#   Convertimos la columna en numerica y cambiamos los 99 en la media
tmp_viv$p3_2 <- as.integer(tmp_viv$p3_2)
tmp_viv$p3_2[tmp_viv$p3_2 == 99] <- mean_value_room

#--------------------NUMERO DE BañosS--------------------
#Comrobamos la posibilidad de datos vacios
countv(tmp_viv$p3_4)

tmp_viv$p3_4[tmp_viv$p3_4 == ""] <- "0"

#   Convertimos la columna en numerica 
tmp_viv$p3_4 <- as.integer(tmp_viv$p3_4)
str(tmp_viv$p3_4)

#--------------------METROS CUADRADOS DE LA VIVIENDA --------------------
empty2  <- tmp_viv$p3_8_1[tmp_viv$p3_8_1 == ""]
empty2

tmp_viv$p3_8_1 <- reem_med(tmp_viv$p3_8_1, c("","998","999"))
tmp_viv$p3_8_1 
#Reemplazar valores en especifico 
#Reemplazarlos por algun otro valor


#--------------------RENTA DE LA VIVIENDA --------------------
empty2 <- tmp_viv$p4_6[tmp_viv$p4_6 == "" | tmp_viv$p4_6== "999888"]
empty2
# AGREGAMOS VALORES 0 EN VEZ DE NULL
tmp_viv$p4_6 <- reem_multiple(tmp_viv$p4_6, c(""), c("0"), tonum=FALSE)
tmp_viv$p4_6
# INTERCAMBIAMOS VALORES
tmp_viv$p4_6 <- reem_med(tmp_viv$p4_6, c("999888","999999"), zeros=c("0"))
head(tmp_viv$folio, n=10)

nrow(tmp_viv)

#                                             SELECTED DATA
#--------------------#--------------------#--------------------#--------------------#--------------------

selviv <- data.frame(folio = tmp_viv$folio, numDormi=tmp_viv$p3_1, numRoom = tmp_viv$p3_2,
                     numbano=tmp_viv$p3_4, mtcuad = tmp_viv$p3_8_1, rentaviv = tmp_viv$p4_6 )
head(selviv, n=10)

# ********************************************************* TSDem *******************************************************************
# SEXO - Es hombre mujer
# EDAD - 
# NIV- nivel educativo
# p2_8 - Situacion civil 
#--------------------SEXO--------------------
tmp_dem <- read.csv(file = "datos/TSDEM.csv", header=TRUE, sep=",", colClasses="character")
tmp_dem <- conversor(tmp_dem)
tmp_dem<- tmp_dem[tmp_dem$n_ren == "01"  , ]

str(tmp_dem$edad)

tmp_dem <- dummy(tmp_dem, tmp_dem$sexo,c(1,2), c("Hombre","Mujer"))
countuni(tmp_dem$sexo)
head(tmp_dem)
#-------------------EDAD--------------------
rep <- as.numeric(tmp_dem$edad)
rep <- tmp_dem[tmp_dem$edad <= 17, ]
countuni(rep$edad)
str(rep$edad)
#Eliminamos a los menores de edad
tmp_dem<- tmp_dem[tmp_dem$edad > 17 & tmp_dem$edad != 98, ]
countuni(tmp_dem$edad)
tmp_dem$edad <- reem_med(tmp_dem$edad, c(99))

#--------------------ESTADO CIVIL--------------------
countuni(tmp_dem$p2_8)

marital_status <- c( "UnionL", "Separado", "Divorci",  "Viudo", "Casado", "Soltero", "NoSabe")

values2 <- c( "1",  "2", "3", "4", "5", "6",  "9" )

tmp_dem <- dummy(tmp_dem,tmp_dem$p2_8,values2, marital_status)
head(tmp_dem)
#--------------------#--------------------#--------------------#--------------------#--------------------
#                                             SELECTED DATA
#--------------------#--------------------#--------------------#--------------------#--------------------

seldem <- data.frame(folio = tmp_dem$folio, Hombre=tmp_dem$Hombre,Mujer=tmp_dem$Mujer , edad= tmp_dem$edad,
                     UnionL=tmp_dem$UnionL, Separado =tmp_dem$Separado, Divorci =tmp_dem$Divorci,Viudo=tmp_dem$Viudo,
                     Casado=tmp_dem$Casado, Soltero=tmp_dem$Soltero, NoSabe=tmp_dem$NoSabe)
head(seldem, n=10)

# -----------------------------------------------MODULO---------------------------------------
#--------------------#--------------------#--------------------#--------------------#--------------------
#                                             SELECTED DATA
#--------------------#--------------------#--------------------#--------------------#--------------------

tmp_mod <- read.csv(file = "datos/TMODULO.csv", header=TRUE, sep=",", colClasses="character")

tmp_mod <- conversor(tmp_mod)
nrow(tmp_mod)
head(tmp_mod)

tmp_mod<- tmp_mod[tmp_mod$n_ren == "01" , ]
nrow(tmp_mod)
nrow(tmp_con)

#----------------DE CUANTAS PROPIEDADES ERES DUEÑO-------------
countv(tmp_mod$p5_2)
countuni(tmp_mod$p5_2)
tmp_mod$p5_2 <- reem_multiple(tmp_mod$p5_2, c(""), c("0"), tonum=TRUE)
countuni(tmp_mod$p5_2)
#-------------------DE CUANTOS NEGOCIOS ERES DUEÑO O COMPARTES------------------------
tmp_mod$dueno_negocio <- 0

tmp_mod$p6_2_1 <- reem_multiple(tmp_mod$p6_2_1, c(""), c("0"), tonum=TRUE)
countuni(tmp_mod$p6_2_1)
tmp_mod$p6_2_2 <- reem_multiple(tmp_mod$p6_2_2, c(""), c("0"), tonum=TRUE)
countuni(tmp_mod$p6_2_2)

tmp_mod$dueno_negocio <- ifelse(as.integer(tmp_mod$p6_2_1)>= 1 | as.integer(tmp_mod$p6_2_2) >= 1, ifelse(as.integer(tmp_mod$p6_2_1)== 0 & as.integer(tmp_mod$p6_2_2) != 0,tmp_mod$p6_2_2, tmp_mod$p6_2_1), 0)
countuni(tmp_mod$dueno_negocio)
countuni(tmp_mod$p6_2_1)
countuni(tmp_mod$p6_2_2)

#--------------------DE CUANTOS AUTOS ES DUEÑO-----------------------------------------
str(tmp_mod$p7_2_1)

tmp_mod$p7_2_1 <- reem_multiple(tmp_mod$p7_2_1, c(""), c("0"), tonum=TRUE)
countuni(tmp_mod$p7_2_1) 
# No considera a los valores zero
tmp_mod$p7_2_1 <- reem_med(tmp_mod$p7_2_1, c(8,9) )
countuni(tmp_mod$p7_2_1)

#--------------------DE CUANTAS MOTOS ES DUEÑO----------------------------------------
countuni(tmp_mod$p7_2_2)
tmp_mod$p7_2_2 <- reem_multiple(tmp_mod$p7_2_2, c(""), c("0"), tonum=TRUE)
tmp_mod$p7_2_2 <- reem_med(tmp_mod$p7_2_2, c(8))

#--------------------CUANTAS TARJETAS DE CREDITO DPARTAMENTAL TIENE-----------------------------------------

#p8_9_1 
countv(tmp_mod$p8_9_1) 
tmp_mod$p8_9_1 <- reem_multiple(tmp_mod$p8_9_1, c(""),c("0"), tonum=TRUE)
tmp_mod$p8_9_1 <- reem_med(tmp_mod$p8_9_1, c(8,9))   
countuni(tmp_mod$p8_9_1) 
#--------------------CUANTAS TARJETAS DE CREDITO BANCARIO TIENE-----------------------------------------
countv(tmp_mod$p8_9_2)
countuni(tmp_mod$p8_9_2)
tmp_mod$p8_9_2 <- reem_multiple(tmp_mod$p8_9_2, c(""),c("0"), tonum=TRUE)
tmp_mod$p8_9_2 <- reem_med(tmp_mod$p8_9_2, c(8,9))   
countuni(tmp_mod$p8_9_2)
#--------------------#--------------------#--------------------#--------------------#--------------------
#                                             SELECTED DATA
#--------------------#--------------------#--------------------#--------------------#--------------------

selmod <- data.frame(folio = tmp_mod$folio, numautos=tmp_mod$p7_2_1, nummotos=tmp_mod$p7_2_2, 
                     numproperty=tmp_mod$p5_2, businessowner=tmp_mod$dueno_negocio, numdepatar=tmp_mod$p8_9_1,
                     numcredtar=tmp_mod$p8_9_2)
nrow(selmod)
#--------------------#--------------------#---UNIR EN UN SOLO DATASE------#--------------------#--------------------
#--------------------#--------------------#--------------------#--------------------#--------------------
nrow(selmod)
nrow(seldem)
nrow(selviv)
nrow(selcon)

me1 <- merge(selcon, selmod, by = "folio",all.x = TRUE)
me2 <- merge(me1, seldem, by = "folio",all.x = TRUE)
me <- merge(me2, selviv, by = "folio",all.x = TRUE)
na_counts <- colSums(is.na(me))
na_counts
nrow(me)
# ELIMINAMOS FILAS CON NA
complete_rows <- complete.cases(me)

# Subset the data frame to keep only complete rows
df_cleaned <- me[complete_rows, ]
nrow(df_cleaned)
head(df_cleaned, n=20)
# GUARDAR COMO ARCHIVO

write.csv(df_cleaned, file = "ML_Dataset.csv", row.names = FALSE)



# OBTENER DATOS PARA REALIZAR LOS MODELOS

#Predicción de la capacidad de pago: El sistema debe poder predecir la capacidad de pago de un cliente,
#utilizando los siguientes datos:
#Ingresos
#Gastos
#Activos

#Segmentación de clientes: El sistema debe poder segmentar clientes en grupos con características similares, 
#utilizando los siguientes datos:

#Características demográficas 
#(Sexo, edad, educación, No. de familiares, estado civil)
#Ocupación
#Tasa de interes de credito de nomina
#Tasa de interes de credito personal
#Tasa de interes de credito de vivienda
#Adeudos totales
#Ingresos totales


# Predicción de la capacidad de pago mensual

# Modelo de Predicción de Probabilidad de Venta de Activos No Financieros

# Modelo de Predicción de Cambio en la Tenencia de Activos No Financieros
#--------------------#--------------------#--------------------#--------------------#--------------------
#Segmentación de clientes
# Predeccion de capacidad de incumplimiento con arboles de decision o regresion logistica
#  "alto riesgo", "riesgo medio" y "bajo riesgo".


