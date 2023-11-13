rm(list=ls(all=TRUE))
options(digits = 17)
#Establecer directorio de trabajo
setwd("Q:/Proyectos/ENFIH/datos/conjunto_de_datos_enfih_2019_csv/conjunto_de_datos_tmodulo_enfih_2019/conjunto_de_datos/Cleaning")


# Creo la tabla concentradora a partir de la tabla de hogares
tmp_hogar1 <- read.csv(file = "datos/THogar.csv", header=TRUE, sep=",", colClasses="character")
#----------------------CREACION DE FUNCIONES-----------------------------
# FUNCION CONVERSORA
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


countuni <- function(column){
  unique_count <- table(column)
  
  # Print the count of unique values
  print(unique_count)
}


  
tmp_hogar1 <- conversor(tmp_hogar1)
tmp_hogar1 <- conversor(tmp_hogar1)

tmp_hogar1$LlaveVI <- paste(tmp_hogar1$upm, tmp_hogar1$viv_sel, sep="")
tmp_hogar1$LlaveHO <- paste(tmp_hogar1$upm, tmp_hogar1$viv_sel, tmp_hogar1$hogar, sep="")

tmp_hogar1$fac_hog <- as.numeric(tmp_hogar1$fac_hog)
tmp_hogar1$h_ppal <- 0
# Variable que Identifaca Hogar Principal
tmp_hogar1$h_ppal <- ifelse(trimws(tmp_hogar1$p2_9) == "" | tmp_hogar1$p2_9 == "1", 1, 0)



# Personas viviendo en la vivienda

datas <- tmp_hogar1[, c("LlaveHO", "p1_1","p3_0","p3_1")]
# VIVIENDAA
# p1_1- Cantidad de personas viviendo en el hogar 
#3_0
#  TIPO DE VIVIENDA:
#    VIVIENDA EN EDIFICIO
 #   VIVIENDA EN CONDOMINIO HORIZONTAL
  #  VIVIENDA EN VECINDAD
   # VIVIENDA EN CUARTO DE AZOTEA
    #LOCAL NO CONSTRUIDO PARA HABITACIÓN
# p3_1 - NUmero d dormitorios
# p3_2 - Numero de curtos en general
# p3_4 -  cuantos baños tiene la casa
# p3_8_1 - cuantos metros cuadrdos tiene el terreno de la vivienda
# p4_6 - cuanto pagan por renta de la vivienda
# p4_13_1 - costo total de la vivienda
# p4_16 - cuantos creditos de vivienda obtuvieron

# TLOC - tamaño de localdiad

tmp_viv <- read.csv(file = "datos/TVivienda.csv", header=TRUE, sep=",", colClasses="character")
nrow(tmp_viv)

#Convertimos todas las letras de columnas en minusculas
tmp_viv <- conversor(tmp_viv)
# LLAVE VIVIENDA
tmp_viv$LlaveVI <-  paste(tmp_viv$upm, tmp_viv$viv_sel, sep="")
 
# EPLORACION DE DATOS

tmp_viv <- tmp_viv

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
tmp_viv$p4_6
head(tmp_viv$p4_6)

#--------------------TLOCALIDAD--------------------
countnum(tmp_viv$tloc) 
# EJEMPLOS
tmp_viv_dummy <- tmp_motos[, c("LlaveHO", "motoincum")]
tmp_deuda_vsec$LlaveVI <- paste(tmp_deuda_vsec$upm, tmp_deuda_vsec$viv_sel, sep="")
tmp_deuda_vsec$LlaveHO <- paste(tmp_deuda_vsec$upm, tmp_deuda_vsec$viv_sel,tmp_deuda_vsec$hogar, sep="")
 
# ********************************************************* TSDem *******************************************************************
# SEXO - Es hombre mujer
# EDAD - 
# NIV- nivel educativo
# p2_8 - Situacion civil 
#--------------------SEXO--------------------
tmp_dem <- read.csv(file = "datos/TSDEM.csv", header=TRUE, sep=",", colClasses="character")
tmp_dem <- conversor(tmp_dem)


str(tmp_dem$edad)

tmp_dem <- dummy(tmp_dem, tmp_dem$sexo,c(1,2), c("Hombre","Mujer"))
countuni(tmp_dem$sexo)
#----------------------------------------------
rep <- tmp_dem[tmp_dem$n_ren == "01"  , ]
nrow(rep)
head(rep, n=10)

#--------------------EDAD--------------------
rep <- as.numeric(tmp_dem$edad)
rep <- tmp_dem[tmp_dem$edad <= 17, ]
countuni(rep$edad)
str(rep$edad)
#Eliminamos a los menores de edad
tmp_dem<- tmp_dem[tmp_dem$edad > 17 & tmp_dem$edad != 98, ]
countuni(tmp_dem$edad)
tmp_dem$edad <- reem_med(tmp_dem$edad, c(99))

#--------------------NIVEL EDUCATIVO--------------------
countv(tmp_dem$niv)
countuni(tmp_dem$niv)
tmp_dem$niv <- reem_mode(tmp_dem$niv,c("","99"))
countuni(tmp_dem$niv)

education_levels <- c(
  "EDNinguno",
  "EDkinder",
  "EDPrim",
  "EDSec",
  "EDTecSec",
  "EdNormal",
  "EDBach",
  "EDTecPrep",
  "EDLic",
  "EDMast",
  "EDDoc"
)
values <- c(
  "00",
  "01",
  "02",
  "03",
  "04",
  "05",
  "06",
  "07",
  "08",
  "09",
  "10"
)
tmp_dem <- dummy(tmp_dem,tmp_dem$niv,values, education_levels)
head(tmp_dem)

#--------------------ESTADO CIVIL--------------------
countuni(tmp_dem$p2_8)

marital_status <- c(
  "Union",
  "Separado",
  "Divorci",
  "Viudo",
  "Casado",
  "Soltero",
  "NoSabe"
)
values2 <- c(
  "1",
  "2",
  "3",
  "4",
  "5",
  "6",
  "9"
)
tmp_dem <- dummy(tmp_dem,tmp_dem$p2_8,values2, marital_status)
head(tmp_dem)

tmp_dem$llavepr <- 0
tmp_dem$llavepr <- paste(tmp_dem$upm, tmp_dem$viv_sel, tmp_dem$hogar, tmp_dem$n_ren, sep="")
tmp_dem$llavepr

# ********************************************************* TSMODULO ******************************************************************
# p4a4 - jerarquia en su puesto de trabajo
# p4a8_1 - ingreso por trabajo
# p5_2 - de cuantas propiedades,terreno es dueño?
# p6_2_1 - de cuantos negocios es dueño
# p7_2_1 - de cuantos autos es dueño

# p7_2_2 - cuantas motos tiene
# p8_9_1 - cuantas tarjetas departamentales tiene


# p9_5a_3 - ¿Cuanto tiene en cuentas de ahorro?
# p9_5a_6 -¿Cuanto tiene en fondos de inversion?
# p9_7 - ¿Cual es su monto ahorrado en su tarjeta de nomina o pension?

tmp_mod <- read.csv(file = "datos/TMODULO.csv", header=TRUE, sep=",", colClasses="character")
tmp_mod
tmp_mod <- conversor(tmp_mod)

countuni(tmp_mod$p7_1_1)
countuni(tmp_mod$p7_2_1)



tmp_mod$llavepr <- 0
tmp_mod$llavepr <- paste(tmp_mod$upm, tmp_mod$viv_sel, tmp_mod$hogar, tmp_mod$n_ren, sep="")
tmp_mod$llavepr 
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#---------------EXPERIMENTO para hacer merge de datos -----------------------------------------------
# SEleccionamos columnas especificas demograficas
demsub <- tmp_dem[, c("llavepr", "edad", "niv")]

# Select specific columns from the financial_data data frame
modsub <- tmp_mod[, c("llavepr","p4a8_1","p8_9_2","p9_5a_3" )]


# ASUMIMOS LA LLAVE COMUN
merged_data <- merge(modsub, demsub, by = "llavepr")
str(merged_data )

#FILTRAMOS
fildat <- merged_data[merged_data$edad >= 18, ]
str(fildat)

countv(fildat$edad)
countuni(fildat$edad)
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#--------------------ESTADO LABORAL--------------------
#### EXPLORACION DE DATOS
### VEMOS LA CNTIAD DE DATOS VACIOS Y NO VACIOS
countv(tmp_mod$p4a3)
 
countuni(tmp_mod$p4a3)
tmp_mod$p4a3

rep <- tmp_mod$p4a3
rep <- as.character(rep)
rep <- reem_multiple(rep, c("1","2","3","4"), c("labora","labora","labora","labora"), tonum=FALSE, match=TRUE)
rep <- reem_multiple(rep, c("5"), c("estudiante"), tonum=FALSE)
rep <- reem_multiple(rep, c("6"), c("no_labora"), tonum=FALSE)
rep <- reem_mode(rep, c(""))
countuni(rep)
tmp_mod$p4a3 <- rep
tmp_mod <- dummy(tmp_mod, tmp_mod$p4a3,c("labora","no_labora","estudiante") ,c("Labora","No_labora","Estudiante"))
countuni(rep)

# -------------------JERARQUI LABORAL------------------------
countuni(tmp_mod$p4a4)
rep <- tmp_mod$p4a4
tmp_mod$p4a4 <- reem_multiple(tmp_mod$p4a4, c("1","2","3","6"), c("Empleado"), tonum=FALSE, match = FALSE)
tmp_mod$p4a4 <- reem_multiple(tmp_mod$p4a4, c("4"), c("Empleador"), tonum=FALSE, match = FALSE)
tmp_mod$p4a4 <- reem_multiple(tmp_mod$p4a4, c("5"), c("Emprendedor"), tonum=FALSE, match = FALSE)
tmp_mod$p4a4 <- reem_mode(tmp_mod$p4a4, c(""))
tmp_mod <- dummy(tmp_mod, tmp_mod$p4a4, c("Empleado","Empleador","Emprendedor"),c("Empleado","Empleador","Emprendedor"))
countuni(tmp_mod$p4a4)

#----------------INGRESO POR TRABAJO-------------
countv(tmp_mod$p4a8_1)
tmp_mod$p4a8_1 <- reem_multiple(tmp_mod$p4a8_1, c("0"), c("999888"), tonum=TRUE)
head(tmp_mod$p4a8_1)
tmp_mod$p4a8_1 <- reem_med(tmp_mod$p4a8_1,c("999888"))
countv(tmp_mod$p4a8_1)

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
rep <- reem_med(tmp_mod$p7_2_1, c(8,9), zero = TRUE )
countuni(rep)

#--------------------DE CUANTAS MOTOS ES DUEÑO----------------------------------------
countuni(tmp_mod$p7_2_2)
tmp_mod$p7_2_2 <- reem_multiple(tmp_mod$p7_2_2, c(""), c("0"), tonum=TRUE)
tmp_mod$p7_2_2 <- reem_med(tmp_mod$p7_2_2, c(8), zero = TRUE )

#--------------------CUANTAS TARJETAS DE CREDITO DPARTAMENTAL TIENE-----------------------------------------

#p8_9_1 
countv(tmp_mod$p8_9_1) 
tmp_mod$p8_9_1 <- reem_multiple(tmp_mod$p8_9_1, c(""),c("0"), tonum=TRUE)
tmp_mod$p8_9_1 <- reem_med(tmp_mod$p8_9_1, c(8,9))   
 
#--------------------CUANTAS TARJETAS DE CREDITO BANCARIO TIENE-----------------------------------------
countv(tmp_mod$p8_9_2)
countuni(tmp_mod$p8_9_2)
tmp_mod$p8_9_2 <- reem_multiple(tmp_mod$p8_9_2, c(""),c("0"), tonum=TRUE)
tmp_mod$p8_9_2 <- reem_med(tmp_mod$p8_9_2, c(8,9))   
countuni(tmp_mod$p8_9_2)
#--------------------CUANTO tiene en cuentas  de ahorro-----------------------------------------
countv(tmp_mod$p9_5a_3)
rep <- reem_multiple(tmp_mod$p9_5a_3, c(""),c("0"), tonum=TRUE)
countv(rep)
rep <- reem_med(rep, c(999999888,999999999))   
tmp_mod$p9_5a_3 <- rep


#----------------------------CUANTO RECIBE POR PARTE DEL GOBIERNO---------------------------------------
countuni(tmp_mod$p10_1_1)
countv(tmp_mod$p10_2_6_1)

rep <- reem_multiple(tmp_mod$p10_2_1_1, c(""),c("0"), tonum=TRUE)
countv(rep)
rep <- reem_med(rep, c(9999888,9999999))   
tmp_mod$p10_2_1_1 <- rep
tmp_pro$p5_16 <- rep
#-----------------------------CUANTO DEBE EM UNA CAJA DE AHORRO----------------------------------------------------
countv(tmp_mod$p8_3_1)
rep <- tmp_mod$p8_3_1
rep <- reem_multiple(rep, c(""),c("0"), tonum=TRUE)
rep <- reem_med(rep, c(999888,999999)) 
tmp_mod$p8_3_1 <- rep

# ############################################# PROPIEDAD ########################################################################
# p5_14 - cuanto le prestaron para pagar su inmobiliario
# p5_16 - cuanto debe todavia
# p - cual es la tasa de interes anual
tmp_pro <- read.csv(file = "datos/TPROPIEDAD.csv", header=TRUE, sep=",", colClasses="character")
tmp_pro
tmp_pro <- conversor(tmp_pro)

str(tmp_pro)
#----------------------------CUANTO LE PRESTARON PARA PAGAR--------------------------------------

countv(tmp_pro$p5_14)
rep <- reem_multiple(tmp_pro$p5_14, c(""),c("0"), tonum=TRUE)
rep <- reem_med(rep, c(99999888, 99999999)) 
tmp_pro$p5_14 <- rep
#----------------------------CUANTO DEBE TODAVIa-------------------------------------------------
countv(tmp_pro$p5_16)
rep <- reem_multiple(tmp_pro$p5_16, c(""),c("0"), tonum=TRUE)
rep <- reem_med(rep, c(99999888, 99999999)) 
countv(rep)

# **********************************************************************************************************************
nrow(tmp_pro)#
nrow(tmp_depar)#
nrow(tmp_banc)#
 
# ********************************************************* TDEPAR *******************************************************************
# p8_13 - cuanto pago el mes pasado en la tarjeta
# p8_14_1 - Que tasa le cobran en la tarjeta?
# p8_15 - cuanto debia al corte del mes pasado
# p8_16 - cuantos meses se ha atrasado en el pago

#-------------------------------QUE TASA LE COBRAN EN LA TARJETA------------------------------------------------
tmp_depar <- read.csv(file = "datos/TDEPAR.csv", header=TRUE, sep=",", colClasses="character")
tmp_depar <- conversor(tmp_depar) 

rep <- tmp_depar$p8_13
countv(rep)
rep <- reem_multiple(rep, c("00000"),c("0"),tonum=TRUE)
rep <- reem_med(rep, c(99888,99999),usezero = TRUE)
tmp_depar$p8_13 <- rep

#-------------------------------QQUE TASA DE INTERES LE COBRAN------------------------------------
rep <- tmp_depar$p8_14_1
countv(rep)
rep <- reem_med(rep, c("99"),usezero = TRUE)
tmp_depar$p8_14_1 <- rep
#-------------------------------CUANTO DEBIA AL CORTE DEL MES PASADO------------------------------------
rep <- tmp_depar$p8_15
countv(rep)
rep <- reem_med(rep, c("999888","999999"),usezero = TRUE)
tmp_depar$p8_15 <- rep
#-------------------------------CUANTOS MESES SE HA ATRASADO----------------------------------
rep <- tmp_depar$p8_16
rep <- reem_med(rep, c("99"),usezero = TRUE)
tmp_depar$p8_16 <- rep

# ********************************************************* TBANCA *******************************************************************
# p8_19 - cuatno pago el mes pasado
# p8_20_1 - cual es la tasa de interes
# p8_21 - cuanto debia
# p8_22 - cuantos meses se ha atrasdo
tmp_banc <- read.csv(file = "datos/TBANCA.csv", header=TRUE, sep=",", colClasses="character")
tmp_banc <- conversor(tmp_banc)

#-------------------------------CUANTOS PAGO EL MES PASADO-------------------------
rep <- tmp_banc$p8_19 
rep <- reem_med(rep, c("99888","99999"),usezero = TRUE)
tmp_banc$p8_19 <- rep

#-------------------------------QUE TASA LE COBRAN EN LA TARJETA------------------------------------------------
rep <- tmp_banc$p8_20_1
rep <- reem_med(rep, c("99"),usezero = TRUE)

tmp_banc$p8_20_1 <- rep
#-------------------------------CUANTO DEBIA AL CORTE DEL MES PASADO------------------------------------
rep <- tmp_banc$p8_21 
countv(rep)
rep <- reem_med(rep, c("999888","999999"),usezero = TRUE)
tmp_banc$p8_21  <- rep
 
#-------------------------------CUANTOS MESES SE HA ATRASADO----------------------------------
rep <- tmp_banc$p8_22 
rep <- reem_med(rep, c("99"),usezero = TRUE)
tmp_banc$p8_22  <- rep



#-----------------------RENOMBRE DE COlUMNAS----------------------------------------------------------
# bacnca


#-----------------------UNION----------------------------------------------------------
nrow(tmp_dem)
nrow(tmp_mod)
nrow(tmp_viv)
nrow(tmp_pro)#
nrow(tmp_depar)#
nrow(tmp_banc)#
tmp_banc$upm_dis
# CREACION DE LLAVES
# LLAVE DE VIVIENDA
crear_llaveviv <- function(dataset){
  dataset$llaveviv <- 0
  dataset$llaveviv <-   paste(dataset$upm, dataset$viv_sel, sep="")
  return(dataset)
}

# LLAVE DE HOGAR
crear_llaveho <- function(dataset){
  dataset$llaveho  <- 0
  dataset$llaveho  <-   paste(dataset$upm, dataset$viv_sel, dataset$hogar, sep="")
  return(dataset)
}

# LLAVE PERSONA
crear_llavepr <- function(dataset){
  dataset$llavepr <- 0
  dataset$llavepr <-   paste(dataset$upm, dataset$viv_sel, dataset$hogar, dataset$n_ren, sep="")
  return(dataset)
}
# --------------------- UNION DE LOS DATASETS----------------------------------------------------------

tmp_pro <- crear_llavepr(tmp_pro)
tmp_depar <- crear_llavepr(tmp_depar)

merged_data <- merge(tmp_depar, tmp_banc, by = "folio",all.x = TRUE)

na_counts <- colSums(is.na(merged_data))
na_counts



# ----------------------LIMPIEZA DE LOS DATOS---------------------------------------------------------



# ------------------------------------------------------------------------------
# Numero de renglon :  para diferenciar  a las personas dentro d euna familia
# CONSEC : clasifica los casos de estudio  (dif tipo de tarjetas, etc)
# ------------------------------------------------------------------------------

# Predeccion de capacidad de incumplimiento con arboles de decision o regresion logistica
#  "alto riesgo", "riesgo medio" y "bajo riesgo".

# Predicción de la capacidad de pago mensual

#Segmentación de clientes


datas$LlaveHO <- tmp_hogar1$LlaveHO



############## AUTOS ########
tmp_autos <- read.csv(file = "datos/TAuto.csv", header=TRUE, sep=",", colClasses="character")
tmp_autos <- conversor(tmp_autos)
tmp_autos$LlaveHO <- paste(tmp_autos$upm, tmp_autos$viv_sel, tmp_autos$hogar, sep="")
tmp_autos$p7_14 
tmp_autos$p7_14 <- ifelse(tmp_autos$p7_14  == "", "00", tmp_autos$p7_14)
tmp_autos$p7_14 <- as.numeric(tmp_autos$p7_14)
#tmp_autos <- tmp_autos[tmp_autos$p7_14 >= 1, ]
tmp_autos$p7_14
tmp_autos$autoincum <- ifelse(tmp_autos$p7_14 > 0, 1, 0)
tmp_autos$autoincum


autos <- tmp_autos[, c("LlaveHO", "autoincum")]


autospr <- table(autos$autoincum) / nrow(autos)

str(autospr) 

############## MOTOS ########
tmp_motos <- read.csv(file = "datos/TMoto.csv", header=TRUE, sep=",", colClasses="character")
tmp_motos <- conversor(tmp_motos)
tmp_motos$LlaveHO <- paste(tmp_motos$upm, tmp_motos$viv_sel, tmp_motos$hogar, sep="")
tmp_motos$p7_23 
tmp_motos$p7_23 <- ifelse(tmp_motos$p7_23  == "", "00", tmp_motos$p7_23)
tmp_motos$p7_23 <- as.numeric(tmp_motos$p7_23)
#tmp_autos <- tmp_autos[tmp_autos$p7_14 >= 1, ]
tmp_motos$p7_23
tmp_motos$motoincum <- ifelse(tmp_motos$p7_23 > 0, 1, 0)
tmp_motos$motoincum

motos <- tmp_motos[, c("LlaveHO", "motoincum")]
motospr <- table(motos$motoincum) / nrow(motos)
motospr


############## Tarjeta DEPARTamental ########
tmp_dept <- read.csv(file = "datos/TDepar.csv", header=TRUE, sep=",", colClasses="character")
tmp_dept <- conversor(tmp_dept)
tmp_dept$LlaveHO <- paste(tmp_dept$upm, tmp_dept$viv_sel, tmp_dept$hogar, sep="")
tmp_dept$p8_16
tmp_dept$p8_16 <- ifelse(tmp_dept$p8_16  == "", "00", tmp_dept$p8_16)
tmp_dept$p8_16 <- as.numeric(tmp_dept$p8_16)
#tmp_autos <- tmp_autos[tmp_autos$p7_14 >= 1, ]
tmp_dept$p8_16
tmp_dept$deptincum <- ifelse(tmp_dept$p8_16 > 0, 1, 0)
tmp_dept$deptincum

undep <- unique(tmp_dept$LlaveHO)

dept <- tmp_dept[, c("LlaveHO", "deptincum")]

deptpr <- table(dept$deptincum) / nrow(dept)
deptpr
############## Tarjeta DE CREDITO ########
tmp_banca <- read.csv(file = "datos/TBANCA.csv", header=TRUE, sep=",", colClasses="character")
tmp_banca <- conversor(tmp_banca)
tmp_banca$LlaveHO <- paste(tmp_banca$upm,tmp_banca$viv_sel, tmp_banca$hogar, sep="")
tmp_banca$p8_22
tmp_banca$p8_22 <- ifelse(tmp_banca$p8_22  == "", "00", tmp_banca$p8_22)
tmp_banca$p8_22 <- as.numeric(tmp_banca$p8_22)
#tmp_autos <- tmp_autos[tmp_autos$p7_14 >= 1, ]
tmp_banca$p8_22
tmp_banca$bancaincum <- ifelse(tmp_banca$p8_22 > 0, 1, 0)
tmp_banca$bancaincum

banca <- tmp_banca[, c("LlaveHO", "bancaincum")]

bancpr <- table(banca$bancaincum) / nrow(banca)
bancpr
############## NOMINA ########
tmp_nomina <- read.csv(file = "datos/TNOMINA.csv", header=TRUE, sep=",", colClasses="character")
tmp_nomina <- conversor(tmp_nomina)
tmp_nomina$LlaveHO <- paste(tmp_nomina$upm,tmp_nomina$viv_sel, tmp_nomina$hogar, sep="")
tmp_nomina$p8_28
tmp_nomina$p8_28 <- ifelse(tmp_nomina$p8_28  == "", "00", tmp_nomina$p8_28)
tmp_nomina$p8_28 <- as.numeric(tmp_nomina$p8_28)
#tmp_autos <- tmp_autos[tmp_autos$p7_14 >= 1, ]
tmp_nomina$p8_28
tmp_nomina$nomincum <- ifelse(tmp_nomina$p8_28 > 0, 1, 0)
tmp_nomina$nomincum

nomina <- tmp_nomina[, c("LlaveHO", "nomincum")]

nominapr <- table(nomina$nomincum) / nrow(nomina)
nominapr

############## PERSONAL ########
tmp_perso <- read.csv(file = "datos/TPERSONAL.csv", header=TRUE, sep=",", colClasses="character")
tmp_perso  <- conversor(tmp_perso )
tmp_perso$LlaveHO <- paste(tmp_perso$upm,tmp_perso$viv_sel, tmp_perso$hogar, sep="")
tmp_perso$p8_44
tmp_perso$p8_44 <- ifelse(tmp_perso$p8_44  == "", "00", tmp_perso$p8_44)
tmp_perso$p8_44<- as.numeric(tmp_perso$p8_44)
#tmp_autos <- tmp_autos[tmp_autos$p7_14 >= 1, ]
tmp_perso$p8_44
tmp_perso$persoincum <- ifelse(tmp_perso$p8_44 > 0, 1, 0)
tmp_perso$persoincum

perso <- tmp_perso[, c("LlaveHO", "persoincum")]
perso_pr <- table(perso$persoincum) / nrow(perso)
perso_pr 



############## Credito  GRPAL ########
tmp_grupal <- read.csv(file = "datos/TGRUPAL.csv", header=TRUE, sep=",", colClasses="character")
tmp_grupal <- conversor(tmp_grupal)
tmp_grupal$LlaveHO <- paste(tmp_grupal$upm,tmp_grupal$viv_sel, tmp_grupal$hogar, sep="")
tmp_grupal$p8_52
tmp_grupal$p8_52 <- ifelse(tmp_grupal$p8_52  == "", "00", tmp_grupal$p8_52 )
tmp_grupal$p8_52 <- as.numeric(tmp_grupal$p8_52)
#tmp_autos <- tmp_autos[tmp_autos$p7_14 >= 1, ]
tmp_grupal$p8_52
tmp_grupal$grupalincum <- ifelse(tmp_grupal$p8_52 > 0, 1, 0)
tmp_grupal$grupalincum

grupal <- tmp_grupal[, c("LlaveHO", "grupalincum")]
grupalpr <- table(grupal$grupalincum) / nrow(grupal)
grupalpr
############## ############## MERGING DATA ############## ############## 
all_tables <- list(nominapr,bancpr,deptpr,autospr, motospr, perso_pr , grupalpr)
combined_data <- do.call(cbind, all_tables)
colnames(combined_data) <- c("CredNomina", "CredTrajeta", "CredDepartamental", "CredAutos","CredMotos","CredPersonal","CredGrupal")
combined_data

percentages <- round(combined_data[2, ] * 100,2)
percentages
# Creacion de un datagrame para guardarlo en csv
data_frame <- data.frame(CredNomina = percentages["CredNomina"],
                         CredTrajeta = percentages["CredTrajeta"],
                         CredDepartamental = percentages["CredDepartamental"],
                         CredAutos = percentages["CredAutos"],
                         CredMotos = percentages["CredMotos"],
                         CredPersonal = percentages["CredPersonal"],
                         CredGrupal = percentages["CredGrupal"])
write.csv(data_frame, file = "deuda_proporcion.csv", row.names = FALSE)

# PLOT THE DATA
bp <- barplot(percentages, col = "blue", main = "Proporción de incumplimiento de pagos por tipo de deuda", 
        ylab = "%", xlab = "Tipo de Deuda", names.arg = colnames(combined_data))
text(bp, percentages, labels = sprintf("%.2f%%", percentages), pos = 3, cex = 0.8, col = "black")

barplot(combined_data, col = "blue", main = "Percentage of '0' Values by Debt Type", ylab = "Percentage", names.arg = colnames(combined_data))


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

# Predeccion de capacidad de incumplimiento con arboles de decision o regresion logistica
#  "alto riesgo", "riesgo medio" y "bajo riesgo".

# Predicción de la capacidad de pago mensual
# Modelo de Predicción de Probabilidad de Venta de Activos No Financieros
#Segmentación de clientes
# Modelo de Predicción de Uso de Fondos de Inversión:
# Modelo de Predicción de Cambio en la Tenencia de Activos No Financieros
