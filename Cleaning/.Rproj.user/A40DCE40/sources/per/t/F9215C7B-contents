




rm(list=ls(all=TRUE))
options(digits = 17)
#Establecer directorio de trabajo
setwd("Q:/Proyectos/ENFIH/datos/conjunto_de_datos_enfih_2019_csv/conjunto_de_datos_tmodulo_enfih_2019/conjunto_de_datos/Cleaning")


# Creo la tabla concentradora a partir de la tabla de hogares
tmp_hogar1 <- read.csv(file = "datos/THogar.csv", header=TRUE, sep=",", colClasses="character")

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
tmp_hogar1 <- conversor(tmp_hogar1)

tmp_hogar1$LlaveVI <- paste(tmp_hogar1$upm, tmp_hogar1$viv_sel, sep="")
tmp_hogar1$LlaveHO <- paste(tmp_hogar1$upm, tmp_hogar1$viv_sel, tmp_hogar1$hogar, sep="")

tmp_hogar1$fac_hog <- as.numeric(tmp_hogar1$fac_hog)
tmp_hogar1$h_ppal <- 0
# Variable que Identifaca Hogar Principal
tmp_hogar1$h_ppal <- ifelse(trimws(tmp_hogar1$p2_9) == "" | tmp_hogar1$p2_9 == "1", 1, 0)
# Variables de Activos NO_Financiero

tmp_hogar1$sacvo_nofn <- 0 #Hogar sin Activo No Financiero
tmp_hogar1$cacvo_nofn <- 0
tmp_hogar1$val_noFin <- 0 # Tenencia y Valor de losee activos financieros por hogar
tmp_hogar1$con_vpal <- 0
tmp_hogar1$val_vpal <- 0 # Tenencia y Valor de la vivienda PRincipal por hogar
tmp_hogar1$con_vsec <- 0
tmp_hogar1$val_vsec <- 0 # Tenencia y Valor de las propiedades distintas a la habitada
tmp_hogar1$con_vehic <- 0
tmp_hogar1$val_vehic <- 0 # Tenencia y Valor de los vehiculos por hogar
tmp_hogar1$con_menaje <- 0
tmp_hogar1$val_menaje <- 0 # Tenencia y Valor del Menaje del hogar ( Solo hogar Principal )
tmp_hogar1$con_ngcio <- 0
tmp_hogar1$val_ngcio <- 0 # Tenencia y valor del (los) negocios del hogar
tmp_hogar1$con_bprod <- 0
tmp_hogar1$val_bprod <- 0 # Tenencia y Valor de los bienes productivos
# Agrego Variables de Activos Financieros Variables agregadas
tmp_hogar1$sacvo_fin <- 0
tmp_hogar1$cacvo_fin <- 0
tmp_hogar1$val_finan <- 0 # tenencia y valor del os activos financierons por hogar
tmp_hogar1$c_ctachqs <- 0
tmp_hogar1$v_ctachqs <- 0 # tenencia y valor del cuenta de ahorro/cheques por hogar
tmp_hogar1$c_nompen <- 0
tmp_hogar1$v_nompen <- 0 # tenencia y valor del cuenta de nomina/pensión por hogar
tmp_hogar1$c_ctagob <- 0
tmp_hogar1$v_Ctagob <- 0 # tenencia y valor del cuenta apoyos de gobierno por hogar
tmp_hogar1$c_prvinvr <- 0
tmp_hogar1$v_prvinvr <- 0 # tenencia y valor del Inversiones/Plazo fijo por hogar
tmp_hogar1$c_afore <- 0
tmp_hogar1$v_afore <- 0 # tenencia y valor del afore por hogar
tmp_hogar1$c_segvid <- 0
tmp_hogar1$v_segvid <- 0 # tenencia y valor del seguro de vida por hogar
tmp_hogar1$c_otroaf <- 0
tmp_hogar1$v_otroaf <- 0 # tenencia y valor de otros activos financieros por hogar

# Agrego Variables de Activos Financieros componentes
tmp_hogar1$c_ahcaja <- 0
tmp_hogar1$v_ahcaja <- 0 # tenencia y valor de ahorro mediante caja de ahorro por hogar
tmp_hogar1$c_ahfami <- 0
tmp_hogar1$v_ahfami <- 0 # tenencia y valor de ahorro con familiares por hogar
tmp_hogar1$c_ahtanda <- 0
tmp_hogar1$v_ahtanda <- 0 # tenencia y valor de ahorro mediante tandas por hogar
tmp_hogar1$c_ahprest <- 0
tmp_hogar1$v_ahprest <- 0 # tenencia y valor de ahorro mediante prestamos por hogar
tmp_hogar1$c_ahotro1 <- 0
tmp_hogar1$v_ahotro1 <- 0 # tenencia y valor de ahorro con otros medios informales por hogar
tmp_hogar1$c_ahcta <- 0
tmp_hogar1$v_ahcta <- 0 # tenencia y valor de ahorro mediante cuenta de ahorro por hogar
tmp_hogar1$c_ahchqs <- 0
tmp_hogar1$v_ahchqs <- 0 # tenencia y valor de ahorro mediante cuentade cheques por hogar
tmp_hogar1$c_ahprv <- 0
tmp_hogar1$v_ahprv <- 0 # tenencia y valor de ahorro mediante Pagares de Plazo fijo por hogar
tmp_hogar1$c_ahinvrs <- 0
tmp_hogar1$v_ahinvrs <- 0 # tenencia y valor de ahorro mediante Inversiones por hogar
tmp_hogar1$c_ahotro2 <- 0
tmp_hogar1$v_ahotro2 <- 0 # tenencia y valor de ahorro mediante otros medios formales por hogar
tmp_hogar1$c_ahnompe <- 0

tmp_hogar1$v_ahnompe <- 0 # tenencia y valor de ahorro mediante cuenta de nomina o pensión por hogar
tmp_hogar1$c_ahctago <- 0
tmp_hogar1$v_ahctago <- 0 # tenencia y valor de ahorro mediante cuentas de apoyo de gobierno por hogar
tmp_hogar1$c_ahafore <- 0
tmp_hogar1$v_ahafore <- 0 # tenencia y valor de ahorro mediante cuenta de afore por hogar
tmp_hogar1$c_ahsegvi <- 0
tmp_hogar1$v_ahsegvi <- 0 # tenencia y valor de ahorro mediante SEGURO DE VIDA por hogar
tmp_hogar1$c_ahot18 <- 0
tmp_hogar1$v_ahot18 <- 0 # tenencia y valor de ahorro con otros medios formales por hogar
tmp_hogar1$c_activos <- 0
tmp_hogar1$v_activos <- 0
# variables de deuda
tmp_hogar1$ccred_vpal <- 0
tmp_hogar1$mnto_vpal <- 0
tmp_hogar1$ccred_vsec <- 0
tmp_hogar1$mnto_vsec <- 0
tmp_hogar1$ccred_ot1 <- 0
tmp_hogar1$mnto_ot1 <- 0
tmp_hogar1$ccred_neg <- 0
tmp_hogar1$mnto_neg <- 0
tmp_hogar1$ccred_agri <- 0
tmp_hogar1$mnto_agri <- 0
tmp_hogar1$ccred_auto <- 0
tmp_hogar1$mnto_auto <- 0
tmp_hogar1$ccred_moto <- 0
tmp_hogar1$mnto_moto <- 0
tmp_hogar1$ccred_ot2 <- 0
tmp_hogar1$mnto_ot2 <- 0
tmp_hogar1$ccred_dptl <- 0
tmp_hogar1$mnto_dptl <- 0
tmp_hogar1$ccred_bnco <- 0
tmp_hogar1$mnto_bnco <- 0
tmp_hogar1$ccred_nom <- 0
tmp_hogar1$mnto_nom <- 0
tmp_hogar1$ccred_edu <- 0
tmp_hogar1$mnto_edu <- 0
tmp_hogar1$ccred_per <- 0
tmp_hogar1$mnto_per <- 0
tmp_hogar1$ccred_gru <- 0
tmp_hogar1$mnto_gru <- 0
tmp_hogar1$Cred_hip <- 0
tmp_hogar1$mto_hipot <- 0
tmp_hogar1$cred_nohip <- 0
tmp_hogar1$mto_nohipo <- 0
tmp_hogar1$cred_tcrd <- 0
tmp_hogar1$mto_tcrd <- 0
tmp_hogar1$cred_nmpe <- 0
tmp_hogar1$mto_nmpe <- 0
tmp_hogar1$cred_aumo <- 0
tmp_hogar1$mto_aumo <- 0
tmp_hogar1$cred_rest <- 0
tmp_hogar1$mto_rest <- 0
tmp_hogar1$cred_tot <- 0
tmp_hogar1$mto_ctot <- 0
tmp_hogar1$sin_deuda <- 0

# *****************INICIO DE SECCION DE ACTIVOS NO FINANCIEROS
# **************************************************************************************
#
# *****Tablas auxiliares para el calculo de activos
# * Los valores No_Responde/No sabe se concideran en tenencia del activo con valor 0
# * Las tablas auxiliares estan a nivel hogar ( UPM, Viv_sel, Hogar)
# * La tabla de variables de vivienda vicvivenda principal y menaje de casa se pegan a la tabla de hogar solo en hogar Principal
# ******Vivienda Principal y menaje de casa

aux_vivienda <- read.csv(file = "datos/TVivienda.csv", header=TRUE, sep=",", colClasses="character")

# CONVIERTO EL NOMBRE DE LAS COLUMNAS EN MINUSCULAS
aux_vivienda <- conversor(aux_vivienda)

# CREO UNA NUEVA IDKEY
aux_vivienda$LlaveVI <- paste(aux_vivienda$upm, aux_vivienda$viv_sel, sep="")
# Se consideran valores que no sean nulos o mayores 0 (mayor a 0 significa tenencia)
aux_vivienda <- aux_vivienda[(as.integer(aux_vivienda$p4_10) > 0 | as.integer(aux_vivienda$p4_55) > 0), ]
# Eliminamos valores donde se indique como 'desconocido'. Si es 'desconocido' entonces se marca ocmo 0
aux_vivienda$con_vpal <- ifelse(as.integer(aux_vivienda$p4_10)>= 1 & as.integer(aux_vivienda$p4_10) <= 999999887, 1, 0)
# Lo mismo pero el valor del menaje cuando es desconocido se marca como 0
aux_vivienda$val_vpal <- ifelse(as.integer(aux_vivienda$p4_10)>= 1 & as.integer(aux_vivienda$p4_10) <= 999999887, as.integer(aux_vivienda$p4_10), 0)

# Valor de la vivienda DEclarado en monto u obtenido en pregunta de rescate
aux_vivienda$con_vpal <- ifelse(as.integer(aux_vivienda$p4_10) %in% c(999999888, 999999999) & aux_vivienda$p4_10a == "1", 1, aux_vivienda$con_vpal)
aux_vivienda$val_vpal <- ifelse(as.integer(aux_vivienda$p4_10) %in% c(999999888, 999999999) & aux_vivienda$p4_10a == "1", 200000, aux_vivienda$val_vpal)
aux_vivienda$con_vpal <- ifelse(as.integer(aux_vivienda$p4_10) %in% c(999999888, 999999999) & aux_vivienda$p4_10a == "2", 1, aux_vivienda$con_vpal)
aux_vivienda$val_vpal <- ifelse(as.integer(aux_vivienda$p4_10) %in% c(999999888, 999999999) & aux_vivienda$p4_10a == "2", 500000, aux_vivienda$val_vpal)
aux_vivienda$con_vpal <- ifelse(as.integer(aux_vivienda$p4_10) %in% c(999999888, 999999999) & aux_vivienda$p4_10a == "3", 1, aux_vivienda$con_vpal)
aux_vivienda$val_vpal <- ifelse(as.integer(aux_vivienda$p4_10) %in% c(999999888, 999999999) & aux_vivienda$p4_10a == "3", 800000, aux_vivienda$val_vpal)
aux_vivienda$con_vpal <- ifelse(as.integer(aux_vivienda$p4_10) %in% c(999999888, 999999999) & aux_vivienda$p4_10a == "4", 1, aux_vivienda$con_vpal)
aux_vivienda$val_vpal <- ifelse(as.integer(aux_vivienda$p4_10) %in% c(999999888, 999999999) & aux_vivienda$p4_10a == "4", 1750000, aux_vivienda$val_vpal)
aux_vivienda$con_vpal <- ifelse(as.integer(aux_vivienda$p4_10) %in% c(999999888, 999999999) & aux_vivienda$p4_10a == "5", 1, aux_vivienda$con_vpal)
aux_vivienda$val_vpal <- ifelse(as.integer(aux_vivienda$p4_10) %in% c(999999888, 999999999) & aux_vivienda$p4_10a == "5", 3750000, aux_vivienda$val_vpal)
aux_vivienda$con_vpal <- ifelse(as.integer(aux_vivienda$p4_10) %in% c(999999888, 999999999) & aux_vivienda$p4_10a == "6", 1, aux_vivienda$con_vpal)
aux_vivienda$val_vpal <- ifelse(as.integer(aux_vivienda$p4_10) %in% c(999999888, 999999999) & aux_vivienda$p4_10a == "6", 7500000, aux_vivienda$val_vpal)
aux_vivienda$con_vpal <- ifelse(as.integer(aux_vivienda$p4_10) %in% c(999999888, 999999999) & aux_vivienda$p4_10a %in% c("8","9"), 1, aux_vivienda$con_vpal)
aux_vivienda$val_vpal <- ifelse(as.integer(aux_vivienda$p4_10) %in% c(999999888, 999999999) & aux_vivienda$p4_10a %in% c("8","9"), 0, aux_vivienda$val_vpal)

# ******Menaje de casa
aux_vivienda$con_menaje <- ifelse(as.integer(aux_vivienda$p4_55) >= 1 & as.integer(aux_vivienda$p4_55) <= 999999887, 1, 0)
aux_vivienda$val_menaje <- ifelse(as.integer(aux_vivienda$p4_55) >= 1 & as.integer(aux_vivienda$p4_55) <= 999999887, as.integer(aux_vivienda$p4_55), 0)
# Marca tenencia 1 y valor cero si el valor del menaje fue NE/NR
aux_vivienda$con_menaje <- ifelse(as.integer(aux_vivienda$p4_55) %in% c(999999888, 999999999), 1, aux_vivienda$con_menaje)
aux_vivienda$val_menaje <- ifelse(as.integer(aux_vivienda$p4_55) %in% c(999999888, 999999999), 0, aux_vivienda$val_menaje)


# ***Vivienda Secundaria
tmp_propiedad <- read.csv(file = "datos/TPropiedad.csv", header=TRUE, sep=",", colClasses="character")
tmp_propiedad <- conversor(tmp_propiedad)
# CREACION DE LLAVES PRIMARIAS
tmp_propiedad$LlaveVI <- paste(tmp_propiedad$upm, tmp_propiedad$viv_sel, sep="")
tmp_propiedad$LlaveHO <- paste(tmp_propiedad$upm, tmp_propiedad$viv_sel, tmp_propiedad$hogar, sep="")
tmp_propiedad$LlaveSD <- paste(tmp_propiedad$upm, tmp_propiedad$viv_sel, tmp_propiedad$hogar, tmp_propiedad$n_ren, sep="")
# Orden de filas dependiendo del IDKEY
tmp_propiedad <- tmp_propiedad[order(tmp_propiedad$LlaveHO), ]
tmp_propiedad$factor <- as.integer(tmp_propiedad$factor)

# Valor de la vivienda Secundaria
tmp_propiedad$val_vsec <- ifelse(as.integer(tmp_propiedad$p5_10) >= 1 & as.integer(tmp_propiedad$p5_10) <= 999999887, as.integer(tmp_propiedad$p5_10), 0)
aux_propiedad <- aggregate(tmp_propiedad[,c("val_vsec")],by = list(tmp_propiedad$LlaveHO),FUN = sum)
aux_propiedad$con_vsec <- 1
# LAS COLUMNAS OBTENIDAS POR EL AGRUPAMIENTO SE RENOMBRAN
colnames(aux_propiedad)[colnames(aux_propiedad) == "Group.1" ] <- "LlaveHO"
colnames(aux_propiedad)[colnames(aux_propiedad) == "x" ] <- "val_vsec"

# **** Bienes Productivos
tmp_Bprod <- read.csv(file = "datos/TModulo.csv", header=TRUE, sep=",", colClasses="character")
tmp_Bprod <- conversor(tmp_Bprod)
tmp_Bprod$LlaveVI <- paste(tmp_Bprod$upm, tmp_Bprod$viv_sel, sep="")
tmp_Bprod$LlaveHO <- paste(tmp_Bprod$upm, tmp_Bprod$viv_sel, tmp_Bprod$hogar, sep="")
tmp_Bprod$LlaveSD <- paste(tmp_Bprod$upm, tmp_Bprod$viv_sel, tmp_Bprod$hogar, tmp_Bprod$n_ren, sep="")
tmp_Bprod <- tmp_Bprod[(tmp_Bprod$p6_23 == "1" ), ] # eliminar registros que no cumplan con la condición
tmp_Bprod <- tmp_Bprod[order(tmp_Bprod$LlaveHO), ] # Ordenar el dataframe por hogar
# LAS COLUMNAS OBTENIDAS POR EL AGRUPAMIENTO SE RENOMBRAN
tmp_Bprod$factor <- as.integer(tmp_Bprod$factor)

# Valor de Bienes Productivos
tmp_Bprod$val_bprod <- ifelse(as.integer(tmp_Bprod$p6_24) >= 1 & as.integer(tmp_Bprod$p6_24) <= 999999887, as.integer(tmp_Bprod$p6_24), 0)
# agrupamos los valores de la segundas propiedades tomando en cuenta el IDKEY
aux_Bprod <- aggregate(tmp_Bprod[,c("val_bprod")],by = list(tmp_Bprod$LlaveHO),FUN = sum) # obtener on df agrupado por hogar
aux_Bprod$con_bprod <- 1
# LAS COLUMNAS OBTENIDAS CONPOR EL AGRUPAMIENTO SE RENOMBRAN
colnames(aux_Bprod)[colnames(aux_Bprod) == "Group.1" ] <- "LlaveHO"
colnames(aux_Bprod)[colnames(aux_Bprod) == "x" ] <- "val_bprod"

# ****Vehículo Auto/Moto/Otros
tmp_vehic <- read.csv(file = "datos/TModulo.csv", header=TRUE, sep=",", colClasses="character")
tmp_vehic <- conversor(tmp_vehic)

tmp_vehic$LlaveVI <- paste(tmp_vehic$upm, tmp_vehic$viv_sel, sep="")
tmp_vehic$LlaveHO <- paste(tmp_vehic$upm, tmp_vehic$viv_sel, tmp_vehic$hogar, sep="")
tmp_vehic$LlaveSD <- paste(tmp_vehic$upm, tmp_vehic$viv_sel, tmp_vehic$hogar, tmp_vehic$n_ren, sep="")
tmp_vehic <- tmp_vehic[(tmp_vehic$p7_1_1 == "1" | tmp_vehic$p7_1_2 == "1" | tmp_vehic$p7_1_3 == "1"), ] # eliminar registros que no cumplan con la condición
tmp_vehic <- tmp_vehic[order(tmp_vehic$LlaveHO), ]# Ordenar el dataframe por hogar
tmp_vehic$factor <- as.integer(tmp_vehic$factor)
tmp_vehic$VAL_vehic <- ifelse(as.integer(tmp_vehic$p7_3_1) >= 1 & as.integer(tmp_vehic$p7_3_1) <= 999999887, as.integer(tmp_vehic$p7_3_1), 0)
tmp_vehic$VAL_vehic <- ifelse(tmp_vehic$VAL_vehic + as.integer(tmp_vehic$p7_3_2) >= 1 & tmp_vehic$VAL_vehic + as.integer(tmp_vehic$p7_3_2) <= 999999887, tmp_vehic$VAL_vehic + as.integer(tmp_vehic$p7_3_2), tmp_vehic$VAL_vehic)
tmp_vehic$VAL_vehic <- ifelse(tmp_vehic$VAL_vehic + as.integer(tmp_vehic$p7_3_3) >= 1 & tmp_vehic$VAL_vehic + as.integer(tmp_vehic$p7_3_3) <= 999999887, tmp_vehic$VAL_vehic +as.integer(tmp_vehic$p7_3_3), tmp_vehic$VAL_vehic)
aux_vehic <- aggregate(tmp_vehic[,c("VAL_vehic")],by = list(tmp_vehic$LlaveHO), FUN = sum) # obtener on df agrupado por hogar
aux_vehic$con_vehic <- 1
colnames(aux_vehic)[colnames(aux_vehic) == "Group.1" ] <- "LlaveHO"
colnames(aux_vehic)[colnames(aux_vehic) == "x" ] <- "val_vehic"

# ****** Negocio
# ************ Dueño Unico
tmp_unico <- read.csv(file = "datos/TUnico.csv", header=TRUE, sep=",", colClasses="character")
tmp_unico <- conversor(tmp_unico)

tmp_unico$LlaveVI <- paste(tmp_unico$upm, tmp_unico$viv_sel, sep="")
tmp_unico$LlaveHO <- paste(tmp_unico$upm, tmp_unico$viv_sel, tmp_unico$hogar, sep="")
tmp_unico$LlaveSD <- paste(tmp_unico$upm, tmp_unico$viv_sel, tmp_unico$hogar, tmp_unico$n_ren, sep="")
tmp_unico <- tmp_unico[order(tmp_unico$LlaveHO), ] # Ordenar el dataframe por hogar
tmp_unico$factor <- as.integer(tmp_unico$factor)
tmp_unico$val_unico <- ifelse(as.integer(tmp_unico$p6_9) >= 1 & as.integer(tmp_unico$p6_9) <= 999999887, as.integer(tmp_unico$p6_9), 0)
aux_unico <- aggregate(tmp_unico[,c("val_unico")],by = list(tmp_unico$LlaveHO), FUN = sum) # obtener on df agrupado por hogar
colnames(aux_unico)[colnames(aux_unico) == "Group.1" ] <- "LlaveHO"
colnames(aux_unico)[colnames(aux_unico) == "x" ] <- "val_unico"


# ************Dueño de una parte
tmp_comparte <- read.csv(file = "datos/TComparte.csv", header=TRUE, sep=",", colClasses="character")
tmp_comparte <- conversor(tmp_comparte)

tmp_comparte$LlaveVI <- paste(tmp_comparte$upm, tmp_comparte$viv_sel, sep="")
tmp_comparte$LlaveHO <- paste(tmp_comparte$upm, tmp_comparte$viv_sel, tmp_comparte$hogar, sep="")
tmp_comparte$LlaveSD <- paste(tmp_comparte$upm, tmp_comparte$viv_sel, tmp_comparte$hogar, tmp_comparte$n_ren, sep="")

tmp_comparte <- tmp_comparte[order(tmp_comparte$LlaveHO), ] # Ordenar el dataframe por hogar
tmp_comparte$factor <- as.integer(tmp_comparte$factor)
tmp_comparte$val_compar <- ifelse(as.integer(tmp_comparte$p6_4) >= 1 & as.integer(tmp_comparte$p6_4) <= 999999887, as.integer(tmp_comparte$p6_4), 0)
aux_comparte <- aggregate(tmp_comparte[,c("val_compar")],by = list(tmp_comparte$LlaveHO), FUN = sum) # obtener on df agrupado por hogar
colnames(aux_comparte)[colnames(aux_comparte) == "Group.1" ] <- "LlaveHO"
colnames(aux_comparte)[colnames(aux_comparte) == "x" ] <- "val_compar"

# * Asigna la tenencia de cada activo no financiero a la tabla de Hogar valor de cada tipo
# ****vivienda Principal y menaje de casa
tmp_hogar1 <- merge(tmp_hogar1, aux_vivienda[, names(aux_vivienda) %in% c("h_ppal", "con_vpal", "val_vpal", "con_menaje", "val_menaje", "LlaveVI") ], by="LlaveVI", all.x = T)
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "con_vpal.x" ] <- "con_vpal"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "val_vpal.x" ] <- "val_vpal"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "con_menaje.x"] <- "con_menaje"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "val_menaje.x"] <- "val_menaje"
tmp_hogar1$con_vpal <- ifelse(tmp_hogar1$h_ppal == 1, tmp_hogar1$con_vpal.y, tmp_hogar1$con_vpal)
tmp_hogar1$val_vpal <- ifelse(tmp_hogar1$h_ppal == 1, tmp_hogar1$val_vpal.y, tmp_hogar1$val_vpal)
tmp_hogar1$con_menaje <- ifelse(tmp_hogar1$h_ppal == 1, tmp_hogar1$con_menaje.y, tmp_hogar1$con_menaje)
tmp_hogar1$val_menaje <- ifelse(tmp_hogar1$h_ppal == 1, tmp_hogar1$val_menaje.y, tmp_hogar1$val_menaje)
tmp_hogar1 <- tmp_hogar1[, !names(tmp_hogar1) %in% c("con_vpal.y", "val_vpal.y", "con_menaje.y", "val_menaje.y") ] # Eliminar columnas


# ****vivienda secundaria
tmp_hogar1 <- merge(tmp_hogar1, aux_propiedad[, names(aux_propiedad) %in% c("con_vsec", "val_vsec", "LlaveHO") ], by="LlaveHO", all.x = T)
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "con_vsec.x" ] <- "con_vsec"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "val_vsec.x" ] <- "val_vsec"
tmp_hogar1$con_vsec = ifelse(is.na(tmp_hogar1$con_vsec.y), 0, tmp_hogar1$con_vsec.y)
tmp_hogar1$val_vsec = ifelse(is.na(tmp_hogar1$val_vsec.y), 0, tmp_hogar1$val_vsec.y)
tmp_hogar1 <- tmp_hogar1[, !names(tmp_hogar1) %in% c("con_vsec.y", "val_vsec.y") ] # Eliminar columnas
# ****vehículo
tmp_hogar1 <- merge(tmp_hogar1, aux_vehic[, names(aux_vehic) %in% c("con_vehic", "val_vehic", "LlaveHO") ], by="LlaveHO", all.x = T)
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "con_vehic.x" ] <- "con_vehic"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "val_vehic.x" ] <- "val_vehic"
tmp_hogar1$con_vehic <- ifelse(is.na(tmp_hogar1$con_vehic.y), 0, tmp_hogar1$con_vehic.y)
tmp_hogar1$val_vehic <- ifelse(is.na(tmp_hogar1$val_vehic.y), 0, tmp_hogar1$val_vehic.y)
tmp_hogar1 <- tmp_hogar1[, !names(tmp_hogar1) %in% c("con_vehic.y", "val_vehic.y") ] # Eliminar columnas
# ****Bienes Productivos
tmp_hogar1 <- merge(tmp_hogar1, aux_Bprod[, names(aux_Bprod) %in% c("con_bprod", "val_bprod", "LlaveHO") ], by="LlaveHO", all.x = T)
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "con_bprod.x" ] <- "con_bprod"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "val_bprod.x" ] <- "val_bprod"
tmp_hogar1$con_bprod <- ifelse(is.na(tmp_hogar1$con_bprod.y), 0, tmp_hogar1$con_bprod.y)
tmp_hogar1$val_bprod <- ifelse(is.na(tmp_hogar1$val_bprod.y), 0, tmp_hogar1$val_bprod.y)
tmp_hogar1 <- tmp_hogar1[, !names(tmp_hogar1) %in% c("con_bprod.y", "val_bprod.y") ] # Eliminar columnas

# *****Negocio
tmp_hogar1 <- merge(tmp_hogar1, aux_unico[, names(aux_unico) %in% c("val_unico", "LlaveHO") ], by="LlaveHO", all.x = T)
tmp_hogar1$con_ngcio <- ifelse(is.na(tmp_hogar1$val_unico), 0, 1)
tmp_hogar1$val_ngcio <- ifelse(is.na(tmp_hogar1$val_unico), 0, tmp_hogar1$val_unico)
tmp_hogar1 <- tmp_hogar1[, !names(tmp_hogar1) %in% c("val_unico") ] # Eliminar columnas
tmp_hogar1 <- merge(tmp_hogar1, aux_comparte[, names(aux_comparte) %in% c("val_compar", "LlaveHO") ], by="LlaveHO", all.x = T)
tmp_hogar1$con_ngcio <- ifelse(is.na(tmp_hogar1$val_compar), tmp_hogar1$con_ngcio, 1)
tmp_hogar1$val_compar <- ifelse(is.na(tmp_hogar1$val_compar), 0, tmp_hogar1$val_compar)
tmp_hogar1$val_ngcio <- tmp_hogar1$val_ngcio + tmp_hogar1$val_compar
tmp_hogar1 <- tmp_hogar1[, !names(tmp_hogar1) %in% c("val_compar") ] # Eliminar columnas
# ********************************************************
# ***HOgares con Activos No financiero
# *** su valor
# *** Hogares Sin Activos No financieros
tmp_hogar1$cacvo_nofn <- ifelse(ifelse(is.na(tmp_hogar1$con_vpal ), 0, tmp_hogar1$con_vpal ) +
                                  ifelse(is.na(tmp_hogar1$con_vsec ), 0, tmp_hogar1$con_vsec ) +
                                  ifelse(is.na(tmp_hogar1$con_vehic ), 0, tmp_hogar1$con_vehic ) +
                                  ifelse(is.na(tmp_hogar1$con_menaje), 0, tmp_hogar1$con_menaje) +
                                  ifelse(is.na(tmp_hogar1$con_ngcio ), 0, tmp_hogar1$con_ngcio ) +
                                  ifelse(is.na(tmp_hogar1$con_bprod ), 0, tmp_hogar1$con_bprod ) > 0 , 1, 0)
tmp_hogar1$val_nofin <- ifelse(is.na(tmp_hogar1$val_vpal ), 0, tmp_hogar1$val_vpal ) +
  ifelse(is.na(tmp_hogar1$val_vsec ), 0, tmp_hogar1$val_vsec ) +
  ifelse(is.na(tmp_hogar1$val_vehic ), 0, tmp_hogar1$val_vehic ) +
  ifelse(is.na(tmp_hogar1$val_menaje), 0, tmp_hogar1$val_menaje) +
  ifelse(is.na(tmp_hogar1$val_ngcio ), 0, tmp_hogar1$val_ngcio ) +
  ifelse(is.na(tmp_hogar1$val_bprod ), 0, tmp_hogar1$val_bprod )
tmp_hogar1$sacvo_nofn <- ifelse(tmp_hogar1$cacvo_nofn == 1, 0, 1)
tmp_hogar1$aux1 <- tmp_hogar1$fac_hog * tmp_hogar1$cacvo_nofn
tmp_hogar1$aux2 <- tmp_hogar1$fac_hog * tmp_hogar1$val_nofin
gpo1 <- sum(aggregate(tmp_hogar1[,c("aux1")],by = list(tmp_hogar1$LlaveHO), FUN = sum, na.rm=TRUE)$x) # obtener on df agrupado por hogar
gpo2 <- sum(aggregate(tmp_hogar1[,c("aux2")],by = list(tmp_hogar1$LlaveHO), FUN = sum, na.rm=TRUE)$x)/1000000 # obtener on df agrupado por hogar
nofinan <- data.frame(list("id" = 0, "desc" = " Activos NO Financieros", "hogares" = gpo1, "monto" = gpo2))
tmp_hogar1 <- tmp_hogar1[, !names(tmp_hogar1) %in% c("aux1","aux2") ] # Eliminar columnas
# **************************************************************************************
# *****************FIN DE SECCION DE ACTIVOS NO FINANCIEROS
# **************************************************************************************
# **************************************************************************************
# *****************INICIO DE SECCION DE ACTIVOS FINANCIEROS
# **************************************************************************************
# *****Tablasauxiliares para el calculo de activos Financiero
# * Los valores No_Responde/No sabe se concideran en tenencia del activo con valor 0
# * Las tablas auxiliares estan a nivel hogar ( UPM, Viv_sel, Hogar)
# * Tabla auxiliar a partir de tmodulo con la info de activos financieros
tmp_financieros <- read.csv(file = "datos/TModulo.csv", header=TRUE, sep=",", colClasses="character")
tmp_financieros <- conversor(tmp_financieros)

tmp_financieros$LlaveVI <- paste(tmp_financieros$upm, tmp_financieros$viv_sel, sep="")
tmp_financieros$LlaveHO <- paste(tmp_financieros$upm, tmp_financieros$viv_sel, tmp_financieros$hogar, sep="")
tmp_financieros$LlaveSD <- paste(tmp_financieros$upm, tmp_financieros$viv_sel, tmp_financieros$hogar, tmp_financieros$n_ren, sep="")
tmp_financieros$Elimina <- ifelse( ifelse(is.na(tmp_financieros$p9_1_1 ) | gsub(" ", "", tmp_financieros$p9_1_1 ) == "", 0, as.integer(tmp_financieros$p9_1_1 )) +
                                     ifelse(is.na(tmp_financieros$p9_1_2 ) | gsub(" ", "", tmp_financieros$p9_1_2 ) == "", 0, as.integer(tmp_financieros$p9_1_2 )) +
                                     ifelse(is.na(tmp_financieros$p9_1_3 ) | gsub(" ", "", tmp_financieros$p9_1_3 ) == "", 0, as.integer(tmp_financieros$p9_1_3 )) +
                                     ifelse(is.na(tmp_financieros$p9_1_4 ) | gsub(" ", "", tmp_financieros$p9_1_4 ) == "", 0, as.integer(tmp_financieros$p9_1_4 )) +
                                     ifelse(is.na(tmp_financieros$p9_1_5 ) | gsub(" ", "", tmp_financieros$p9_1_5 ) == "", 0, as.integer(tmp_financieros$p9_1_5 )) +
                                     ifelse(is.na(tmp_financieros$p9_3_1 ) | gsub(" ", "", tmp_financieros$p9_3_1 ) == "", 0, as.integer(tmp_financieros$p9_3_1 )) +
                                     ifelse(is.na(tmp_financieros$p9_3_2 ) | gsub(" ", "", tmp_financieros$p9_3_2 ) == "", 0, as.integer(tmp_financieros$p9_3_2 )) +
                                     ifelse(is.na(tmp_financieros$p9_3_3 ) | gsub(" ", "",tmp_financieros$p9_3_3 ) == "", 0, as.integer(tmp_financieros$p9_3_3 )) +
                                     ifelse(is.na(tmp_financieros$p9_3_4 ) | gsub(" ", "", tmp_financieros$p9_3_4 ) == "", 0, as.integer(tmp_financieros$p9_3_4 )) +
                                     ifelse(is.na(tmp_financieros$p9_3_5 ) | gsub(" ", "", tmp_financieros$p9_3_5 ) == "", 0, as.integer(tmp_financieros$p9_3_5 )) +
                                     ifelse(is.na(tmp_financieros$p9_3_6 ) | gsub(" ", "", tmp_financieros$p9_3_6 ) == "", 0, as.integer(tmp_financieros$p9_3_6 )) +
                                     ifelse(is.na(tmp_financieros$p9_3_7 ) | gsub(" ", "", tmp_financieros$p9_3_7 ) == "", 0, as.integer(tmp_financieros$p9_3_7 )) +
                                     ifelse(is.na(tmp_financieros$p9_10 ) | gsub(" ", "", tmp_financieros$p9_10 ) == "", 0, as.integer(tmp_financieros$p9_10 )) +
                                     ifelse(is.na(tmp_financieros$p9_15_1) | gsub(" ", "", tmp_financieros$p9_15_1) == "", 0, as.integer(tmp_financieros$p9_15_1)) > 0, "No", "Si")
tmp_financieros <- tmp_financieros[ tmp_financieros$Elimina != "Si" , ] # eliminar registros que cumplan con la condición
# Eliminar columnas innecesarias
tmp_financieros <- tmp_financieros[, names(tmp_financieros) %in% c("LlaveVI", "LlaveHO", "LlaveSD", "p9_1_1", "p9_1_2", "p9_1_3", "p9_1_4", "p9_1_5", "p9_3_7", "p9_15_1", "p9_10", "p9_3_5", "p9_3_6", "p9_3_2", "p9_3_3", "p9_3_4", "p9_3_1", "p9_2_1", "p9_2_2", "p9_2_3", "p9_2_4", "p9_2_5", "p9_5a_3", "p9_5a_4", "p9_5a_5", "p9_5a_6", "p9_5a_7", "p9_7", "p9_9", "p9_11", "p9_16_1") ] # Eliminar columnas
# ***Agrego a la tabla auxiliar los campos necesarios para la preparación de la info
# **** ( Tenencia del instrumento/ tratamiento a no especificados
tmp_financieros$c_ahcaja <- 0
tmp_financieros$v_ahcaja <- 0
tmp_financieros$c_ahfami <- 0
tmp_financieros$v_ahfami <- 0
tmp_financieros$c_ahtanda <- 0
tmp_financieros$v_ahtanda <- 0
tmp_financieros$c_ahprest <- 0
tmp_financieros$v_ahprest <- 0
tmp_financieros$c_ahotro1 <- 0
tmp_financieros$v_ahotro1 <- 0
tmp_financieros$c_ahcta <- 0
tmp_financieros$v_ahcta <- 0
tmp_financieros$c_ahchqs <- 0
tmp_financieros$v_ahchqs <- 0
tmp_financieros$c_ahprv <- 0
tmp_financieros$v_ahprv <- 0
tmp_financieros$c_ahinvrs <- 0
tmp_financieros$v_ahinvrs <- 0
tmp_financieros$c_ahotro2 <- 0
tmp_financieros$v_ahotro2 <- 0
tmp_financieros$c_ahnompe <- 0
tmp_financieros$v_ahnompe <- 0
tmp_financieros$c_ahctago <- 0
tmp_financieros$v_ahctago <- 0
tmp_financieros$c_ahafore <- 0
tmp_financieros$v_ahafore <- 0
tmp_financieros$c_ahsegvi <- 0
tmp_financieros$v_ahsegvi <- 0
tmp_financieros$c_ahot18 <- 0
tmp_financieros$v_ahot18 <- 0

# ******* Seleccion de tenencia y tratamiento a No_Rsponde/No_Sabe
tmp_financieros$c_ahcaja <- ifelse(tmp_financieros$p9_1_1 == "1", 1, tmp_financieros$c_ahcaja)
tmp_financieros$v_ahcaja <- ifelse(tmp_financieros$p9_1_1 == "1" & as.integer(tmp_financieros$p9_2_1) >= 1 & as.integer(tmp_financieros$p9_2_1) <= 999887 , as.integer(tmp_financieros$p9_2_1) , tmp_financieros$v_ahcaja)
tmp_financieros$c_ahfami <- ifelse(tmp_financieros$p9_1_2 == "1", 1,tmp_financieros$c_ahfami)
tmp_financieros$v_ahfami <- ifelse(tmp_financieros$p9_1_2 == "1" & as.integer(tmp_financieros$p9_2_2) >= 1 & as.integer(tmp_financieros$p9_2_2) <= 999887 , as.integer(tmp_financieros$p9_2_2) , tmp_financieros$v_ahfami)
tmp_financieros$c_ahtanda <- ifelse(tmp_financieros$p9_1_3 == "1", 1, tmp_financieros$c_ahtanda)
tmp_financieros$v_ahtanda <- ifelse(tmp_financieros$p9_1_3 == "1" & as.integer(tmp_financieros$p9_2_3) >= 1 & as.integer(tmp_financieros$p9_2_3) <= 999887 , as.integer(tmp_financieros$p9_2_3) , tmp_financieros$v_ahtanda)
tmp_financieros$c_ahprest <- ifelse(tmp_financieros$p9_1_4 == "1", 1, tmp_financieros$c_ahprest)
tmp_financieros$v_ahprest <- ifelse(tmp_financieros$p9_1_4 == "1" & as.integer(tmp_financieros$p9_2_4) >= 1 & as.integer(tmp_financieros$p9_2_4) <= 999887 , as.integer(tmp_financieros$p9_2_4) , tmp_financieros$v_ahprest)
tmp_financieros$c_ahotro1 <- ifelse(tmp_financieros$p9_1_5 == "1", 1, tmp_financieros$c_ahotro1)
tmp_financieros$v_ahotro1 <- ifelse(tmp_financieros$p9_1_5 == "1" & as.integer(tmp_financieros$p9_2_5) >= 1 & as.integer(tmp_financieros$p9_2_5) <= 999887 , as.integer(tmp_financieros$p9_2_5) , tmp_financieros$v_ahotro1)
tmp_financieros$c_ahcta <- ifelse(tmp_financieros$p9_3_3 == "1", 1, tmp_financieros$c_ahcta)
tmp_financieros$v_ahcta <- ifelse(tmp_financieros$p9_3_3 == "1" & as.integer(tmp_financieros$p9_5a_3) >= 1 & as.integer(tmp_financieros$p9_5a_3) <= 999999887, as.integer(tmp_financieros$p9_5a_3), tmp_financieros$v_ahcta)
tmp_financieros$c_ahchqs <- ifelse(tmp_financieros$p9_3_4 == "1", 1, tmp_financieros$c_ahchqs)
tmp_financieros$v_ahchqs <- ifelse(tmp_financieros$p9_3_4 == "1" & as.integer(tmp_financieros$p9_5a_4) >= 1 & as.integer(tmp_financieros$p9_5a_4) <= 999999887, as.integer(tmp_financieros$p9_5a_4), tmp_financieros$v_ahchqs)
tmp_financieros$c_ahprv <- ifelse(tmp_financieros$p9_3_5 == "1", 1, tmp_financieros$c_ahprv)
tmp_financieros$v_ahprv <- ifelse(tmp_financieros$p9_3_5 == "1" & as.integer(tmp_financieros$p9_5a_5) >= 1 & as.integer(tmp_financieros$p9_5a_5) <= 999999887, as.integer(tmp_financieros$p9_5a_5), tmp_financieros$v_ahprv)
tmp_financieros$c_ahinvrs <- ifelse(tmp_financieros$p9_3_6 == "1", 1, tmp_financieros$c_ahinvrs)
tmp_financieros$v_ahinvrs <- ifelse(tmp_financieros$p9_3_6 == "1" & as.integer(tmp_financieros$p9_5a_6) >= 1 & as.integer(tmp_financieros$p9_5a_6) <= 999999887, as.integer(tmp_financieros$p9_5a_6), tmp_financieros$v_ahinvrs)
tmp_financieros$c_ahotro2 <- ifelse(tmp_financieros$p9_3_7 == "1", 1, tmp_financieros$c_ahotro2)
tmp_financieros$v_ahotro2 <- ifelse(tmp_financieros$p9_3_7 == "1" & as.integer(tmp_financieros$p9_5a_7) >= 1 & as.integer(tmp_financieros$p9_5a_7) <= 999999887, as.integer(tmp_financieros$p9_5a_7), tmp_financieros$v_ahotro2)
tmp_financieros$c_ahnompe <- ifelse(tmp_financieros$p9_3_1 == "1", 1, tmp_financieros$c_ahnompe)
tmp_financieros$v_ahnompe <- ifelse(tmp_financieros$p9_3_1 == "1" & as.integer(tmp_financieros$p9_7) >= 1 & as.integer(tmp_financieros$p9_7) <= 9999887 , as.integer(tmp_financieros$p9_7) , tmp_financieros$v_ahnompe)
tmp_financieros$c_ahctago <- ifelse(tmp_financieros$p9_3_2 == "1", 1, tmp_financieros$c_ahctago)
tmp_financieros$v_ahctago <- ifelse(tmp_financieros$p9_3_2 == "1" & as.integer(tmp_financieros$p9_9) >= 1 & as.integer(tmp_financieros$p9_9) <= 999999887, as.integer(tmp_financieros$p9_9) , tmp_financieros$v_ahctago)
tmp_financieros$c_ahafore <- ifelse(tmp_financieros$p9_10 == "1", 1, tmp_financieros$c_ahafore)
tmp_financieros$v_ahafore <- ifelse(tmp_financieros$p9_10 == "1" & as.integer(tmp_financieros$p9_11) >= 1 & as.integer(tmp_financieros$p9_11) <= 999999887, as.integer(tmp_financieros$p9_11) , tmp_financieros$v_ahafore)
tmp_financieros$c_ahsegvi <- ifelse(tmp_financieros$p9_15_1 == "1", 1, tmp_financieros$c_ahsegvi)
tmp_financieros$v_ahsegvi <- ifelse(tmp_financieros$p9_15_1 == "1" & as.integer(tmp_financieros$p9_16_1) >= 1 & as.integer(tmp_financieros$p9_16_1) <= 999887 , as.integer(tmp_financieros$p9_16_1), tmp_financieros$v_ahsegvi)

# ***** Agrego la informacion tratada a nivel Hogar (Tenencia y valor del instrumento)
aux_financieros <- aggregate(tmp_financieros[,c("c_ahcaja")],by = list(tmp_financieros$LlaveHO), FUN = max, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(aux_financieros)[colnames(aux_financieros) == "Group.1" ] <- "LlaveHO"
colnames(aux_financieros)[colnames(aux_financieros) == "x" ] <- "c_ahcaja"
gpo1 <- aggregate(tmp_financieros[,c("v_ahcaja")],by = list(tmp_financieros$LlaveHO), FUN = sum, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(gpo1)[colnames(gpo1) == "Group.1" ] <- "LlaveHO"
colnames(gpo1)[colnames(gpo1) == "x" ] <- "v_ahcaja"
aux_financieros <- merge(aux_financieros, gpo1, by = "LlaveHO", all.x = T)
gpo1 <- aggregate(tmp_financieros[,c("c_ahfami")],by = list(tmp_financieros$LlaveHO), FUN = max, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(gpo1)[colnames(gpo1) == "Group.1" ] <- "LlaveHO"
colnames(gpo1)[colnames(gpo1) == "x" ] <- "c_ahfami"
aux_financieros <- merge(aux_financieros, gpo1, by = "LlaveHO", all.x = T)
gpo1 <- aggregate(tmp_financieros[,c("v_ahfami")],by = list(tmp_financieros$LlaveHO), FUN = sum, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(gpo1)[colnames(gpo1) == "Group.1" ] <- "LlaveHO"
colnames(gpo1)[colnames(gpo1) == "x" ] <- "v_ahfami"
aux_financieros <- merge(aux_financieros, gpo1, by = "LlaveHO", all.x = T)
gpo1 <- aggregate(tmp_financieros[,c("c_ahtanda")],by = list(tmp_financieros$LlaveHO), FUN = max, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(gpo1)[colnames(gpo1) == "Group.1" ] <- "LlaveHO"
colnames(gpo1)[colnames(gpo1) == "x" ] <- "c_ahtanda"
aux_financieros <- merge(aux_financieros, gpo1, by = "LlaveHO", all.x = T)
gpo1 <- aggregate(tmp_financieros[,c("v_ahtanda")],by = list(tmp_financieros$LlaveHO), FUN = sum, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(gpo1)[colnames(gpo1) == "Group.1" ] <- "LlaveHO"
colnames(gpo1)[colnames(gpo1) == "x" ] <- "v_ahtanda"
aux_financieros <- merge(aux_financieros, gpo1, by = "LlaveHO", all.x = T)
gpo1 <- aggregate(tmp_financieros[,c("c_ahprest")],by = list(tmp_financieros$LlaveHO), FUN = max, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(gpo1)[colnames(gpo1) == "Group.1" ] <- "LlaveHO"
colnames(gpo1)[colnames(gpo1) == "x" ] <- "c_ahprest"
aux_financieros <- merge(aux_financieros, gpo1, by = "LlaveHO", all.x = T)
gpo1 <- aggregate(tmp_financieros[,c("v_ahprest")],by = list(tmp_financieros$LlaveHO), FUN = sum, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(gpo1)[colnames(gpo1) == "Group.1" ] <- "LlaveHO"
colnames(gpo1)[colnames(gpo1) == "x" ] <- "v_ahprest"
aux_financieros <- merge(aux_financieros, gpo1, by = "LlaveHO", all.x = T)
gpo1 <- aggregate(tmp_financieros[,c("c_ahotro1")],by = list(tmp_financieros$LlaveHO), FUN = max, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(gpo1)[colnames(gpo1) == "Group.1" ] <- "LlaveHO"
colnames(gpo1)[colnames(gpo1) == "x" ] <- "c_ahotro1"
aux_financieros <- merge(aux_financieros, gpo1, by = "LlaveHO", all.x = T)
gpo1 <- aggregate(tmp_financieros[,c("v_ahotro1")],by = list(tmp_financieros$LlaveHO), FUN = sum, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(gpo1)[colnames(gpo1) == "Group.1" ] <- "LlaveHO"
colnames(gpo1)[colnames(gpo1) == "x" ] <- "v_ahotro1"
aux_financieros <- merge(aux_financieros, gpo1, by = "LlaveHO", all.x = T)
gpo1 <- aggregate(tmp_financieros[,c("c_ahcta")],by = list(tmp_financieros$LlaveHO), FUN = max, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(gpo1)[colnames(gpo1) == "Group.1" ] <- "LlaveHO"
colnames(gpo1)[colnames(gpo1) == "x" ] <- "c_ahcta"
aux_financieros <- merge(aux_financieros, gpo1, by = "LlaveHO", all.x = T)
gpo1 <- aggregate(tmp_financieros[,c("v_ahcta")],by = list(tmp_financieros$LlaveHO), FUN = sum, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(gpo1)[colnames(gpo1) == "Group.1" ] <- "LlaveHO"
colnames(gpo1)[colnames(gpo1) == "x" ] <- "v_ahcta"
aux_financieros <- merge(aux_financieros, gpo1, by = "LlaveHO", all.x = T)
gpo1 <- aggregate(tmp_financieros[,c("c_ahchqs")],by = list(tmp_financieros$LlaveHO), FUN = max, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(gpo1)[colnames(gpo1) == "Group.1" ] <- "LlaveHO"
colnames(gpo1)[colnames(gpo1) == "x" ] <- "c_ahchqs"
aux_financieros <- merge(aux_financieros, gpo1, by = "LlaveHO", all.x = T)
gpo1 <- aggregate(tmp_financieros[,c("v_ahchqs")],by = list(tmp_financieros$LlaveHO), FUN = sum, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(gpo1)[colnames(gpo1) == "Group.1" ] <- "LlaveHO"
colnames(gpo1)[colnames(gpo1) == "x" ] <- "v_ahchqs"
aux_financieros <- merge(aux_financieros, gpo1, by = "LlaveHO", all.x = T)
gpo1 <- aggregate(tmp_financieros[,c("c_ahprv")],by = list(tmp_financieros$LlaveHO), FUN = max, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(gpo1)[colnames(gpo1) == "Group.1" ] <- "LlaveHO"
colnames(gpo1)[colnames(gpo1) == "x" ] <- "c_ahprv"
aux_financieros <- merge(aux_financieros, gpo1, by = "LlaveHO", all.x = T)
gpo1 <- aggregate(tmp_financieros[,c("v_ahprv")],by = list(tmp_financieros$LlaveHO), FUN = sum, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(gpo1)[colnames(gpo1) == "Group.1" ] <- "LlaveHO"
colnames(gpo1)[colnames(gpo1) == "x" ] <- "v_ahprv"
aux_financieros <- merge(aux_financieros, gpo1, by = "LlaveHO", all.x = T)
gpo1 <- aggregate(tmp_financieros[,c("c_ahinvrs")],by = list(tmp_financieros$LlaveHO), FUN = max, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(gpo1)[colnames(gpo1) == "Group.1" ] <- "LlaveHO"
colnames(gpo1)[colnames(gpo1) == "x" ] <- "c_ahinvrs"
aux_financieros <- merge(aux_financieros, gpo1, by = "LlaveHO", all.x = T)
gpo1 <- aggregate(tmp_financieros[,c("v_ahinvrs")],by = list(tmp_financieros$LlaveHO), FUN = sum, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(gpo1)[colnames(gpo1) == "Group.1" ] <- "LlaveHO"
colnames(gpo1)[colnames(gpo1) == "x" ] <- "v_ahinvrs"
aux_financieros <- merge(aux_financieros, gpo1, by = "LlaveHO", all.x = T)
gpo1 <- aggregate(tmp_financieros[,c("c_ahotro2")],by = list(tmp_financieros$LlaveHO), FUN = max, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(gpo1)[colnames(gpo1) == "Group.1" ] <- "LlaveHO"
colnames(gpo1)[colnames(gpo1) == "x" ] <- "c_ahotro2"
aux_financieros <- merge(aux_financieros, gpo1, by = "LlaveHO", all.x = T)
gpo1 <- aggregate(tmp_financieros[,c("v_ahotro2")],by = list(tmp_financieros$LlaveHO), FUN = sum, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(gpo1)[colnames(gpo1) == "Group.1" ] <- "LlaveHO"
colnames(gpo1)[colnames(gpo1) == "x" ] <- "v_ahotro2"
aux_financieros <- merge(aux_financieros, gpo1, by = "LlaveHO", all.x = T)
gpo1 <- aggregate(tmp_financieros[,c("c_ahnompe")],by = list(tmp_financieros$LlaveHO), FUN = max, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(gpo1)[colnames(gpo1) == "Group.1" ] <- "LlaveHO"
colnames(gpo1)[colnames(gpo1) == "x" ] <- "c_ahnompe"
aux_financieros <- merge(aux_financieros, gpo1, by = "LlaveHO", all.x = T)
gpo1 <- aggregate(tmp_financieros[,c("v_ahnompe")],by = list(tmp_financieros$LlaveHO), FUN = sum, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(gpo1)[colnames(gpo1) == "Group.1" ] <- "LlaveHO"
colnames(gpo1)[colnames(gpo1) == "x" ] <- "v_ahnompe"
aux_financieros <- merge(aux_financieros, gpo1, by = "LlaveHO", all.x = T) 

gpo1 <- aggregate(tmp_financieros[,c("c_ahctago")],by = list(tmp_financieros$LlaveHO), FUN = max, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(gpo1)[colnames(gpo1) == "Group.1" ] <- "LlaveHO"
colnames(gpo1)[colnames(gpo1) == "x" ] <- "c_ahctago"
aux_financieros <- merge(aux_financieros, gpo1, by = "LlaveHO", all.x = T)
gpo1 <- aggregate(tmp_financieros[,c("v_ahctago")],by = list(tmp_financieros$LlaveHO), FUN = sum, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(gpo1)[colnames(gpo1) == "Group.1" ] <- "LlaveHO"
colnames(gpo1)[colnames(gpo1) == "x" ] <- "v_ahctago"
aux_financieros <- merge(aux_financieros, gpo1, by = "LlaveHO", all.x = T)
gpo1 <- aggregate(tmp_financieros[,c("c_ahafore")],by = list(tmp_financieros$LlaveHO), FUN = max, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(gpo1)[colnames(gpo1) == "Group.1" ] <- "LlaveHO"
colnames(gpo1)[colnames(gpo1) == "x" ] <- "c_ahafore"
aux_financieros <- merge(aux_financieros, gpo1, by = "LlaveHO", all.x = T)
gpo1 <- aggregate(tmp_financieros[,c("v_ahafore")],by = list(tmp_financieros$LlaveHO), FUN = sum, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(gpo1)[colnames(gpo1) == "Group.1" ] <- "LlaveHO"
colnames(gpo1)[colnames(gpo1) == "x" ] <- "v_ahafore"
aux_financieros <- merge(aux_financieros, gpo1, by = "LlaveHO", all.x = T)
gpo1 <- aggregate(tmp_financieros[,c("c_ahsegvi")],by = list(tmp_financieros$LlaveHO), FUN = max, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(gpo1)[colnames(gpo1) == "Group.1" ] <- "LlaveHO"
colnames(gpo1)[colnames(gpo1) == "x" ] <- "c_ahsegvi"
aux_financieros <- merge(aux_financieros, gpo1, by = "LlaveHO", all.x = T)
gpo1 <- aggregate(tmp_financieros[,c("v_ahsegvi")],by = list(tmp_financieros$LlaveHO), FUN = sum, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(gpo1)[colnames(gpo1) == "Group.1" ] <- "LlaveHO"
colnames(gpo1)[colnames(gpo1) == "x" ] <- "v_ahsegvi"
aux_financieros <- merge(aux_financieros, gpo1, by = "LlaveHO", all.x = T)
gpo1 <- aggregate(tmp_financieros[,c("c_ahot18")],by = list(tmp_financieros$LlaveHO), FUN = max, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(gpo1)[colnames(gpo1) == "Group.1" ] <- "LlaveHO"
colnames(gpo1)[colnames(gpo1) == "x" ] <- "c_ahot18"
aux_financieros <- merge(aux_financieros, gpo1, by = "LlaveHO", all.x = T)
gpo1 <- aggregate(tmp_financieros[,c("v_ahot18")],by = list(tmp_financieros$LlaveHO), FUN = sum, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(gpo1)[colnames(gpo1) == "Group.1" ] <- "LlaveHO"
colnames(gpo1)[colnames(gpo1) == "x" ] <- "v_ahot18"
aux_financieros <- merge(aux_financieros, gpo1, by = "LlaveHO", all.x = T)
# ****Actualizo la informacion de activos financieros por hogar en la tabla concentradora del hogar (Tenenciay valor del instrumento)
tmp_hogar1 <- merge(tmp_hogar1, aux_financieros, by = "LlaveHO", all.x = T)
tmp_hogar1$c_ahcaja.x <- ifelse( tmp_hogar1$c_ahcaja.y == 1, 1, tmp_hogar1$c_ahcaja.x)
tmp_hogar1$v_ahcaja.x <- ifelse( tmp_hogar1$c_ahcaja.y == 1, tmp_hogar1$v_ahcaja.y , tmp_hogar1$v_ahcaja.x)
tmp_hogar1$c_ahfami.x <- ifelse( tmp_hogar1$c_ahfami.y == 1, 1, tmp_hogar1$c_ahfami.x)
tmp_hogar1$v_ahfami.x <- ifelse( tmp_hogar1$c_ahfami.y == 1, tmp_hogar1$v_ahfami.y , tmp_hogar1$v_ahfami.x)
tmp_hogar1$c_ahtanda.x <- ifelse( tmp_hogar1$c_ahtanda.y == 1, 1, tmp_hogar1$c_ahtanda.x)
tmp_hogar1$v_ahtanda.x <- ifelse( tmp_hogar1$c_ahtanda.y == 1, tmp_hogar1$v_ahtanda.y, tmp_hogar1$v_ahtanda.x)
tmp_hogar1$c_ahprest.x <- ifelse( tmp_hogar1$c_ahprest.y == 1, 1, tmp_hogar1$c_ahprest.x)
tmp_hogar1$v_ahprest.x <- ifelse( tmp_hogar1$c_ahprest.y == 1, tmp_hogar1$v_ahprest.y,tmp_hogar1$v_ahprest.x)
tmp_hogar1$c_ahotro1.x <- ifelse( tmp_hogar1$c_ahotro1.y == 1, 1, tmp_hogar1$c_ahotro1.x)
tmp_hogar1$v_ahotro1.x <- ifelse( tmp_hogar1$c_ahotro1.y == 1, tmp_hogar1$v_ahotro1.y, tmp_hogar1$v_ahotro1.x)
tmp_hogar1$c_ahcta.x <- ifelse( tmp_hogar1$c_ahcta.y == 1, 1, tmp_hogar1$c_ahcta.x)
tmp_hogar1$v_ahcta.x <- ifelse( tmp_hogar1$c_ahcta.y == 1, tmp_hogar1$v_ahcta.y , tmp_hogar1$v_ahcta.x)
tmp_hogar1$c_ahchqs.x <- ifelse( tmp_hogar1$c_ahchqs.y == 1, 1, tmp_hogar1$c_ahchqs.x)
tmp_hogar1$v_ahchqs.x <- ifelse( tmp_hogar1$c_ahchqs.y == 1, tmp_hogar1$v_ahchqs.y , tmp_hogar1$v_ahchqs.x)
tmp_hogar1$c_ahprv.x <- ifelse( tmp_hogar1$c_ahprv.y == 1, 1, tmp_hogar1$c_ahprv.x)
tmp_hogar1$v_ahprv.x <- ifelse( tmp_hogar1$c_ahprv.y == 1, tmp_hogar1$v_ahprv.y , tmp_hogar1$v_ahprv.x)
tmp_hogar1$c_ahinvrs.x <- ifelse( tmp_hogar1$c_ahinvrs.y == 1, 1, tmp_hogar1$c_ahinvrs.x)
tmp_hogar1$v_ahinvrs.x <- ifelse( tmp_hogar1$c_ahinvrs.y == 1, tmp_hogar1$v_ahinvrs.y, tmp_hogar1$v_ahinvrs.x)
tmp_hogar1$c_ahotro2.x <- ifelse( tmp_hogar1$c_ahotro2.y == 1, 1, tmp_hogar1$c_ahotro2.x)
tmp_hogar1$v_ahotro2.x <- ifelse( tmp_hogar1$c_ahotro2.y == 1, tmp_hogar1$v_ahotro2.y, tmp_hogar1$v_ahotro2.x)
tmp_hogar1$c_ahnompe.x <- ifelse( tmp_hogar1$c_ahnompe.y == 1, 1, tmp_hogar1$c_ahnompe.x)
tmp_hogar1$v_ahnompe.x <- ifelse( tmp_hogar1$c_ahnompe.y == 1, tmp_hogar1$v_ahnompe.y, tmp_hogar1$v_ahnompe.x)
tmp_hogar1$c_ahctago.x <- ifelse( tmp_hogar1$c_ahctago.y == 1, 1, tmp_hogar1$c_ahctago.x)
tmp_hogar1$v_ahctago.x <- ifelse( tmp_hogar1$c_ahctago.y == 1, tmp_hogar1$v_ahctago.y, tmp_hogar1$v_ahctago.x)
tmp_hogar1$c_ahafore.x <- ifelse( tmp_hogar1$c_ahafore.y == 1, 1, tmp_hogar1$c_ahafore.x)
tmp_hogar1$v_ahafore.x <- ifelse( tmp_hogar1$c_ahafore.y == 1, tmp_hogar1$v_ahafore.y, tmp_hogar1$v_ahafore.x)
tmp_hogar1$c_ahsegvi.x <- ifelse( tmp_hogar1$c_ahsegvi.y == 1, 1, tmp_hogar1$c_ahsegvi.x)
tmp_hogar1$v_ahsegvi.x <- ifelse( tmp_hogar1$c_ahsegvi.y == 1, tmp_hogar1$v_ahsegvi.y, tmp_hogar1$v_ahsegvi.x)
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "c_ahcaja.x" ] <- "c_ahcaja"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "v_ahcaja.x" ] <- "v_ahcaja"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "c_ahfami.x" ] <- "c_ahfami"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "v_ahfami.x" ] <- "v_ahfami"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "c_ahtanda.x"] <- "c_ahtanda"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "v_ahtanda.x"] <- "v_ahtanda"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "c_ahprest.x"] <- "c_ahprest"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "v_ahprest.x"] <- "v_ahprest"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "c_ahotro1.x"] <- "c_ahotro1"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "v_ahotro1.x"] <- "v_ahotro1"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "c_ahcta.x" ] <- "c_ahcta"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "v_ahcta.x" ] <- "v_ahcta"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "c_ahchqs.x" ] <- "c_ahchqs"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "v_ahchqs.x" ] <- "v_ahchqs"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "c_ahprv.x" ] <- "c_ahprv"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "v_ahprv.x" ] <- "v_ahprv"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "c_ahinvrs.x"] <- "c_ahinvrs"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "v_ahinvrs.x"] <- "v_ahinvrs"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "c_ahotro2.x"] <- "c_ahotro2"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "v_ahotro2.x"] <- "v_ahotro2"

colnames(tmp_hogar1)[colnames(tmp_hogar1) == "c_ahnompe.x"] <- "c_ahnompe"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "v_ahnompe.x"] <- "v_ahnompe"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "c_ahctago.x"] <- "c_ahctago"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "v_ahctago.x"] <- "v_ahctago"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "c_ahafore.x"] <- "c_ahafore"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "v_ahafore.x"] <- "v_ahafore"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "c_ahsegvi.x"] <- "c_ahsegvi"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "v_ahsegvi.x"] <- "v_ahsegvi"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "c_ahot18.x"] <- "c_ahot18"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "v_ahot18.x"] <- "v_ahot18"
tmp_hogar1 <- tmp_hogar1[, !names(tmp_hogar1) %in% c("c_ahcaja.y","v_ahcaja.y","c_ahfami.y","v_ahfami.y",
                                                     "c_ahtanda.y","v_ahtanda.y","c_ahprest.y","v_ahprest.y",
                                                     "c_ahotro1.y","v_ahotro1.y","c_ahcta.y","v_ahcta.y","c_ahchqs.y",
                                                     "v_ahchqs.y","c_ahprv.y","v_ahprv.y","c_ahinvrs.y","v_ahinvrs.y",
                                                     "c_ahotro2.y","v_ahotro2.y","c_ahnompe.y","v_ahnompe.y","c_ahctago.y",
                                                     "v_ahctago.y","c_ahafore.y","v_ahafore.y","c_ahsegvi.y","v_ahsegvi.y",
                                                     "c_ahot18.y","v_ahot18.y")]

# **************************************************************************************
# ***Agrupa los conceptos que desagregan los activos Financieros (Ref tabulado 18)
tmp_hogar1$c_ctachqs <- ifelse(ifelse(is.na(tmp_hogar1$c_ahchqs), 0 , tmp_hogar1$c_ahchqs) +
                                 ifelse(is.na(tmp_hogar1$c_ahcta ), 0 , tmp_hogar1$c_ahcta ) > 0, 1, 0)
tmp_hogar1$v_ctachqs <- ifelse(is.na(tmp_hogar1$v_ahcta ), 0, tmp_hogar1$v_ahcta) +
  ifelse(is.na(tmp_hogar1$v_ahchqs), 0, tmp_hogar1$v_ahchqs)
tmp_hogar1$c_nompen <- tmp_hogar1$c_ahnompe
tmp_hogar1$v_nompen <- tmp_hogar1$v_ahnompe
tmp_hogar1$c_afore <- tmp_hogar1$c_ahafore
tmp_hogar1$v_afore <- tmp_hogar1$v_ahafore
tmp_hogar1$c_ctagob <- tmp_hogar1$c_ahctago
tmp_hogar1$v_ctagob <- tmp_hogar1$v_ahctago
tmp_hogar1$c_segvid <- tmp_hogar1$c_ahsegvi
tmp_hogar1$v_segvid <- tmp_hogar1$v_ahsegvi
tmp_hogar1$c_prvinvr <- ifelse(ifelse(is.na(tmp_hogar1$c_ahprv ), 0, tmp_hogar1$c_ahprv) +
                                 ifelse(is.na(tmp_hogar1$c_ahinvrs), 0, tmp_hogar1$c_ahinvrs) >= 1, 1, 0)
tmp_hogar1$v_prvinvr <- ifelse(is.na(tmp_hogar1$v_ahprv ), 0, tmp_hogar1$v_ahprv ) +
  ifelse(is.na(tmp_hogar1$v_ahinvrs), 0, tmp_hogar1$v_ahinvrs)
tmp_hogar1$c_otroaf <- ifelse(ifelse(is.na(tmp_hogar1$c_ahcaja ), 0, tmp_hogar1$c_ahcaja) +
                                ifelse(is.na(tmp_hogar1$c_ahfami ), 0, tmp_hogar1$c_ahfami) +
                                ifelse(is.na(tmp_hogar1$c_ahtanda), 0, tmp_hogar1$c_ahtanda) +
                                ifelse(is.na(tmp_hogar1$c_ahprest), 0, tmp_hogar1$c_ahprest) +
                                ifelse(is.na(tmp_hogar1$c_ahotro1), 0, tmp_hogar1$c_ahotro1) +
                                ifelse(is.na(tmp_hogar1$c_ahotro2), 0, tmp_hogar1$c_ahotro2) > 0, 1, 0)
tmp_hogar1$v_otroaf <- ifelse(is.na(tmp_hogar1$v_ahcaja ), 0, tmp_hogar1$v_ahcaja ) +
  ifelse(is.na(tmp_hogar1$v_ahfami ), 0, tmp_hogar1$v_ahfami ) +
  ifelse(is.na(tmp_hogar1$v_ahtanda), 0, tmp_hogar1$v_ahtanda) + 
  ifelse(is.na(tmp_hogar1$v_ahprest), 0, tmp_hogar1$v_ahprest) +
  ifelse(is.na(tmp_hogar1$v_ahotro1), 0, tmp_hogar1$v_ahotro1) +
  ifelse(is.na(tmp_hogar1$v_ahotro2), 0, tmp_hogar1$v_ahotro2)
# ***Obtengo hogares con activo Financiero y su valor
tmp_hogar1$cacvo_fin <- ifelse(ifelse(is.na(tmp_hogar1$c_ctachqs), 0, tmp_hogar1$c_ctachqs) +
                                 ifelse(is.na(tmp_hogar1$c_afore ), 0, tmp_hogar1$c_afore ) +
                                 ifelse(is.na(tmp_hogar1$c_nompen ), 0, tmp_hogar1$c_nompen ) +
                                 ifelse(is.na(tmp_hogar1$c_ctagob ), 0, tmp_hogar1$c_ctagob ) +
                                 ifelse(is.na(tmp_hogar1$c_segvid ), 0, tmp_hogar1$c_segvid ) +
                                 ifelse(is.na(tmp_hogar1$c_prvinvr), 0, tmp_hogar1$c_prvinvr) +
                                 ifelse(is.na(tmp_hogar1$c_otroaf ), 0, tmp_hogar1$c_otroaf ) > 0, 1, tmp_hogar1$cacvo_fin)
tmp_hogar1$val_finan <- ifelse(is.na(tmp_hogar1$v_ctachqs), 0, tmp_hogar1$v_ctachqs) +
  ifelse(is.na(tmp_hogar1$v_nompen ), 0, tmp_hogar1$v_nompen ) +
  ifelse(is.na(tmp_hogar1$v_afore ), 0, tmp_hogar1$v_afore ) +
  ifelse(is.na(tmp_hogar1$v_ctagob ), 0, tmp_hogar1$v_ctagob ) +
  ifelse(is.na(tmp_hogar1$v_segvid ), 0, tmp_hogar1$v_segvid ) +
  ifelse(is.na(tmp_hogar1$v_prvinvr), 0, tmp_hogar1$v_prvinvr) +
  ifelse(is.na(tmp_hogar1$v_otroaf ), 0, tmp_hogar1$v_otroaf )

tmp_hogar1$aux1 <- tmp_hogar1$fac_hog * tmp_hogar1$cacvo_fin
tmp_hogar1$aux2 <- tmp_hogar1$fac_hog * tmp_hogar1$val_finan
gpo1 <- sum(aggregate(tmp_hogar1[,c("aux1")],by = list(tmp_hogar1$LlaveHO), FUN = sum, na.rm=TRUE)$x) # obtener on df agrupado por hogar
gpo2 <- sum(aggregate(tmp_hogar1[,c("aux2")],by = list(tmp_hogar1$LlaveHO), FUN = sum, na.rm=TRUE)$x)/1000000 # obtener on df agrupado por hogar
nofinan <- rbind(nofinan, c(1, "+Activos Financieros", gpo1, gpo2))
tmp_hogar1 <- tmp_hogar1[, !names(tmp_hogar1) %in% c("aux1","aux2") ] # Eliminar columnas

# **************************************************************************************
# *****************FIN DE SECCION DE ACTIVOS FINANCIEROS
# **************************************************************************************
# *****************INICIO DE SECCION DE DEUDAS
# **************************************************************************************
# ****Tablas auxiliares para Limpieza y transformacion de
tmp_deuda_vpal <- read.csv(file = "datos/TVivienda.csv", header=TRUE, sep=",", colClasses="character")
tmp_deuda_vpal <- conversor(tmp_deuda_vpal)


  
tmp_deuda_vpal$LlaveVI <- paste(tmp_deuda_vpal$upm, tmp_deuda_vpal$viv_sel, sep="")
# Eliminar columnas innecesarias
tmp_deuda_vpal <- tmp_deuda_vpal[, names(tmp_deuda_vpal) %in% c("LlaveVI", "p4_17", "p4_33", "p4_40", "p4_42") ] # Eliminar columnas
tmp_deuda_vpal <- tmp_deuda_vpal[(tmp_deuda_vpal$p4_17 == "1" | tmp_deuda_vpal$p4_40 == "1" ), ] # eliminar registros que no cumplan con la condición
tmp_deuda_vpal <- tmp_deuda_vpal[order(tmp_deuda_vpal$LlaveVI), ]
tmp_deuda_vsec <- read.csv(file = "datos/TPropiedad.csv", header=TRUE, sep=",", colClasses="character")
tmp_deuda_vsec <- conversor(tmp_deuda_vsec)
tmp_deuda_vsec$LlaveVI <- paste(tmp_deuda_vsec$upm, tmp_deuda_vsec$viv_sel, sep="")
tmp_deuda_vsec$LlaveHO <- paste(tmp_deuda_vsec$upm, tmp_deuda_vsec$viv_sel,tmp_deuda_vsec$hogar, sep="")
# Eliminar columnas innecesarias
tmp_deuda_vsec <- tmp_deuda_vsec[, names(tmp_deuda_vsec) %in% c("LlaveVI", "LlaveHO", "p5_12", "p5_16") ] # Eliminar columnas
tmp_deuda_vsec <- tmp_deuda_vsec[(tmp_deuda_vsec$p5_12 == "1" ), ] # eliminar registros que no cumplan con la condición
tmp_deuda_vsec <- tmp_deuda_vsec[order(tmp_deuda_vsec$LlaveHO), ]
tmp_deuda_t__deptal <- read.csv(file = "datos/TDepar.csv", header=TRUE, sep=",", colClasses="character")
tmp_deuda_t__deptal <- conversor(tmp_deuda_t__deptal)
tmp_deuda_t__deptal$LlaveVI <- paste(tmp_deuda_t__deptal$upm, tmp_deuda_t__deptal$viv_sel, sep="")
tmp_deuda_t__deptal$LlaveHO <- paste(tmp_deuda_t__deptal$upm, tmp_deuda_t__deptal$viv_sel, tmp_deuda_t__deptal$hogar, sep="")
# Eliminar columnas innecesarias
tmp_deuda_t__deptal <- tmp_deuda_t__deptal[, names(tmp_deuda_t__deptal) %in% c("LlaveVI", "LlaveHO", "p8_15") ] # Eliminar columnas
tmp_deuda_t__deptal <- tmp_deuda_t__deptal[order(tmp_deuda_t__deptal$LlaveHO), ]
tmp_deuda_t_bnco <- read.csv(file = "datos/TBanca.csv", header=TRUE, sep=",", colClasses="character")
tmp_deuda_t_bnco <- conversor(tmp_deuda_t_bnco)
tmp_deuda_t_bnco$LlaveVI <- paste(tmp_deuda_t_bnco$upm, tmp_deuda_t_bnco$viv_sel, sep="")
tmp_deuda_t_bnco$LlaveHO <- paste(tmp_deuda_t_bnco$upm, tmp_deuda_t_bnco$viv_sel, tmp_deuda_t_bnco$hogar, sep="")
# Eliminar columnas innecesarias
tmp_deuda_t_bnco <- tmp_deuda_t_bnco[, names(tmp_deuda_t_bnco) %in% c("LlaveVI", "LlaveHO", "p8_21") ] # Eliminar columnas
tmp_deuda_t_bnco <- tmp_deuda_t_bnco[order(tmp_deuda_t_bnco$LlaveHO), ]
# ************* credito de vehiculos
# Auto y Moto
tmp_automoto <- read.csv(file = "datos/TModulo.csv", header=TRUE, sep=",", colClasses="character")
tmp_automoto <- conversor(tmp_automoto)
tmp_automoto$LlaveVI <- paste(tmp_automoto$upm, tmp_automoto$viv_sel, sep="")
tmp_automoto$LlaveHO <- paste(tmp_automoto$upm, tmp_automoto$viv_sel, tmp_automoto$hogar, sep="")
tmp_automoto <- tmp_automoto[(tmp_automoto$p7_4_1 == "1" |
                                tmp_automoto$p7_4_2 == "1" |
                                ifelse(gsub(" ", "", tmp_automoto$p7_5_1) == "", 0, as.integer(tmp_automoto$p7_5_1)) > 0 |
                                ifelse(gsub(" ", "", tmp_automoto$p7_5_2) == "", 0, as.integer(tmp_automoto$p7_5_2)) > 0), ] # eliminar registros que no cumplan con la condición
tmp_automoto$factor <- as.integer(tmp_automoto$factor)
tmp_automoto <- aggregate(tmp_automoto[,c("factor")],by = list(tmp_automoto$LlaveHO), FUN = sum, na.rm=TRUE)
colnames(tmp_automoto)[colnames(tmp_automoto) == "Group.1"] <- "LlaveHO"
colnames(tmp_automoto)[colnames(tmp_automoto) == "x" ] <- "factor"
gpo1 <- read.csv(file = "datos/TModulo.csv", header=TRUE, sep=",", colClasses="character")
gpo1 <- conversor(gpo1)
gpo1$LlaveHO <- paste(gpo1$upm, gpo1$viv_sel, gpo1$hogar, sep="")
gpo1$uni <- 1
gpo1 <- gpo1[gpo1$p7_4_1 == "1", ]
gpo1 <- aggregate(gpo1[,c("uni")],by = list(gpo1$LlaveHO), FUN = sum, na.rm=TRUE)
colnames(gpo1)[colnames(gpo1) == "Group.1"] <- "LlaveHO"
gpo1$p7_4_1 <- 1
gpo2 <- read.csv(file = "datos/TModulo.csv", header=TRUE, sep=",", colClasses="character")
gpo2 <- conversor(gpo2)
gpo2$LlaveHO <- paste(gpo2$upm, gpo2$viv_sel, gpo2$hogar, sep="")
gpo2$uni <- 1
gpo2 <- gpo2[gpo2$p7_4_2 == "1", ]
gpo2 <- aggregate(gpo2[,c("uni")],by = list(gpo2$LlaveHO), FUN = sum, na.rm=TRUE)
colnames(gpo2)[colnames(gpo2) == "Group.1"] <- "LlaveHO"
gpo2$p7_4_2 <- 1
tmp_automoto <- merge(tmp_automoto, gpo1, by = "LlaveHO", all.x = T)
tmp_automoto <- merge(tmp_automoto, gpo2, by = "LlaveHO", all.x = T)
tmp_automoto$p7_4_1 <- ifelse(is.na(tmp_automoto$p7_4_1), 0, 1)
tmp_automoto$p7_4_2 <- ifelse(is.na(tmp_automoto$p7_4_2), 0, 1)
tmp_automoto$p7_12 <- 0
tmp_automoto$p7_21 <- 0
# Eliminar columnas innecesarias
tmp_automoto <- tmp_automoto[, names(tmp_automoto) %in% c("LlaveVI", "LlaveHO", "factor", "p7_4_1", "p7_4_2", "p7_12", "p7_21") ] # Eliminar columnas
#Solo Auto
tmp_auto <- read.csv(file = "datos/TAuto.csv", header=TRUE, sep=",", colClasses="character")
tmp_auto <- conversor(tmp_auto)
tmp_auto$LlaveHO <- paste(tmp_auto$upm, tmp_auto$viv_sel, tmp_auto$hogar, sep="")
tmp_auto$p7_12 <- as.integer(tmp_auto$p7_12)
tmp_auto <- tmp_auto[(tmp_auto$p7_12 >= 1 & tmp_auto$p7_12 <= 9999887), ] # eliminar registros que no cumplan con la condición
tmp_auto <- aggregate(tmp_auto[,c("p7_12")], by=list("LlaveHO"=tmp_auto$LlaveHO), FUN = sum, na.rm=TRUE)# obtener on df agrupado por hogar
#Solo Moto
tmp_moto <- read.csv(file = "datos/TMoto.csv", header=TRUE, sep=",", colClasses="character")
tmp_moto <- conversor(tmp_moto)
tmp_moto$LlaveHO <- paste(tmp_moto$upm, tmp_moto$viv_sel, tmp_moto$hogar, sep="")
tmp_moto$p7_21 <- as.integer(tmp_moto$p7_21)
tmp_moto <- tmp_moto[(tmp_moto$p7_21 >= 1 & tmp_moto$p7_21 <= 9999887), ] # eliminar registros que no cumplan con la condición
tmp_moto <- aggregate(tmp_moto[,c("p7_21")], by = list("LlaveHO"=tmp_moto$LlaveHO), FUN = sum, na.rm=TRUE)# obtener on df agrupado por hogar
tmp_automoto <- merge(tmp_automoto, tmp_auto, by="LlaveHO", all.x = T)
tmp_automoto <- merge(tmp_automoto, tmp_moto, by="LlaveHO", all.x = T)
tmp_automoto$p7_12 <- ifelse(is.na(tmp_automoto$x.x), 0, tmp_automoto$x.x)
tmp_automoto$p7_21 <- ifelse(is.na(tmp_automoto$x.y), 0, tmp_automoto$x.y)
tmp_automoto <- tmp_automoto[, !names(tmp_automoto) %in% c("x.x","x.y")]
  
# *****credito nomina/personal
tmp_deuda_nomper <- read.csv(file = "datos/TModulo.csv", header=TRUE, sep=",", colClasses="character")
tmp_deuda_nomper <- conversor(tmp_deuda_nomper)
tmp_deuda_nomper$LlaveHO <- paste(tmp_deuda_nomper$upm, tmp_deuda_nomper$viv_sel, tmp_deuda_nomper$hogar, sep="")
tmp_deuda_nomper <- tmp_deuda_nomper[(ifelse(gsub(" ", "", tmp_deuda_nomper$p8_9_3) == "", 0, as.integer(tmp_deuda_nomper$p8_9_3)) > 0 |
                                        ifelse(gsub(" ", "", tmp_deuda_nomper$p8_9_5) == "", 0, as.integer(tmp_deuda_nomper$p8_9_5)) > 0), ] # eliminar registros que no cumplan con la condición
tmp_deuda_nomper$factor <- as.integer(tmp_deuda_nomper$factor)
tmp_deuda_nomper <- aggregate(tmp_deuda_nomper[,c("factor")], by = list("LlaveHO"=tmp_deuda_nomper$LlaveHO), FUN = sum, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(tmp_deuda_nomper)[colnames(tmp_deuda_nomper) == "x"] <- "factor"
tmp_deuda_nomper$p8_25 <- 0
tmp_deuda_nomper$p8_41 <- 0
tmp_deuda_nomina <- read.csv(file = "datos/TNomina.csv", header=TRUE, sep=",", colClasses="character")
tmp_deuda_nomina <- conversor(tmp_deuda_nomina)
tmp_deuda_nomina$LlaveHO <- paste(tmp_deuda_nomina$upm, tmp_deuda_nomina$viv_sel, tmp_deuda_nomina$hogar, sep="")
tmp_deuda_nomina <- tmp_deuda_nomina[(ifelse(gsub(" ", "", tmp_deuda_nomina$p8_25) == "", 0, as.integer(tmp_deuda_nomina$p8_25)) >= 1 &
                                        ifelse(gsub(" ", "", tmp_deuda_nomina$p8_25) == "", 0, as.integer(tmp_deuda_nomina$p8_25)) <= 999887), ]
tmp_deuda_nomina$p8_25 <- as.integer(tmp_deuda_nomina$p8_25)
tmp_deuda_nomina <- aggregate(tmp_deuda_nomina[,c("p8_25")], by = list("LlaveHO"=tmp_deuda_nomina$LlaveHO), FUN = sum, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(tmp_deuda_nomina)[colnames(tmp_deuda_nomina) == "x"] <- "p8_25"
tmp_deuda_personal <- read.csv(file = "datos/TPersonal.csv", header=TRUE, sep=",", colClasses="character")
tmp_deuda_personal  <- conversor(tmp_deuda_personal)
tmp_deuda_personal$LlaveHO <- paste(tmp_deuda_personal$upm, tmp_deuda_personal$viv_sel, tmp_deuda_personal$hogar, sep="")
tmp_deuda_personal <- tmp_deuda_personal[(ifelse(gsub(" ", "", tmp_deuda_personal$p8_41) == "", 0, as.integer(tmp_deuda_personal$p8_41)) >= 1 &
                                            ifelse(gsub(" ", "", tmp_deuda_personal$p8_41) == "", 0, as.integer(tmp_deuda_personal$p8_41)) <= 999887), ]
tmp_deuda_personal$p8_41 <- as.integer(tmp_deuda_personal$p8_41)
tmp_deuda_personal <- aggregate(tmp_deuda_personal[,c("p8_41")], by = list("LlaveHO"=tmp_deuda_personal$LlaveHO), FUN = sum, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(tmp_deuda_personal)[colnames(tmp_deuda_personal) == "x"] <- "p8_41"
tmp_deuda_nomper <- merge(tmp_deuda_nomper, tmp_deuda_nomina , by="LlaveHO", all.x = T)
tmp_deuda_nomper <- merge(tmp_deuda_nomper, tmp_deuda_personal, by="LlaveHO", all.x = T)
tmp_deuda_nomper$p8_25.x <- ifelse(is.na(tmp_deuda_nomper$p8_25.y), 0, tmp_deuda_nomper$p8_25.y)
tmp_deuda_nomper$p8_41.x <- ifelse(is.na(tmp_deuda_nomper$p8_41.y), 0, tmp_deuda_nomper$p8_41.y)
colnames(tmp_deuda_nomper)[colnames(tmp_deuda_nomper) == "p8_25.x"] <- "p8_25"
colnames(tmp_deuda_nomper)[colnames(tmp_deuda_nomper) == "p8_41.x"] <- "p8_41"
tmp_deuda_nomper <- tmp_deuda_nomper[, !names(tmp_deuda_nomper) %in% c("p8_25.y","p8_41.y")]
# *****credito grupal
tmp_grupal <- read.csv(file = "datos/TModulo.csv", header=TRUE, sep=",", colClasses="character")
tmp_grupal <- conversor(tmp_grupal)
tmp_grupal <- tmp_grupal[( tmp_grupal$p8_8_6 == "1" ), ] # eliminar registros que no cumplan con la condición
tmp_grupal$LlaveHO <- paste(tmp_grupal$upm, tmp_grupal$viv_sel, tmp_grupal$hogar, sep="")
tmp_grupal$factor <- as.integer(tmp_grupal$factor)
tmp_grupal <- aggregate(tmp_grupal[,c("factor")], by = list("LlaveHO"=tmp_grupal$LlaveHO), FUN = sum, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(tmp_grupal)[colnames(tmp_grupal) == "x"] <- "factor"
tmp_grupal$p8_8_6 <- 1
tmp_grupal$p8_49 <- 0
aux_grupal <- read.csv(file = "datos/TGrupal.csv", header=TRUE, sep=",", colClasses="character")
aux_grupal <- conversor(aux_grupal)
aux_grupal$p8_49 <- as.integer(aux_grupal$p8_49)
aux_grupal <- aux_grupal[( aux_grupal$p8_49 >= 1 & aux_grupal$p8_49 <= 999887 ), ] # eliminar registros que no cumplan con la condición
aux_grupal$LlaveHO <- paste(aux_grupal$upm, aux_grupal$viv_sel, aux_grupal$hogar, sep="")
aux_grupal <- aggregate(aux_grupal[,c("p8_49")], by = list("LlaveHO"=aux_grupal$LlaveHO), FUN = sum, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(aux_grupal)[colnames(aux_grupal) == "x"] <- "p8_49"
tmp_grupal <- merge(tmp_grupal, aux_grupal, by = "LlaveHO", all.x = T)
tmp_grupal$p8_49.x <- ifelse(is.na(tmp_grupal$p8_49.y), 0, tmp_grupal$p8_49.y)
colnames(tmp_grupal)[colnames(tmp_grupal) == "p8_49.x"] <- "p8_49"
tmp_grupal <- tmp_grupal[, !names(tmp_grupal) %in% c("p8_49.y")]
# *****credito edicativo
tmp_educativo <- read.csv(file = "datos/TModulo.csv", header=TRUE, sep=",", colClasses="character")
tmp_educativo <- conversor(tmp_educativo)
tmp_educativo <- tmp_educativo[( tmp_educativo$p8_9_4 == "1" ), ] # eliminar registros que no cumplan con la condición
tmp_educativo$LlaveHO <- paste(tmp_educativo$upm, tmp_educativo$viv_sel, tmp_educativo$hogar, sep="")
tmp_educativo$factor <- as.integer(tmp_educativo$factor)
tmp_educativo <- aggregate(tmp_educativo[,c("factor")], by = list("LlaveHO"=tmp_educativo$LlaveHO), FUN = sum, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(tmp_educativo)[colnames(tmp_educativo) == "x"] <- "factor"
tmp_educativo$p8_9_4 <- 1
tmp_educativo$p8_34 <- 0
aux_educativo <- read.csv(file = "datos/TEduca.csv", header=TRUE, sep=",", colClasses="character")
aux_educativo <- conversor(aux_educativo)
aux_educativo <- conversor(aux_educativo)
aux_educativo$p8_34 <- as.integer(aux_educativo$p8_34)
aux_educativo <- aux_educativo[( aux_educativo$p8_34 >= 1 & aux_educativo$p8_34 <= 999887 ), ] # eliminar registros que no cumplan con la condición
aux_educativo$LlaveHO <- paste(aux_educativo$upm, aux_educativo$viv_sel, aux_educativo$hogar, sep="")
aux_educativo <- aggregate(aux_educativo[,c("p8_34")], by = list("LlaveHO"=aux_educativo$LlaveHO), FUN = sum, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(aux_educativo)[colnames(aux_educativo) == "x"] <- "p8_34"
tmp_educativo <- merge(tmp_educativo, aux_educativo, by = "LlaveHO", all.x = T)
tmp_educativo$p8_34.x <- ifelse(is.na(tmp_educativo$p8_34.y), 0, tmp_educativo$p8_34.y)
colnames(tmp_educativo)[colnames(tmp_educativo) == "p8_34.x"] <- "p8_34"
tmp_educativo <- tmp_educativo[, !names(tmp_educativo) %in% c("p8_34.y")]
# *****credito Bienes productivos
tmp_cred_agricola <- read.csv(file = "datos/TModulo.csv", header=TRUE, sep=",", colClasses="character")
tmp_cred_agricola <- conversor(tmp_cred_agricola)
tmp_cred_agricola <- tmp_cred_agricola[( tmp_cred_agricola$p6_25 == "1" ), ] # eliminar registros que no cumplan con la condición
tmp_cred_agricola$LlaveHO <- paste(tmp_cred_agricola$upm, tmp_cred_agricola$viv_sel, tmp_cred_agricola$hogar, sep="")
tmp_cred_agricola$p6_27 <- as.integer(tmp_cred_agricola$p6_27)
tmp_cred_agricola <- aggregate(tmp_cred_agricola[,c("p6_27")], by = list("LlaveHO"=tmp_cred_agricola$LlaveHO), FUN = sum, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(tmp_cred_agricola)[colnames(tmp_cred_agricola) == "x"] <- "p6_27"
tmp_cred_agricola$p6_25 <- 1
# ******Credito Negocio
tmp_cred_neg <- read.csv(file = "datos/TUnico.csv", header=TRUE, sep=",", colClasses="character")
tmp_cred_neg <- conversor(tmp_cred_neg)
tmp_cred_neg <- tmp_cred_neg[( tmp_cred_neg$p6_11 == "1" ), ] # eliminar registros que no cumplan con la condición
tmp_cred_neg$p6_17 <- ifelse(as.integer(tmp_cred_neg$p6_17) >= 1 & as.integer(tmp_cred_neg$p6_17) <= 9999887, as.integer(tmp_cred_neg$p6_17), 0)
tmp_cred_neg$LlaveHO <- paste(tmp_cred_neg$upm, tmp_cred_neg$viv_sel, tmp_cred_neg$hogar, sep="")
tmp_cred_neg <- aggregate(tmp_cred_neg[,c("p6_17")], by =list("LlaveHO"=tmp_cred_neg$LlaveHO), FUN = sum, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(tmp_cred_neg)[colnames(tmp_cred_neg) == "x"] <- "p6_17"
tmp_cred_neg$p6_11 <- 1
#********Créditos_informales
tmp_cred_Ot2 <- read.csv(file = "datos/TModulo.csv", header=TRUE, sep=",", colClasses="character")
tmp_cred_Ot2 <- conversor(tmp_cred_Ot2)
tmp_cred_Ot2 <- tmp_cred_Ot2[( tmp_cred_Ot2$p8_1_1 == "1" | tmp_cred_Ot2$p8_1_2 == "1" | tmp_cred_Ot2$p8_1_3 == "1" | tmp_cred_Ot2$p8_1_4 == "1" ), ] # eliminar registros que no cumplan con la condición
tmp_cred_Ot2$LlaveHO <- paste(tmp_cred_Ot2$upm, tmp_cred_Ot2$viv_sel, tmp_cred_Ot2$hogar, sep="")
tmp_cred_Ot2$p8_4_1 <- ifelse(as.integer(tmp_cred_Ot2$p8_4_1) >= 1 & as.integer(tmp_cred_Ot2$p8_4_1) <= 999887, as.integer(tmp_cred_Ot2$p8_4_1), 0)
tmp_cred_Ot2$p8_4_2 <- ifelse(as.integer(tmp_cred_Ot2$p8_4_2) >= 1 & as.integer(tmp_cred_Ot2$p8_4_2) <= 999887, as.integer(tmp_cred_Ot2$p8_4_2), 0)
tmp_cred_Ot2$p8_4_3 <- ifelse(as.integer(tmp_cred_Ot2$p8_4_3) >= 1 & as.integer(tmp_cred_Ot2$p8_4_3) <= 999887, as.integer(tmp_cred_Ot2$p8_4_3), 0)
tmp_cred_Ot2$p8_4_4 <- ifelse(as.integer(tmp_cred_Ot2$p8_4_4) >= 1 & as.integer(tmp_cred_Ot2$p8_4_4) <= 999887, as.integer(tmp_cred_Ot2$p8_4_4), 0)
tmp_cred_Ot2 <- aggregate(tmp_cred_Ot2[,c("p8_4_1", "p8_4_2", "p8_4_3", "p8_4_4")], by = list("LlaveHO"=tmp_cred_Ot2$LlaveHO), FUN = sum, na.rm=TRUE)# obtener on df agrupado por hogar
tmp_cred_Ot2$cred_inf <- 1

# ********Créditos_informales Asociados a la vivienda
tmp_cred_Ot1 <- read.csv(file = "datos/TVivienda.csv", header=TRUE, sep=",", colClasses="character")
tmp_cred_Ot1 <- conversor(tmp_cred_Ot1)
tmp_cred_Ot1 <- tmp_cred_Ot1[( tmp_cred_Ot1$p4_43_1 == "1" | tmp_cred_Ot1$p4_43_2 == "1" | tmp_cred_Ot1$p4_43_3 == "1" | tmp_cred_Ot1$p4_43_4 == "1" | tmp_cred_Ot1$p4_51 == "1"), ] # eliminar registros que no cumplan con la condición
tmp_cred_Ot1$LlaveVI <- paste(tmp_cred_Ot1$upm, tmp_cred_Ot1$viv_sel, sep="")
# Eliminar columnas innecesarias
tmp_cred_Ot1 <- tmp_cred_Ot1[, names(tmp_cred_Ot1) %in% c("LlaveVI", "p4_43_1", "p4_46_1", "p4_43_2", "p4_46_2", "p4_43_3", "p4_46_3", "p4_43_4", "p4_46_4", "p4_51", "p4_54") ] # Eliminar columnas
tmp_cred_Ot1$cred_inf_v <- 1
# ********************************************************
# **** Deuda
tmp_deuda_vpal$uni <- 1
tmp_hogar1 <- merge(tmp_hogar1, tmp_deuda_vpal, by = "LlaveVI", all.x = T)
tmp_hogar1$uni <- ifelse(is.na(tmp_hogar1$uni), 0, tmp_hogar1$uni)
tmp_hogar1$ccred_vpal <- ifelse(tmp_hogar1$h_ppal == 1 & tmp_hogar1$uni == 1, 1, 0)
tmp_hogar1$mnto_vpal <- ifelse(tmp_hogar1$h_ppal != 1 | tmp_hogar1$uni != 1 | tmp_hogar1$p4_33 %in% c("999999888", "999999999"), 0, as.integer(tmp_hogar1$p4_33)) +
  ifelse(tmp_hogar1$h_ppal != 1 | tmp_hogar1$uni != 1 | tmp_hogar1$p4_42 %in% c("999999888", "999999999"), 0, as.integer(tmp_hogar1$p4_42))
tmp_hogar1 <- tmp_hogar1[, !names(tmp_hogar1) %in% c("uni") ] # Eliminar columnas
# *****Vivienda secundaria
aux_deuda_vsec <- tmp_deuda_vsec
aux_deuda_vsec$p5_16 <- as.integer(aux_deuda_vsec$p5_16)
aux_deuda_vsec$p5_16 <- ifelse(aux_deuda_vsec$p5_16 %in% c(99999888, 99999999), 0, aux_deuda_vsec$p5_16)
aux_deuda_vsec <- aggregate(aux_deuda_vsec[,c("p5_16")], by = list("LlaveHO"=aux_deuda_vsec$LlaveHO), FUN = sum, na.rm=TRUE)# obtener on df agrupado por hogar
aux_deuda_vsec$ccred_vsec <- 1
colnames(aux_deuda_vsec)[colnames(aux_deuda_vsec) == "x"] <- "mnto_vsec"
tmp_hogar1 <- merge(tmp_hogar1, aux_deuda_vsec, by = "LlaveHO", all.x = T)
tmp_hogar1$ccred_vsec.x <- ifelse(is.na(tmp_hogar1$ccred_vsec.y), 0, 1)
tmp_hogar1$mnto_vsec.x <- ifelse(tmp_hogar1$ccred_vsec.x == 1, tmp_hogar1$mnto_vsec.y, 0)
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "ccred_vsec.x"] <- "ccred_vsec"
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "mnto_vsec.x"] <- "mnto_vsec"
tmp_hogar1 <- tmp_hogar1[, !names(tmp_hogar1) %in% c("mnto_vsec.y","ccred_vsec.y")]
# ***Credito Hipotecario credito de vivienda Principal + credito vivienda secundari
tmp_hogar1$cred_hip = ifelse(tmp_hogar1$ccred_vpal + tmp_hogar1$ccred_vsec > 0, 1, 0)
tmp_hogar1$mto_hipot = tmp_hogar1$mnto_vpal + tmp_hogar1$mnto_vsec
# **********************************************
# ***creditos Tarjeta bacaria y/o departamental
# *** Departamental
aux_deuda_t__deptal <- tmp_deuda_t__deptal
aux_deuda_t__deptal$p8_15 <- as.integer(aux_deuda_t__deptal$p8_15)
aux_deuda_t__deptal$p8_15 <- ifelse(aux_deuda_t__deptal$p8_15 %in% c(999888, 999999), 0, aux_deuda_t__deptal$p8_15)
aux_deuda_t__deptal <- aggregate(aux_deuda_t__deptal[,c("p8_15")], by = list("LlaveHO"=aux_deuda_t__deptal$LlaveHO), FUN = sum, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(aux_deuda_t__deptal)[colnames(aux_deuda_t__deptal) == "x"] <- "mto_dept"
aux_deuda_t__deptal$c_deptal <- 1
tmp_hogar1 <- merge(tmp_hogar1, aux_deuda_t__deptal, by = "LlaveHO", all.x = T)
tmp_hogar1$cred_tcrd <- ifelse(is.na(tmp_hogar1$c_deptal), 0, 1)
tmp_hogar1$mto_dept <- ifelse(is.na(tmp_hogar1$mto_dept), 0, tmp_hogar1$mto_dept)
tmp_hogar1$mto_tcrd <- ifelse(tmp_hogar1$cred_tcrd == 0 , 0, tmp_hogar1$mto_dept)
# *** Bancaria
aux_deuda_t_bnco <- tmp_deuda_t_bnco
aux_deuda_t_bnco$p8_21 <- as.integer(aux_deuda_t_bnco$p8_21)
aux_deuda_t_bnco$p8_21 <- ifelse(aux_deuda_t_bnco$p8_21 %in% c(999888, 999999), 0, aux_deuda_t_bnco$p8_21)
aux_deuda_t_bnco <- aggregate(aux_deuda_t_bnco[,c("p8_21")], by = list("LlaveHO"=aux_deuda_t_bnco$LlaveHO), FUN = sum, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(aux_deuda_t_bnco)[colnames(aux_deuda_t_bnco) == "x"] <- "mto_bnc"
aux_deuda_t_bnco$c_deptal <- 1
tmp_hogar1 <- merge(tmp_hogar1, aux_deuda_t_bnco, by = "LlaveHO", all.x = T)
tmp_hogar1$c_deptal.x <- ifelse(is.na(tmp_hogar1$c_deptal.x), 0, tmp_hogar1$c_deptal.x)
tmp_hogar1$c_deptal.y <- ifelse(is.na(tmp_hogar1$c_deptal.y), 0, tmp_hogar1$c_deptal.y)
tmp_hogar1$mto_bnc <- ifelse(is.na(tmp_hogar1$mto_bnc), 0, tmp_hogar1$mto_bnc)
tmp_hogar1$cred_tcrd <- ifelse(tmp_hogar1$c_deptal.x == 1 | tmp_hogar1$c_deptal.y == 1 , 1, 0)
tmp_hogar1$mto_tcrd <- ifelse(tmp_hogar1$cred_tcrd == 1, tmp_hogar1$mto_tcrd + tmp_hogar1$mto_bnc, tmp_hogar1$mto_tcrd)
colnames(tmp_hogar1)[colnames(tmp_hogar1) == "c_deptal.x"] <- "c_deptal"
tmp_hogar1 <- tmp_hogar1[, !names(tmp_hogar1) %in% c("c_deptal.y")]
# **********************************************
# ***creditos vehiculos
tmp_automoto$uni <- 1
tmp_hogar1 <- merge(tmp_hogar1, tmp_automoto, by = "LlaveHO", all.x = T)
tmp_hogar1$cred_aumo <- ifelse(tmp_hogar1$uni == 1, 1, tmp_hogar1$cred_aumo)
tmp_hogar1$cred_aumo <- ifelse(is.na(tmp_hogar1$cred_aumo), 0, tmp_hogar1$cred_aumo)
tmp_hogar1$mto_aumo <- ifelse(tmp_hogar1$uni == 1, tmp_hogar1$p7_12 + tmp_hogar1$p7_21,tmp_hogar1$cred_aumo)
tmp_hogar1$mto_aumo <- ifelse(is.na(tmp_hogar1$mto_aumo), 0, tmp_hogar1$mto_aumo)
tmp_hogar1 <- tmp_hogar1[, !names(tmp_hogar1) %in% c("uni", "p7_4_1", "p7_4_2", "factor", "LlaveVI.y", "p7_12", "p7_21")]
# **********************************************
# ***creditos nomina y/o personal
tmp_deuda_nomper$uni <- 1
tmp_hogar1 <- merge(tmp_hogar1, tmp_deuda_nomper, by = "LlaveHO", all.x = T)
tmp_hogar1$cred_nmpe <- ifelse(tmp_hogar1$uni == 1, 1, tmp_hogar1$cred_nmpe)
tmp_hogar1$mto_nmpe <- ifelse(tmp_hogar1$uni == 1, tmp_hogar1$p8_25 + tmp_hogar1$p8_41, tmp_hogar1$mto_nmpe)
tmp_hogar1$cred_nmpe <- ifelse(is.na(tmp_hogar1$cred_nmpe), 0, tmp_hogar1$cred_nmpe)
tmp_hogar1$mto_nmpe <- ifelse(is.na(tmp_hogar1$mto_nmpe ), 0, tmp_hogar1$mto_nmpe )
tmp_hogar1 <- tmp_hogar1[, !names(tmp_hogar1) %in% c("uni", "p8_25", "p8_41", "factor", "x")]

# *****************************************************************
# **** Otros créditos
# ********** Creditos informales asociados a la vivienda Principal
tmp_cred_Ot1$uni <- 1
tmp_hogar1 <- merge(tmp_hogar1, tmp_cred_Ot1, by = "LlaveVI", all.x = T)
tmp_hogar1$uni <- ifelse(is.na(tmp_hogar1$uni ), 0, as.integer(tmp_hogar1$uni ))
tmp_hogar1$p4_43_1 <- ifelse(is.na(tmp_hogar1$p4_43_1 ), 0, as.integer(tmp_hogar1$p4_43_1 ))
tmp_hogar1$p4_46_1 <- ifelse(is.na(tmp_hogar1$p4_46_1 ), 0, as.integer(tmp_hogar1$p4_46_1 ))
tmp_hogar1$p4_43_2 <- ifelse(is.na(tmp_hogar1$p4_43_2 ), 0, as.integer(tmp_hogar1$p4_43_2 ))
tmp_hogar1$p4_46_2 <- ifelse(is.na(tmp_hogar1$p4_46_2 ), 0, as.integer(tmp_hogar1$p4_46_2 ))
tmp_hogar1$p4_43_3 <- ifelse(is.na(tmp_hogar1$p4_43_3 ), 0, as.integer(tmp_hogar1$p4_43_3 ))
tmp_hogar1$p4_46_3 <- ifelse(is.na(tmp_hogar1$p4_46_3 ), 0, as.integer(tmp_hogar1$p4_46_3 ))
tmp_hogar1$p4_43_4 <- ifelse(is.na(tmp_hogar1$p4_43_4 ), 0, as.integer(tmp_hogar1$p4_43_4 ))
tmp_hogar1$p4_46_4 <- ifelse(is.na(tmp_hogar1$p4_46_4 ), 0, as.integer(tmp_hogar1$p4_46_4 ))
tmp_hogar1$p4_51 <- ifelse(is.na(tmp_hogar1$p4_51 ), 0, as.integer(tmp_hogar1$p4_51 ))
tmp_hogar1$p4_54 <- ifelse(is.na(tmp_hogar1$p4_54 ), 0, as.integer(tmp_hogar1$p4_54 ))
tmp_hogar1$cred_inf_v <- ifelse(is.na(tmp_hogar1$cred_inf_v), 0, as.integer(tmp_hogar1$cred_inf_v))

tmp_hogar1$p4_46_1 <- ifelse(!tmp_hogar1$p4_46_1 %in% c(0, 999999888, 999999999) & tmp_hogar1$h_ppal == 1 & tmp_hogar1$uni == 1, tmp_hogar1$p4_46_1, 0)
tmp_hogar1$p4_46_2 <- ifelse(!tmp_hogar1$p4_46_2 %in% c(0, 9999888 , 9999999 ) & tmp_hogar1$h_ppal == 1 & tmp_hogar1$uni == 1, tmp_hogar1$p4_46_2, 0)
tmp_hogar1$p4_46_3 <- ifelse(!tmp_hogar1$p4_46_3 %in% c(0, 999999888, 999999999) & tmp_hogar1$h_ppal == 1 & tmp_hogar1$uni == 1, tmp_hogar1$p4_46_3, 0)
tmp_hogar1$p4_46_4 <- ifelse(!tmp_hogar1$p4_46_4 %in% c(0, 999999888, 999999999) & tmp_hogar1$h_ppal == 1 & tmp_hogar1$uni == 1, tmp_hogar1$p4_46_4, 0)
tmp_hogar1$p4_54 <- ifelse(!tmp_hogar1$p4_54 %in% c(0, 999999888, 999999999) & tmp_hogar1$h_ppal == 1 & tmp_hogar1$uni == 1, tmp_hogar1$p4_54 , 0)
tmp_hogar1$cred_rest <- ifelse(tmp_hogar1$h_ppal == 1 & tmp_hogar1$uni == 1, 1, tmp_hogar1$cred_rest)
tmp_hogar1$mto_rest <- ifelse(tmp_hogar1$cred_rest == 1, tmp_hogar1$p4_46_1 + tmp_hogar1$p4_46_2 +
                                tmp_hogar1$p4_46_3 + tmp_hogar1$p4_46_4 +
                                tmp_hogar1$p4_54, tmp_hogar1$mto_rest)
tmp_hogar1 <- tmp_hogar1[, !names(tmp_hogar1) %in% c("uni","p4_43_1", "p4_46_1", "p4_43_2", "p4_46_2", "p4_43_3", "p4_46_3", "p4_43_4", "p4_46_4", "p4_51", "p4_54", "cred_inf_v")]
# ********** Creditos grupal
aux_grupal <- tmp_grupal
aux_grupal$p8_49 <- ifelse(aux_grupal$p8_49 %in% c(999888,999999), 0, aux_grupal$p8_49)
aux_grupal <- aggregate(aux_grupal[,c("p8_49")], by = list("LlaveHO"=aux_grupal$LlaveHO), FUN = sum, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(aux_grupal)[colnames(aux_grupal) == "x"] <- "p8_49"
aux_grupal$uni <- 1
tmp_hogar1 <- merge(tmp_hogar1, aux_grupal, by = "LlaveHO", all.x = T)
tmp_hogar1$uni <- ifelse(is.na(tmp_hogar1$uni ), 0, tmp_hogar1$uni )
tmp_hogar1$p8_49 <- ifelse(is.na(tmp_hogar1$p8_49), 0, tmp_hogar1$p8_49)
tmp_hogar1$cred_rest <- ifelse(tmp_hogar1$uni == 1 , 1, tmp_hogar1$cred_rest)
tmp_hogar1$mto_rest <- ifelse(tmp_hogar1$uni == 1 , tmp_hogar1$mto_rest + tmp_hogar1$p8_49, tmp_hogar1$mto_rest)
tmp_hogar1 <- tmp_hogar1[, !names(tmp_hogar1) %in% c("uni", "p8_49")]
## ********** Creditos educativo
tmp_educativo$uni <- 1
tmp_hogar1 <- merge(tmp_hogar1, tmp_educativo, by = "LlaveHO", all.x = T)
tmp_hogar1$uni <- ifelse(is.na(tmp_hogar1$uni ), 0, tmp_hogar1$uni )
tmp_hogar1$p8_34 <- ifelse(is.na(tmp_hogar1$p8_34), 0, tmp_hogar1$p8_34)
tmp_hogar1$cred_rest <- ifelse(tmp_hogar1$uni == 1 , 1, tmp_hogar1$cred_rest)
tmp_hogar1$mto_rest <- ifelse(tmp_hogar1$uni == 1 , tmp_hogar1$mto_rest + tmp_hogar1$p8_34, tmp_hogar1$mto_rest)
tmp_hogar1 <- tmp_hogar1[, !names(tmp_hogar1) %in% c("uni", "p8_34")]
# ********** otros Creditos
tmp_cred_Ot2$uni <- 1
tmp_hogar1 <- merge(tmp_hogar1, tmp_cred_Ot2, by = "LlaveHO", all.x = T)
tmp_hogar1$uni <- ifelse(is.na(tmp_hogar1$uni ), 0, tmp_hogar1$uni )
tmp_hogar1$p8_4_1 <- ifelse(is.na(tmp_hogar1$p8_4_1), 0, tmp_hogar1$p8_4_1)
tmp_hogar1$p8_4_2 <- ifelse(is.na(tmp_hogar1$p8_4_2), 0, tmp_hogar1$p8_4_2)
tmp_hogar1$p8_4_3 <- ifelse(is.na(tmp_hogar1$p8_4_3), 0, tmp_hogar1$p8_4_3)
tmp_hogar1$p8_4_4 <- ifelse(is.na(tmp_hogar1$p8_4_4), 0, tmp_hogar1$p8_4_4)
tmp_hogar1$cred_rest <- ifelse(tmp_hogar1$uni == 1 , 1, tmp_hogar1$cred_rest)
tmp_hogar1$mto_rest <- ifelse(tmp_hogar1$uni == 1 , tmp_hogar1$mto_rest + tmp_hogar1$p8_4_1 +
                                tmp_hogar1$p8_4_2 + tmp_hogar1$p8_4_3 + tmp_hogar1$p8_4_4, tmp_hogar1$mto_rest)
tmp_hogar1 <- tmp_hogar1[, !names(tmp_hogar1) %in% c("uni", "p8_4_1", "p8_4_2", "p8_4_3", "p8_4_4", "cred_inf")]
# ********** Credito Agricola
tmp_cred_agricola$uni <- 1
tmp_hogar1 <- merge(tmp_hogar1, tmp_cred_agricola, by = "LlaveHO", all.x = T)
tmp_hogar1$uni <- ifelse(is.na(tmp_hogar1$uni ), 0, tmp_hogar1$uni )
tmp_hogar1$p6_27 <- ifelse(is.na(tmp_hogar1$p6_27), 0, tmp_hogar1$p6_27)
tmp_hogar1$p6_27 <- ifelse(tmp_hogar1$p6_27 %in% c(9999888, 9999999), 0,tmp_hogar1$p6_27)
tmp_hogar1$cred_rest <- ifelse(tmp_hogar1$uni == 1 , 1, tmp_hogar1$cred_rest)
tmp_hogar1$mto_rest <- ifelse(tmp_hogar1$uni == 1 , tmp_hogar1$mto_rest + tmp_hogar1$p6_27, tmp_hogar1$mto_rest)
tmp_hogar1 <- tmp_hogar1[, !names(tmp_hogar1) %in% c("uni", "p6_25", "p6_27", "factor.y")]
# ********** Credito Negocio
aux_cred_neg <- tmp_cred_neg
aux_cred_neg$p6_17 <- ifelse(aux_cred_neg$p6_17 %in% c(9999888, 9999999), 0, aux_cred_neg$p6_17)
aux_cred_neg <- aggregate(tmp_cred_neg[,c("p6_17")], by = list("LlaveHO"=tmp_cred_neg$LlaveHO), FUN = sum, na.rm=TRUE)# obtener on df agrupado por hogar
colnames(aux_cred_neg)[colnames(aux_cred_neg) == "x"] <- "p6_17"
aux_cred_neg$uni <- 1
tmp_hogar1 <- merge(tmp_hogar1, aux_cred_neg, by = "LlaveHO", all.x = T)
tmp_hogar1$uni <- ifelse(is.na(tmp_hogar1$uni ), 0, tmp_hogar1$uni )
tmp_hogar1$p6_17 <- ifelse(is.na(tmp_hogar1$p6_17), 0, tmp_hogar1$p6_17)
tmp_hogar1$cred_rest <- ifelse(tmp_hogar1$uni == 1 , 1, tmp_hogar1$cred_rest)
tmp_hogar1$mto_rest <- ifelse(tmp_hogar1$uni == 1 , tmp_hogar1$mto_rest + tmp_hogar1$p6_17, tmp_hogar1$mto_rest)
tmp_hogar1 <- tmp_hogar1[, !names(tmp_hogar1) %in% c("uni", "p6_17", "factor.x")]
# ****************************
# ****Hogares con credito No hipotecario
tmp_hogar1$cred_nohip <- ifelse(tmp_hogar1$cred_tcrd + tmp_hogar1$cred_aumo +
                                  tmp_hogar1$cred_nmpe + tmp_hogar1$cred_rest > 0, 1, 0)
tmp_hogar1$mto_nohipo <- tmp_hogar1$mto_tcrd + tmp_hogar1$mto_aumo +
                         tmp_hogar1$mto_nmpe + tmp_hogar1$mto_rest

# ****************************
# ****Hogares con credito
tmp_hogar1$cred_tot <- ifelse(tmp_hogar1$cred_hip + tmp_hogar1$cred_nohip > 0, 1, 0)
tmp_hogar1$mto_ctot <- tmp_hogar1$mto_hipot + tmp_hogar1$mto_nohipo
tmp_hogar1$aux1 <- tmp_hogar1$fac_hog * tmp_hogar1$cred_tot
tmp_hogar1$aux2 <- tmp_hogar1$fac_hog * tmp_hogar1$mto_ctot

gpo1 <- sum(aggregate(tmp_hogar1[,c("aux1")],by = list(tmp_hogar1$LlaveHO), FUN = sum, na.rm=TRUE)$x) # obtener on df agrupado por hogar
gpo2 <- sum(aggregate(tmp_hogar1[,c("aux2")],by = list(tmp_hogar1$LlaveHO), FUN = sum, na.rm=TRUE)$x)/1000000 # obtener on df agrupado por hogar
nofinan <- rbind(nofinan, c(2, "-Deuda", gpo1, gpo2))

# CREACION DEL DATASET
# columna de deusas
rel_ingxdeuda <- tmp_hogar1[, c("LlaveHO", "mto_ctot")]
rel_ingxdeuda
dataset <- na.omit(rel_ingxdeuda)
str(dataset)
dataset <- conversor(dataset)
# Quitamos los ceros
deudas <- dataset[dataset$mto_ctot != 0, ]
 
# llamamos el dataset del ingreso anual
ing_anual <- read.csv(file = "ingresoanual.csv", header=TRUE, sep=",", colClasses="character")
ingreso_men <- ing_anual[, c("llaveho", "ingxtrab")]
ingreso_men$ingxtrab <- as.numeric(ingreso_men$ingxtrab)

str(ingreso_men)

#ingreso_men <- data.frame(llaveho = ing_anual$llaveho, ingxtrab = ing_anual$ingxtrab / 12)
ingreso_men$ingxtrab <- ingreso_men$ingxtrab / 12
ingreso_men <- ingreso_men[ingreso_men$ingxtrab !=0,]
ingreso_men 
# unimos las dos tablas
relacion_deuxing <- merge(deudas,ingreso_men,by = "llaveho", all.x = T)
relacion_deuxing
relacion_deuxing <- na.omit(relacion_deuxing)
relacion_deuxing

# Export the dataset to a CSV file
write.csv(relacion_deuxing, file = "rel_ing_deu.csv", row.names = FALSE)


# Create a scatterplot
plot(relacion_deuxing$ingxtrab, relacion_deuxing$mto_ctot, 
     main = "Relacion entre ingreso y deudas",  # Title of the plot
     xlab = "Ingreso de nomina",             # Label for the x-axis
     ylab = "Deuda total",             # Label for the y-axis
     pch = 20,                          # Type of point (16 is a filled circle)
     col = "blue"                       # Color of the points
)
grid()
# NORMALIZACION

# Function to perform Min-Max scaling
min_max_scaling <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Normalize the "x" and "y" columns using the min_max_scaling function
relacion_deuxing$ingxtrabN <- min_max_scaling(relacion_deuxing$ingxtrab)
relacion_deuxing$mto_ctotN <- min_max_scaling(relacion_deuxing$mto_ctot)
plot(relacion_deuxing$ingxtrabN, relacion_deuxing$mto_ctotN, 
     main = "Relacion entre ingreso y deudas",  # Title of the plot
     xlab = "Ingreso de nomina",             # Label for the x-axis
     ylab = "Deuda total",             # Label for the y-axis
     pch = 16,                          # Type of point (16 is a filled circle)
     col = "blue"                       # Color of the points
)

# Create a scatterplot
plot(relacion_deuxing$ingxtrab, relacion_deuxing$mto_ctot, main = "Scatterplot with Exponential Regression Line", xlab = "X-axis", ylab = "Y-axis")

# Fit an exponential regression model
exp_model <- lm(log(relacion_deuxing$mto_ctot) ~ relacion_deuxing$ingxtrab)  # Take the natural logarithm of y for an exponential relationship

# Add the regression line to the plot
abline(exp_model, col = "red")


na_indices <- which(is.na(relacion_deuxing$mto_ctotN))

inf_indices <- which(is.infinite(relacion_deuxing$mto_ctotN))
inf_indices
# --------------------------------------------------------------
# ******************************************************
# ****Riqueza Neta
tmp_hogar1$riq_net <- tmp_hogar1$val_nofin + tmp_hogar1$val_finan - tmp_hogar1$mto_ctot
tmp_hogar1$tipo_riq <- ifelse(tmp_hogar1$riq_net < 0, 1, -1)# Hogares con riqueza Negativa
tmp_hogar1$tipo_riq <- ifelse(tmp_hogar1$riq_net == 0, 2, tmp_hogar1$tipo_riq)# Hogares con riqueza Cero
tmp_hogar1$tipo_riq <- ifelse(tmp_hogar1$riq_net > 0, 3, tmp_hogar1$tipo_riq)# Hogares con riqueza Positiva
tmp_hogar1$tipo_riq <- ifelse(tmp_hogar1$riq_net == 0 & tmp_hogar1$cacvo_nofn == 1 &
                                tmp_hogar1$val_noFin == 0 & tmp_hogar1$cacvo_fin == 1 &
                                tmp_hogar1$val_finan == 0 & tmp_hogar1$cred_tot == 1 &
                                tmp_hogar1$mto_ctot == 0, 0, tmp_hogar1$tipo_riq)# Hogares con riqueza NO Especificada

tmp_hogar1$criq_net <- ifelse(tmp_hogar1$tipo_riq > 0, 1, 0)
tmp_hogar1$aux1 <- tmp_hogar1$fac_hog * tmp_hogar1$criq_net
tmp_hogar1$aux2 <- tmp_hogar1$fac_hog * tmp_hogar1$riq_net
# CREAR LA DISTRIBUCION DE LA RIQUEZA
riqueza <- tmp_hogar1$riq_net

riqueza <- riqueza[!is.na(riqueza)]
min(riqueza)
max(riqueza)

# Define income range boundaries based on representative values
rangos_riqueza <- cut(riqueza,
                     breaks = c(min(riqueza),0, 10000, 100000, 500000, 1000000,2000000, max(riqueza)),
                     labels = c('< $0','$10,000', '$100,000', '$500,000', '$1,000,000', '$2,000,000','$5,000,000+'),
                     include.lowest = TRUE)

rangos_riqueza

# Create a table of counts by income range
riqueza_counts <- table(rangos_riqueza)
riqueza_counts

# Convert the table to a data frame for better readability
riqueza_df <- as.data.frame(riqueza_counts)
riqueza_df

# Rename the columns for clarity
colnames(riqueza_df) <- c("RangoRiqueza", "Frecuen")

# Print the result
print(riqueza_df)


# Create a barplot of income range counts
barplot(riqueza_df$Frecuen, names.arg = riqueza_df$RangoRiqueza, 
        main = "Distribucion de Riqueza", xlab = "Rango de Riquezas", ylab = "Frecuencia",
        col = "lightblue", border = "black")


write.csv(riqueza_df, file = "riqueza_dist.csv", row.names = FALSE)

# *****************************************************

tmp_hogar1
gpo1 <- sum(aggregate(tmp_hogar1[,c("aux1")],by = list(tmp_hogar1$LlaveHO), FUN = sum, na.rm=TRUE)$x) # obtener on df agrupado por hogar
gpo2 <- sum(aggregate(tmp_hogar1[,c("aux2")],by = list(tmp_hogar1$LlaveHO), FUN = sum, na.rm=TRUE)$x)/1000000 # obtener on df agrupado por hogar
nofinan <- rbind(nofinan, c(2, "=Riqueza Neta", gpo1, gpo2))
nofinan
# *****************************************************
str(tmp_hogar1)


