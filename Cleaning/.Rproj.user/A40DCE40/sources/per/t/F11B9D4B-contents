
########################################################################################
# Encuesta Nacional sobre las Finanzas de los Hogares (ENFIH) 2019. #
# #
# Programa en código R para el cálculo ingreso corriente efectivo total de los hogares.#
# #
# 03 de noviembre 2021 versión 1.0.0.0 #
########################################################################################
rm(list=ls(all=TRUE))
options(digits = 17)
# Establecer directorio de trabajo
setwd("Q:/Proyectos/ENFIH/FinancialAnalyticsWebsite/Cleaning")
work <- read.csv(file = "conjunto_de_datos_tmodulo_enfih_2019.csv", header=TRUE, sep=",", colClasses="character")
work$LlaveVI <- paste(work$UPM_DIS, work$VIV_SEL, sep="")
work$LlaveHO <- paste(work$UPM_DIS, work$VIV_SEL, work$HOGAR, sep="")
work$LlaveSD <- paste(work$UPM_DIS, work$VIV_SEL, work$HOGAR, work$N_REN, sep="")

# Cambiar el tamaño de las letras
# Get the column names
col_names <- names(work)


# Replace all uppercase letters with their lowercase counterparts in column names
new_col_names <- chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz", col_names)

# Set the modified column names back to the data frame
names(work) <- new_col_names

names(work)

work$c_ingtra <- 0
work$v_ingtra <- 0
work$c_ingrta <- 0
work$v_ingrta <- 0
work$c_inginv <- 0
work$v_inginv <- 0
work$c_inggob <- 0
work$v_inggob <- 0
work$c_ingpen <- 0
work$v_ingpen <- 0
work$c_ingfpai <- 0
work$v_ingfpai <- 0
work$c_ingfext <- 0
work$v_ingfext <- 0
print("So far so good - 1")
work$c_ingalq <- 0
work$v_ingalq <- 0
work$c_ingemp <- 0
work$v_ingemp <- 0
work$c_ingneg <- 0
work$v_ingneg <- 0
work$c_ingbeca <- 0
work$v_ingbeca <- 0
work$c_ing2t <- 0
work$v_ing2t <- 0
work$c_ingotin <- 0
work$v_ingotin <- 0
# ** Asigno el valor "No especificado" a los registros que en la pregunta de tenencia del ingreso
# ** declararon no sabe
# **
work$p10_2_1_1 <- ifelse(work$p10_1_1 == "9", "9999999", work$p10_2_1_1 )

work$p10_2_2_1 <- ifelse(work$p10_1_2 == "9", "9999999", work$p10_2_2_1 )
work$p10_2_3_1 <- ifelse(work$p10_1_3 == "9", "9999999", work$p10_2_3_1 )
work$p10_2_4_1 <- ifelse(work$p10_1_4 == "9", "9999999", work$p10_2_4_1 )
work$p10_2_5_1 <- ifelse(work$p10_1_5 == "9", "9999999", work$p10_2_5_1 )
work$p10_2_6_1 <- ifelse(work$p10_1_6 == "9", "9999999", work$p10_2_6_1 )
work$p10_2_7_1 <- ifelse(work$p10_1_7 == "9", "9999999", work$p10_2_7_1 )
work$p10_2_8_1 <- ifelse(work$p10_1_8 == "9", "9999999", work$p10_2_8_1 )
work$p10_2_9_1 <- ifelse(work$p10_1_9 == "9", "9999999", work$p10_2_9_1 )
work$p10_2_10_1 <- ifelse(work$p10_1_10 == "9", "9999999", work$p10_2_10_1) 

# **** Anualizo el Ingreso por trabajo y elimino del calculo los ingresos de única
#         ocasión y los valores "No especificados"
# ***** indico si la persona recibe ese ingreso
work$p4a8_2[is.na(work$p4a8_2)] <- "9"
work$p4a8_1[is.na(work$p4a8_1)] <- "999888"

work$p4a8_2<- ifelse(work$p4a8_2 == "" | work$p4a8_2 == " ", "9", work$p4a8_2)
work$p4a8_1 <- ifelse(work$p4a8_1 == "" | work$p4a8_1 == " ", "999888", work$p4a8_1)


work$p4a8_2
 work$p4a8_1
cat("Number of missing values  in p4a8_2"," is:", sum(is.na(work$p4a8_2)), "\n")
cat("Number of missing values  in p4a8_1"," is:", sum(is.na(work$p4a8_1)), "\n")
# Replace empty strings with "0" in a specific column


work$c_ingtra <- ifelse(work$p4a8_2 == "1", 1, work$c_ingtra)
work$v_ingtra <- ifelse(work$p4a8_2 == "1", as.integer(work$p4a8_1) * 52, work$v_ingtra)
work$c_ingtra <- ifelse(work$p4a8_2 == "2", 1, work$c_ingtra)
work$v_ingtra <- ifelse(work$p4a8_2 == "2", as.integer(work$p4a8_1) * 24, work$v_ingtra)
work$c_ingtra <- ifelse(work$p4a8_2 == "3", 1, work$c_ingtra)
work$v_ingtra <- ifelse(work$p4a8_2 == "3", as.integer(work$p4a8_1) * 12, work$v_ingtra)
work$c_ingtra <- ifelse(work$p4a8_2 == "4", 1, work$c_ingtra)
work$v_ingtra <- ifelse(work$p4a8_2 == "4", as.integer(work$p4a8_1) * 1 , work$v_ingtra)
work$c_ingtra <- ifelse(work$p4a8_2 == "9", 1, work$c_ingtra)
work$v_ingtra <- ifelse(work$p4a8_2 == "9", 0, work$v_ingtra)

work$c_ingtra <- ifelse(gsub(" ", "", work$p4a8_2)=="" & work$p4a8_1 == "999888", 1,work$c_ingtra)

work$v_ingtra <- ifelse(gsub(" ", "", work$p4a8_2) == "" & work$p4a8_1 == "999888", 0, work$v_ingtra)



print("So far so good - 2")


# **** Anualizo el Ingreso por renta y elimino del calculo los ingresos de única
#       ocasión y los valores "No especificados"
# ***** indico si la persona recibe ese ingreso
work$c_ingrta <- ifelse(as.integer(work$p10_2_6_1) > 0 & work$p10_2_6_2 == "1", 1,
                        work$c_ingrta)
work$v_ingrta <- ifelse(as.integer(work$p10_2_6_1) > 0 & work$p10_2_6_2 == "1",
                        as.integer(work$p10_2_6_1) * 52, work$v_ingrta)
work$c_ingrta <- ifelse(as.integer(work$p10_2_6_1) > 0 & work$p10_2_6_2 == "2", 1,
                        work$c_ingrta)
work$v_ingrta <- ifelse(as.integer(work$p10_2_6_1) > 0 & work$p10_2_6_2 == "2",
                        as.integer(work$p10_2_6_1) * 24, work$v_ingrta)
work$c_ingrta <- ifelse(as.integer(work$p10_2_6_1) > 0 & work$p10_2_6_2 == "3", 1,
                        work$c_ingrta)
work$v_ingrta <- ifelse(as.integer(work$p10_2_6_1) > 0 & work$p10_2_6_2 == "3", as.integer(work$p10_2_6_1) * 12, work$v_ingrta)
work$c_ingrta <- ifelse(as.integer(work$p10_2_6_1) > 0 & work$p10_2_6_2 == "4", 1,
                        work$c_ingrta)
work$v_ingrta <- ifelse(as.integer(work$p10_2_6_1) > 0 & work$p10_2_6_2 == "4",
                        as.integer(work$p10_2_6_1) * 6, work$v_ingrta)
work$c_ingrta <- ifelse(as.integer(work$p10_2_6_1) > 0 & work$p10_2_6_2 == "5", 1,
                        work$c_ingrta)
work$v_ingrta <- ifelse(as.integer(work$p10_2_6_1) > 0 & work$p10_2_6_2 == "5",
                        as.integer(work$p10_2_6_1) * 4, work$v_ingrta)
work$c_ingrta <- ifelse(as.integer(work$p10_2_6_1) > 0 & work$p10_2_6_2 == "6", 1,
                        work$c_ingrta)
work$v_ingrta <- ifelse(as.integer(work$p10_2_6_1) > 0 & work$p10_2_6_2 == "6",
                        as.integer(work$p10_2_6_1) * 2, work$v_ingrta)
work$c_ingrta <- ifelse(as.integer(work$p10_2_6_1) > 0 & work$p10_2_6_2 == "7", 1,
                        work$c_ingrta)
work$v_ingrta <- ifelse(as.integer(work$p10_2_6_1) > 0 & work$p10_2_6_2 == "7",
                        as.integer(work$p10_2_6_1) * 1, work$v_ingrta)
work$c_ingrta <- ifelse(as.integer(work$p10_2_6_1) > 0 & work$p10_2_6_2 == "8", 0,
                        work$c_ingrta)
work$v_ingrta <- ifelse(as.integer(work$p10_2_6_1) > 0 & work$p10_2_6_2 == "8",
                        as.integer(work$p10_2_6_1) * 0, work$v_ingrta)
work$c_ingrta <- ifelse(work$p10_2_6_1 %in% c("9999888","9999999"), 1, work$c_ingrta)
work$v_ingrta <- ifelse(work$p10_2_6_1 %in% c("9999888","9999999"), 0, work$v_ingrta)
# 4

# **** Anualizo el Ingreso por inversiones y elimino del calculo los ingresos de
# única ocasión y los valores "No especificados"
# ***** indico si la persona recibe ese ingreso
work$c_inginv <- ifelse(as.integer(work$p10_2_7_1) > 0 & work$p10_2_7_2 == "1", 1,
                        work$c_inginv)
work$v_inginv <- ifelse(as.integer(work$p10_2_7_1) > 0 & work$p10_2_7_2 == "1",
                        as.integer(work$p10_2_7_1) * 52, work$v_inginv)
work$c_inginv <- ifelse(as.integer(work$p10_2_7_1) > 0 & work$p10_2_7_2 == "2", 1,
                        work$c_inginv)
work$v_inginv <- ifelse(as.integer(work$p10_2_7_1) > 0 & work$p10_2_7_2 == "2",
                        as.integer(work$p10_2_7_1) * 24, work$v_inginv)
work$c_inginv <- ifelse(as.integer(work$p10_2_7_1) > 0 & work$p10_2_7_2 == "3", 1,
                        work$c_inginv)
work$v_inginv <- ifelse(as.integer(work$p10_2_7_1) > 0 & work$p10_2_7_2 == "3",
                        as.integer(work$p10_2_7_1) * 12, work$v_inginv)
work$c_inginv <- ifelse(as.integer(work$p10_2_7_1) > 0 & work$p10_2_7_2 == "4", 1,
                        work$c_inginv)
work$v_inginv <- ifelse(as.integer(work$p10_2_7_1) > 0 & work$p10_2_7_2 == "4",
                        as.integer(work$p10_2_7_1) * 6, work$v_inginv)
work$c_inginv <- ifelse(as.integer(work$p10_2_7_1) > 0 & work$p10_2_7_2 == "5", 1,
                        work$c_inginv)
work$v_inginv <- ifelse(as.integer(work$p10_2_7_1) > 0 & work$p10_2_7_2 == "5",
                        as.integer(work$p10_2_7_1) * 4, work$v_inginv)
work$c_inginv <- ifelse(as.integer(work$p10_2_7_1) > 0 & work$p10_2_7_2 == "6", 1,
                        work$c_inginv)
work$v_inginv <- ifelse(as.integer(work$p10_2_7_1) > 0 & work$p10_2_7_2 == "6",
                        as.integer(work$p10_2_7_1) * 2, work$v_inginv)
work$c_inginv <- ifelse(as.integer(work$p10_2_7_1) > 0 & work$p10_2_7_2 == "7", 1,
                        work$c_inginv)
work$v_inginv <- ifelse(as.integer(work$p10_2_7_1) > 0 & work$p10_2_7_2 == "7",
                        as.integer(work$p10_2_7_1) * 1, work$v_inginv)
work$c_inginv <- ifelse(as.integer(work$p10_2_7_1) > 0 & work$p10_2_7_2 == "8", 0,
                        work$c_inginv)
work$v_inginv <- ifelse(as.integer(work$p10_2_7_1) > 0 & work$p10_2_7_2 == "8",
                        as.integer(work$p10_2_7_1) * 0, work$v_inginv)
work$c_inginv <- ifelse(work$p10_2_7_1 %in% c("9999888","9999999"), 1, work$c_inginv)
work$v_inginv <- ifelse(work$p10_2_7_1 %in% c("9999888","9999999"), 0, work$v_inginv)
# **** Anualizo el Ingreso por Programas de Gobierno y elimino del calculo los 

#     ingresos de única ocasión y los valores "No especificados"
# ***** indico si la persona recibe ese ingreso
work$c_inggob <- ifelse(as.integer(work$p10_2_1_1) > 0 & work$p10_2_1_2 == "1", 1,
                        work$c_inggob)
work$v_inggob <- ifelse(as.integer(work$p10_2_1_1) > 0 & work$p10_2_1_2 == "1",
                        as.integer(work$p10_2_1_1) * 52, work$v_inggob)
work$c_inggob <- ifelse(as.integer(work$p10_2_1_1) > 0 & work$p10_2_1_2 == "2", 1,
                        work$c_inggob)
work$v_inggob <- ifelse(as.integer(work$p10_2_1_1) > 0 & work$p10_2_1_2 == "2",
                        as.integer(work$p10_2_1_1) * 24, work$v_inggob)
work$c_inggob <- ifelse(as.integer(work$p10_2_1_1) > 0 & work$p10_2_1_2 == "3", 1,
                        work$c_inggob)
work$v_inggob <- ifelse(as.integer(work$p10_2_1_1) > 0 & work$p10_2_1_2 == "3",
                        as.integer(work$p10_2_1_1) * 12, work$v_inggob)
work$c_inggob <- ifelse(as.integer(work$p10_2_1_1) > 0 & work$p10_2_1_2 == "4", 1,
                        work$c_inggob)
work$v_inggob <- ifelse(as.integer(work$p10_2_1_1) > 0 & work$p10_2_1_2 == "4",
                        as.integer(work$p10_2_1_1) * 6, work$v_inggob)
work$c_inggob <- ifelse(as.integer(work$p10_2_1_1) > 0 & work$p10_2_1_2 == "5", 1,
                        work$c_inggob)
work$v_inggob <- ifelse(as.integer(work$p10_2_1_1) > 0 & work$p10_2_1_2 == "5",
                        as.integer(work$p10_2_1_1) * 4, work$v_inggob)
work$c_inggob <- ifelse(as.integer(work$p10_2_1_1) > 0 & work$p10_2_1_2 == "6", 1,
                        work$c_inggob)
work$v_inggob <- ifelse(as.integer(work$p10_2_1_1) > 0 & work$p10_2_1_2 == "6",
                        as.integer(work$p10_2_1_1) * 2, work$v_inggob)
work$c_inggob <- ifelse(as.integer(work$p10_2_1_1) > 0 & work$p10_2_1_2 == "7", 1,
                        work$c_inggob)
work$v_inggob <- ifelse(as.integer(work$p10_2_1_1) > 0 & work$p10_2_1_2 == "7",
                        as.integer(work$p10_2_1_1) * 1, work$v_inggob)
work$c_inggob <- ifelse(as.integer(work$p10_2_1_1) > 0 & work$p10_2_1_2 == "8", 0,
                        work$c_inggob)
work$v_inggob <- ifelse(as.integer(work$p10_2_1_1) > 0 & work$p10_2_1_2 == "8",
                        as.integer(work$p10_2_1_1) * 0, work$v_inggob)
work$c_inggob <- ifelse(work$p10_2_1_1 %in% c("9999888","9999999"), 1, work$c_inggob)
work$v_inggob <- ifelse(work$p10_2_1_1 %in% c("9999888","9999999"), 0, work$v_inggob)


# **** Anualizo el Ingreso por Pensiones y jubilaciones y elimino del calculo los
# ingresos de única ocasión y los valores "No especificados"
# ***** indico si la persona recibe ese ingreso
work$c_ingpen <- ifelse(as.integer(work$p10_2_2_1) > 0 & work$p10_2_2_2 == "1", 1,
                        work$c_ingpen)
work$v_ingpen <- ifelse(as.integer(work$p10_2_2_1) > 0 & work$p10_2_2_2 == "1",
                        as.integer(work$p10_2_2_1) * 52, work$v_ingpen)
work$c_ingpen <- ifelse(as.integer(work$p10_2_2_1) > 0 & work$p10_2_2_2 == "2", 1,
                        work$c_ingpen)
work$v_ingpen <- ifelse(as.integer(work$p10_2_2_1) > 0 & work$p10_2_2_2 == "2",
                        as.integer(work$p10_2_2_1) * 24, work$v_ingpen)
work$c_ingpen <- ifelse(as.integer(work$p10_2_2_1) > 0 & work$p10_2_2_2 == "3", 1,
                        work$c_ingpen)
work$v_ingpen <- ifelse(as.integer(work$p10_2_2_1) > 0 & work$p10_2_2_2 == "3",
                        as.integer(work$p10_2_2_1) * 12, work$v_ingpen)
work$c_ingpen <- ifelse(as.integer(work$p10_2_2_1) > 0 & work$p10_2_2_2 == "4", 1,
                        work$c_ingpen)
work$v_ingpen <- ifelse(as.integer(work$p10_2_2_1) > 0 & work$p10_2_2_2 == "4",
                        as.integer(work$p10_2_2_1) * 6, work$v_ingpen)
work$c_ingpen <- ifelse(as.integer(work$p10_2_2_1) > 0 & work$p10_2_2_2 == "5", 1,
                        work$c_ingpen)
work$v_ingpen <- ifelse(as.integer(work$p10_2_2_1) > 0 & work$p10_2_2_2 == "5",
                        as.integer(work$p10_2_2_1) * 4, work$v_ingpen)
work$c_ingpen <- ifelse(as.integer(work$p10_2_2_1) > 0 & work$p10_2_2_2 == "6", 1,
                        work$c_ingpen)
work$v_ingpen <- ifelse(as.integer(work$p10_2_2_1) > 0 & work$p10_2_2_2 == "6",
                        as.integer(work$p10_2_2_1) * 2, work$v_ingpen)
work$c_ingpen <- ifelse(as.integer(work$p10_2_2_1) > 0 & work$p10_2_2_2 == "7", 1,
                        work$c_ingpen)
work$v_ingpen <- ifelse(as.integer(work$p10_2_2_1) > 0 & work$p10_2_2_2 == "7",
                        as.integer(work$p10_2_2_1) * 1, work$v_ingpen)
work$c_ingpen <- ifelse(as.integer(work$p10_2_2_1) > 0 & work$p10_2_2_2 == "8", 0,
                        work$c_ingpen)
work$v_ingpen <- ifelse(as.integer(work$p10_2_2_1) > 0 & work$p10_2_2_2 == "8",
                        as.integer(work$p10_2_2_1) * 0, work$v_ingpen)
work$c_ingpen <- ifelse(work$p10_2_2_1 %in% c("9999888","9999999"), 1, work$c_ingpen)
work$v_ingpen <- ifelse(work$p10_2_2_1 %in% c("9999888","9999999"), 0, work$v_ingpen)

# **** Anualizo el Ingreso por Ingresos por Familiares en el Pais y elimino del
#   calculo los ingresos de única ocasión y los valores "No especificados"
# ***** indico si la persona recibe ese ingreso

work$c_ingfpai <- ifelse(as.integer(work$p10_2_3_1) > 0 & work$p10_2_3_2 == "1", 1,
                         work$c_ingfpai)
work$v_ingfpai <- ifelse(as.integer(work$p10_2_3_1) > 0 & work$p10_2_3_2 == "1",
                         as.integer(work$p10_2_3_1) * 52, work$v_ingfpai)
work$c_ingfpai <- ifelse(as.integer(work$p10_2_3_1) > 0 & work$p10_2_3_2 == "2", 1,
                         work$c_ingfpai)
work$v_ingfpai <- ifelse(as.integer(work$p10_2_3_1) > 0 & work$p10_2_3_2 == "2",
                         as.integer(work$p10_2_3_1) * 24, work$v_ingfpai)
work$c_ingfpai <- ifelse(as.integer(work$p10_2_3_1) > 0 & work$p10_2_3_2 == "3", 1,
                         work$c_ingfpai)
work$v_ingfpai <- ifelse(as.integer(work$p10_2_3_1) > 0 & work$p10_2_3_2 == "3",
                         as.integer(work$p10_2_3_1) * 12, work$v_ingfpai)
work$c_ingfpai <- ifelse(as.integer(work$p10_2_3_1) > 0 & work$p10_2_3_2 == "4", 1,
                         work$c_ingfpai)
work$v_ingfpai <- ifelse(as.integer(work$p10_2_3_1) > 0 & work$p10_2_3_2 == "4",
                         as.integer(work$p10_2_3_1) * 6, work$v_ingfpai)
work$c_ingfpai <- ifelse(as.integer(work$p10_2_3_1) > 0 & work$p10_2_3_2 == "5", 1,
                         work$c_ingfpai)
work$v_ingfpai <- ifelse(as.integer(work$p10_2_3_1) > 0 & work$p10_2_3_2 == "5",
                         as.integer(work$p10_2_3_1) * 4, work$v_ingfpai)
work$c_ingfpai <- ifelse(as.integer(work$p10_2_3_1) > 0 & work$p10_2_3_2 == "6", 1,
                         work$c_ingfpai)
work$v_ingfpai <- ifelse(as.integer(work$p10_2_3_1) > 0 & work$p10_2_3_2 == "6",
                         as.integer(work$p10_2_3_1) * 2, work$v_ingfpai)
work$c_ingfpai <- ifelse(as.integer(work$p10_2_3_1) > 0 & work$p10_2_3_2 == "7", 1,
                         work$c_ingfpai)
work$v_ingfpai <- ifelse(as.integer(work$p10_2_3_1) > 0 & work$p10_2_3_2 == "7",
                         as.integer(work$p10_2_3_1) * 1, work$v_ingfpai)
work$c_ingfpai <- ifelse(as.integer(work$p10_2_3_1) > 0 & work$p10_2_3_2 == "8", 0,
                         work$c_ingfpai)
work$v_ingfpai <- ifelse(as.integer(work$p10_2_3_1) > 0 & work$p10_2_3_2 == "8",
                         as.integer(work$p10_2_3_1) * 0, work$v_ingfpai)
work$c_ingfpai <- ifelse(work$p10_2_3_1 %in% c("9999888","9999999"), 1, work$c_ingfpai)
work$v_ingfpai <- ifelse(work$p10_2_3_1 %in% c("9999888","9999999"), 0, work$v_ingfpai)
# **** Anualizo el Ingreso por Ingresos por Familiares en el Extranjero y elimino del
#     calculo los ingresos de única ocasión y los valores "No especificados"
# ***** indico si la persona recibe ese ingreso
work$c_ingfext <- ifelse(as.integer(work$p10_2_4_1) > 0 & work$p10_2_4_2 == "1", 1,
                         work$c_ingfext)
work$v_ingfext <- ifelse(as.integer(work$p10_2_4_1) > 0 & work$p10_2_4_2 == "1",
                         as.integer(work$p10_2_4_1) * 52, work$v_ingfext)
work$c_ingfext <- ifelse(as.integer(work$p10_2_4_1) > 0 & work$p10_2_4_2 == "2", 1,
                         work$c_ingfext)
work$v_ingfext <- ifelse(as.integer(work$p10_2_4_1) > 0 & work$p10_2_4_2 == "2",
                         as.integer(work$p10_2_4_1) * 24, work$v_ingfext)

work$c_ingfext <- ifelse(as.integer(work$p10_2_4_1) > 0 & work$p10_2_4_2 == "3", 1,
                         work$c_ingfext)
work$v_ingfext <- ifelse(as.integer(work$p10_2_4_1) > 0 & work$p10_2_4_2 == "3",
                         as.integer(work$p10_2_4_1) * 12, work$v_ingfext)
work$c_ingfext <- ifelse(as.integer(work$p10_2_4_1) > 0 & work$p10_2_4_2 == "4", 1,
                         work$c_ingfext)
work$v_ingfext <- ifelse(as.integer(work$p10_2_4_1) > 0 & work$p10_2_4_2 == "4",
                         as.integer(work$p10_2_4_1) * 6, work$v_ingfext)
work$c_ingfext <- ifelse(as.integer(work$p10_2_4_1) > 0 & work$p10_2_4_2 == "5", 1,
                         work$c_ingfext)
work$v_ingfext <- ifelse(as.integer(work$p10_2_4_1) > 0 & work$p10_2_4_2 == "5",
                         as.integer(work$p10_2_4_1) * 4, work$v_ingfext)
work$c_ingfext <- ifelse(as.integer(work$p10_2_4_1) > 0 & work$p10_2_4_2 == "6", 1,
                         work$c_ingfext)
work$v_ingfext <- ifelse(as.integer(work$p10_2_4_1) > 0 & work$p10_2_4_2 == "6",
                         as.integer(work$p10_2_4_1) * 2, work$v_ingfext)
work$c_ingfext <- ifelse(as.integer(work$p10_2_4_1) > 0 & work$p10_2_4_2 == "7", 1,
                         work$c_ingfext)
work$v_ingfext <- ifelse(as.integer(work$p10_2_4_1) > 0 & work$p10_2_4_2 == "7",
                         as.integer(work$p10_2_4_1) * 1, work$v_ingfext)
work$c_ingfext <- ifelse(as.integer(work$p10_2_4_1) > 0 & work$p10_2_4_2 == "8", 0,
                         work$c_ingfext)
work$v_ingfext <- ifelse(as.integer(work$p10_2_4_1) > 0 & work$p10_2_4_2 == "8",
                         as.integer(work$p10_2_4_1) * 0, work$v_ingfext)
work$c_ingfext <- ifelse(work$p10_2_4_1 %in% c("9999888","9999999"), 1, work$c_ingfext)
work$v_ingfext <- ifelse(work$p10_2_4_1 %in% c("9999888","9999999"), 0, work$v_ingfext)

# **** Anualizo el Ingreso por Ingresos por Alquiler de algun bien y elimino del
# calculo los ingresos de única ocasión y los valores "No especificados"
# ***** indico si la persona recibe ese ingreso
work$c_ingalq <- ifelse(as.integer(work$p10_2_5_1) > 0 & work$p10_2_5_2 == "1", 1,
                        work$c_ingalq)
work$v_ingalq <- ifelse(as.integer(work$p10_2_5_1) > 0 & work$p10_2_5_2 == "1",
                        as.integer(work$p10_2_5_1) * 52, work$v_ingalq)
work$c_ingalq <- ifelse(as.integer(work$p10_2_5_1) > 0 & work$p10_2_5_2 == "2", 1,
                        work$c_ingalq)
work$v_ingalq <- ifelse(as.integer(work$p10_2_5_1) > 0 & work$p10_2_5_2 == "2",
                        as.integer(work$p10_2_5_1) * 24, work$v_ingalq)
work$c_ingalq <- ifelse(as.integer(work$p10_2_5_1) > 0 & work$p10_2_5_2 == "3", 1,
                        work$c_ingalq)
work$v_ingalq <- ifelse(as.integer(work$p10_2_5_1) > 0 & work$p10_2_5_2 == "3",
                        as.integer(work$p10_2_5_1) * 12, work$v_ingalq)
work$c_ingalq <- ifelse(as.integer(work$p10_2_5_1) > 0 & work$p10_2_5_2 == "4", 1,
                        work$c_ingalq)
work$v_ingalq <- ifelse(as.integer(work$p10_2_5_1) > 0 & work$p10_2_5_2 == "4",
                        as.integer(work$p10_2_5_1) * 6, work$v_ingalq)
work$c_ingalq <- ifelse(as.integer(work$p10_2_5_1) > 0 & work$p10_2_5_2 == "5", 1,
                        work$c_ingalq)
work$v_ingalq <- ifelse(as.integer(work$p10_2_5_1) > 0 & work$p10_2_5_2 == "5",
                        as.integer(work$p10_2_5_1) * 4, work$v_ingalq)
work$c_ingalq <- ifelse(as.integer(work$p10_2_5_1) > 0 & work$p10_2_5_2 == "6", 1,
                        work$c_ingalq)
work$v_ingalq <- ifelse(as.integer(work$p10_2_5_1) > 0 & work$p10_2_5_2 == "6",
                        as.integer(work$p10_2_5_1) * 2, work$v_ingalq)
work$c_ingalq <- ifelse(as.integer(work$p10_2_5_1) > 0 & work$p10_2_5_2 == "7", 1,
                        work$c_ingalq)
work$v_ingalq <- ifelse(as.integer(work$p10_2_5_1) > 0 & work$p10_2_5_2 == "7",
                        as.integer(work$p10_2_5_1) * 1, work$v_ingalq)
work$c_ingalq <- ifelse(as.integer(work$p10_2_5_1) > 0 & work$p10_2_5_2 == "8", 0,
                        work$c_ingalq)
work$v_ingalq <- ifelse(as.integer(work$p10_2_5_1) > 0 & work$p10_2_5_2 == "8",
                        as.integer(work$p10_2_5_1) * 0, work$v_ingalq)

work$c_ingalq <- ifelse(work$p10_2_5_1 %in% c("9999888","9999999"), 1, work$c_ingalq)
work$v_ingalq <- ifelse(work$p10_2_5_1 %in% c("9999888","9999999"), 0, work$v_ingalq)
# **** Anualizo el Ingreso por Venta o empeño de algun bien 
# ingresos de única ocasión y los valores "No especificados"
# ***** indico si la persona recibe ese ingreso
work$c_ingemp <- ifelse(as.integer(work$p10_2_8_1) > 0 & work$p10_2_8_2 == "1", 1,
                        work$c_ingemp)
work$v_ingemp <- ifelse(as.integer(work$p10_2_8_1) > 0 & work$p10_2_8_2 == "1",
                        as.integer(work$p10_2_8_1) * 52, work$v_ingemp)
work$c_ingemp <- ifelse(as.integer(work$p10_2_8_1) > 0 & work$p10_2_8_2 == "2", 1,
                        work$c_ingemp)
work$v_ingemp <- ifelse(as.integer(work$p10_2_8_1) > 0 & work$p10_2_8_2 == "2",
                        as.integer(work$p10_2_8_1) * 24, work$v_ingemp)
work$c_ingemp <- ifelse(as.integer(work$p10_2_8_1) > 0 & work$p10_2_8_2 == "3", 1,
                        work$c_ingemp)
work$v_ingemp <- ifelse(as.integer(work$p10_2_8_1) > 0 & work$p10_2_8_2 == "3",
                        as.integer(work$p10_2_8_1) * 12, work$v_ingemp)
work$c_ingemp <- ifelse(as.integer(work$p10_2_8_1) > 0 & work$p10_2_8_2 == "4", 1,
                        work$c_ingemp)
work$v_ingemp <- ifelse(as.integer(work$p10_2_8_1) > 0 & work$p10_2_8_2 == "4",
                        as.integer(work$p10_2_8_1) * 6, work$v_ingemp)
work$c_ingemp <- ifelse(as.integer(work$p10_2_8_1) > 0 & work$p10_2_8_2 == "5", 1,
                        work$c_ingemp)
work$v_ingemp <- ifelse(as.integer(work$p10_2_8_1) > 0 & work$p10_2_8_2 == "5",
                        as.integer(work$p10_2_8_1) * 4, work$v_ingemp)
work$c_ingemp <- ifelse(as.integer(work$p10_2_8_1) > 0 & work$p10_2_8_2 == "6", 1,
                        work$c_ingemp)
work$v_ingemp <- ifelse(as.integer(work$p10_2_8_1) > 0 & work$p10_2_8_2 == "6",
                        as.integer(work$p10_2_8_1) * 2, work$v_ingemp)
work$c_ingemp <- ifelse(as.integer(work$p10_2_8_1) > 0 & work$p10_2_8_2 == "7", 1,
                        work$c_ingemp)
work$v_ingemp <- ifelse(as.integer(work$p10_2_8_1) > 0 & work$p10_2_8_2 == "7",
                        as.integer(work$p10_2_8_1) * 1, work$v_ingemp)
work$c_ingemp <- ifelse(as.integer(work$p10_2_8_1) > 0 & work$p10_2_8_2 == "8", 0,
                        work$c_ingemp)
work$v_ingemp <- ifelse(as.integer(work$p10_2_8_1) > 0 & work$p10_2_8_2 == "8",
                        as.integer(work$p10_2_8_1) * 0, work$v_ingemp)
work$c_ingemp <- ifelse(work$p10_2_8_1 %in% c("9999888","9999999"), 1, work$c_ingemp)
work$v_ingemp <- ifelse(work$p10_2_8_1 %in% c("9999888","9999999"), 0, work$v_ingemp)


# **** Anualizo el Ingreso por Utilidades o ganancias de algun bien y elimino del
#     calculo los ingresos de única ocasión y los valores "No especificados"
# ***** indico si la persona recibe ese ingreso
work$c_ingneg <- ifelse(as.integer(work$p10_2_9_1) > 0 & work$p10_2_9_2 == "1", 1,
                        work$c_ingneg)
work$v_ingneg <- ifelse(as.integer(work$p10_2_9_1) > 0 & work$p10_2_9_2 == "1",
                        as.integer(work$p10_2_9_1) * 52, work$v_ingneg)
work$c_ingneg <- ifelse(as.integer(work$p10_2_9_1) > 0 & work$p10_2_9_2 == "2", 1,
                        work$c_ingneg)
work$v_ingneg <- ifelse(as.integer(work$p10_2_9_1) > 0 & work$p10_2_9_2 == "2",
                        as.integer(work$p10_2_9_1) * 24, work$v_ingneg)
work$c_ingneg <- ifelse(as.integer(work$p10_2_9_1) > 0 & work$p10_2_9_2 == "3", 1,
                        work$c_ingneg)
work$v_ingneg <- ifelse(as.integer(work$p10_2_9_1) > 0 & work$p10_2_9_2 == "3",
                        as.integer(work$p10_2_9_1) * 12, work$v_ingneg)
work$c_ingneg <- ifelse(as.integer(work$p10_2_9_1) > 0 & work$p10_2_9_2 == "4", 1,
                        work$c_ingneg)
work$v_ingneg <- ifelse(as.integer(work$p10_2_9_1) > 0 & work$p10_2_9_2 == "4",
                        as.integer(work$p10_2_9_1) * 6, work$v_ingneg)
work$c_ingneg <- ifelse(as.integer(work$p10_2_9_1) > 0 & work$p10_2_9_2 == "5", 1,
                        work$c_ingneg)
work$v_ingneg <- ifelse(as.integer(work$p10_2_9_1) > 0 & work$p10_2_9_2 == "5",
                        as.integer(work$p10_2_9_1) * 4, work$v_ingneg)
work$c_ingneg <- ifelse(as.integer(work$p10_2_9_1) > 0 & work$p10_2_9_2 == "6", 1,
                        work$c_ingneg)
work$v_ingneg <- ifelse(as.integer(work$p10_2_9_1) > 0 & work$p10_2_9_2 == "6",
                        as.integer(work$p10_2_9_1) * 2, work$v_ingneg)
work$c_ingneg <- ifelse(as.integer(work$p10_2_9_1) > 0 & work$p10_2_9_2 == "7", 1,
                        work$c_ingneg)
work$v_ingneg <- ifelse(as.integer(work$p10_2_9_1) > 0 & work$p10_2_9_2 == "7",
                        as.integer(work$p10_2_9_1) * 1, work$v_ingneg)
work$c_ingneg <- ifelse(as.integer(work$p10_2_9_1) > 0 & work$p10_2_9_2 == "8", 0,
                        work$c_ingneg)
work$v_ingneg <- ifelse(as.integer(work$p10_2_9_1) > 0 & work$p10_2_9_2 == "8",
                        as.integer(work$p10_2_9_1) * 0, work$v_ingneg)
work$c_ingneg <- ifelse(work$p10_2_9_1 %in% c("9999888","9999999"), 1, work$c_ingneg)
work$v_ingneg <- ifelse(work$p10_2_9_1 %in% c("9999888","9999999"), 0, work$v_ingneg)

# **** Anualizo el Ingreso por becas y elimino del calculo los ingresos de única
# ocasión y los valores "No especificados"
# ***** indico si la persona recibe ese ingreso
work$c_ingbeca <- ifelse(as.integer(work$p10_2_10_1) > 0 & work$p10_2_10_2 == "1", 1,
                         work$c_ingbeca)
work$v_ingbeca <- ifelse(as.integer(work$p10_2_10_1) > 0 & work$p10_2_10_2 == "1",
                         as.integer(work$p10_2_10_1) * 52, work$v_ingbeca)
work$c_ingbeca <- ifelse(as.integer(work$p10_2_10_1) > 0 & work$p10_2_10_2 == "2", 1,
                         work$c_ingbeca)
work$v_ingbeca <- ifelse(as.integer(work$p10_2_10_1) > 0 & work$p10_2_10_2 == "2",
                         as.integer(work$p10_2_10_1) * 24, work$v_ingbeca)
work$c_ingbeca <- ifelse(as.integer(work$p10_2_10_1) > 0 & work$p10_2_10_2 == "3", 1,
                         work$c_ingbeca)
work$v_ingbeca <- ifelse(as.integer(work$p10_2_10_1) > 0 & work$p10_2_10_2 == "3",
                         as.integer(work$p10_2_10_1) * 12, work$v_ingbeca)
work$c_ingbeca <- ifelse(as.integer(work$p10_2_10_1) > 0 & work$p10_2_10_2 == "4", 1,
                         work$c_ingbeca)
work$v_ingbeca <- ifelse(as.integer(work$p10_2_10_1) > 0 & work$p10_2_10_2 == "4",
                         as.integer(work$p10_2_10_1) * 6, work$v_ingbeca)
work$c_ingbeca <- ifelse(as.integer(work$p10_2_10_1) > 0 & work$p10_2_10_2 == "5", 1,
                         work$c_ingbeca)
work$v_ingbeca <- ifelse(as.integer(work$p10_2_10_1) > 0 & work$p10_2_10_2 == "5",
                         as.integer(work$p10_2_10_1) * 4, work$v_ingbeca)
work$c_ingbeca <- ifelse(as.integer(work$p10_2_10_1) > 0 & work$p10_2_10_2 == "6", 1,
                         work$c_ingbeca)
work$v_ingbeca <- ifelse(as.integer(work$p10_2_10_1) > 0 & work$p10_2_10_2 == "6",
                         as.integer(work$p10_2_10_1) * 2, work$v_ingbeca)
work$c_ingbeca <- ifelse(as.integer(work$p10_2_10_1) > 0 & work$p10_2_10_2 == "7", 1,
                         work$c_ingbeca)
work$v_ingbeca <- ifelse(as.integer(work$p10_2_10_1) > 0 & work$p10_2_10_2 == "7",
                         as.integer(work$p10_2_10_1) * 1, work$v_ingbeca)
work$c_ingbeca <- ifelse(as.integer(work$p10_2_10_1) > 0 & work$p10_2_10_2 == "8", 0,
                         work$c_ingbeca)
work$v_ingbeca <- ifelse(as.integer(work$p10_2_10_1) > 0 & work$p10_2_10_2 == "8",
                         as.integer(work$p10_2_10_1) * 0, work$v_ingbeca)
work$c_ingbeca <- ifelse(work$p10_2_10_1 %in% c("9999888","9999999"), 1, work$c_ingbeca)
work$v_ingbeca <- ifelse(work$p10_2_10_1 %in% c("9999888","9999999"), 0, work$v_ingbeca)

# **** Anualizo el Ingreso por equivalente a trabajo de algun bien y elimino del
#calculo los ingresos de única ocasión y los valores "No especificados"
# ***** indico si la persona recibe ese ingreso
work$c_ing2t <- ifelse(as.integer(work$p10_4_1) > 0 & work$p10_3c == "1" & work$p10_4_2
                       == "1", 1, work$c_ing2t) 
work$v_ing2t <- ifelse(as.integer(work$p10_4_1) > 0 & work$p10_3c == "1" & work$p10_4_2== "1", as.integer(work$p10_4_1) * 52, work$v_ing2t)
work$c_ing2t <- ifelse(as.integer(work$p10_4_1) > 0 & work$p10_3c == "1" & work$p10_4_2
                       == "2", 1, work$c_ing2t)
work$v_ing2t <- ifelse(as.integer(work$p10_4_1) > 0 & work$p10_3c == "1" & work$p10_4_2
                       == "2", as.integer(work$p10_4_1) * 24, work$v_ing2t)
work$c_ing2t <- ifelse(as.integer(work$p10_4_1) > 0 & work$p10_3c == "1" & work$p10_4_2
                       == "3", 1, work$c_ing2t)
work$v_ing2t <- ifelse(as.integer(work$p10_4_1) > 0 & work$p10_3c == "1" & work$p10_4_2
                       == "3", as.integer(work$p10_4_1) * 12, work$v_ing2t)
work$c_ing2t <- ifelse(as.integer(work$p10_4_1) > 0 & work$p10_3c == "1" & work$p10_4_2
                       == "4", 1, work$c_ing2t)
work$v_ing2t <- ifelse(as.integer(work$p10_4_1) > 0 & work$p10_3c == "1" & work$p10_4_2
                       == "4", as.integer(work$p10_4_1) * 6, work$v_ing2t)
work$c_ing2t <- ifelse(as.integer(work$p10_4_1) > 0 & work$p10_3c == "1" & work$p10_4_2
                       == "5", 1, work$c_ing2t)
work$v_ing2t <- ifelse(as.integer(work$p10_4_1) > 0 & work$p10_3c == "1" & work$p10_4_2
                       == "5", as.integer(work$p10_4_1) * 4, work$v_ing2t)
work$c_ing2t <- ifelse(as.integer(work$p10_4_1) > 0 & work$p10_3c == "1" & work$p10_4_2
                       == "6", 1, work$c_ing2t)
work$v_ing2t <- ifelse(as.integer(work$p10_4_1) > 0 & work$p10_3c == "1" & work$p10_4_2
                       == "6", as.integer(work$p10_4_1) * 2, work$v_ing2t)
work$c_ing2t <- ifelse(as.integer(work$p10_4_1) > 0 & work$p10_3c == "1" & work$p10_4_2
                       == "7", 1, work$c_ing2t)
work$v_ing2t <- ifelse(as.integer(work$p10_4_1) > 0 & work$p10_3c == "1" & work$p10_4_2
                       == "7", as.integer(work$p10_4_1) * 1, work$v_ing2t)
work$c_ing2t <- ifelse(as.integer(work$p10_4_1) > 0 & work$p10_3c == "1" & work$p10_4_2
                       == "8", 0, work$c_ing2t)
work$v_ing2t <- ifelse(as.integer(work$p10_4_1) > 0 & work$p10_3c == "1" & work$p10_4_2
                       == "8", as.integer(work$p10_4_1) * 0, work$v_ing2t)
work$c_ing2t <- ifelse(work$p10_3c == "1" & work$p10_4_1 %in% c("9999888","9999999"), 1,
                       work$c_ing2t)
work$v_ing2t <- ifelse(work$p10_3c == "1" & work$p10_4_1 %in% c("9999888","9999999"), 0,
                       work$v_ing2t)

# **** Anualizo el Otros Ingresos del calculo los ingresos de única ocasión y los
#   "No especificados"
# ***** indico si la persona recibe ese ingreso
work$c_ingotin <- ifelse(as.integer(work$p10_4_1) > 0 & work$p10_3c != "1" &
                           work$p10_4_2 == "1", 1, work$c_ingotin)
work$v_ingotin <- ifelse(as.integer(work$p10_4_1) > 0 & work$p10_3c != "1" &
                           work$p10_4_2 == "1", as.integer(work$p10_4_1) * 52, work$v_ingotin)
work$c_ingotin <- ifelse(as.integer(work$p10_4_1) > 0 & work$p10_3c != "1" &
                           work$p10_4_2 == "2", 1, work$c_ingotin)
work$v_ingotin <- ifelse(as.integer(work$p10_4_1) > 0 & work$p10_3c != "1" &
                           work$p10_4_2 == "2", as.integer(work$p10_4_1) * 24, work$v_ingotin)
work$c_ingotin <- ifelse(as.integer(work$p10_4_1) > 0 & work$p10_3c != "1" &
                           work$p10_4_2 == "3", 1, work$c_ingotin)
work$v_ingotin <- ifelse(as.integer(work$p10_4_1) > 0 & work$p10_3c != "1" &
                           work$p10_4_2 == "3", as.integer(work$p10_4_1) * 12, work$v_ingotin)
work$c_ingotin <- ifelse(as.integer(work$p10_4_1) > 0 & work$p10_3c != "1" &
                           work$p10_4_2 == "4", 1, work$c_ingotin)
work$v_ingotin <- ifelse(as.integer(work$p10_4_1) > 0 & work$p10_3c != "1" &
                           work$p10_4_2 == "4", as.integer(work$p10_4_1) * 6, work$v_ingotin)
work$c_ingotin <- ifelse(as.integer(work$p10_4_1) > 0 & work$p10_3c != "1" &
                           work$p10_4_2 == "5", 1, work$c_ingotin)
work$v_ingotin <- ifelse(as.integer(work$p10_4_1) > 0 & work$p10_3c != "1" &
                           work$p10_4_2 == "5", as.integer(work$p10_4_1) * 4, work$v_ingotin)
work$c_ingotin <- ifelse(as.integer(work$p10_4_1) > 0 & work$p10_3c != "1" &
                           work$p10_4_2 == "6", 1, work$c_ingotin)
work$v_ingotin <- ifelse(as.integer(work$p10_4_1) > 0 & work$p10_3c != "1" &
                           work$p10_4_2 == "6", as.integer(work$p10_4_1) * 2, work$v_ingotin)
work$c_ingotin <- ifelse(as.integer(work$p10_4_1) > 0 & work$p10_3c != "1" & work$p10_4_2 == "7", 1, work$c_ingotin)
work$v_ingotin <- ifelse(as.integer(work$p10_4_1) > 0 & work$p10_3c != "1" &
                           work$p10_4_2 == "7", as.integer(work$p10_4_1) * 1, work$v_ingotin)
work$c_ingotin <- ifelse(as.integer(work$p10_4_1) > 0 & work$p10_3c != "1" &
                           work$p10_4_2 == "8", 0, work$c_ingotin)
work$v_ingotin <- ifelse(as.integer(work$p10_4_1) > 0 & work$p10_3c != "1" &
                           work$p10_4_2 == "8", as.integer(work$p10_4_1) * 0, work$v_ingotin)
work$c_ingotin <- ifelse(work$p10_4_1 %in% c("9999888","9999999") & work$p10_3c != "1",
                         1, work$c_ingotin)
work$v_ingotin <- ifelse(work$p10_4_1 %in% c("9999888","9999999") & work$p10_3c != "1",
                         0, work$v_ingotin)

# *****Consulta de Control obtiene Expansion del valor del ingreso por concepto
control <- work
control$factor <- as.integer(work$factor)
control$v_ingtra <- control$v_ingtra * control$factor
control$v_ingrta <- control$v_ingrta * control$factor
control$v_inginv <- control$v_inginv * control$factor
control$v_inggob <- control$v_inggob * control$factor
control$v_ingpen <- control$v_ingpen * control$factor
control$v_ingfpai <- control$v_ingfpai* control$factor
control$v_ingfext <- control$v_ingfext* control$factor
control$v_ingalq <- control$v_ingalq * control$factor
control$v_ingemp <- control$v_ingemp * control$factor
control$v_ingneg <- control$v_ingneg * control$factor
control$v_ingbeca <- control$v_ingbeca* control$factor
control$v_ing2t <- control$v_ing2t * control$factor
control$v_ingotin <- control$v_ingotin* control$factor
Control <- cbind(sum(control$v_ingtra ),sum(control$v_ingrta ),
                 sum(control$v_inginv ),sum(control$v_inggob ),
                 sum(control$v_ingpen ),sum(control$v_ingfpai),
                 sum(control$v_ingfext),sum(control$v_ingalq ),
                 sum(control$v_ingemp ),sum(control$v_ingneg ),
                 sum(control$v_ingbeca),sum(control$v_ing2t ),
                 sum(control$v_ingotin))

# ***** Agrupa los conceptos del ingreso es sus cuatro componentes
# ** Por trabajo- por Rentas - por Inversiones - Otros Ingresos
# *** (nivel persona)
# *** suma los valores
# *** identifica la tenencia
ing_anual <- work
ing_anual$c_ingxtrab <- ifelse(ing_anual$c_ingtra + ing_anual$c_ing2t > 0, 1, 0) 
ing_anual$ingxtrab <- ing_anual$v_ingtra + ing_anual$v_ing2t
ing_anual$c_ingxrnta <- ing_anual$c_ingrta
ing_anual$ingxrnta <- ing_anual$v_ingrta
ing_anual$c_ingxinv <- ing_anual$c_inginv
ing_anual$ingxinv <- ing_anual$v_inginv
ing_anual$c_otrosing <- ifelse(ing_anual$c_inggob + ing_anual$c_ingpen +
                                 ing_anual$c_ingfpai + ing_anual$c_ingfext +
                                 ing_anual$c_ingalq + ing_anual$c_ingemp +
                                 ing_anual$c_ingneg + ing_anual$c_ingbeca + ing_anual$c_ingotin >= 1, 1, 0)
ing_anual$otrosing <- ing_anual$v_inggob + ing_anual$v_ingpen +
  ing_anual$v_ingfpai + ing_anual$v_ingfext +
  ing_anual$v_ingalq + ing_anual$v_ingemp +
  ing_anual$v_ingneg + ing_anual$v_ingbeca + ing_anual$v_ingotin

# ***** Agrupa a nivel Hogar los componentes del Ingreso
# ***** Marca el hogar la tenencia del ingreso si al menos un integrante tiene ese concepto
# *** (Con la función max al evaluar las variables binarias identifica el hogar)
gpo1 <- aggregate(ing_anual[,c("c_ingxtrab")],by = list(ing_anual$llaveho), FUN = max, na.rm=TRUE)

colnames(gpo1)[colnames(gpo1) == "x"] <- "c_ingxtrab"

gpo2 <- aggregate(ing_anual[,c("ingxtrab")],by = list(ing_anual$llaveho), FUN = sum, na.rm=TRUE)
colnames(gpo2)[colnames(gpo2) == "x"] <- "ingxtrab"

gpo3 <- aggregate(ing_anual[,c("c_ingxrnta")],by = list(ing_anual$llaveho), FUN = max, na.rm=TRUE)
colnames(gpo3)[colnames(gpo3) == "x"] <- "c_ingxrnta"

gpo4 <- aggregate(ing_anual[,c("ingxrnta")],by = list(ing_anual$llaveho), FUN = sum, na.rm=TRUE)
colnames(gpo4)[colnames(gpo4) == "x"] <- "ingxrnta"
gpo5 <- aggregate(ing_anual[,c("c_ingxinv")],by = list(ing_anual$llaveho), FUN = max, na.rm=TRUE)
colnames(gpo5)[colnames(gpo5) == "x"] <- "c_ingxinv"
gpo6 <- aggregate(ing_anual[,c("ingxinv")],by = list(ing_anual$llaveho), FUN = sum, na.rm=TRUE)
colnames(gpo6)[colnames(gpo6) == "x"] <- "ingxinv"
gpo7 <- aggregate(ing_anual[,c("c_otrosing")],by = list(ing_anual$llaveho), FUN = max, na.rm=TRUE)
colnames(gpo7)[colnames(gpo7) == "x"] <- "c_otrosing"
gpo8 <- aggregate(ing_anual[,c("otrosing")],by = list(ing_anual$llaveho), FUN = sum, na.rm=TRUE)
colnames(gpo8)[colnames(gpo8) == "x"] <- "otrosing"
ing_anual_x_hogar <- merge( gpo1, gpo2, by = "Group.1", all.x = T)
ing_anual_x_hogar <- merge(ing_anual_x_hogar, gpo3, by = "Group.1", all.x = T)
ing_anual_x_hogar <- merge(ing_anual_x_hogar, gpo4, by = "Group.1", all.x = T)
ing_anual_x_hogar <- merge(ing_anual_x_hogar, gpo5, by = "Group.1", all.x = T)
ing_anual_x_hogar <- merge(ing_anual_x_hogar, gpo6, by = "Group.1", all.x = T)
ing_anual_x_hogar <- merge(ing_anual_x_hogar, gpo7, by = "Group.1", all.x = T)
ing_anual_x_hogar <- merge(ing_anual_x_hogar, gpo8, by = "Group.1", all.x = T)
colnames(ing_anual_x_hogar)[colnames(ing_anual_x_hogar) == "Group.1"] <- "llaveho"
ing_anual_x_hogar$ing_total <- 0


# ***Obtengo el ingreso total 
ing_anual_x_hogar$ing_total <- ing_anual_x_hogar$ingxtrab + ing_anual_x_hogar$ingxrnta + ing_anual_x_hogar$ingxinv + ing_anual_x_hogar$otrosing
# *** Asigno tipo de Hogar 1 a los hogares con Ingreso Positivo
ing_anual_x_hogar$tipo_ing <- ifelse(ing_anual_x_hogar$ing_total > 0, 1, 0)
ing_anual_x_hogar$destiping <- ifelse(ing_anual_x_hogar$ing_total > 0, "Con Ingreso Positivo", "")
# *** Asigno tipo de Hogar 1 a los hogares con Ingreso No especificado
ing_anual_x_hogar$tipo_ing <- ifelse(ing_anual_x_hogar$c_ingxtrab == 1 & ing_anual_x_hogar$ingxtrab == 0 & ing_anual_x_hogar$c_ingxrnta == 1 & ing_anual_x_hogar$ingxrnta == 0 & ing_anual_x_hogar$c_ingxinv == 1 & ing_anual_x_hogar$ingxinv == 0 & ing_anual_x_hogar$c_otrosing == 1 & ing_anual_x_hogar$otrosing == 0, 3, ing_anual_x_hogar$tipo_ing)
ing_anual_x_hogar$destiping <- ifelse(ing_anual_x_hogar$c_ingxtrab == 1 & ing_anual_x_hogar$ingxtrab == 0 & ing_anual_x_hogar$c_ingxrnta == 1 & ing_anual_x_hogar$ingxrnta == 0 & ing_anual_x_hogar$c_ingxinv == 1 & ing_anual_x_hogar$ingxinv == 0 & ing_anual_x_hogar$c_otrosing == 1 & ing_anual_x_hogar$otrosing == 0, "Con Ingreso No especificado", ing_anual_x_hogar$destiping)

# ***** De los hogares con ingresos no Especificados
# ***** obtengo sus integrantres y cuantos de ellos inidicaron ingreso NE
# ***** Tomo la pregunta 10_1_1
aux1<- work
aux1$integ <- 1
aux1$ing_ne <- ifelse(aux1$p10_1_1 == "9", 1, 0)
tmp_hog_c_int_ing_0 <- aggregate(aux1[,c("integ", "ing_ne")],by = list(aux1$llaveho), FUN = sum, na.rm=TRUE)
colnames(tmp_hog_c_int_ing_0)[colnames(tmp_hog_c_int_ing_0) == "Group.1"] <- "llaveho"
tmp_hog_c_int_ing_0 <- merge(ing_anual_x_hogar, tmp_hog_c_int_ing_0, by = "llaveho", all.x = T)
tmp_hog_c_int_ing_0 <- tmp_hog_c_int_ing_0[(tmp_hog_c_int_ing_0$tipo_ing == 3 ), ]
tmp_hog_c_int_ing_0 <- tmp_hog_c_int_ing_0[(tmp_hog_c_int_ing_0$integ != tmp_hog_c_int_ing_0$ing_ne), ]

# *** Asigno tipo de Hogar 2 a los hogares con Ingreso cero
# *** y a los que en la consulta anterior ( tmp_hog_c_int_ing_0 ) su numeros de integrantes con Ingreso NE
# *** sea distinto al numero total de integrantes del hogar ( almenos unos de los integrantes tiene ingreso cero)
ing_anual_x_hogar$tipo_ing <- ifelse(ing_anual_x_hogar$ing_total == 0 & ing_anual_x_hogar$tipo_ing != 3, 2, ing_anual_x_hogar$tipo_ing)
ing_anual_x_hogar$destiping <- ifelse(ing_anual_x_hogar$ing_total == 0 & ing_anual_x_hogar$tipo_ing != 3, "Con Ingreso cero", ing_anual_x_hogar$destiping)


work <- work[, names(work) %in% c("llaveho","factor", "p4a8_1", "p4a8_2", "c_ingtra", "v_ingtra", "p10_1_6", "p10_2_6_1", "p10_2_6_2", "c_ingrta", "v_ingrta", "p10_1_7", "p10_2_7_1", "p10_2_7_2", "c_inginv", "v_inginv", "p10_1_1", "p10_2_1_1", "p10_2_1_2", "c_inggob", "v_inggob", "p10_1_2", "p10_2_2_1", "p10_2_2_2", "c_ingpen", "v_ingpen", "p10_1_3", "p10_2_3_1", "p10_2_3_2", "c_ingfpai", "v_ingfpai", "p10_1_4", "p10_2_4_1", "p10_2_4_2", "c_ingfext", "v_ingfext", "p10_1_5", "p10_2_5_1", "p10_2_5_2", "c_ingalq", "v_ingalq", "p10_1_8", "p10_2_8_1", "p10_2_8_2", "c_ingemp", "v_ingemp", "p10_1_9", "p10_2_9_1", "p10_2_9_2", "c_ingneg", "v_ingneg", "p10_1_10", "p10_2_10_1", "p10_2_10_2", "c_ingbeca", "v_ingbeca", "p10_3", "p10_4_1", "p10_3c", "p10_4_2", "c_ing2t", "v_ing2t", "c_ingotin", "v_ingotin") ] # Eliminar columnas
xs<-data.frame(cbind(work$llaveho, work$factor, work$p4a8_1, work$p4a8_2, work$c_ingtra, work$v_ingtra, work$p10_1_6, work$p10_2_6_1, work$p10_2_6_2, work$c_ingrta, work$v_ingrta, work$p10_1_7, work$p10_2_7_1, work$p10_2_7_2, work$c_inginv, work$v_inginv, work$p10_1_1, work$p10_2_1_1, work$p10_2_1_2, work$c_inggob, work$v_inggob, work$p10_1_2, work$p10_2_2_1, work$p10_2_2_2, work$c_ingpen, work$v_ingpen, work$p10_1_3, work$p10_2_3_1, work$p10_2_3_2, work$c_ingfpai, work$v_ingfpai, work$p10_1_4, work$p10_2_4_1, work$p10_2_4_2, work$c_ingfext, work$v_ingfext, work$p10_1_5, work$p10_2_5_1, work$p10_2_5_2, work$c_ingalq, work$v_ingalq, work$p10_1_8, work$p10_2_8_1, work$p10_2_8_2, work$c_ingemp, work$v_ingemp, work$p10_1_9, work$p10_2_9_1, work$p10_2_9_2, work$c_ingneg, work$v_ingneg, work$p10_1_10, work$p10_2_10_1, work$p10_2_10_2, work$c_ingbeca, work$v_ingbeca, work$p10_3, work$p10_4_1, work$p10_3c, work$p10_4_2, work$c_ing2t, work$v_ing2t, work$c_ingotin, work$v_ingotin))

names(xs)<-c("llaveho","factor", "p4a8_1", "p4a8_2", "c_ingtra", "v_ingtra", "p10_1_6", "p10_2_6_1", "p10_2_6_2", "c_ingrta", "v_ingrta", "p10_1_7", "p10_2_7_1", "p10_2_7_2", "c_inginv", "v_inginv", "p10_1_1", "p10_2_1_1", "p10_2_1_2", "c_inggob", "v_inggob", "p10_1_2", "p10_2_2_1", "p10_2_2_2", "c_ingpen", "v_ingpen", "p10_1_3", "p10_2_3_1", "p10_2_3_2", "c_ingfpai", "v_ingfpai", "p10_1_4", "p10_2_4_1", "p10_2_4_2", "c_ingfext", "v_ingfext", "p10_1_5", "p10_2_5_1", "p10_2_5_2", "c_ingalq", "v_ingalq", "p10_1_8", "p10_2_8_1", "p10_2_8_2", "c_ingemp", "v_ingemp", "p10_1_9", "p10_2_9_1", "p10_2_9_2", "c_ingneg", "v_ingneg", "p10_1_10", "p10_2_10_1", "p10_2_10_2", "c_ingbeca", "v_ingbeca", "p10_3", "p10_4_1", "p10_3c", "p10_4_2", "c_ing2t", "v_ing2t", "c_ingotin", "v_ingotin")
 
write.csv(ing_anual_x_hogar, file = "ingresoanual.csv", row.names = FALSE, col.names = TRUE)

dataset1 <- ing_anual_x_hogar[, c("llaveho", "ing_total")]

head(dataset1)

dataset1$label <- 0
dataset1$label  <- incomelabel_array
head(dataset1)


# Aggregate function to get count and mean
result <- aggregate(ing_total ~ label, data = dataset1, FUN = function(x) c(count = length(x), mean = mean(x)))
head(result)

colnames(result) <- c( "RangoIngresos", "X")

head(result)

class(result)
head(income_counts_df)
# dataset2 <- result[, c("RangoIngresos", "X")]
dataset2 <- result[, c("RangoIngresos", 2)]

head(dataset2)

colnames(result)


# Define income range boundaries based on representative values
income_ranges <- cut(ing_anual_x_hogar$ing_total, 
                     breaks = c(0, 240000, 288000, 480000, 520000, 900000, max(ing_anual_x_hogar$ing_total)),
                     labels = c("Bajo-bajo", "Bajo-alto", "Medio-bajo", "Medio-alto", "Alto-bajo", "Alto-alto"),
                     include.lowest = TRUE)

income_ranges

# Assuming 'income_ranges' is your factor variable
incomelabel_array <- as.array(as.character(income_ranges))
nrow(incomelabel_array)





# Create a table of counts by income range
income_counts <- table(income_ranges)
income_counts

# Convert the table to a data frame for better readability
income_counts_df <- as.data.frame(income_counts)
income_counts_df

# Rename the columns for clarity
colnames(income_counts_df) <- c("RangoIngresos", "Frecuen")

# Print the result
print(income_counts_df)

#CREATE CSV FILE

write.csv(income_counts_df, file = "IngAnualFrec.csv", row.names = FALSE, col.names = TRUE)

head(income_counts_df)


# Create a barplot of income range counts
barplot(income_counts_df$Frecuen, names.arg = income_counts_df$RangoIngresos, 
        main = "Distribucion de ingresos", xlab = "Categorias de ingresos", ylab = "Frecuen.",
        col = "lightblue", border = "black")


nrow(income_ranges)

nrow(ing_anual_x_hogar)


# 2 - Frecuencia de tipos de ingresos diferentes al de trabajo

# Definimos las columnas seleccionadas
selected_columns <- c("c_ingxrnta", "c_ingxinv", "c_otrosing")
# Nuevos nombres de las columnas
new_column_names <- c("Renta", "Inversiones", "Otros Ingresos")

# Subset the original data frame to select the columns
selected_data <- ing_anual_x_hogar[selected_columns]

# Rename the selected columns
colnames(selected_data) <- new_column_names

# Calculate the frequency of '1's in selected columns
income_frequency <- colSums(selected_data[,])

# Print the result
class(income_frequency)
length(income_frequency)

#Turn it into dataframe
income_df <- data.frame(
  "TiposDeIngresos" = names(income_frequency),
  "Frecuencia" = as.numeric(income_frequency)
  )

income_df

#SAVE THE DATA INTO A CSV
write.csv(income_df, file = "FrecuIngresos.csv", row.names = FALSE)

# Create a pie chart
pie(income_frequency, labels = names(income_frequency), main = "Ingresos diferentes al laboral")

# -------------SCALLED-----------

# Create a pie chart with percentages as labels
pie(income_frequency, labels = paste0(round(100 * income_frequency / sum(income_frequency), 1), "%"), 
    main = "Distribution of Income Types")





