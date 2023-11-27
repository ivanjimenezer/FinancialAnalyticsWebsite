import pandas as pd 
import pymysql
from sqlalchemy import create_engine
# Archivo en el mismo directorio que contiene la información de conexion al la DB
from keys import *

#  connect to mysql
#  HOST,USER,PASS, etc son variables que estan dentro del archivo keys.py 
# y son importadas para mantener en secreto los datos al subirlos a git

# objeto para la conexion a la DB
conn = pymysql.connect(
    host=HOST,
    user=USER,
    password=PASS,
    database= DB,
    port=3306,
    charset='utf8'
)

engine = create_engine(f"mysql+pymysql://{USER}:{PASS}@{HOST}:{3306}/{DB}?charset=utf8")

#Creacion de datasets desde archivos csv

# create DATASETS
df1 = pd.read_csv("../Cleaning/TipoDeudaPro.csv", encoding='utf-8')
print("Proporciones de tipo de deuda")
print(df1.head())

    # CREATE TABLE TipoDeuda (
    #     id INT AUTO_INCREMENT PRIMARY KEY,
    #     Deuda VARCHAR(50) NOT NULL,
    #     Porcentaje FLOAT NOT NULL
    # );

print("---------------------------------------------")
# FRECUENCIA DE GRUPOS DE INGRESOS ANUALES - Grafico de barras
df2 = pd.read_csv("../Cleaning/IngAnualFrec.csv", encoding='utf-8')
print("Ingreso ANUAL por grupos")
print(df2.head())

# CREATE TABLE IngreAnualDist (
#     id INT AUTO_INCREMENT PRIMARY KEY,
#     RangoIngreso VARCHAR(25),
#     Frecuencia INT,
#     Promedio VARCHAR(25)
#     );

print("---------------------------------------------")
# Frecuencia de Ingresos diferentes al salario - GRAFICO DE PASTEL
df3 = pd.read_csv("../Cleaning/FrecuIngresos.csv", encoding='utf-8')
print("IngresoS")
print(df3.head())

# CREATE TABLE OtrosIngresos(
#     id INT AUTO_INCREMENT PRIMARY KEY,
#     IngresosTipos VARCHAR(30),
#     Frecuencia INT
#     );

print("---------------------------------------------")

# RELACION ENTRE INGRESO Y DEUDA - DIAGRAMA DE DISPERSION
df4 = pd.read_csv("../Cleaning/rel_ing_deu.csv", encoding='utf-8')
print("Relacion Ingreso Deuda")
print(df4.head())

# CREATE TABLE RelaIngreDeuda(
#     id INT AUTO_INCREMENT PRIMARY KEY,
#     DeudaTotal float,
#     IngresoAnual float
#     );

print("---------------------------------------------")
#Distribución de riqueza - GRAFICA DE BARRAS
df5 = pd.read_csv("../Cleaning/riqueza_distv2.csv", encoding='utf-8')
print("Distribucion de riqueza")
print(df5.head())

# CREATE TABLE DistRiqueza(
#     id INT AUTO_INCREMENT PRIMARY KEY,
#     Rangos VARCHAR(35),
#     Frecuencia INT
#     );
print("---------------------------------------------")

# Insercion de datos a la BD desde el dataset
# INSERT THE DATA INTO THE MYSQL TABLE

# TIPO DE DEUDA INTO 
# Assuming 'Deuda' and 'Porcentaje' are the columns in your DataFrame 'df1'
# Assuming 'df1' is your DataFrame
df1.rename(columns={'TipoDeuda': 'Deuda',"%":"Porcentaje"}, inplace=True)  # Renaming 'Deuda' to match 'Tipo Deuda' in MySQL
df1.to_sql('TipoDeuda', con=engine, if_exists='append', index=False)
 
 
#INGRESO ANUAL POR GRUPOS
df2.rename(columns={'RangoIngresos': 'RangoIngreso',"Frecuen":"Frecuencia"}, inplace=True)  # Renaming 'Deuda' to match 'Tipo Deuda' in MySQL
# print(df2)
df2.to_sql('IngreAnualDist', con=engine, if_exists='append', index=False)
 

# Ingresos diferentes al salario
df3.rename(columns={'TiposDeIngresos': 'IngresosTipos',"Frecuen":"Frecuencia"}, inplace=True)  # Renaming 'Deuda' to match 'Tipo Deuda' in MySQL
df3.to_sql('OtrosIngresos', con=engine, if_exists='append', index=False)

# Relacion ENTRE DEUDA E INGRESO
df4.to_sql('RelaIngreDeuda', con=engine, if_exists='append', index=False)

# Distribucion de riqueza - DistRiqueza
df5.rename(columns={'RangoRiqueza': 'Rangos',"Frecuen":"Frecuencia"}, inplace=True)  # Renaming 'Deuda' to match 'Tipo Deuda' in MySQL
print(df5)
df5.to_sql('DistRiqueza', con=engine, if_exists='append', index=False)


 
 
 
 
 
 
 
 
 
 
 
 
 

