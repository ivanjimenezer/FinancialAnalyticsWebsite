# Import flask and datetime module for showing date and time
import os
from flask import Flask, render_template, request, url_for, redirect,jsonify
import datetime
import mysql.connector

x = datetime.datetime.now()

# Initializing flask app
#instanciamos
app=Flask(__name__)

#COnexcion a MySQL
conexion = mysql.connector.connect(
    host='localhost',
    user='root',
    password='19175',
    database='anfin',
    port=3306,
    charset='utf8'
)

def get_table_data(cursor, table_name):
    try:
        sql = f"SELECT * FROM {table_name}"
        cursor.execute(sql)
        table_data = cursor.fetchall()
        return table_data
    except Exception as ex:
        print(f'Error fetching data for {table_name}: {ex}')
        return None

@app.route('/dataplot')
def dataplots():
    data={}
    tables_names = ['IngreAnualDist','TipoDeuda','OtrosIngresos','RelaIngreDeuda','DistRiqueza']
    try:
        cursor = conexion.cursor(dictionary=True)
        # Primera tabla - Tabla de distribución de ingresos
        data['IngreAnualDist'] = get_table_data(cursor,'IngreAnualDist')
        # Segunda tabla - Tabla de tipos de deuda 
        data['TipoDeuda'] = get_table_data(cursor, 'TipoDeuda')
        # Tercera tabla - 
        data['OtrosIngresos'] = get_table_data(cursor, 'OtrosIngresos')
        # Cuarta table - Tabla sobre la relacion entre deuda e ingresos para un scatterplot
        #RelaIngreDeuda = get_table_data(cursor, 'RelaIngreDeuda')
        #data['RelaIngreDeuda'] = RelaIngreDeuda
        # Quinta tabla - Tabla para histograma sobre la distribucion de riquezas 
        data['DistRiqueza'] = get_table_data(cursor, 'DistRiqueza')
        #Mensaje de exito
        data['mensaje'] = 'EXITO'
    except Exception as ex:
        msn =  f'Error: {ex}'
        print(msn)
        data['mensaje'] = 'Error fetching data from the databasev2: ' + msn
    return jsonify(data)


# Route for seeing a data
@app.route('/')
def index():

    return render_template('index.html')

	
# Running app
if __name__ == '__main__':
	app.run(debug=True, port=8080)
