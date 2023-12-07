# Moudulos y librerias a importar
import os
from flask import Flask, render_template, request, url_for, redirect,jsonify
import datetime
import mysql.connector
import joblib
import plotly.express as px
import numpy as np

x = datetime.datetime.now()

# We initialize the flask app with an instance using the  object app
app=Flask(__name__, static_folder='static', template_folder='templates')

#COnexcion a MySQL
conexion = mysql.connector.connect(
    host='localhost',
    user='root',
    password='duke_pass', #esta es mi contra jusjus
    database='anfin',
    port=3307, #Cambié el puerto jusjus
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


def ValuePredictor(to_predict_list):
    print("Parte 2")
    to_predict = np.array(to_predict_list).reshape(1, 12)
    loaded_model = joblib.load("static/ML/kmeans_model.joblib")  # ML Model loading
    result = loaded_model.predict(to_predict)
    return result[0]

@app.route('/result', methods=['POST'])
def result():
    if request.method == 'POST':
        print("Parte 1")
        # We convert the forms object into a Dictionary
        to_predict_list = request.form.to_dict()
        # We convert the dictionary into a list with only the values
        to_predict_list = list(to_predict_list.values())
        # Convert the strings into int's
        to_predict_list = list(map(int, to_predict_list))
        result = ValuePredictor(to_predict_list)
        if int(result) == 0:
            prediction = 'Grupo 0'
        elif int(result) == 1:
            prediction = 'Grupo 1'
        elif int(result) == 2:
            prediction = 'Grupo 2'
        elif int(result) == 3:
            prediction = 'Grupo 3'
        else:
            prediction = 'Grupo Desconocido'

        return jsonify({'resultado': prediction}) 


# RUTA PARA OBTENER LOS DATOS DE LAS TABLAS
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
        RelaIngreDeuda = get_table_data(cursor, 'RelaIngreDeuda')
        data['RelaIngreDeuda'] = RelaIngreDeuda
        # Quinta tabla - Tabla para histograma sobre la distribucion de riquezas 
        data['DistRiqueza'] = get_table_data(cursor, 'DistRiqueza')


        # Crear gráfico de barras con Plotly Express
        fig = px.bar(data['IngreAnualDist'], x='RangoIngreso', y='Frecuencia', title='Distribución de Ingresos Anuales')

        # Guardar el gráfico en formato HTML
        graph_html = fig.to_html(full_html=False)

        # Agregar el gráfico al diccionario de datos
        data['graph'] = graph_html


        #Mensaje de exito
        data['mensaje'] = 'EXITO'
    except Exception as ex:
        msn =  f'Error: {ex}'
        print(msn)
        data['mensaje'] = 'Error fetching data from the databasev2: ' + msn
    # SE RETORNA UN ARCHIVO JSON CON LOS DATOS
    #return jsonify(data)
    return render_template('Home/mexico.html', graph_html=data['graph'])

# Route for seeing the formulary that inserts data
@app.route('/')
def index():

    return render_template('Home/home.html')

@app.route('/admin')
def admin():

    return render_template('Home/admin.html')
    

@app.route('/mexico')
def mexico():

    return render_template('Home/mexico.html')

@app.route('/login')
def login():

    return render_template('Home/login.html')
    


# Running app
if __name__ == '__main__':
	app.run(debug=True, port=8080)
