
Este repositorio contiene 4 carpetas:

- document: contiene un archivo llamado "Taller 2- Documento Entrega" en el cual se detallan todos nuestros resultados del análisis planteado en el Problem Set 2.

- problem set2: documento PDF que contiene el el ejercicio requerido para el segundo taller.

- scripts: esta carpeta contiene 4 scripts utilizados para el desarrollo del Problem Set 2, así:

					 -- "1_Cleaning", en script realizamos el cargue de los datos, el alistamiento de las bases de datos para trabajar en adelante e incorporamos estadísticas      				 			descriptivas de la información que será analizada.
					 -- "2_Classification", en este script desarrollamos modelos para aproximarnos a la clasificación de los hogares como "Pobres" y "No Pobres", cada modelo tiene  								diferentes métodos de regularización y de balanceo de las muestras.
					 -- "3_Regression", en este script desarrollamos modelos para intentar predecir los ingresos de la población y con ello realizar una clasificación de los 									hogares como "Pobres" y "No Pobres", con diferentes métodos de regularización y balanceo de las muestras.
					 -- "4_Comparison", en este script realizamos las predicciones sobre los archivos "test_hogares.csv" y "test_personas.csv", a partir de los mejores modelos 								identificados en los pasos anteriores y generamos el archivo que sería cargado en la plataforma Kaggle, con nuestros intentos de clasificar correctamente 							los hogares como "Pobre" y "No Pobre".
					 
					 Es importante tener en cuenta que por la capacidad de almacenamiento de GitHub los archivos con los cuales se desarrolla el Problem Set 2 fueron descargados 					 de la página https://www.kaggle.com/competitions/uniandes-bdml-20231-ps2 y trabajados localmente, por lo que para ejecutar el script "1_Cleaning" debe 								 descagar estos archivos y fijar el directorio correctamente donde haya generado la descarga. A continuación se detallan los archivos que deben ser 										 descargados, conservando el mismo nombre::
					  - test_hogares.csv
					  - test_personas.csv
					  - train_hogares.csv
					  - train_personas.csv

-views: esta carpeta contiene las gráficas y tablas que obtuvimos al desarrollar el Problema Set 2.
