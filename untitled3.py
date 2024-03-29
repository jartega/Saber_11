# -*- coding: utf-8 -*-
"""Untitled3.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1D2weLtaQAHYs4sBWMM3uf72PxFwIFbgY
"""



import pandas as pd
import matplotlib.pyplot as plt
#Importación de datos

from google.colab import files
files.upload()
file_name1 ='Saber_11__2019-1.csv'
data = pd.read_csv(file_name1)

# Revisar los primeras 5 lineas de los datos cargados 
data.head(5)

### Seleccionar datos del Dataframe

x1 = data['PUNT_INGLES']  #Seleccionar una columna
x2 = data['DESEMP_INGLES']  #Seleccionar una columna
x3 = data['PERCENTIL_INGLES']  #Seleccionar una columna
x4 = data['COLE_CALENDARIO']
x={'PUNT_INGLES','DESEMP_INGLES','PERCENTIL_INGLES','COLE_CALENDARIO'}
df = pd.DataFrame (x)
plt.hist(x1, bins = 10)

###
from sklearn.cross_validation import train_test_split
from sklearn.tree import DecisionTreeClassifier
from sklearn.metrics import classification_report
import sklearn.metrics
import seaborn as sb
from sklearn.cluster import KMeans
from sklearn.metrics import pairwise_distances_argmin_min

#Eliminamos los datos con valores missing porque Python no puede hacer árboles con datos missing
data_clean = data.dropna()

data_clean.describe()

data_clean.DESEMP_INGLES

data_clean.replace('Más de 30 horas',31)
data_clean.replace('Entre 1 y 3 horas',2)
data_clean.replace('30 minutos o menos',0.5)
data_clean.replace('Más de 3 horas',3)
data_clean.replace('Entre 30 y 60 minutos',1)

data_clean.ESTU_DEDICACIONINTERNET

#Indicamos las variables predictoras y debajo la variable objetivo. Cada uno con los nombres de las variables que tenéis en el fichero csv.
predictors = data_clean[['ESTU_DEDICACIONLECTURADIARIA', 'ESTU_DEDICACIONINTERNET','COLE_CALENDARIO','COLE_BILINGUE','COLE_AREA_UBICACION']]
targets = data_clean.PUNT_INGLES

predictors.head()

import numpy as np
predictors.describe(include=np.object)

Set1=pd.concat([predictors,targets])

data_clean.boxplot(by='DESEMP_INGLES', 
                       column=['PUNT_INGLES'], 
                       grid=False)

#Creamos la muestra de entrenamiento y de test, tanto para predictores como para la variable objetivo, siendo test el 30%
from sklearn.model_selection import train_test_split
pred_train, pred_test, tar_train, tar_test = train_test_split(predictors, targets, test_size = 0.3, random_state = 123)

#Comprobamos el tamaño de las diferentes muestras (pred=predictora; tar=target, objetivo). La salida en cada caso es una pareja de datos: el tamaño de la muestra y el número de variables
pred_train.shape
pred_test.shape
tar_train.shape
tar_test.shape

from sklearn.metrics import confusion_matrix
from sklearn.metrics import classification_report
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.decomposition import PCA
from sklearn.tree import DecisionTreeClassifier

from pylab import rcParams

from imblearn.under_sampling import NearMiss
from imblearn.over_sampling import RandomOverSampler
from imblearn.combine import SMOTETomek
from imblearn.ensemble import BalancedBaggingClassifier

from collections import Counter

# Commented out IPython magic to ensure Python compatibility.
#set up graphic style in this case I am using the color scheme from xkcd.com
rcParams['figure.figsize'] = 14, 8.7 # Golden Mean


# %matplotlib inline

import pandas as pd
df=pd.get_dummies(data_clean, columns=['ESTU_DEDICACIONLECTURADIARIA','ESTU_DEDICACIONINTERNET','COLE_CALENDARIO','COLE_BILINGUE','COLE_AREA_UBICACION'])

df.head(3)

from sklearn.cluster import KMeans

