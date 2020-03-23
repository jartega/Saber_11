# -*- coding: utf-8 -*-
"""ModeloSaber.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1D2weLtaQAHYs4sBWMM3uf72PxFwIFbgY
"""

# Commented out IPython magic to ensure Python compatibility.
import sklearn.metrics
import seaborn as sb
import numpy as np
from operator import add

from sklearn.metrics import confusion_matrix
from sklearn.metrics import classification_report
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.decomposition import PCA
from sklearn.tree import DecisionTreeClassifier
from sklearn.cluster import KMeans
from sklearn.metrics import pairwise_distances_argmin_min
from pylab import rcParams
from sklearn.cross_validation import train_test_split

from imblearn.under_sampling import NearMiss
from imblearn.over_sampling import RandomOverSampler
from imblearn.combine import SMOTETomek
from imblearn.ensemble import BalancedBaggingClassifier

from collections import Counter
import pandas as pd
import matplotlib.pyplot as plt
# %matplotlib inline

# Seaborn for visualization
import seaborn as sns
sns.set(font_scale = 2)

# Imputing missing values and scaling values
from sklearn.preprocessing import Imputer, MinMaxScaler

# Machine Learning Models
from sklearn.linear_model import LinearRegression
from sklearn.ensemble import RandomForestRegressor, GradientBoostingRegressor
from sklearn.svm import SVR
from sklearn.neighbors import KNeighborsRegressor

# Hyperparameter tuning
from sklearn.model_selection import RandomizedSearchCV, GridSearchCV

"""#Importación de datos"""

from google.colab import files
files.upload()
file_name1 ='Saber_11__2019-1.csv'
data = pd.read_csv(file_name1)

# Revisar los primeras 5 lineas de los datos cargados 
data.head(5)

#Eliminamos los datos con valores missing porque Python no puede hacer árboles con datos missing
data_clean = data.dropna()

data_clean.describe()

data_clean.DESEMP_INGLES

data_clean.boxplot(by='DESEMP_INGLES', 
                       column=['PUNT_INGLES'], 
                       grid=False)

import pandas as pd
df=pd.get_dummies(data_clean, columns=['ESTU_DEDICACIONLECTURADIARIA','ESTU_DEDICACIONINTERNET','COLE_CALENDARIO','COLE_BILINGUE','COLE_AREA_UBICACION','DESEMP_INGLES'])

"""Identifico las variables dummy nuevas"""

df.describe()

## seleccion de cvariables
dfinal = pd.DataFrame(df, columns = ['COLE_CALENDARIO_A','COLE_AREA_UBICACION_RURAL','COLE_BILINGUE_S','ESTU_DEDICACIONINTERNET_-',
'ESTU_DEDICACIONINTERNET_30 minutos o menos',
'ESTU_DEDICACIONINTERNET_Entre 1 y 3 horas',
'ESTU_DEDICACIONINTERNET_Entre 30 y 60 minutos',
'ESTU_DEDICACIONINTERNET_Más de 3 horas',
'ESTU_DEDICACIONINTERNET_No Navega Internet',
'ESTU_DEDICACIONLECTURADIARIA_-',
'ESTU_DEDICACIONLECTURADIARIA_30 minutos o menos',
'ESTU_DEDICACIONLECTURADIARIA_Entre 1 y 2 horas',
'ESTU_DEDICACIONLECTURADIARIA_Entre 30 y 60 minutos',
'ESTU_DEDICACIONLECTURADIARIA_Más de 2 horas',
'ESTU_DEDICACIONLECTURADIARIA_No leo por entretenimiento'])

dfinal

### Seleccionar datos del Dataframe para analisis individual

x1 = data['PUNT_INGLES']  #Seleccionar una columna
x2 = data['DESEMP_INGLES']  #Seleccionar una columna
x3 = data['PERCENTIL_INGLES']  #Seleccionar una columna
x4 = data['COLE_CALENDARIO']
x5 = df['COLE_CALENDARIO_A']
x6 = df['COLE_AREA_UBICACION_RURAL']
x7 = df['COLE_BILINGUE_S']
x8 = df['DESEMP_INGLES_B1']
x9 = df['DESEMP_INGLES_B+']
x10 = df['ESTU_DEDICACIONINTERNET_-']
x11 = df['ESTU_DEDICACIONINTERNET_30 minutos o menos']
x12 = df['ESTU_DEDICACIONINTERNET_Entre 1 y 3 horas']
x13 = df['ESTU_DEDICACIONINTERNET_Entre 30 y 60 minutos']
x14 = df['ESTU_DEDICACIONINTERNET_Más de 3 horas']
x15 = df['ESTU_DEDICACIONINTERNET_No Navega Internet']
x16 = df['ESTU_DEDICACIONLECTURADIARIA_-']
x17 = df['ESTU_DEDICACIONLECTURADIARIA_30 minutos o menos']
x18 = df['ESTU_DEDICACIONLECTURADIARIA_Entre 1 y 2 horas']
x19 = df['ESTU_DEDICACIONLECTURADIARIA_Entre 30 y 60 minutos']
x20 = df['ESTU_DEDICACIONLECTURADIARIA_Más de 2 horas']
x21 = df['ESTU_DEDICACIONLECTURADIARIA_No leo por entretenimiento']

x21

#funcion para sumar 2 listas
a = x8
b = x9

c = tuple(map(lambda x, y: x + y, a, b))
NivelAlto=list(c)
print(NivelAlto)
### adicionar una lista a un dataframe
target = pd.DataFrame(NivelAlto)

# Al seleccionar la lista de listas se genera las variables como filas y los valores son columnas

subset =[df['COLE_CALENDARIO_A'],df['COLE_AREA_UBICACION_RURAL'], df['COLE_BILINGUE_S'], df['DESEMP_INGLES_B1'], df['DESEMP_INGLES_B+'],df['ESTU_DEDICACIONINTERNET_-'],
df['ESTU_DEDICACIONINTERNET_30 minutos o menos'],
df['ESTU_DEDICACIONINTERNET_Entre 1 y 3 horas'],
df['ESTU_DEDICACIONINTERNET_Entre 30 y 60 minutos'],
df['ESTU_DEDICACIONINTERNET_Más de 3 horas'],
df['ESTU_DEDICACIONINTERNET_No Navega Internet'],
df['ESTU_DEDICACIONLECTURADIARIA_-'],
df['ESTU_DEDICACIONLECTURADIARIA_30 minutos o menos'],
df['ESTU_DEDICACIONLECTURADIARIA_Entre 1 y 2 horas'],
df['ESTU_DEDICACIONLECTURADIARIA_Entre 30 y 60 minutos'],
df['ESTU_DEDICACIONLECTURADIARIA_Más de 2 horas'],
df['ESTU_DEDICACIONLECTURADIARIA_No leo por entretenimiento']]

subset

str(subset)

subset

subsetObj = pd.DataFrame(subset)

subsetObj.head()

## Calculo de coeficientes de pearson
features = pd.concat([dfinal, target], axis = 1)

test=pd.concat([pred_test,tar_test],axis=1)
features.describe()

#Creamos la muestra de entrenamiento y de test, tanto para predictores como para la variable objetivo, siendo test el 30%
from sklearn.model_selection import train_test_split
pred_train, pred_test, tar_train, tar_test = train_test_split(dfinal, target, test_size = 0.3, random_state = 123)

from mlxtend.frequent_patterns import apriori

frequent_itemsets = apriori(features > 0, min_support=0.06, use_colnames=True)
frequent_itemsets.head()

from mlxtend.frequent_patterns import association_rules
rules = association_rules(frequent_itemsets, metric="confidence", min_threshold=0.8)
rules.head()

from mlxtend.frequent_patterns import apriori

frequent_itemsets = apriori(test > 0, min_support=0.06, use_colnames=True)
frequent_itemsets.head()

from mlxtend.frequent_patterns import association_rules
rules = association_rules(frequent_itemsets, metric="confidence", min_threshold=0.8)
rules.head()

rules = association_rules(frequent_itemsets, metric="lift", min_threshold=1)
rules.head()

## CLASIFICADOR KNN pred_train, tar_train ##
from sklearn.neighbors import KNeighborsClassifier
knn=KNeighborsClassifier(n_neighbors=1)
tau=0.9
knn.fit(pred_train, tar_train)

tree=DecisionTreeClassifier(random_state=0)
tree.fit(pred_train, tar_train)
print('Precisión en el conjunto de entrenamiento:{:.3f}'.format(tree.score(pred_train,tar_train)))
print('Precisión en el conjunto de prueba:{:-3f}'.format(tree.score(pred_test,tar_test)))

frequent_itemsets = apriori(features > 0, min_support=0.06, use_colnames=True)
frequent_itemsets.head()

# Función para calcular el error absoluto medio otra medida de desempeno de los modelos
def mae(y_true, y_pred):
    return np.mean(abs(y_true - y_pred))

baseline_guess = np.median(target)

print('The baseline guess is a score of %0.2f' % baseline_guess)
print("Baseline Performance on the test set: MAE = %0.4f" % mae(tar_test, baseline_guess))

!pip install pydotplus

### Prueba de como graficar clusters

from sklearn.datasets.samples_generator import make_blobs

pred_train, tar_train = make_blobs(n_samples=50,
                  centers=3,
                  random_state=0,
                  cluster_std=0.60)

feature_names = ['pred_train', 'tar_train']
import matplotlib.pyplot as plt

plt.scatter(X[:, 0], X[:, 1], c=y)
plt.xlabel(feature_names[0])
plt.ylabel(feature_names[1])
plt.show()

from sklearn.tree import DecisionTreeClassifier

tree = DecisionTreeClassifier(random_state=0).fit(pred_train, tar_train)
from sklearn.tree import export_graphviz
from pydotplus import graph_from_dot_data

dot_data = export_graphviz(tree,
                           feature_names=feature_names)

graph = graph_from_dot_data(dot_data)
graph.write_png('tree.png')

from sklearn.ensemble import RandomForestClassifier
from sklearn.datasets import make_classification


forest=RandomForestClassifier(n_estimators=400, max_depth=6, random_state=0)

forest.fit(pred_train,tar_train)

print('Precisión en el conjunto de entrenamiento:{:.3f}'.format(forest.score(pred_train,tar_train)))

print('Precisión en el conjunto de prueba:{:-3f}'.format(forest.score(pred_test,tar_test)))

"""Entrenamiento de modelos"""

# Machine Learning Models
from sklearn.linear_model import LinearRegression
from sklearn.ensemble import RandomForestRegressor, GradientBoostingRegressor
from sklearn.svm import SVR
from sklearn.neighbors import KNeighborsRegressor

# Function to calculate mean absolute error
def mae(y_true, y_pred):
    return np.mean(abs(y_true - y_pred))

# Takes in a model, trains the model, and evaluates the model on the test set
def fit_and_evaluate(model):
    
    # Train the model
    model.fit(dfinal, target)
    
    # Make predictions and evalute
    model_pred = model.predict(pred_test)
    model_mae = mae(tar_test, model_pred)
    
    # Return the performance metric
    return model_mae


lr = LinearRegression()
lr_mae = fit_and_evaluate(lr)

print('Linear Regression Performance on the test set: MAE = %0.4f' % lr_mae)

# Function to calculate mean absolute error
def mae(y_true, y_pred):
    return np.mean(abs(y_true - y_pred))

# Takes in a model, trains the model, and evaluates the model on the test set
def fit_and_evaluate(model):
    
    # Train the model
    model.fit(dfinal, target)
    
    # Make predictions and evalute
    model_pred = model.predict(pred_test)
    model_mae = mae(tar_test, model_pred)
    
    # Return the performance metric
    return model_mae

random_forest = RandomForestRegressor(random_state=60)
random_forest_mae = fit_and_evaluate(random_forest)

print('Random Forest Regression Performance on the test set: MAE = %0.4f' % random_forest_mae)

gradient_boosted = GradientBoostingRegressor(random_state=60)
gradient_boosted_mae = fit_and_evaluate(gradient_boosted)

print('Gradient Boosted Regression Performance on the test set: MAE = %0.4f' % gradient_boosted_mae)

knn = KNeighborsRegressor(n_neighbors=10)
knn_mae = fit_and_evaluate(knn)

print('K-Nearest Neighbors Regression Performance on the test set: MAE = %0.4f' % knn_mae)