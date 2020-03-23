##Cargar paquetes
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readxl")
install.packages("gmodels")
install.packages("Hmisc")
install.packages("ggthemes")
#####
options(scipen = 999)
library(dplyr)
library(ggplot2) 
library(readxl)
library(gmodels)
library(Hmisc)
library(ggthemes)


#Importando datos de pruebas saber 11 de archivo csv

df <- read.csv("~/Diplomado Data Science/Modelo 7/PRAI 7/Saber_11__2019-1.csv")
View(df)

head(df, 10)
names (df)[1] = "ESTU_TIPODOCUMENTO"
dim(df)
### tipo de columnas
glimpse(df)

### Descripcion de las variables
summary(df)
### str(), head(), tail(), class()


###
class(df)

###Para convertir varias variables a factor 
##Las guardamos en un vector
cols<- c("PERIODO",
"COLE_CODIGO_ICFES",
"COLE_COD_DANE_ESTABLECIMIENTO",
"COLE_COD_DANE_SEDE",
"COLE_COD_MCPIO_UBICACION",
"COLE_COD_DEPTO_UBICACION",
"ESTU_COD_MCPIO_PRESENTACION",
"ESTU_COD_DEPTO_PRESENTACION",
"PERCENTIL_LECTURA_CRITICA",
"DESEMP_LECTURA_CRITICA",
"PERCENTIL_MATEMATICAS",
"DESEMP_MATEMATICAS",
"PERCENTIL_C_NATURALES",
"DESEMP_C_NATURALES",
"PERCENTIL_SOCIALES_CIUDADANAS",
"DESEMP_SOCIALES_CIUDADANAS",
"PERCENTIL_INGLES",
"PERCENTIL_GLOBAL",
"ESTU_INSE_INDIVIDUAL",
"ESTU_NSE_INDIVIDUAL",
"ESTU_NSE_ESTABLECIMIENTO")

#Le aplicamos la función a todas las columnas con lapply  y lo asigno al dataframe
df[cols] <- lapply(df[cols], factor)
### tipo de columnas
cols<- c("PERIODO",
         "COLE_CODIGO_ICFES",
         "COLE_COD_DANE_ESTABLECIMIENTO",
         "COLE_COD_DANE_SEDE",
         "COLE_COD_MCPIO_UBICACION",
         "COLE_COD_DEPTO_UBICACION",
         "ESTU_COD_MCPIO_PRESENTACION",
         "ESTU_COD_DEPTO_PRESENTACION",
         "PERCENTIL_LECTURA_CRITICA",
         "DESEMP_LECTURA_CRITICA",
         "PERCENTIL_MATEMATICAS",
         "DESEMP_MATEMATICAS",
         "PERCENTIL_C_NATURALES",
         "DESEMP_C_NATURALES",
         "PERCENTIL_SOCIALES_CIUDADANAS",
         "DESEMP_SOCIALES_CIUDADANAS",
         "PERCENTIL_INGLES",
         "PERCENTIL_GLOBAL",
         "ESTU_INSE_INDIVIDUAL",
         "ESTU_NSE_INDIVIDUAL",
         "ESTU_NSE_ESTABLECIMIENTO")

### Descripcion de las variables
summary(df[4])

### Calculo de edad
install.packages("lubridate")
vhoy <- Sys.Date()
vagniohoy <- format(vhoy, "%Y")
vagniohoy <- as.numeric(vagniohoy)
vhoy

df$fecha <- as.Date(df$ESTU_FECHANACIMIENTO, format="%d/%m/%Y")
class(df$fecha)
df$edad <- as.numeric((vhoy-df$fecha)/365)
class(df$edad)
describe(df$edad)
summary(df$edad)

### Validacion de edad vs tipo de documento
x<-as.numeric(df$edad)
summary(x)
outliers_edad <- boxplot(x)$out




















#### Funcion de outlier
# df es el dataFrame que recibimos (ej. activity)
# colNameData es la columna de los datos (ej. "steps")
# colNameBy es la columna por la que trocearemos (ej. "userId")
outliersReplace <- function(df, colNameData, colNameBy){
  # creamos una nueva columna llamada igual que colNameData pero con .R
  colNameData.R <- paste(colNameData, "R", sep=".")
  df[colNameData.R] <- df[colNameData]
  
  # obtenemos los IDs por los que partir el dataframe
  IDs <- unique(df[,c(colNameBy)])
  for (id in IDs){
    data <- df[df[colNameBy] == id, c(colNameData) ]
    
    Q  <- quantile(data)
    minimo <- Q[1]    # valor minimo
    Q1     <- Q[2]    # primer cuartil
    Me     <- Q[3]    # mediana
    Q3     <- Q[4]    # tercer cuartil
    maximo <- Q[5]    # valor maximo
    IQR    <- Q3 - Q1
    
    lowLimit  <- max(minimo, Q1 - 1.5*IQR)
    highLimit <- min(maximo, Q3 + 1.5*IQR)
    
    # todos los valores donde colNameBy es igual a id
    # y el valor de colNameData es > Q3 + 1.5 * IQR
    # lo reemplazamos por la mediana
    df[df[colNameBy] == id & df[colNameData] > highLimit, c(colNameData.R)] <- Me
    
    # lo mismo para el umbral inferior
    df[df[colNameBy] == id & df[colNameData] < lowLimit, c(colNameData.R)] <- Me
    
    cat(paste("El", colNameBy, id, "la mediana(", colNameData, ") ==", Me, "\n", sep=" " ))
    
  }
  df   # devolvemos el valor del dataFrame
}
##
df$edad <- outliersReplace(df,"edad","ESTU_CONSECUTIVO")

plot(df$ESTU_TIPODOCUMENTO,df$edad)

### Validacion de duplicidad en variable identificadoras del registro

nrow(df[duplicated(df), ])
df[duplicated(df), ]


### Validacion de nulos
nrow(df[is.na(df), ])


### Analisis de frecuencia de variables categoricas seleccionadas
predictoras<- c("ESTU_TIPODOCUMENTO",
"ESTU_GENERO",
"ESTU_CONSECUTIVO",
"ESTU_TIENEETNIA",
"ESTU_COD_RESIDE_MCPIO",
"FAMI_ESTRATOVIVIENDA",
"FAMI_EDUCACIONPADRE",
"FAMI_EDUCACIONMADRE",
"FAMI_TIENEINTERNET",
"FAMI_TIENESERVICIOTV",
"FAMI_TIENECOMPUTADOR",
"FAMI_TIENECONSOLAVIDEOJUEGOS",
"FAMI_NUMLIBROS",
"FAMI_SITUACIONECONOMICA",
"ESTU_DEDICACIONLECTURADIARIA",
"ESTU_DEDICACIONINTERNET",
"ESTU_HORASSEMANATRABAJA",
"COLE_CALENDARIO",
"COLE_BILINGUE",
"COLE_AREA_UBICACION",
"edad")

target<-("DESEMP_INGLES")

         
summary(df[cols])

### 
library(Hmisc)
boxplot(x=COLE_AREA_UBICACION,y=DESEMP_INGLES)
table(df$COLE_AREA_UBICACION, df$COLE_BILINGUE)
table(df$ESTU_TIPODOCUMENTO)


#### Seleccion de variables segun el calendario del colegion

muestra<-select(df,"ESTU_TIPODOCUMENTO",
       "ESTU_GENERO",
       "ESTU_CONSECUTIVO",
       "ESTU_TIENEETNIA",
       "ESTU_COD_RESIDE_MCPIO",
       "FAMI_ESTRATOVIVIENDA",
       "FAMI_EDUCACIONPADRE",
       "FAMI_EDUCACIONMADRE",
       "FAMI_TIENEINTERNET",
       "FAMI_TIENESERVICIOTV",
       "FAMI_TIENECOMPUTADOR",
       "FAMI_TIENECONSOLAVIDEOJUEGOS",
       "FAMI_NUMLIBROS",
       "FAMI_SITUACIONECONOMICA",
       "ESTU_DEDICACIONLECTURADIARIA",
       "ESTU_DEDICACIONINTERNET",
       "ESTU_HORASSEMANATRABAJA",
       "COLE_CALENDARIO",
       "COLE_BILINGUE",
       "COLE_AREA_UBICACION",
       "DESEMP_INGLES","PUNT_INGLES","edad")
class(muestra)
saber_A <- filter(muestra, COLE_CALENDARIO == 'A', COLE_BILINGUE == 'N')
saber_NO_A <- filter(muestra, COLE_CALENDARIO != 'A', COLE_BILINGUE == 'N')



#### Encontrar reglas por algoritmos de asociacion

install.packages("arules")
library(arules)

### Seleccion de columnas
seleccion_col <- select(saber_A, ESTU_CONSECUTIVO, ESTU_DEDICACIONINTERNET, DESEMP_INGLES)
head(seleccion_col)
### Numero de estudiantes por nivel
estudiantes_A_nivel <- saber_A %>%
  group_by(ESTU_DEDICACIONINTERNET,
           #ESTU_TIPODOCUMENTO,
           #ESTU_GENERO,
           #ESTU_TIENEETNIA,
           #ESTU_COD_RESIDE_MCPIO,
           #FAMI_ESTRATOVIVIENDA,
           #FAMI_EDUCACIONPADRE,
           #FAMI_EDUCACIONMADRE,
           #FAMI_TIENEINTERNET,
           #FAMI_TIENESERVICIOTV,
           #FAMI_TIENECOMPUTADOR,
           FAMI_TIENECONSOLAVIDEOJUEGOS,
           #FAMI_NUMLIBROS,
           #FAMI_SITUACIONECONOMICA,
           ESTU_DEDICACIONLECTURADIARIA,
           #ESTU_HORASSEMANATRABAJA,
           COLE_AREA_UBICACION,
           #edad,
           DESEMP_INGLES) %>%
  tally

## Relacion de Ancho vs Longitud por especie
install.packages("ggplot2")
install.packages("ggplot")
dataset<-estudiantes_A_nivel
ggplot(data=dataset,aes(x=dataset$ESTU_DEDICACIONLECTURADIARIA, y=dataset$ESTU_DEDICACIONINTERNET,color=dataset$DESEMP_INGLES)) + geom_point() + theme_minimal()

x<-dataset[,1:4]
y<-dataset[ ,5]


x### particion de datos para entrenamiento para los estudiantes de calendario A de colegio no bilibue
install.packages("caret")
library(caret)
set.seed(3456)
trainIndex <- createDataPartition(dataset$DESEMP_INGLES, p = .7, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

sample <- sample.int(n = nrow(dataset), size = floor(.7*nrow(dataset)), replace = F)
train <- dataset[sample, ]
test  <- dataset[-sample, ]

# Run algorithms using 10-fold cross validation
control<-trainControl(method = "cv", number = 10)
metric<-"Accuracy"


#Linear Algorithms: LDA

# Linear Discriminant Analysis (LDA)  
set.seed(7)
fit.lda <- train(dataset$n~., data=dataset, method="lda", metric=metric, trControl=control)

#Nonlinear Algorithms: CART and kNN

# Classification and Regression Trees (CART)
set.seed(7)
fit.cart <- train(dataset$n~., data=dataset, method="rpart", metric=metric, trControl=control)

# k-Nearest Neighbors (kNN)
set.seed(7)
fit.knn <- train(dataset$n~., data=dataset, method="knn", metric=metric, trControl=control)

#Advanced Algorithms: SVM and RF

# Support Vector Machines (SVM)
set.seed(7)
fit.svm <- train(dataset$n~., data=dataset, method="svmRadial", metric=metric, trControl=control)


# Random Forest (RF)
set.seed(7)
fit.rf <- train(y~., data=dataset, method="rf", metric=metric, trControl=control)
