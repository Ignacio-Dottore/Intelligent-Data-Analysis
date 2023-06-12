#Cargamos las librerías


library(readxl)#lectura
library(dplyr) #manipulacion de datos
library(kableExtra) #tablas
library(ggplot2) #graficos
library(tidyverse) #manipulacion de datos
library(ggpubr) #para juntar
library(ggforce) # grafico de violin y jitter
library(GGally) # ggpairs
library(corrplot) # para correlogramas
library(ggbeeswarm)
library(sos)
library(ggcorrplot)
library(gridExtra)  
library(reshape2)
library(modeest)
library(magrittr)
library(MASS)
library(plyr)
library(moments)
library(knitr)
library(fmsb)
library(psych)


# Ejercicio 1. Sea la matriz de varianzas y covarianzas poblacionales:
# Correspondiente al vector aleatorio X = (X1,X2,X3)′ de media 0.
# a) Hallar los autovalores y autovectores de la matriz de varianzas y covarianzas.

X <- matrix(c(3,1,1,1,3,1,1,1,5),nrow=3,byrow=T)
X

#Esta es una matriz de covarianza porque es cuadrada, la diagonal principal es positiva y es simetrica

kable(X) %>% kable_styling(bootstrap_options="striped",full_width=F,position="left")

# Autovalores y autovectores

autovalores <- eigen(X)$values
autovalores
autovectores <- eigen(X)$vectors
autovectores

kable(autovalores)
kable(autovectores) %>% kable_styling(bootstrap_options="striped",full_width=F,position="left")
# b) Escribir la expresión de las componentes principales Y = (Y1, Y2, Y3)′ e indique que proporción de la variabilidad explica cada una de ellas.


$Y_1 = -0.4082483 X_1 - 0.4082483 X_2 - 0.8164966 X_3$

$Y_2 = -0.5773503 X_1 - 0.5773503 X_2 + 0.5773503 X_3$

$Y_3 = 0.7071068  X_1 - 0.7071068 X_2 + 0.000000 X_3$


porc_explicado= 100*autovalores/sum(autovalores)
porc_explicado

kable(porc_explicado) %>% row_spec(1:3,bold = T,color="black") 

# c) Hallar los loadings de la primer componente principal.

df

pca <- prcomp(X, scale = TRUE,
              center = TRUE, retx = T)
pca

# d) Hallar los scores de las primeras dos componentes principales correspondientes a la observación X=(2,2,1).

# No la podemos estandarizar porque no tenemos toda la base de los datos 


      



# Ejercicio 2. Considerando los datos de la base chalets.xls, se pide:
#   a) Graficar el boxplot de cada una de las variables. Indicar, si se observa, la presencia de valores atípicos.
# b) Graficar los diagramas de dispersión de las variables de a pares. Estimar la
# presencia de correlación entre variables a partir de estos gráficos, indicando
# si le parece fuerte y el signo de las mismas.
# c) Calcular el vector de medias y la matriz de varianzas y covarianzas muestral.
# d) Hallar la matriz de correlación muestral. Verificar las estimaciones realizadas visualmente.
# e) A partir de estas observaciones, le parece razonable pensar en un análisis
# de componentes principales para reducir la dimensión del problema?.
# f) Hallar la primera componente principal y graficar sus coeficientes median-
#   te barras verticales.
# g) Indicar qué porcentaje de la variabilidad total logra explicar esta compo-
#   nente. Explicar si se trata de una componente de tamaño o de forma. Es
# posible ordenar las promotoras en función de esta componente?. Si la res-
#   puesta es afirmativa, cual es la mayor y cual la menor; si es negativa, expli-
#   car por qué no es posible ordenarlos


# Ejercicio 3. Dado el siguiente conjunto de datos:


# a) Calcule la matriz de covarianza, los autovalores y autovectores.
# b) Las componentes principales y su contribución porcentual a la varianza
# total.
# c) Grafique los datos en R22 en la base original yen la base de los dos primerosejes.
# d) Repita los cálculos con los datos estandarizados. Interprete los resultados
# obtenidos
# e) Verifique que los dos primeros autovectores son ortogonales entre sí. Represente gráficamente estos dos vectores en un gráfico bidimensional y trace
# rectas desde el origen hasta la ubicación de cada uno de los vectores en el gráfico.


# Ejercicio 4. Sea S la matriz de varianzas y covarianzas poblacionales:

# Correspondiente al vector aleatorio X = (X1,X2,X3)′ donde:
#   X1 puntuación media obtenida en las asignaturas de econometría
# X2 puntuación media obtenida en las asignaturas de derecho
# X3 puntuación media obtenida en asignaturas libres
# Los datos corresponden a un conjunto d alumnos de la carrera de econo-
#   mía.
# a) Calcule los autovalores de la matriz S.
# b) Interprete la segunda componente principal sabiendo que el autovector correspondiente: e1 = (0, 5744;−0, 5744; 0, 5744).
#   c) Como se debería interpretar si un estudiante tuviera segunda una puntuación en la componente principal muy inferior a la de sus compañeros?.
# d) ¿Cuántas componentes principales serán necesarias para explicar al menos el 80% de la variabilidad total del conjunto?


# Ejercicio 5. El siguiente conjunto de datos de la tabla 1 se refiere a 20 observaciones de suelo, donde se midió:
# x1: contenido de arena,
# x2: contenido de cieno,
# x3: contenido de arcilla,
# x4: contenido de materia orgánica
# x5: acidez, según PH.
# Compare los resultados del Análisis en Componentes Principales para la matriz de covarianza y para la matriz de correlación.
# a) Los porcentajes de variabilidad que logran explicar cada una de las componentes son los mismos?.
# b) Cambia el orden de las componentes?
# c) Cambian los loadings de las componentes?
# d) Cuál de los dos análisis le parece más adecuado y por qué?.


# Ejercicio 6. La tabla gorriones.xls contiene datos de 49 aves, 21 de los cuales sobrevivieron a una tormenta. Se pide:
# a) Estandarice las variables y calcule la matriz de covarianzas para las variables estandarizadas.
# b) Verifique que ésta es la matriz de correlación de las variables originales.
# c) Le parece adecuado en este caso un análisis de componentes principales.
# ¿Qué indica el autovalor para una componente principal?
# d) ¿Cuántas componentes son necesarias para explicar el 80% de la varianza total? 
# Realice el grafico de sedimentación, fundamente su respuesta coneste gráfico.
# e) ¿Cuál es la expresión de la primer componente principal?
# f) ¿Cómo queda expresada la primer componente principal? (en función del autovector correspondiente y de las variables).
# g) Encuentre las coordenadas del pájaro 11 en las nuevas componentes.
# h) Represente gráficamente en el plano. (Eje 1 vs 2, 1 vs 3, 2 vs 3). Interprete los tres primeros ejes.
# i) Realice un gráfico donde se observen los gorriones en los nuevos ejes 1 y 2, y resalte con distinto color el grupo de los que sobrevivieron.
# j) Utilice el Análisis en Componentes Principales como método para encontrar outliers.


# Ejercicio 7. Con el objetivo de oObtener índices útiles para la gestión hospitalaria
# basados en técnicas estadísticas multivariantes descriptivas se se recogió información
# del Hospital de Algeciras correspondiente a los ingresos hospitalarios del periodo 2007-
#   2008.
# Se estudiaron las variables habitualmente monitorizadas por el Servicio Andaluz de Salud, del Sistema Nacional de Salud Español:
# NI: número de ingresos
# MO: mortalidad
# RE: número de reingresos
# NE: número de consultas externas
# NE: número de consultas externas
# ES: número de estancias
# ES: número de estancias
# Las variables se midieron en un total de 22486 ingresos.
# En la siguiente tabla se aprecia la Distribución de los valores obtenidos en
# las variables listadas por los servicios del hospital de Algeciras( Andalucía,Espana):

# La idea central del ACP es conseguir la simplificación de un conjunto de datos, generalmente cuantitativos, 
# procedentes de un conjunto de variables interrelacionadas. Este objetivo se alcanza obteniendo, a partir de combinaciones lineales de
# las variables originalmente medidas, un nuevo conjunto de igual número de variables, no correlacionadas, 
# llamadas componentes principales (CP) en las cuales permanece la variabilidad presente en los datos originales, y 
# que al ordenarlas decrecientemente por su varianza, nos permiten explicar el fenómeno de estudio con las primeras CP. 
#Verificar que las primeras dos componentes principales son: 

# Y1 = 0,5380NI+0,5126ES+0,4081IF+0,2635MO−0,1561NE−0,2535RE−0,3511ICM 
# Y2 = 0,5524MO+0,4952RE+0,4696ICM+0,3756ES+0,2867NE+0,05778IF−0,04908NI
# a) Grafique las cargas y explicar la interpretación de las componentes principales.
# b) Qué porcentaje de variabilidad logra captar cada una de ellas?. Grafique el scree plot.
# c) Le parece adecuado considerar dos componentes principales?. 
# d) Hallar la correlación entre las nuevas variables y las originales. 
# e) Ordenar los servicios en función de su puntuación en cada una de las dos primeras componentes principales. 
# Indicar cuales son los servicios más demandados y los más complejos.
# f) Representar un biplot y buscar servicios similares, asociaciones entre las variables. 
# Verificar en este grafico la representación de las variables originales en las componentes.