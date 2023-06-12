install.packages("dplyr")
install.packages("modeest")
install.packages("magrittr")
install.packages("MASS")
install.packages("ggplot2")
install.packages("GGally")
install.packages("scales")
install.packages("lifecycle")
install.packages("plyr")
install.packages("moments")
install.packages("h2o")
install.packages("caret")
install.packages("reshape2")
install.packages(c("nycflights13", "gapminder", "Lahman"))
install.packages("gridExtra")
install.packages("ggbeeswarm")
install.packages("sos")
install.packages("knitr")
install.packages("rmarkdown")
install.packages("fmsb")
install.packages("ggparcoord")
install.packages("psych")
findFn("laply")
library(ggbeeswarm)
library(sos)
library(gridExtra)  
library(reshape2)
library(modeest)
library(magrittr)
library(MASS)
library(ggplot2)
library(GGally)
library(plyr)
library(moments)
library(knitr)
library(fmsb)
library(GGally)
library(dplyr)
library(ggplot2)
library(readxl)
library(psych)

knitr::spin("TP1.R")

recepcionistas <- read_excel("recepcionistas.xls")

#' EJERCICIO 1

#' (a) Calcule en promedio por juez de cada una de las aspirantes. ?Cu?l le parece que seleccionaria cada uno de ellos? ?Existe coincidencia?

promedio_juez1 <- rowMeans(recepcionistas[,2:4])#%>% cat(promedio_juez1,sep="\n")
promedio_juez2 <- rowMeans(recepcionistas[,5:7])#%>% cat(promedio_juez2,sep="\n") 

recepcionistas %>%
  group_by(candidatos)%>%
  summarise(PromJ1 = promedio_juez1,PromJ2 = promedio_juez2)


colnames(test) <- (c('recepcionista','feature','valor'))

 
#' El juez 1 seleccionaria a Mariana y el juez 2 a Maia. Hay coincidencia de promedios con Sabrina entre los dos jueces.

#' (b) Calcule el promedio de cada una de las aspirantes tomando en cuenta todos los rubros y ambos jueces.

promedio_ambos_jueces <- rowMeans(recepcionistas[,2:7])

recepcionistas %>%
  group_by(candidatos)%>%
  summarise(Promedioambos = promedio_ambos_jueces)


#' (c) Transformar las puntuaciones observadas de modo tal que cada una de las seis variables tenga media 0 y dispersi?n 1. ?Cual seria el objetivo de esta transformacion?

standar1 <- scale(recepcionistas[,2:7])
standar1

#' El objetivo de la transformacion es para que se pueda hacer una comparacion entre categorias.

#' (d) Transformar las puntuaciones de modo tal que cada candidata tenga para cada juez media 0 y dispersi?n 1. ?Cual ser?a el objetivo de esta transformaci?n?
  
standarjuez1 <- scale(promedio_juez1,center=TRUE,scale=TRUE)
standarjuez1

standarjuez2 <- scale(promedio_juez2,center=TRUE,scale=TRUE)
standarjuez2

standardall <- as.data.frame(standarjuez1,standarjuez2)
standardall

#' (e) Grafique los perfiles multivariados de cada una de las candidatas para ambas transformaciones. ?Que observa?

plot(standarjuez1)

plot(standarjuez2)

ggparcoord(recepcionistas, columns = (2:7)) 

#'EJERCICIO 2

internet <- read_excel("Internet2013.xls")

#' (a) Clasificar las variables de la base. Para las variables numericas construir un grafico de coordenadas paralelas.

str(internet)

internet_numericas <- internet[,c(2,3,5,7,8,9,10)]
internet_numericas

ggparcoord(internet_numericas, columns = c(2:7),groupColumn="Nacionalidad",showPoints=TRUE) 

#' (b) Construir la tabla de frecuencias de la variable sexo. ?Hay algun valor que llame la atencion? ?Que tipo de error considera que es?


internet$Sexo=factor(internet$Sexo,levels=c(0,1,2),labels=c("Indef","Masculino","Femenino"))

freq_sexo <- table(internet$Sexo)
freq_sexo

barplot(freq_sexo)

#'Llama la atencion que hay 1 valor como 0. Es un error de tipeo.

#' (c) Ordenar los datos por la variable Edad. ?Encontro algun valor extranio? ?Qu? tipo de error puede ser? Construir la tabla de frecuencias de la variable Sitio.
#' ?Encuentra algun valor que le llame la atencion? ?Qu? tipo de error puede ser?

edad_sort <- sort(internet$Edad)
edad_sort
barplot(edad_sort)

#'Se encontro una edad negativa. Es un error de tipeo.

freq_sitio <- table(internet$Sitio)
barplot(freq_sitio)

#' 8 y 28 no estan clasificados como tipos de sitio. Es un error de tipeo en los datos.

#' (d) Proceda de forma similar para las variables Temperatura, Autos y Cigarrillos.

freq_temp <- table(internet$Temperatura)
barplot(freq_temp)

freq_autos <- table(internet$Autos)
barplot(freq_autos)

freq_ciga <- table(internet$Cigarrillos)
barplot(freq_ciga)

#' (e) Elimine de la base los valores que no son posibles y que seguramente corresponde a un error de tipeo. 
#' Detalle valores/registros que le hayan llamado la atenci?n pero no deban ser eliminados necesariamente.

internet2 <- internet[!(internet$Sexo == "0" | internet$Edad < 0 | internet$Sitio == "8"| internet$Sitio == "28" | internet$Edad > 100),]

#' (f) ?Para cu?les de las variables tiene sentido calcular la media? ?Y la mediana?

#' Se deberia usar la media cuando los valores tienen una distribucion simetrica y la mediana cuando no lo son. 
#' Tambien se puede usar la mediana cuando hay outliers que distorsionan la media.
#' En nuestro caso tiene sentido calcular la media para: Temperatura, Autos
#' y la mediana para: Cigarillo, Edad

boxplot(internet2[,3:10])

#' (h) Calcular la desviacion intercuartil y detectar presencia de valores salvajes () moderados y severos.
iqr <-  sapply(internet2[,3:10],IQR)
iqr

#' EJERCICIO 3. Graficos univariados y multivariados (Datos: Gorriones.xls)

gorriones <- read_excel("gorriones.xlsx")

#' (a) Indicar en cada caso de que tipo de variable se trata.

str(gorriones)
 #' pajaro    : ordinal
 #' largototal: cuantitativa discreta
 #' extension : cuantitativa discreta
 #' cabeza    : cuantitativa continua
 #' humero    : cuantitativa continua
 #' esternon  : cuantitativa continua
 #' sobrevida : Categorica binaria o logica
 #' 
gorriones$sobrevida <- factor(gorriones$sobrevida,levels=c(-1,1),labels=c("Muerto","Vivo"))

#' (b) Confeccionar un informe para cada variable( univariado).

#' Media, Mediana, Cuartiles
summary(gorriones)

#' Desvio estandar
st_dev <- sapply(gorriones[,2:6],sd)
st_dev

#' Rango intercuartilico
iqr2 <-  sapply(gorriones[,2:6],IQR)
iqr2

#' Coeficiente de Variacion

cv_fun <- function(x) {
  sd(x)/mean(x)*100
}
cv <-  sapply(gorriones[,2:6],cv_fun)
cv

cv_l <- lapply(gorriones[,2:6],cv_fun)
cv_l

mad_gorriones <- sapply(gorriones[,2:6],mad)
mad_gorriones

skew <- sapply(gorriones[,2:6],skewness)
skew

kurt <- sapply(gorriones[,2:6],kurtosis)
kurt
#' (c) Realizar en el caso que corresponda un histograma. Ensayar el numero de intervalos que conviene en cada variable, 
#' indicar si utiliza algun criterio
#' Para el numero de intervalos tomamos la propuesta de Freedman y Diaconis en ggplot2 se tiene que calcular manual 
#' pero en la libreria normal se puede usar hist(,breaks="FD")

hist_normal <- hist(gorriones$largototal,breaks="FD")
hist_normal

bw <-function(x) {2 * IQR(x) / length(x)^(1/3)}
hist_1_ggplot <- ggplot() + geom_histogram(aes(x=gorriones$cabeza),binwidth=bw)
hist_1_ggplot

hist_2_ggplot <- ggplot() + geom_histogram(aes(gorriones$humero),binwidth=bw)
hist_2_ggplot
                                             
hist_3_ggplot <- ggplot() + geom_histogram(aes(gorriones$esternon),binwidth=bw)
hist_3_ggplot                                             
                                             
grid.arrange(hist_1_ggplot,hist_2_ggplot,hist_3_ggplot, ncol = 3)                                              
                                             
#' (d) Realizar un boxplot comparativo para cada una de estas variables particionando por el grupo definido por la supervivencia. ?Le parece que
#' alguna de estas variables est? relacionada con la supervivencia, es decir que toma valores muy distintos en ambos grupos?
#' Analizar en todos los casos la presencia de outliers.

box_1 <- boxplot(gorriones[,2:3],notch=TRUE)
box_2 <- boxplot(gorriones[,4:6],notch=TRUE,main="Box 2")
box_all <- boxplot(gorriones[,2:7],notch=TRUE)
box_surv <- boxplot(gorriones[,4:7],notch=TRUE)

gg1 <- ggplot(gorriones, aes(x=sobrevida,y=cabeza)) + geom_boxplot(fill="orange")
gg1

gg2 <- ggplot(gorriones, aes(x=sobrevida,y=humero)) + geom_boxplot(fill="blue")
gg2

gg3 <- ggplot(gorriones, aes(x=sobrevida,y=esternon)) + geom_boxplot(fill="red")
gg3

grid.arrange(gg1,gg2,gg3, ncol = 3)

#' 1. How do the median values compare? We can compare the vertical line in each box to determine which dataset has a higher median value.
#' En este caso se ve una leve diferencia en supervivencia si se compara esta variable con cabeza y esternon. Esto esta indicando que los gorriones 
#' con cabeza y esternon mas grande sobreviven menos.

#' 2. How does the dispersion compare? We can compare the length of each box 
#' (which represents the distance between Q1 and Q3 - the interquartile range) to determine which dataset is more spread out.
#' Para el boxplot que compara supervivencia con humero se ve que hay mas dispersion de los datos de los gorriones muertos que los vivos

#' 3. How does the skewness compare? The closer the vertical line is to Q1, 
#' the more positively skewed the dataset. The closer the vertical line is to Q3, the more negatively skewed the dataset.
#' Se puede ver que para cabeza los datos estan casi normalmente distribuidos. Para los de humero hay asimetria negativa y para los de esternon asimetria positiva.
 
#' 4. Are outliers present? In box plots, outliers are typically represented by tiny 
#' that extend beyond either whisker. An observation is defined to be an outlier if it meets one of the following criteria:
#' An observation is less than Q1 - 1.5*IQR
#' An observation is greater than Q3 + 1.5*IQR


#' Se visualiza un outlier en el boxplot que compara supervivencia con humero

#' (e) Construir gr?ficos bivariados para las todas las variables, particionando por el grupo de supervivencia (un color para cada grupo).
#' ?Observa alguna regularidad que pueda explicar la supervivencia?


variables <- gorriones[,2:6]
pairplot <- pairs(gorriones[,2:6],col=gorriones$sobrevida)

#' (f) Construir la matriz de diagramas de dispersi?n. ?Considera que algun par de estas medidas est?n relacionadas? 
#' Estudiar si la asociaci?n de algunas de estas medidas es diferente en alguno de los grupos.

pairplot2 <- ggpairs(gorriones[,2:7],aes(color=sobrevida))
pairplot2

#' EJERCICIO 4 - Base de datos razaperros

perros <- read_excel("razaperros.xls")
perros

perros_df <- data.frame(perros)
perros_df


#' (a) Realizar un gr?fico de estrellas por raza (utilizando las variables tama?o, peso, velocidad, inteligencia y afectividad.
var <- perros_df[2:7]
razas <- perros_df[1]
razas

#' radarperros <- radarchart(var,cglty=1,cglcol="gray",plwd=2,plty=1)
#' legend(legend=razas)

#' (b) Idem por funcion.
#' (c) Idem por agresividad.
#' (d) En el primer grafico se observan estrellas similares. Le parece que las razas son parecidas?


#' EJERCICIO 5 - Matriz de covarianzas
gorriones <- read_excel("gorriones.xlsx")

#' (a) Dimension de la base de datos (n= número de observaciones, p = cantidad de variables observadas sobre cada individuo).
print(paste("El dataset contiene ", toString(dim(gorriones)[1]), " observaciones"))
print(paste("El dataset contiene ", toString(dim(gorriones)[2]-1), " variables"))

#' (b) Hallar el vector de medias, la matriz de varianzas y covarianzas y la matriz de correlaciones. ¿Que caracteristicas tienen estas matrices?
vec_medias <- sapply(gorriones[,2:7], mean)
matriz_var <- var(gorriones[,2:7])
matriz_cov <- cov(gorriones[,2:7])
matriz_cor <- round(cor(gorriones[,2:7]),2)

#' (c) Explicar que representa el elemento m11 de la matriz de varianzas y co-varianzas, idem para el elemento m31.
#' m11 es la covarianza entre largototal y largotoal
#' m31 es la covarianza entre cabeza y largototal

#' (d) Explicar que representa el elemento m22 de la matriz de correlaciones, ídem para el elemento m13.
#' m22 representa la correlacion entre extension sobre si misma
#' m13 representa la correlacion entre el largo total y la cabeza. Puede decirse que existe una correlacion positiva entre ambas variables

#' (e) Relacionar los elementos m21, m11 y m22 de la matriz de varianzas y covarianzas con el elemento m12 de la matriz de correlaciones.

#' (f) Hallar una nueva variable e incorporarla en la base de Gorriones: Diferencia entre el largo total y el largo del humero. 
#' Llamemosla: Diferencia de Largos.
gorriones2 <- gorriones %>%
  mutate (diferencia_de_largos = largototal-humero)

gorriones2

#' (g) Calcular nuevamente el vector de medias y las matrices de varianzas y covarianzas y la matriz de correlaciones de la nueva base de datos. 
#' Relacionar el nuevo vector de medias con el anterior.
vec_medias2 <-sapply(gorriones2[,2:7], mean)
vec_medias2
matriz_var2 <-var(gorriones2[,2:8])
matriz_var2
matriz_cov2 <-cov(gorriones2[,2:7])
matriz_cov2
matriz_cor2 <-round(cor(gorriones2[,2:8]),2)
matriz_cor2

#' (h) Hallar la traza de las cuatro matrices. Explicar el significado de cada uno de los resultados. 
#' ¿Que traza/s no aumentan al aumentar una variable? Explique.

traza_var2 <- tr(matriz_var2)
traza_var2
traza_cov2 <- tr(matriz_cov2)
traza_cov2
traza_cor2 <- tr(matriz_cor2)
traza_cor2

#' La traza de la matriz de correlaciones ya que los valores de la diagonal siempre van a valer 1 y justamente se usa esta diagonal para calcular la traza

#' EJERCICIO 6 - Propiedades de la matriz de Covarianzas
recepcionistas2 <- read_excel("recepcionistas.xls")

#' (a) Calcular el vector de medias e interpretar los valores.
sapply(recepcionistas2[,2:7], mean)

#' (b) Hallar las matrices de varianzas y covarianzas y de correlaciones para la submatriz de puntuaciones del primer juez,
#'  idem para el segundo juez. idem para el conjunto total.
#'  
var(recepcionistas2[,2:4])
round(cor(recepcionistas2[,2:4]),2)

var(recepcionistas2[,5:7])
round(cor(recepcionistas2[,5:7]),2)

var(recepcionistas2[,2:7])
round(cor(recepcionistas2[,2:7]),2)

#' (c) ¿Se puede decir que la suma de las dos primeras submatrices darán como resultado la matriz del grupo total?
#'  Si no es asi por favor explique por que no.

#' (d) ¿Se cumple esta relación para las trazas? y para el vector de medias? y para los vectores de medianas?
#' 
sapply(recepcionistas[,2:7], mean)
sapply(recepcionistas[,2:7], median)

#' Ejercicio 7 (Medidas de Posición y Escala robustas). (Datos: Internet.2013 )

internet3 <- read_excel("Internet2013.xls")

#' 1. Seleccione las variables numéricas del archivo y agregue 5 observaciones que no sean atipicas en forma univariada pero si lo sean en forma
#' multivariada. Utilice las medidas robustas para detectar estos valores.

#' 2. Ahora agregue cuatro observaciones que sean outliers pero aparezcan enmascaradas. Utilice estrategias robustas para detectar su presencia.

