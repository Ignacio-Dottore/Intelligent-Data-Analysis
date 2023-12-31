---
title: "TP1 AID Austral Virtual"
author: "Débora"
date: '2022-06-19'
output: html_document
---

```{r setup, warning=FALSE, cache=FALSE, message=FALSE,include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Ejercicio 1
Seis candidatas son evaluadas para el puesto de recepcionista en una empresa, para ello pasan por dos entrevistas. En la primera las evalúa el responsable de recursos humanos de la empresa (juez 1) y en la segunda el responsable del área de la cual van a depender (juez 2). La asignación de puntajes es en
cordialidad, presencia y manejo de idiomas. Los puntajes asignados independientemente por estos jueces se encuentran en el archivo recepcionistas.xls.

```{r}
#Cargamos las librerías
library(readxl)#lectur
library(dplyr) #manipulacion de datos
library(kableExtra) #tablas
library(ggplot2) #graficos
library(tidyverse) #manipulacion de datos
library(ggpubr) #para juntar
library(ggforce) # grafico de violin y jitter
library(GGally) # ggpairs
library(corrplot) # para correlogramas
```

-   (a) Calcule en promedio por juez de cada una de las aspirantes. ¿Cuál le parece que seleccionaría cada uno de ellos? ¿Existe coincidencia?

```{r}
#leemos los datos
recepcionistas <- read_excel("recepcionistas.xls")
recepcionistas

```
```{r}
#separamos  las puntuaciones por juez
juez1=recepcionistas[,2:4]
juez2=recepcionistas[,5:7]
```


```{r}
#calculamos los promedios por candidato para cada juez
prom_j1=apply(juez1,1,mean)
prom_j2=apply(juez2,1,mean)
#buscamos el máximo promedio por juez y vemos a quien corresponde
cand=recepcionistas%>%dplyr::select(candidatos)%>%unlist()
max_j1=cand[which.max(prom_j1)]
max_j1
max_j2=cand[which.max(prom_j2)]
max_j2
#coinciden los máximos?
```

-  (b) Calcule el promedio de cada una de las aspirantes tomando en cuenta
todos los rubros y ambos jueces.

```{r}
puntuaciones=select(recepcionistas,cord.juez1:idiom.juez2)
promxcand=data.frame("Cand"=cand,"Prom"=round(apply(puntuaciones,1,mean),2))
rownames(promxcand)<-NULL
promxcand
```

-  (c) Transformar las puntuaciones observadas de modo tal que cada una de
las seis variables tenga media 0 y dispersión 1. ¿Cuál sería el objetivo de
esta transformación?

```{r}
#estandarizamos por columna
punt_stand=data.frame(puntuaciones%>%scale())
punt_stand
#queremos hacer comparables las diferentes puntuaciones
```

-  (d) Transformar las puntuaciones de modo tal que cada candidata tenga para
cada juez media 0 y dispersión 1. ¿Cuál sería el objetivo de esta transformación?

```{r}
sal2=data.frame(cand,t(t(juez1)%>%scale()%>%round(2)))
rownames(sal2)<-NULL
sal2 %>%
  kbl(caption = "Puntuaciones Estandarizadas Juez 1") %>%
  kable_classic(full_width = F, html_font = "Cambria")

sal2

sal3=data.frame(cand,t(t(juez2)%>%scale()%>%round(2)))
rownames(sal3)<-NULL
sal3 %>%
  kbl(caption = "Puntuaciones Estandarizadas Juez 2") %>%
  kable_classic(full_width = F, html_font = "Cambria")

sal3
```

- (e) Grafique los perfiles multivariados de cada una de las candidatas para
ambas transformaciones. ¿Qué observa?
```{r}
#damos nueva forma a los datos del juez 1
dj1= gather(sal2, key="rubro", value="valor", cord.juez1:idiom.juez1)
dj1
RUBRO=as.factor(dj1$rubro)
Candi=as.factor(dj1$cand)
plot1=ggplot(dj1,aes(x=RUBRO,y=valor))+geom_line(aes(x=RUBRO,y=valor,group =Candi,color=Candi))+ labs(title = "juez 1") +
    theme(axis.text.x  = element_text(angle=90))
#damos nueva forma a los datos del juez 2
dj2= gather(sal3, key="rubro", value="valor", cord.juez2:idiom.juez2)
dj2
Rubro=as.factor(dj2$rubro)
Candi=as.factor(dj2$cand)
plot2=ggplot(dj2,aes(x=Rubro,y=valor))+geom_line(aes(x=Rubro,y=valor,group =Candi,color=Candi))+ labs(title = "juez 2")+theme(axis.text.x  = element_text(angle=90))
ggarrange(plot1,plot2)
```

# Ejercicio 3

## Gráficos univariados y multivariados

### (Datos: Gorriones.xls)

Base de datos: Se han registrado para 49 gorriones las siguientes variables
zoo-métricas:
 - a) Largo total
 - b) Extensión alar
 - c) Largo del pico y cabeza
 - d) Largo del húmero
 - e) Largo de la quilla del esternón
 - f) Sobrevida (1) Si, (-1) No


- a Indicar en cada caso de que tipo de variable se trata.

```{r}
# leemos la base de datos
gorriones <- read_excel("C:/Users/Debora/Dropbox/CristinaBrio/gorriones.xlsx")
gorriones
```
 Largototal, extension, cabeza, humero, esternon son variables continuas (mediciones)
 por su parte sobrevida es categórica (la vamos a considerar un factor).

 - b Confeccionar un informe para cada variable( univariado).

```{r}
gorriones%>%str() # nos muestra que hay en cada variable
gorriones%>%glimpse() # nos muestra como esta almacenada cada variable
gorriones %>%select(largototal:esternon) %>% summary() # nos hace un resumen para cada variable
sobrev=as.factor(gorriones$sobrevida)
```

 - c Realizar en el caso que corresponda un histograma. Ensayar el número
de intervalos que conviene en cada variable, indicar si utiliza algún
criterio.

```{r}
ggplot(gorriones) + 
  geom_histogram(bins = 10, aes(x = largototal, fill = factor(sobrevida)), color = 'black') + facet_grid(sobrevida~., scales = 'free') +
  xlab("largo total") + 
  ylab("Frecuencia") + 
  ggtitle("Distribución de la variable Largo Total para los distintos Sobrevida") +
  theme_minimal()
```
```{r}
ggplot(gorriones) + 
  geom_density( aes(x = cabeza, fill = factor(sobrevida)), color = 'black') + facet_grid(sobrevida~., scales = 'free') +
  xlab("cabeza") + xlim(c(30,35))+ 
  ylab("Frecuencia") + 
  ggtitle("Distribución de la variable Cabeza para los distintos Sobrevida") +
  theme_minimal()
```


(d) Realizar un boxplot comparativo para cada una de estas variables particionando
por el grupo definido por la supervivencia. ¿Le parece que
alguna de estas variables está relacionada con la supervivencia, es decir
que toma valores muy distintos en ambos grupos? Analizar en todos los
casos la presencia de outliers.

```{r}
ggplot(gorriones,aes(x=sobrev,y=largototal,fill=sobrev))+geom_boxplot(notch=TRUE)+
  scale_fill_manual(values=c("#FE2E2E","#819FF7")) +# cambiamos los colores
  xlab("Sobrevida")+ylab("Largo Total")
```
```{r}
gorriones %>% 
    ggplot(aes(x = sobrev, 
             y = cabeza)) +
  geom_violin(trim = FALSE, 
              aes(color = sobrev)) +
  stat_summary(fun.data = "mean_sdl",  
               fun.args = list(mult = 1), 
               geom = "pointrange", 
               color = "black") +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))
```
```{r}

gorriones %>% 
   ggplot(aes(x = sobrev, 
             y = humero)) +
  geom_violin(aes(fill = sobrev), trim = FALSE) + 
  geom_boxplot(width = 0.15)+
  scale_fill_manual(values = c( "#FC4E07","brown")) +
  theme(legend.position = "none")
```
```{r}
gorriones %>% 
    ggplot(aes(x = sobrev, 
             y = esternon)) +
  geom_sina(aes(color = sobrev), size = 2)+
  scale_color_manual(values = c( "purple", "#FC4E07")) +
  theme(legend.position = "none")
```
```{r}
g2 <- gorriones %>%
  ggplot(aes(x=sobrev, y=extension, fill=sobrev)) + 
    geom_violin() +
    theme(
      legend.position="none",
      plot.title = element_text(size=10, # tamaño
                                hjust = 0.5, # centrado
                                face="bold", # estilo
                                color = "black")
  ) +ylim(c(220,260))+
  labs( x="sobrevida", y="Extensión", 
        title="Distribución de Extensión  por Supervivencia")+geom_boxplot(width = 0.3,fill="white")
g2
```


(e) Construir gráficos bivariados para las todas las variables, particionando
por el grupo de supervivencia (un color para cada grupo). ¿Observa
alguna regularidad que pueda explicar la supervivencia?

```{r}
gorr=gorriones%>%select(largototal:esternon)

ggpairs(gorr,
        upper = list(continuous = "density", combo = "box_no_facet"),
        lower = list(continuous = "points" , combo ="dot_no_facet"))+
  theme_bw()
```

(f) Construir la matriz de diagramas de dispersión. ¿Considera que algún
par de estas medidas están relacionadas? Estudiar si la asociación de algunas
de estas medidas es diferente en alguno de los grupos

```{r}
M= cor(gorr)
corrplot.mixed(M, upper = "pie",     
         lower = "ellipse")
```

# Ejercicio 5

## Matriz de covarianzas: (Datos Gorriones.xls)

### Para esta base de datos, interesa:

 - (a) Dimensión de la base de datos (n= número de observaciones, p= cantidad
de variables observadas sobre cada individuo).

```{r}
View(gorriones)
dim(gorriones) # la dimensión dice 7 columnas pero son 6 variables y 49 registros
#la primer columna corresponde a el orden del registro
```


 - (b) Hallar el vector de medias, la matriz de varianzas y covarianzas y la matriz
de correlaciones. ¿Qué características tienen estas matrices?

```{r}
vector_medias=apply(gorr,2,mean)
vector_medias
```
```{r}
matriz_covarianzas=var(gorr)
round(matriz_covarianzas,3)
```

```{r}
matriz_correlacion=cor(gorr)
round(matriz_correlacion,3)
```


 - (c) Explicar que representa el elemento m11 de la matriz de varianzas y covarianzas,
ídem para el elemento m31.

**m11=var(largototal)**

**m31=covarianza(largototal, cabeza)**

 - (d) Explicar que representa el elemento m22 de la matriz de correlaciones,
ídem para el elemento m13.

**m22=cor(extension,extension)=1** (todos los elementos diagonales son 1)
**m13=m31=cor(largotal,cabeza)**


 - (e) Relacionar los elementos m21,m11ym22 de la matriz de varianzas y covarianzas
con el elemento m12 de la matriz de correlaciones.
