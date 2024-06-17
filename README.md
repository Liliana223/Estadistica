---
title: "Resolución Actividad 3 máster Bioinformática UNIR (2024)"
author: "Astrid Liliana Vargas Sanchez, María Isabel García Sejas"
date: "2024-06-18"
output: 
  html_document:
    theme: cerulean
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

**Abrir, explorar y preprocesar la base de datos:**
```{r message=FALSE}
#Carga de librerías que se van a utilizar
library(dplyr)
library(ggplot2)
library(factoextra)
library(nortest)
library(moments)
```

```{r message=FALSE}
# Leer los datos y establecer el directorio de trabajo
setwd("~/Maestria Lili/Primer semestre/Estadistica y R para ciencias de la salud/Código R/Actividad 3")
df <- read.csv("alimentos_nutrientes_4900.csv")

# Verificar los datos NA y eliminarlos
colSums(is.na(df))
df_complete <- df[complete.cases(df),]
any(is.na(df_complete))

# Estandarizar los datos con las variables de alimentos y nutrientes
alimentos_y_nutrientes <- select (df_complete, (28:177))

alimnutr_scale <- scale(alimentos_y_nutrientes)
```

**Aplicar un PCA a los datos de alimentos y nutrientes para reducir la dimensionalidad y resumir la información en componentes principales.**

```{r message=FALSE}
# Extraer los valores propios/varianzas de los componentes principales
pca<- prcomp(alimnutr_scale, scale=TRUE)
eigenvalues<- get_eigenvalue(pca)
eigenvalues

# Visualizar los eigenvalues de los componentes principales
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50))

#Representación de los primeros dos componentes principales

plot(pca$x[ ,1], pca$x[ ,2]) # Encontramos varias muestras pero no se encuentran claramente diferenciadas
```

**Crear gráficos descriptivos que se ajusten al tipo de análisis de los componentes principales.**

```{r message=FALSE}
# Visualizar el gráfico de correlación variable (muestra las relaciones entre todas las variables)
fviz_pca_var(pca, col.var = "black")

fviz_pca_var(pca, col.var = "cos2", 
             geom.var = "arrow", 
             labelsize = 2, 
             repel = TRUE)+
  theme_minimal()+
  labs(title = "Gráfico de correlación de variables alimentos y nutrientes")

# Visualizar la importancia de cada variable por dimensiones
fviz_contrib(pca, choice = "var", axes = 1, top = 10)
fviz_contrib(pca, choice = "var", axes = 2, top = 10)
fviz_cos2(pca, choice = "var", axes = 1:2, top = 10) # Las variables mas relevantes son los nutrientes 2,4,7,8,10,11,12,13,14,19
```


```{r message=FALSE}
# Selección de variables que aportan información relevante a los resultados

relev <- select(alimentos_y_nutrientes, 133, 135, 138:139, 141:145, 150)
relev2 <- scale(relev)

# Realizar PCA
pca_relev <- prcomp(relev2, scale=TRUE)

# Obtener los componentes principales (scores)  ###### Tabla PCA componentes y R2. ####### FALTA SACAR R2
pca_scores <- as.data.frame(pca_relev$x)

# Determinación del numero de factores
eigenvalues2<- get_eigenvalue(pca_relev)
eigenvalues2

# Consultar las cargas para cada variable del componente 1  ###### Tabla PCA cargas ##########
scores <- (pca_relev$rotation[ ,1])
scores

# Consultar las cargas para cada variable del componente 1  ########## Tabla PCA cargas ###########
scores2 <- (pca_relev$rotation[ ,2])
scores2

# Visualizar los eigenvalues de las variables mas relevantes
fviz_eig(pca_relev, addlabels = TRUE, ylim = c(0, 90))

fviz_pca_var(pca_relev, col.var = "black")

fviz_pca_var(pca_relev, col.var = "cos2", 
             geom.var = "arrow", 
             labelsize = 2, 
             repel = TRUE)+
  theme_minimal()+
  labs(title = "Gráfico de correlación de variables alimentos y nutrientes",
       subtitle = "Variables mas relevantes") # Las variables con valores altos de cos2 se colorearán en “azul claro. Mayor cos2 = mayor calidad de la variable = más importante

# Grafico 2

fviz_pca_ind(pca_relev, geom.ind = "point", 
             col.ind = "#FC4E07", 
             axes = c(1, 2), 
             pointsize = 1.5)+
  theme_minimal()+
  labs(title = "Gráfico de correlación de variables")
```

**Crear una tabla descriptiva que resuma las variables descriptivas y las variables más importantes obtenidas a través de los PCAS, incluido los valores p.**

```{r}
# Creación de un dataset con las variables descriptivas y las variables mas importantes obtenidas del PCA

descr <- select(df_complete, 20:27, 160, 162, 165:166, 168:172, 177 )
```
• Para este ejercicio se evalúa la normalidad de los datos con el método ad.test: Anderson-Darling test.

```{r}

pvalues <- matrix(NA, nrow = 18, ncol = 1)

for(i in 1:18) { 
  adtest_result <- ad.test(descr[, i])
  pvalues[i, ] <- adtest_result$p.value
}

pvalues ######### Tabla normalidad de datos ########## 

```
*Se obtuvo un resultado de 3.7e-24 lo que significa que es un valor estadísticamente significativo p<0.001. Los datos no siguen una distribución normal*
Los datos de la normalidad de las variables evaluadas se reflejan en la tabla adjuntada en el documento de word.

**Para mejorar la calidad del análisis puedes generar una nueva variable de terciles a partir de las variables de los componentes para medir el grado de adhesión a cada componente. Por ejemplo, aquellos en un tercil == 1 significa menor adherencia al componente, y un tercil== 3 significa mayor adherencia al componente. El formato de la tabla puede guiarte, tal y como se puede ver en la Tabla 2 del documento en PDF que se os adjunta con nombre Ejemplo de artículo actividad grupal.pdf.**

• CONTRASTE DE HIPÓTESIS #1. Adherencia a componentes

- H0: Tercil==1 [Alimentos y nutrientes] = Tercil==3 [Alimentos y nutriente] -> p≥0.05. No adherencia
- H1: Tercil==1 [Alimentos y nutrientes] ≠ Tercil==3 [Alimentos y nutriente] -> p<0.05. Adherencia

Para este ejercicio se emplea wilcox.test para comparar una variable con dos categorías (Terciles) con variables continuas (Alimentos y nutrientes)

```{r}
# Extraer los resultados de las variables
var <- get_pca(pca)
var

cosvar <- as.matrix(var$cos2)
var$cos2

percentiles <- c(0.33, 0.66)

quantile(cosvar[, 1], probs = percentiles)

interc1 <- matrix(NA, nrow = 150, ncol = 150)

for(i in 1:150) { 
  interc_result1 <- quantile(cosvar[, i], probs = percentiles)
  interc1[i, ] <- interc_result1
}
interc1
```
