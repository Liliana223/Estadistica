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
```

```{r message=FALSE}
# Leer los datos y establecer el directorio de trabajo
setwd("~/Maestria Lili/Primer semestre/Estadistica y R para ciencias de la salud/Código R/Actividad 3")
df <- read.csv("alimentos_nutrientes_4900.csv")

# Verificar los datos NA y eliminarlos
colSums(is.na(df))
df_complete <- df[complete.cases(df),]
any(is.na(df_complete))

# Estandarizar los datos
variables_numericas <- select(df_complete, -(1:3), -5, -(7:19)) ###Eliminar si es necesario
alimentos_y_nutrientes <- select (df_complete, (28:177))

varnum_scale <- scale(variables_numericas)
alimnutr_scale <- scale(alimentos_y_nutrientes)

# Extraer los valores propios/varianzas de los componentes principales
pca<- prcomp(alimentos_y_nutrientes, scale=TRUE)
eigenvalues<- get_eigenvalue(pca)
eigenvalues

# Visualizar los eigenvalues de los componentes principales
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50))

# Extraer los resultados de las variables
var <- get_pca(pca)
var

# Visualizar el gráfico de correlación variable (muestra las relaciones entre todas las variables)
fviz_pca_var(pca, col.var = "black")

fviz_pca_var(pca, col.var = "cos2", 
             geom.var = "arrow", 
             labelsize = 2, 
             repel = TRUE)

# Visualizar la importancia de cada variable por dimensiones
fviz_contrib(pca, choice = "var", axes = 1, top = 10)
fviz_contrib(pca, choice = "var", axes = 2, top = 10)
fviz_cos2(pca, choice = "var", axes = 1:2, top = 10) # Las variables mas relevantes son los nutrientes 2,4,7,8,10,11,12,13,14,19

# Selección de variables que aportan información relevante a los resultados

relev <- select(alimentos_y_nutrientes, 133, 135, 138:139, 141:145,150)
pca_relev <- prcomp(relev, scale=TRUE)
eigenvalues2<- get_eigenvalue(pca_relev)
eigenvalues2

# Visualizar los eigenvalues de los componentes principales
fviz_eig(pca_relev, addlabels = TRUE, ylim = c(0, 90))

fviz_pca_var(pca_relev, col.var = "black")

fviz_pca_var(pca_relev, col.var = "cos2", 
             geom.var = "arrow", 
             labelsize = 2, 
             repel = TRUE) # Las variables con valores altos de cos2 se colorearán en “azul claro. Mayor cos2 = mayor calidad de la variable = más importante

# Grafico 2

fviz_pca_ind(pca_relev, geom.ind = "point", 
             col.ind = "#FC4E07", 
             axes = c(1, 2), 
             pointsize = 1.5) 
```
