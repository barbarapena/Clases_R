##Clase 1_11.agosto

data(uspop)

uspop[c(5,4,7)]
# Vectores y diferentes funciones
x=  c(2,4,6,8)
y=8+x
y

#iris
data(iris)
iris
iris["Species"]
iris$Species
## Filas, columnas
iris[1:50,5]


####  TidyData
#Cada fila es una variable, cada, cada observacion es una fila, cada celda es un valor
# Una tabla de contingencia
data("HairEyeColor")
HairEyeColor# <- no tidy, el messy es mas adecuado para mostrar a la gente, pero no para trabajarlo en R


## Dylyr
install.packages("tidyverse")
install.packages("dplyr")

library(tidyverse)
library(dplyr)
data("iris")

#-----------------------------------------------------
#Group_by and summarize

#Para summarize, pongo la funcion, el DF, y luego el nombre de la variable que quiero, y la función que aplico 
Summary.Petal <- summarize( iris, Mean.Petal.Length=mean(Petal.Length),SD.Petal.Length=sd(Petal.Length))

# Puedo ocupar summarize y group_by juntos, por ejemplo para hacer el mismo resumen, pero por especie.

por_especie<-group_by(iris, Species)
DF2<- summarize( por_especie, Mean.Petal.Length=mean(Petal.Length),SD.Petal.Length=sd(Petal.Length))
DF2

# Puedo agrupar por más de una variable a la vez, se usa otra base de datos, pq iris solo tiene la especie como variable categorica

#Cual es la media de las millas por galon, eficiencia, para autos de distintos cilindros y automatico manual
data("mtcars")
DF3 <-group_by(mtcars, cyl,am)
DF4<- summarise(DF3, Consumo_promedio=mean(mpg))

#-----------------------------------------------------

#Mutate, para generar variable nuevas, por ejemplo, caballos de fuerza/peso auto

DF5<- mutate(mtcars, Ratio.hp.wt= hp/wt)
DF5

#----------------------------------------------------
#Pipeline ( %>% ) Varias operaciones de forma secuencias, varios parentesis anidados

abs(round(sqrt(log(4)),2))

4 %>% log %>% sqrt %>% round(2) %>% abs 

iris %>%  group_by(Species) %>%summarise(Mean_Sepal_L=mean(Sepal.Length))

DF6<- iris %>%
  mutate(Ratio_PEtal_epal=Petal.Length/Sepal.Length) %>% 
  group_by(Species) %>% 
  summarise(Mean_ratio=mean(Ratio_PEtal_epal),SD_ratio=sd(Ratio_PEtal_epal))
DF6

#---------------------------------------------------
#summarise_all, toma todas las variables, hace el resumen que le pido (en este caso, mean y sd)
DF7 <- iris %>% 
  group_by(Species) %>% 
  summarise_all( .funs=list(Mean=mean, SD= sd, Median=median))
DF7
  
DF8 <- iris %>% 
  group_by(Species) %>% 
  summarise_at(.vars= c("Sepal.Length", "Sepal.Width"), .funs=list(Mean=mean, SD= sd, Median=median))

DF8

#---------------------------------------------------
#Filter, filtrar los datos, para seleccionar datos que cumplen una condición, y botar los otros

DF9 <- iris %>% filter(Species != "versicolor") %>% 
  group_by(Species) %>% summarise_all(.funs=list(Mean=mean,SD=sd))
DF9

DF10 <- iris %>% filter(Species != "virginica", Petal.Length>=4, Petal.Width>=1.5)
DF10

##Si quiero seleccionar caracteristicas precisas, por ejemplo, autos con 6 y 8 cilidros
DF11 <- mtcars %>%  filter (cyl %in% c(6,8)) %>% 
  group_by(am) %>%  summarise(Consumo=mean(mpg))

#si quiero saber cuantas objetos hay con mis parametros
DF12 <- iris %>% filter(Petal.Length>4.5) %>% 
  group_by(Species) %>% summarise(Numero=n())
DF12
# .funs,es la lista de las funciones, si no le pongo el nombre a las funciones, va a quedar como funs...
# .vars, es para pedirle que haga el resumen solo para las variables que le indico 

#--------------------------------------------------
#Select, para seleccionar variables
data(mpg)
mpg
#categorias unicas  una variable
mpg$class %>% unique()

mpg_chico <- mpg %>%  filter(class=="suv") %>% 
  select(cty,hwy,cyl)
mpg_chico

DF13 <- iris %>% select(starts_with("Petal"), Species)
DF13




### Tarea 1 
#Usando la base de datos del repositorio del ministerio de ciencias, 
#genera un dataframe que responda lo siguiente:
#1.-  ¿Que proporción de las comunas ha tenido en algun momento mas de 50 casos por cada 100.000 habitantes?
#2.- Genera un dataframe, donde aparezca para cada comuna que haya tenido sobre 50 casos por cada 100.000 habitantes, 
#cuantos días ha tenido sobre ese valor.
#3 .- Genera una tabla de cuales comunas han tenido sobre 50 casos por cada 100.000 habitantes y de esas comunas 
#crea una variable que sea la prevalencia máxima de dicha comuna.

Casos_Activos <- read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto19/CasosActivosPorComuna_std.csv")

colnames(Casos_Activos)<- make.names(colnames(Casos_Activos))

# Respuesta 1
Df1 <- mutate(Casos_Activos, casos.100milhabitantes= (Casos.activos/Poblacion)*100000) %>% filter(casos.100milhabitantes>=50) 
n_comunas_totales<- unique(Casos_Activos$Comuna) %>% length()
n_comunas_mayor_50 <- unique(Df1$Comuna) %>% length()
Proporcion<-n_comunas_mayor_50/n_comunas_totales

# Respuesta 2
Df2<-  Df1 %>% group_by(Comuna) %>% summarise(N=n())

#Respuesta 3
Df3 <- Df1 %>%group_by(Comuna) %>% filter(casos.100milhabitantes==max(casos.100milhabitantes)) %>%
  rename(Prevalencia_maxima=casos.100milhabitantes) %>% select(Comuna,Prevalencia_maxima) %>% arrange(desc(Prevalencia_maxima))

## 4.- Ve cuales son las 10 comunas que han tneido la mayor mediana de prevalencia, para cada una de ellas,
# Genera una tabla con la mediana, prevalencia máxima y fecha que alcanzó la prevalencia máxima
Df4<- Df1 %>% arrange(desc(casos.100milhabitantes)) %>% rename(Prevalencia_max=casos.100milhabitantes)%>% slice_max(Prevalencia_max,n=10)

Prevalencia_max_top_10<- Df4[1:10,]
