### Clase 4 Derek
# Visualizaci?n de datos en R.
install.packages("bookdown")
install.packages("hexbin")
library(hexbin)
library(bookdown)
library(tidyverse)
#ggplot(data.frame, aes(nombres_de_columnas_con_la_que_quiero_trabajar)) 
#-geom_algo(argumentos, aes(columnas)) -theme_algo()estilo <- cambio estetico.
#-Personalizacion_de_ejes_y_leyendas

##Scatter plots comparando dos variables------------

data("diamonds")
ggplot(diamonds, aes(x=carat, y=price)) + geom_point(aes(color=cut))+ theme_dark()

#debo poner dentro del aes la column a uar, y argumentos, como color
ggplot(diamonds, aes(x=carat, y=price)) + 
  geom_point(aes(color=cut), alpha=0.2)+ 
  theme_dark()

ggplot(diamonds, aes(x=carat, y=price)) + 
  geom_point(aes(shape=cut, color=color), alpha=0.2)+ 
  theme_classic()
## para mtcars vamos a camiar el tama?o # agregue una tercera dimencion con size=hp

data("mtcars") 
ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point(aes(size=hp)) + 
  theme_classic()

##para transformar una variable continua en una discreta, como hp,
ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point(aes(size=hp, shape=hp)) + 
  theme_classic() + scale_shape_binned()

## Fill, solo para ?reas ! no para puntos , notch, intervalo confianza de la mediana
data("iris")
ggplot(iris, aes(x= Species, y=Sepal.Width)) +
  geom_boxplot(notch = T, aes(fill=Species))

ggplot(iris, aes(x= Species, y=Sepal.Width)) +
  geom_boxplot(notch = T, aes(color=Species))

#Poner los colores manualmente, mieae color brewe
ggplot(iris, aes(x= Species, y=Sepal.Width)) +
  geom_boxplot(notch = T, aes(fill=Species)) +
  scale_fill_manual(values=c("red", "blue", "yellow"))

ggplot(iris, aes(x= Species, y=Sepal.Width)) +
  geom_boxplot(notch = T, aes(fill=Species)) +
  scale_fill_viridis_b(name= "Especie")


# Una variable categorica una continua

# geom_jitter (variabilidad de los puntos en el eje y)
ggplot(iris,aes(x=Species, y=Sepal.Width))+
  geom_jitter(aes(color=Species))

#geom_violin
ggplot(iris,aes(x=Species, y=Sepal.Width))+
  geom_violin(fill="red") + coord_flip()

##Se pueden combianr, pero el orden es importante, porque va por capaz, siendo lo ultimo que se agrega en la linea lo que queda encima
ggplot(iris,aes(x=Species, y=Sepal.Width))+
  geom_violin() +geom_jitter(aes(color=Species))+ coord_flip()

ggplot(iris,aes(x=Species, y=Sepal.Width))+
  geom_violin() + geom_boxplot()+ 
  geom_jitter(aes(color=Species))+coord_flip()

#alpha=0, violines y boxplot vacios
ggplot(iris,aes(x=Species, y=Sepal.Width))+
  geom_violin(alpha=0) + geom_boxplot(alpha=0)+ 
  geom_jitter(aes(color=Species))+coord_flip()

## Como reordenar variables?
ggplot(iris,aes(x=Species, y=Sepal.Width))+
  geom_boxplot(notch = T)

ggplot(iris,aes(x=fct_reorder(Species, Sepal.Width), y=Sepal.Width))+
  geom_boxplot(notch = T)

ggplot(iris,aes(x=fct_reorder(Species, Sepal.Width, .desc=T), y=Sepal.Width))+
  geom_boxplot(notch = T) + xlab ("Especie") + ylab("Ancho Sepalo")

ggplot(iris,aes(x=fct_relevel(Species, "setosa", "virginica"), y=Sepal.Width))+
  geom_boxplot(notch = T, aes(fill=Species)) +
  xlab ("Especie") + ylab("Ancho Sepalo")



## Para vairables continuas
geom_point()
geom_smooth()
geom_line()
geom_hex()
geom_rux


ggplot(diamonds, aes(x=carat, y=price)) + 
  geom_hex()+ scale_fill_viridis_c()

ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_smooth() + geom_point()

ggplot(mtcars, aes(x=wt, y=mpg)) +
   geom_point()
#La linea que agrega smotth, es una linea de detendecia
ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_smooth()

#pero puedo cambiarla por una liena de regresion lineal
ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_smooth(method=lm, fill="yellow", alpha=0.5) + geom_point()



### Con esto, descargamos una base de datos. 
githubURL <- ("https://raw.githubusercontent.com/derek-corcoran-barrios/derek-corcoran-barrios.github.io/master/Clase4/TempHum.rds")
download.file(githubURL, "TempHum.rds", method = "curl")
TempHum <- read_rds("TempHum.rds") %>% mutate(Mes = as.numeric(Mes))

CU <- TempHum %>% filter(Ciudad_localidad == "Curic?")

ggplot(CU, aes(x=Mes, y=Temperatura)) + geom_point() +geom_smooth(method=lm)

ggplot(CU, aes(x=Mes, y=Temperatura)) + geom_point() +
  stat_smooth(method=lm, formula= y~ x+ I(x^2) + I(x^4))

CU2<- CU %>%  
  pivot_longer(cols=c("Temperatura", "Humedad"), names_to="Unidad", values_to="Medida")

ggplot(CU2, aes(x=Mes, y=Medida)) + geom_point(aes(color=Unidad)) +
  stat_smooth(method=lm, formula= y~ x+ I(x^2) + I(x^4), aes(fill=Unidad))



### Varios gr?ficos juntos
## si quiero poner varias ciudades? 
Algunos <- TempHum %>% filter(Ciudad_localidad %in% 
                                c("Arica", "Rapa Nui", "La Serena", "Valpara?so", 
                                  "Quinta Normal", "Concepci?n", "Valdivia", 
                                  "Punta Arenas")) %>% pivot_longer(cols = c(Temperatura, 
                                                                             Humedad), names_to = "Unidad", values_to = "medida")
ggplot(Algunos, aes(x= Mes, y= medida)) + 
  geom_point(aes(color=Ciudad_localidad)) +
  stat_smooth(method=lm, formula= y~ x+ I(x^2), 
              aes(fill=Ciudad_localidad, color=Ciudad_localidad))

ggplot(Algunos, aes(x= Mes, y= medida)) + 
  geom_point() +
  stat_smooth(method=lm, formula= y~ x+ I(x^2))+
  facet_wrap(~Ciudad_localidad, ncol=2)

ggplot(Algunos, aes(x= Mes, y= medida)) + 
  geom_point() +
  stat_smooth(method=lm, formula= y~ x+ I(x^2))+
  facet_wrap(~fct_reorder(Ciudad_localidad, Unidad), ncol=2)


 ## Mapas en ggplot2
## Ver curso de GMIT

