
#Clase 5
library(tidyverse)
library(broom)

#Abrir base de datos

data("CO2")
View(CO2)

#Exploremos la data 
# ¿que variable explica mejor la variación de uptake en esta base de datos?

### La especie?
#Recordar que notch marca el intervalo de confianza del 95% de la mediana. 
ggplot(CO2, aes(x= Type, y= uptake)) + geom_boxplot(aes(fill=Type), notch = T) + theme_classic()

## El tratamiento ?
#Recordar que notch marca el intervalo de confianza del 95% de la mediana. 
ggplot(CO2, aes(x= Treatment, y= uptake)) + geom_boxplot(aes(fill= Treatment), notch = T) + theme_classic()

#La concentración?
ggplot(CO2, aes(x= conc, y= uptake)) + geom_point() + theme_classic()

###¿Como determiams un buen modelo?

# alguna_funcion(Y~ X1 + X2 + ...+  Xn, data= data.frame)
 
Y= variale repsuesta (Captacion CO2)
~ = Explicado por
Xn= Variable explicativa n (Subespeice, tratamiento,etc)

data.frame= Base de daos CO2
alguna_funcion: El modelo  a testear (nuestra simplificaci{on de la realida})

algunos modelos
prueba t
ANOVA
modelo lineal simple
modelo lineal generalizado
modelo aditivo
modelo no lineal
modelos lineales mixtos
bossted regression trees

# Cual usamos ahora ?

# modelo lineal, capatacion de CO" explicado por ecotipo (variable categorica)
Fit1 <- lm(formula = uptake ~ Type, data = CO2)
# Esto es equivalente a un ANOVA, si estamos ocupando un modelo lineal simple con una variable categorica

# Usar brrom para sacarle mas a tu modelo (glance)
para ver los daos generales del modelo
library(broom)
#PAra el modelo general

Est1<- glance(Fit1)
Est1 %>% select(r.squared:sigma, df:AIC)

Est1 %>%  select(r.squared, adj.r.squared, p.value, nobs )

Est2 <-tidy(Fit1)

#Augment, para predicciones y residuales del modelo
augment(Fit1)

Est3 <- augment(Fit1)
hist(Est3$.resid)


## Seleccion de modelos

## Uno por lo general trbaaja con varios modelos
### En general uno lo hace en base a AIC
### K= nummero de parametros, en caso de Fit 1, intercepto y ecotipo
### Ln (L) ajust, mas positivo mejor, mas negatico es malo, loglik
###AIC=2K-2ln(L)

ggplot(CO2, aes(x=conc, y=uptake, color=Type)) + geom_point(aes(shape= Treatment))

### COmparacion de modelos
Fit1 <- lm(formula = uptake ~ Type, data = CO2)
ggplot(CO2, aes(X=conc, y=uptake, color=Type)) + geom_point()

Fit2 <- lm(uptake ~ Treatment, data = CO2)
Fit3 <- lm(uptake ~ conc, data = CO2)
Fit4 <- lm(uptake ~ Type + Treatment + conc, data = CO2)
Fit5 <- lm(uptake ~ Type + conc + I(log(conc)), data = CO2)
Fit6 <- lm(uptake ~ Type:Treatment + conc + I(log(conc)), data = CO2)

#Seleccion de modelos
Modelo1 <- glance(Fit1) %>% dplyr::select(r.squared, AIC) %>% mutate(Modelo = "Fit1")
Modelo2 <- glance(Fit2) %>% dplyr::select(r.squared, AIC) %>% mutate(Modelo = "Fit2")
Modelo3 <- glance(Fit3) %>% dplyr::select(r.squared, AIC) %>% mutate(Modelo = "Fit3")
Modelo4 <- glance(Fit4) %>% dplyr::select(r.squared, AIC) %>% mutate(Modelo = "Fit4")
Modelo5 <- glance(Fit5) %>% dplyr::select(r.squared, AIC) %>% mutate(Modelo = "Fit5")
Modelo6 <- glance(Fit6) %>% dplyr::select(r.squared, AIC) %>% mutate(Modelo = "Fit6")


#### Ranking de modelos, el mejor es el con menor AIC
Modelos <- bind_rows(Modelo1, Modelo2, Modelo3, Modelo4, 
                     Modelo5, Modelo6) %>% arrange(AIC) %>% mutate(DeltaAIC = AIC - 
                                                            min(AIC))

### Como saber que variable predictora usaR? y como sber como interaccion?
ggplot(CO2, aes(x=conc, y=uptake, group=Plant)) +
  geom_point(aes(color= Type, shape=Treatment)) + 
  geom_line(aes(color=Type, lty=Treatment))+ theme_bw()

## Al mirar el grafico, vemos que la especie hace una diferencia, y que el tratamiento tambien, pero dicho efecto
# depende de la especie.
Fit6... 
###lty=tipo de linea

hist(augment(Fit1)$.resid)
hist(augment(Fit6)$.resid) ## distribucion mas normal

###GLM


### intall
install.packages("rticles")
tinytex::install_tinytex()

