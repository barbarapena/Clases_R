#clase 2 Dereck 18. agosto.2020

# Investigación Reproducible. 
install.packages("rmarkdown")
install.packages("kableExtra")
install.packages("knitr")
library(rmarkdown)
library(kableExtra)
library(knitr)
library(tidyverse)

library(git)
#Esto en la terminal de Rstudio(pestaña al lado de la consola)
git config --global user.email "bpena1@uc.cl"
git config --global user.name "barbarapena"


#importar los datos de hoy
plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv')
actions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/actions.csv')
threats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/threats.csv') 

## Install pacman
# es un paquete es un manejadro de paquetes, si no tienes un paquete lo instala
install.packages("pacman")
library(pacman)


### Para las tablas
data("msleep")
Tabla <- msleep %>%  
  group_by(vore) %>% 
  summarise_at ("sleep_total", .funs = list(Mean=mean, SD=sd )) %>%  
  filter(!is.na(vore)) %>% 
  arrange(desc(Mean))
