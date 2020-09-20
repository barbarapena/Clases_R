##Clase 3
### paquete tidyverso
install.packages("tidyverse")
library(tidyverse)

contagiados <- read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto19/CasosActivosPorComuna.csv")


contagiados1 <- contagiados %>% pivot_longer(cols=starts_with("2020"))
contagiados2 <- contagiados %>% pivot_longer(cols=starts_with("2020"), names_to= "Fecha")
contagiados3 <- contagiados %>% pivot_longer(cols=starts_with("2020"), names_to= "Fecha", values_to= "Infectados")


#Lubridate
contagiados3 <- contagiados %>% 
  pivot_longer(cols=starts_with("2020"), names_to= "Fecha", values_to= "Infectados") %>% 
  mutate(Fecha=lubridate::ymd(Fecha))

#Prevalencia, de la fecha más reciente
contagiados3 <- contagiados %>% 
  pivot_longer(cols=starts_with("2020"), names_to= "Fecha", values_to= "Infectados") %>% 
  mutate(Fecha=lubridate::ymd(Fecha), prevalencia=100000*(Infectados/Poblacion)) %>% 
  filter(Fecha==max(Fecha),!is.na('Codigo comuna')) %>% 
  arrange(desc(prevalencia))

#Pivot_wider lo contrario a pivot_longer
data("fish_encounters")
unique(fish_encounters$fish) %>% length()
unique(fish_encounters$station) %>% length()


Para_captura<- fish_encounters %>% pivot_wider(names_from = "station" ,
            values_from= "seen")
Para_captura_1<- fish_encounters %>% pivot_wider(names_from = "station" ,
                                               values_from= "seen",
                                               values_fill=0)

#otra base de datos
data("warpbreaks")
head(warpbreaks)

Breaks<- warpbreaks %>%  
  pivot_wider(names_from = tension, 
              values_from=breaks, 
              values_fn=mean) %>% summarise_if(is.numeric(~round()))


#Joins, para unir bases de datos distintas con columnas en comun

Episodes <- read_csv("https://raw.githubusercontent.com/derek-corcoran-barrios/The_office/master/The_Office_Episodes_per_Character.csv")
words <- read_csv("https://raw.githubusercontent.com/derek-corcoran-barrios/The_office/master/The_office_Words.csv")
stop_words <- read_csv("https://raw.githubusercontent.com/derek-corcoran-barrios/The_office/master/stop_words.csv")

###Full join

Episodios_words<- full_join(Episodes,words)

##Personas que aparecen en más epispdios
Episodes_top_10<-Episodes %>% slice_max(order_by = n_episodes, n=10)
Episodes_top_10_words<-Episodes %>% slice_max(order_by = n_episodes, n=10) %>% left_join(words)

#palabras mas utilizadas
Top_words<- Episodes_top_10_words %>%
  anti_join(stop_words) %>%
  group_by(word) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  slice_max(order_by = n, n=20)


##

Episodes_top_10_words_full<-Episodes %>% slice_max(order_by = n_episodes, n=10) %>% full_join(words)

### Separar por personaje
Top_words_por_personaje<- Episodes_top_10_words %>%
  anti_join(stop_words) %>%
  group_by(word,speaker) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  group_by(speaker) %>% 
  slice_max(order_by = n, n=10)

####Tarea_1
#Ver para cada temporada cuales son los 10 personajes con más palabras (cambia en el tiempo?)
#Quien aumenta y quien disminuye mas?
#Crear una columna de delta palabras entre temporadas por personaje por episodio de la serie
