#'
#'---
#'title: "Analisis de Flexibilidad en Atletas Chilenos"
#'author: "Samuel Montalvo, Ph.D."
#'date: "10/20/2022"
#'output: html_document
#'---


#' primero importamos la data
library(readxl)
Data <- read_excel("Data.xlsx")
View(Data)

#'Lista de paquetes para analisar la data
library(dplyr)
library(table1)
library(ggplot2)
library(ggprism)
library(rstatix)
library(kableExtra)

#' Veremos primero los nombres de la data
names(Data)

#' Hombres y mujeres no estan escritos completos por lo tanto tendremos que darle un nombre completo
Data$Sexo  <- recode_factor(Data$Sexo , 'M'= "Mujer", 'H' = "Hombre")

#' Tambien hubo un caso donde un participante esta como "NTeam" y los demas estan como "No Team", se asume que que ambos son No_Team
Data$Grupo  <- recode_factor(Data$Grupo, 'NTeam'= "No Team", "No Team"="No Team",
                             'Team'='Team')


#' La data se tiene que convertir de caracteres a numericos y cambiar los "," por ".", tambien cambiamos los labels para usar la data mas rapido
Data$Peso <- as.numeric(gsub(",",".",Data$Peso))
label(Data$Peso) <- "Peso (kg)"
Data$Talla <- as.numeric(gsub(",",".",Data$Talla))
label(Data$Talla) <- "Talla (m)"
Data$IMC <- as.numeric(gsub(",",".",Data$IMC))
label(Data$IMC) <- "Indice de masa corporal (kg/m2)"
Data$Años <- as.numeric(gsub(",",".",Data$Años))
label(Data$Años) <- "Edad (años)"
Data$`Sit_&_Reach` <- as.numeric(gsub(",",".",Data$`Sit_&_Reach`))
Data <- rename(Data, Sit_Reach ="Sit_&_Reach" )
label(Data$Sit_Reach) <- "Sit & Reach (cm)"


#' Ahora creamos una table con la data descriptiva
table <- table1(~ Edad + Peso +Talla + IMC +
         Años + Sit_Reach | Sexo*Grupo, data=Data, overall=FALSE)
t1kable(table)

#' La siguiente es una figura de boxplot con puntos individuales para observar mejor la data,
#' en la cual podemos ver que si, aquellos que estan en Team tienen un valor mayor en Sit&Reach que aquellos que no.
Sit_Reach_figura <- Data %>%
  ggplot(aes(x = Grupo, y = Sit_Reach, fill = Sexo,show.legend = TRUE)) +
  geom_line(aes(group=ID), position = position_dodge(0.5)) +
  geom_point(aes(fill=Sexo,group=ID),size=2.5,shape=21,
             position = position_dodge(0.3),show.legend = TRUE) +
  geom_boxplot(aes(x = Grupo, y = Sit_Reach, fill = Sexo),outlier.shape
               = NA, alpha = .5, width = .35, colour = "black",show.legend = TRUE) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+ theme_bw() +
  ylab('Sit & Reach (cm)')+
  xlab('Group') + theme_prism()
Sit_Reach_figura
ggsave("Sit_Reach_figura.png")

#' ahora vemos si la distribucion de la data es normal o no con el test de shapiro wilk
#' data esta normalmente distribuida lo cual nos indica que podemos seguir con un test parametrico
Data %>% group_by(Grupo) %>% shapiro_test(Sit_Reach)


#' prueba independiente de T
Data %>% t_test(Sit_Reach ~ Grupo)

#' Los valores nos indican que exite un p muy por debajo del 0.01, lo cual no sindica que en nuestr hipotesis inicial es confirmada y aquellos que son parte del equipo
#' tienen una flexibilidad mayor de la cadena posterior en mayor proporcion que aquellos que no parte del equipo.
