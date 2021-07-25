#=====================================================================

#El host se muestra en el "dashboard" de la instancia como "DNS de IPv4 pública"
library(DBI)
library(RMySQL)
library(VIM)
db <- dbConnect(RMySQL::MySQL(),
                dbname = "pokemon",
                host = "ec2-18-207-181-49.compute-1.amazonaws.com",
                user = "usuario",
                password = rstudioapi::askForPassword("Database password"),
                Port = 3306)
#Clave: User1234+

# Luego se puede hacer consultas usando dbGetQuery y SQL
poke <- dbGetQuery(db,'SELECT * FROM pokemones')
poke

###### Realizando analisis univariado #############
summary(poke)

class(poke)

# Conversión de valores perdidos a NA (por defecto están como 0)

poke$Type2[poke$Type2==""] <- NA


# Revisando datos perdidos #

apply(is.na(poke), 2, mean)
apply(is.na(poke), 2, sum) 


# Visualizando graficamente 
aggr(poke,number=TRUE, sortComb=TRUE, sortVar=TRUE, only.miss=TRUE)

# para numericas
poke$type2_num<-ifelse(is.na(poke$Type2),NA,1) 
poke$type1_num<-ifelse(is.na(poke$Type2),NA,1) 

library(dplyr)
poke$T1_num=case_when(
  poke$Type1  == 'Bug' ~ 1,
  poke$Type1  == 'Dark' ~ 2,
  poke$Type1  == 'Dragon' ~ 3,
  poke$Type1  == 'Electric' ~ 4,
  poke$Type1  == 'Fairy' ~ 5,
  poke$Type1  == 'Fighting' ~ 6,
  poke$Type1  == 'Fire' ~ 7,
  poke$Type1  == 'Flying' ~ 8,
  poke$Type1  == 'Ghost' ~ 9,
  poke$Type1  == 'Grass' ~ 10,
  poke$Type1  == 'Ground' ~ 11,
  poke$Type1  == 'Ice' ~ 12,
  poke$Type1  == 'Normal' ~ 13,
  poke$Type1  == 'Poison' ~ 14,
  poke$Type1  == 'Psychic' ~ 15,
  poke$Type1  == 'Rock' ~ 16,
  poke$Type1  == 'Steel' ~ 17,
  poke$Type1  == 'Water' ~ 18
)

poke$Leg_num=case_when(
  poke$Legendary  == 'False\r' ~ 0,
  poke$Legendary  == 'True\r' ~ 1)
  
names(poke)
table(poke$Legendary)

pks_btl_attr<- c('Attack','Defense','HP','Speed','SpAtk','SpDef','Generation', 'Leg_num','T1_num','type2_num')
poke[,pks_btl_attr]

x11()
matrixplot(poke[,pks_btl_attr])

# Immputando los NA (debido a que estos NA corresponden aquellos que no tienen poder TIPO2 creamos una nueva categoria)#

poke[is.na(poke)] <- "No aplica"

# Analisis de Outliers #

library(ggplot2)





