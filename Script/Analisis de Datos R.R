#=====================================================================

#El host se muestra en el "dashboard" de la instancia como "DNS de IPv4 p?blica"
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

# Conversi?n de valores perdidos a NA (por defecto est?n como 0)

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

# ExploraciÃ³n de Datos
# Para determinar si se hace un anÃ¡lisis de outlier por chi-square o Mahalanobis
poke2 = poke[-c(1,2,3,4,11,12,13,14,15,16)]

head(poke2,5)
summary(poke2)
boxplot(poke2)
pairs(poke2) 

par(mfrow = c(3,2))

hist(poke2$HP)
hist(poke2$Attack)
hist(poke2$Defense)
hist(poke2$SpAtk)
hist(poke2$SpDef)
hist(poke2$Speed)

# Outliers usando la PuntuaciÃ³n Z
# """""""""""""""""""""""""""""""

is.outlier_z <- function(x, k=3) {
  return(abs(scale(x)) > k)           # scale: (x-media)/desv_est
}

is.outlier_z(poke2)

################# Variable HP ####################
# Ãndices (T/F) del Rango Intercuartilico
idx_outliers_hp <- is.outlier_z(poke2$HP, k=3)
which(idx_outliers_hp)

# RI atÃ­picos
poke2$HP[idx_outliers_hp]

# Registros asociados con RI AtÃ­pico
poke2[idx_outliers_hp, ]

# SegÃºn la puntuaciÃ³n Z se identifican 3 observaciones con valores atÃ­picos 
# para Al, con K = 3 desv estÃ¡ndar.

# Al:
par(mfrow=c(1,1))


#################################################


install.packages('robustbase')
library(robustbase)

# RI:
par(mfrow=c(3,2))

adjbox(poke2$HP) # Se observan 2 observaciones outliers   
adjbox(poke2$Attack) # Se observan 1 observaciones outliers   
adjbox(poke2$Defense) # Se observan 1 observaciones outliers   
adjbox(poke2$SpAtk) # Se observan 4 observaciones outliers   
adjbox(poke2$SpDef)  # Se observan 2 observaciones outliers y algunas observaciones con valores atÃ­picos    
adjbox(poke2$Speed) # Se observan 4 observaciones outliers   


#Evaluar valores outlier con la distancia de Mahalanobis

#a) calculamos la distancia de mahalanobis para nuestros datos.
par(mfrow=c(1,1))
dm2 <- mahalanobis(poke2, colMeans(poke2), cov(poke2))
barplot(dm2, main="Mahalanobis")
which.max(dm2)

# DistribuciÃ³n Chi-Cuadrado: Punto de Corte 
p <- 1-0.001
dof = ncol(poke2)
k <- (qchisq(p, dof))
idx_outliers <- which(dm2 > k)
idx_outliers
poke2[idx_outliers,]

#Con el 99.9% de la distribuciÃ³n se encuentran 5 observaciones posiblemente
#outliers multivariados.

# GrÃ¡fico de Ojiva
plot(sort(dm2), ppoints(nrow(poke2)), xlab="DM al cuadrado ordenada", 
     ylab="Probabilidad Acumulada")
abline(v = qchisq(p,dof), col = "red")

# QQ-plot:
x <- qchisq(ppoints(nrow(poke2)), dof)
y <- dm2
qqplot(x, y, main=expression("Q-Q plot para"~~{chi^2}[nu==6]))
abline(0, 1, col="red")

#Con estos grÃ¡ficos se confirma la existencia de observaciones outliers multivariadas.

#Se crea la tabla final la cual será importada a la instancia EC2
poke3=poke[,c(1:12)]
poke3$Legendary=case_when(
  poke$Legendary  == 'False\r' ~ 'False',
  poke$Legendary  == 'True\r' ~ 'True')
write.csv(poke3,"poke_fin.csv")

################################# FIN ############################################################
