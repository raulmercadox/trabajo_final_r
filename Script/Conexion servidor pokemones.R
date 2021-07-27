#=====================================================================

#El host se muestra en el “dashboard” de la instancia como “DNS de IPv4 pública”
library(DBI)
library(RMySQL)
db <- dbConnect(RMySQL::MySQL(),
                dbname = "pokemon",
                host = "ec2-18-207-181-49.compute-1.amazonaws.com",
                user = "usuario",
                password = rstudioapi::askForPassword("Database password"),
                Port = 3306)

# Luego se puede hacer consultas usando dbGetQuery y SQL
pokemo <- dbGetQuery(db,'SELECT * FROM pokemones')
pokemo



# Luego se puede hacer consultas usando dbGetQuery y SQL
color <- dbGetQuery(db,'SELECT * FROM pkm_class_color')
color


# Luego se puede hacer consultas usando dbGetQuery y SQL
pokemo_final <- dbGetQuery(db,'SELECT * FROM poke_final')
pokemo_final




#Clave: User1234+

