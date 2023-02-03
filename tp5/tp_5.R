rm(list = ls())

library(DBI)
library(sf)
source(file = "tp5/connexion_db.R")
conn<-connecter()
DBI::dbListTables(conn)

str(conn)
# Objet PqConnection avec 5 débouchées

DBI::dbListFields(conn,"popnaiss_com")
# 17 variables (nivgeo, pop age_40)

# 3 Copie de table popnaiss_com
copie_popnaiss<-dbSendQuery(conn=conn,
            "SELECT * FROM popnaiss_com")
str(copie_popnaiss)
#' Objet de type PqResult
#' Contient infos de connection
#' Difficile à utiliser

# 4
copie2_popnaiss<-dbGetQuery(conn=conn,
                            "SELECT * FROM popnaiss_com")
str(copie2_popnaiss)
# Objet dataframe

# 5 Requête sql avec filtre
dbGetQuery(conn = conn,
           "SELECT * FROM popnaiss_com
           WHERE codgeo='35238'"
           )

# 6 Affichage avec jointure
DBI::dbListFields(conn,"popnaiss_com")
DBI::dbListFields(conn,"bpe21_metro")
dbGetQuery(conn = conn,
           "SELECT * FROM popnaiss_com
           INNER JOIN bpe21_metro
           ON popnaiss_com.codgeo=bpe21_metro.depcom
           WHERE codgeo='35238'"
)
#' Le "tri" est géré par le serveur postgre et bénéficie de l'optimisation
#' des bases de données relationnelles
#' rmq: contient la geometry


# Réalisons des requêtes sql sur les serveurs, mais avec la syntaxe dplyr
library(dplyr)
library(dbplyr)

# Connexion à la table popnaiss
popnaiss<-tbl(conn,"popnaiss_com")
str(popnaiss) # ! ce n'est pas un data.frame
# C'est les coordonnées dans la base de données de la table d'intérêt

# Reprise de la question 5
popnaiss %>% 
  filter(codgeo=="35047") %>% 
  show_query()
# show_query affiche la traduction sql de notre requête


pop_bruz <- popnaiss %>% 
  filter(codgeo=="35047") %>% 
  collect()
# Le collect() correspond au dbGetQuery
# A partir du collect(), les données sont chargées en
# mémoire et traitées par R
str(pop_bruz)

# 7b
equipement<-tbl(conn, "bpe21_metro")
equip_bruz <- popnaiss %>% 
  inner_join(equipement,by=c("codgeo"="depcom")) %>% 
  filter(codgeo=="35047") %>% 
  show_query() %>% 
  collect()
  

##########
# Partie 3
##########
# 1

bpe21_metro_coord<-tbl(conn, "bpe21_metro")
DBI::dbListFields(conn,"bpe21_metro")
bpe_dep50<-bpe21_metro_coord %>% 
  filter(dep=='50') %>% 
  select(id,depcom,dom,sdom,typequ,geometry) %>% 
  # show_query %>% 
  collect()
str(bpe_dep50)
# tibble and dataframe, pas un sf

# 2
# bpe21_metro_coord_sf<-tbl(conn, "bpe21_metro")
# bpe_dep50_sf<-bpe21_metro_coord_sf %>% 
#   filter(dep=='50') %>% 
#   select(id,depcom,dom,sdom,typequ,geometry) %>% 
#   # show_query %>% 
#   collect()
#   sf::
#   str(bpe_dep50_sf)
# ne fonctionne pas, besoin de trouver fonction adequate
bpe_dep50_sf<-st_read(conn,query="
                      SELECT ID, DEPCOM, DOM, SDOM,TYPEQU, GEOMETRY
                      FROM bpe21_metro
                      WHERE DEP='50';")
str(bpe_dep50_sf)

# 3
st_crs(bpe_dep50_sf)
# Lambert 93
bpe21_01_coord<-tbl(conn,"bpe21_04")
st_crs(st_read(
  conn,
  query=
    "SELECT *
    FROM bpe21_04"
))
#UTM zone 405/ RGR92 / EPS: G2975
dbGetQuery(conn, "SELECT DISTINCT(ST_SRID(geometry)) FROM bpe21_04;")


# 4
st_crs(st_read(
  conn,
  query=
    "SELECT *
    FROM bpe21_04"
))

DBI::dbListFields(conn,"bpe21_metro")
bpe21_metro_coord %>% 
  group_by(reg) %>% 
  summarise(n_matern=count('D107')) %>% 
  arrange(desc(n_matern)) %>% 
  show_query() %>% 
  collect()

# 5 

cinemas_bpe<-sf::st_read(conn,query=
                           "SELECT *
                         FROM bpe21_metro
                         WHERE TYPEQU='F303'")
sorbonne_buffer<-data.frame(x=2.34297,y=48.84864) %>% 
  st_as_sf(coords=c("x","y"),crs=4326) %>%
  # utilise 4326 pour convertir coordonnées en point sf
  st_transform(2154) %>% 
  #convertit en lambert 93
  st_buffer(1000)
  # Rayon de 1000 mètres
str(sorbonne_buffer)
plot(sorbonne_buffer)
# On retrouve un polygone

cinema_1km_sorbonne_list<-st_within(cinemas_bpe,
                                    sorbonne_buffer)
str(cinema_1km_sorbonne_list)
cinema_1km_sorbonne_list
# Lorsque 1, le cinéma est dans le buffer, sinon empty
cinema_1km_sorbonne_list<-cinemas_bpe %>% 
  filter(lengths(cinema_1km_sorbonne_list)>0)
cinema_1km_sorbonne_list

#ou
cinema_1km_sorbonne_list2<-st_contains(sorbonne_buffer,
                                    cinemas_bpe)





