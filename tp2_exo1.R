#st_read du package sf. Vous rajouterez
#également la commande suivante au sein de la fonction options = "ENCODING=WINDOWS-1252".
#Quelles informations apparaissent dans la console ?
library(sf)
library(dplyr)
library(units)

carte_commune<-st_read("fonds/commune_francemetro_2021.shp", options = "ENCODING=WINDOWS-1252")
#' nbre d'éléments
#' Simple feature collection with 34836 features and 16 fields
#' Format
#' Geometry type: MULTIPOLYGON
#' Limites de la carte 
#' Bounding box:  xmin: 99225.97 ymin: 6049647 xmax: 1242375 ymax: 7110480
#' Référence de la projection
#' Projected CRS: RGF93 / Lambert-93

summary(carte_commune)

# Nous trouvons des variable habituelles des datagrame (character, numerical)
# et une variable geometry contenant des multiplygon

View(carte_commune)
head(carte_commune, 10)$geometry
str(carte_commune)

st_crs(carte_commune)
#' Projection RFG93/Lambert-93

# Créer une table “communes_Bretagne” ne contenant que les communes bretones. Ne conserver que les
# colonnes (code, libelle, epc, dep, surf) en utilisant la fonction select() de dplyr. Votre table contient-elle
# uniquement les 5 variables sélectionnées ?

summary(as.factor(carte_commune$reg))
communes_Bretagne<-carte_commune %>% 
  filter(reg=='53') %>% 
  select(code,libelle,epc,dep,surf)

# La table contient les 5 variables sélectionnées + la geometry

str(communes_Bretagne)
# L'objet reste un objet sf

plot(communes_Bretagne) # Une carte par variable
plot(communes_Bretagne$surf)  # Un graphe surface*n.index
plot(communes_Bretagne[,c(5)]) # carte sur la variable surface

st_geometry(communes_Bretagne)  # extrait la variable geometry
plot(st_geometry(communes_Bretagne)) # crée une carte vierge

# Créer une variable de surface appelée “surf2” en utilisant les fonctions st_area() sur votre variable de
# geometry. En quelle unité est la variable créée 

communes_Bretagne<-communes_Bretagne %>% 
  mutate(surf2=st_area(geometry)) %>% 
  mutate(surf3=units::set_units(surf2,"km^2"))
str(communes_Bretagne$surf3)
# surf2 est donné en m^2
# surf3 est donné en km^2

# Les surfaces ne sont pas les même --> Evolution des frontières des communes d'une année à l'autre/ 
# prise en compte des lacs ?

dept_bretagne<-communes_Bretagne %>% 
  group_by(dep) %>% 
  summarise(superficie=sum(surf3))

plot(dept_bretagne[,2])

fond_carte<-st_union(summarize(communes_Bretagne%>% 
                                 group_by(dep) %>% 
                                 summarise(superficie=sum(surf3))
))
fond_carte<-st_union(summarize(communes_Bretagne
))
plot(fond_carte)

# Créer une table “centroid_dept_bretagne” contenant les centroïdes des départements bretons.






  