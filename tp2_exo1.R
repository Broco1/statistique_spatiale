# récupérer avec git clone $GIT_REPOSITORY


#st_read du package sf. Vous rajouterez
#également la commande suivante au sein de la fonction options = "ENCODING=WINDOWS-1252".
#Quelles informations apparaissent dans la console ?
library(sf)
library(dplyr)
library(units)
library(ggplot2)

carte_commune<-st_read("fonds/commune_francemetro_2021.shp", options = "ENCODING=WINDOWS-1252")
# Attention, il faut uploader tous le fichier zip

#' nbre d'éléments
#' Simple feature collection with 34836 features and 16 fields
#' Format
#' Geometry type: MULTIPOLYGON
#' Limites de la carte 
#' Bounding box:  xmin: 99225.97 ymin: 6049647 xmax: 1242375 ymax: 7110480
#' Référence de la projection
#' Projected CRS: RGF93 / Lambert-93
#' 
#' # pour récupérer un dataframe st_drop

summary(carte_commune)

# Nous trouvons des variable habituelles des datagrame (character, numerical)
# et une variable geometry contenant des multiplygon

#View(carte_commune)
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

# Les surfaces ne sont pas les même --> manque de précision des fonds de carte 
# (plus de précision pas nécessaire pour la représentation graphique)

dept_bretagne<-communes_Bretagne %>% 
  group_by(dep) %>% 
  summarise(superficie=sum(surf3))

plot(dept_bretagne[,2])

fond_carte1<-st_union(summarize(communes_Bretagne%>% 
                                 group_by(dep) %>% 
                                 summarise(superficie=sum(surf3))
))
fond_carte2<-st_union(summarize(communes_Bretagne
))
plot(fond_carte1)
plot(fond_carte1)
# Remarque le group_by et la fonction st_union réalisent la même population

# Créer une table “centroid_dept_bretagne” contenant les centroïdes des départements bretons.

?st_centroid
st_centroid(communes_Bretagne)
plot(st_centroid(communes_Bretagne %>% 
                   group_by(dep) %>% 
                   summarise(superficie=sum(surf3))) %>% 
       select(superficie))
centroid_dept_bretagne<-st_centroid(communes_Bretagne %>% 
                                      group_by(dep) %>% 
                                      summarise(superficie=sum(surf3)))
ggplot()+
  geom_sf(data=centroid_dept_bretagne, fill='steelblue',col='grey45')
ggplot()+
  geom_sf(data=communes_Bretagne, fill='steelblue',col='grey45')+
  theme_void()


str(centroid_dept_bretagne)
# La géométrie est un sfc_POINT ; ce sont des points
plot(dept_bretagne %>% 
       select(geometry))
plot(centroid_dept_bretagne %>% select(geometry), add=T)

ggplot()+
  geom_sf(data=dept_bretagne,fill='steelblue')+
  geom_sf(data=centroid_dept_bretagne)+
  theme_light()

# Ajouter le nom du départment dans le fond de centroïdes. La variable aura pour nom dept_lib.
# Plusieurs solutions sont possibles, la plus propre étant d’utiliser une petite table de passage et de la
# fusionner avec le fond de centroïdes.
transition_dept_nom<-data.frame(dep=c("22","29","35","56"),
                           dept_lib=c("Côtes d'Armor","Finistère","Ille-et-Villaine","Morbihan"))

centroid_dept_bretagne<-centroid_dept_bretagne %>% 
  left_join(transition_dept_nom,by=c("dep"="dep"))

ggplot()+
  geom_sf(data=dept_bretagne,fill='steelblue')+
  geom_sf(data=centroid_dept_bretagne)+
  #geom_sf_label(data=centroid_dept_bretagne, aes(label=dept_lib),size=4)+
  geom_sf_text(data=centroid_dept_bretagne, aes(label=dept_lib),size=2)+
  theme_light()

st_intersects(centroid_dept_bretagne,communes_Bretagne)
# Le premier centroid côtes d'armor est dans la commune 148 (ligne) au code Insee 22170
st_intersection(centroid_dept_bretagne,communes_Bretagne)
# Même résultat, mais avec des polygones plutôt que des listes

st_distance(centroid_dept_bretagne[centroid_dept_bretagne$dep=="22",],
            communes_Bretagne[communes_Bretagne$libelle=="Saint-Brieuc",])
# La distance en le centre des côtes d'armor et son chef lieu est d'environ 9 km
set_units(st_distance(centroid_dept_bretagne[centroid_dept_bretagne$dep=="22",],
                      communes_Bretagne[communes_Bretagne$libelle=="Saint-Brieuc",]),
          value = km)

buffer<-st_buffer(centroid_dept_bretagne,20000)
plot(buffer$geometry)
str(buffer)

st_intersection(buffer,
              communes_Bretagne %>% 
                filter(libelle%in%c("Saint-Brieuc","Quimper","Rennes","Vannes")))
