# git clone GIT_REPOSITORY
# git add *.R






# Si on veut enlever du git add
# git status
# git rm --cached *.xlsx *.shp *.shx *.zip
# etc

# Chargement des packages

library(readxl)
library(dplyr)
library(sf)
library(mapsf)
library(classInt)
library(leaflet)

# Import
carte_commune<-st_read("fonds/commune_francemetro_2021.shp",  options = "ENCODING=WINDOWS-1252")
pop_legales<-  read_excel("data/Pop_legales_2019.xlsx") %>% 
  mutate(
    COM=ifelse(substr(COM,1,2)=="75","75056",COM)
  ) %>% 
  # A ce niveau, plusieurs lignes pour 75056 les communes de Paris
  group_by(COM) %>% 
  summarise(PMUN19=sum(PMUN19), .groups = 'drop') # .groups défait les groupes


# Jointure
carte_ext<-carte_commune %>% 
  left_join(pop_legales,by=c("code"="COM")) %>% 
  mutate(densite=PMUN19/surf)
str(carte_ext)
summary(carte_ext$densite)
hist(carte_ext$densite)
boxplot(carte_ext$densite)

# Carte densité

plot(carte_ext[,"densite"],border=FALSE)

plot(carte_ext[,"densite"],breaks='quantile',nbreaks = 20, main='quantile break',border=F)

plot(carte_ext[,"densite"],breaks='jenks',main='jenks break', main='quantile break',border=FALSE)
# Maximise la variance interclasse
# plot(carte_ext[,"densite",border=FALSE],breaks='fisher', main='fisher break')

plot(carte_ext[,"densite"],breaks='sd', main='std error break',border=FALSE)

plot(carte_ext[,"densite"],breaks='box', main='box break',border=FALSE)

plot(carte_ext[,"densite"],breaks='pretty', main='pretty break',border=FALSE)
?plot

# Essai classInt
str(classIntervals(carte_ext$densite))
classIntervals(carte_ext$densite,style = 'quantile',nbreaks=20)
classIntervals(carte_ext$densite,style = 'jenks')
classIntervals(carte_ext$densite,style = 'sd')
classIntervals(carte_ext$densite,style = 'equal')
classIntervals(carte_ext$densite,style = 'pretty')


carte_ext<-carte_ext %>% 
  mutate(
    densite_cat= cut(
      densite,
      breaks = c(0,40,163.25,1000,8000,27310),
      include.lowest = TRUE,
      right = FALSE,
      ordered_result = TRUE) #représente un factor
  )
# 40 est la médiane
# la moyenne est autour de 163
# Distinguer les ville et périphéries (<8000) des grands centres urbains (>8000)

table(carte_ext$densite_cat)

pal1 <- RColorBrewer::brewer.pal(n = 5, name = "YlOrRd")

plot(carte_ext[,"densite_cat"], border=FALSE, pal=pal1)





objet_decoupe <- classIntervals(
  carte_ext$densite,
  style = "quantile",
  n = 5
)

###########
# Partie 2
###########

carte_dep<-read_sf("fonds/dep_francemetro_2021.shp", options = "ENCODING=WINDOWS-1252")
pauvrete<-read_excel("data/Taux_pauvrete_2018.xlsx")
str(carte_dep)
str(pauvrete)
dep_ext<-carte_dep %>% 
  left_join(pauvrete %>% select(-Dept),
            by=c("code"="Code"))
mer<-read_sf("fonds/merf_2021.shp", options = "ENCODING=WINDOWS-1252")


str(dep_ext)
boxplot(dep_ext$Tx_pauvrete)
hist(dep_ext$Tx_pauvrete)
mf_map(dep_ext, var="Tx_pauvrete",type='choro',breaks='quantile', border=FALSE)
mf_map(dep_ext, var="Tx_pauvrete",type='choro',breaks='fisher',nbreaks=4)
# fisher met mieux en avant les extrèmes dans ce cas


# pdf(file="macarte",width=9, height=1)

mf_map(dep_ext, var="Tx_pauvrete",
       type='choro',
       breaks=c(0,13,17,25,max(dep_ext$Tx_pauvrete)),
       leg_pos = NA)

# Zoom IdF
dep_paris<-dep_ext %>% 
  filter(code %in% c("75","92","93","94"))

mf_inset_on(
  x=dep_ext,
  pos = "topright",
  cex = .2
)
mf_init(dep_paris)
mf_map(
  dep_paris,
  var="Tx_pauvrete",
  type = 'choro',
  breaks=c(0,13,17,25,max(dep_ext$Tx_pauvrete)),
  leg_pos = NA,
  add=TRUE
  
)
# ajout label
mf_label(
  dep_paris,
  var="code",
  col="black"
)
#On quitte l'encadré
mf_inset_off()

# Ajout de la mer
mf_map(mer, var="Tx_pauvrete",
       type='choro',
       leg_pos = NA,
       add=TRUE)

# Rajoutons légende titre et source
mf_legend(
  type='choro',
  title = 'taux de pauvreté en %',
  val = c("","Moins de 13",'De 13 à 17',"De 17 à 23"),
  pal='Mint',
  pos='left'
)
mf_layout(
  title = "Taux de pauvreté par département en 2018",
  credits = "source: Insee - IGN - Insee - 2021"
)

# dev.off()






