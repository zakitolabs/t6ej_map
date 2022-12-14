# t6ej_map
##project evaluation maps
if(!require(pacman)){install.packages("pacman");
  library(pacman)}
p_load(sf, ggplot2,tmap, htmltools, leaflet, dplyr, tidycensus, tidyverse,htmlwidgets)

#census_api_key("df2f5878761e8a41acf659ae12051f018c362421")
#setwd("C:/oahumpo/others/adwuma/gwrc/samples/t6ej")
#common vars
geog <- c("block group")
state <- c("hi")
cty <- c("honolulu")###change this for each county

#2 disabled
disabled <- c("C21007_001E","C21007_005E", "C21007_008E", "C21007_012E", "C21007_015E", "C21007_020E", "C21007_023E", "C21007_027E", "C21007_030E")
disabled= get_acs(geography = geog, variables =disabled, state = state, county = cty, year=2018,output = "wide")
disabled <- disabled %>% mutate(pDis = (C21007_005E + C21007_008E + C21007_012E + C21007_015E + C21007_020E + C21007_023E + C21007_027E + C21007_030E)/C21007_001E*100)
disabled <- disabled %>% replace(is.na(.), 0) %>% select(GEOID,pDis)

#3 zveh
zveh <- c("B25044_001E","B25044_003E","B25044_010E")
zveh= get_acs(geography = geog, variables =zveh, state = state, county = cty, year=2018,output = "wide")
zveh <- zveh %>% mutate(pZero = (B25044_003E + B25044_010E)/B25044_001E*100)
zveh <- zveh %>% replace(is.na(.), 0) %>% select(GEOID,pZero)

#4 old
old <- c("B01001_001E","B01001_020E","B01001_021E","B01001_022E","B01001_023E","B01001_024E","B01001_025E","B01001_044E","B01001_045E","B01001_046E","B01001_047E","B01001_048E","B01001_049E")
old= get_acs(geography = geog, variables =old, state = state, county = cty,year=2018,output = "wide")
old <- old %>% mutate(pOld = (B01001_020E + B01001_021E + B01001_022E + B01001_023E +
                                B01001_024E + B01001_025E + B01001_044E + B01001_045E +
                                B01001_046E + B01001_047E + B01001_048E + B01001_049E)/B01001_001E*100)
old <- old %>% replace(is.na(.), 0) %>% select(GEOID,pOld)

#8 under 18
u18 <- c("B01001_001E","B01001_003E","B01001_004E",
         "B01001_005E","B01001_006E","B01001_027E",
         "B01001_028E","B01001_029E","B01001_030E")
u18= get_acs(geography = geog, variables =u18, state = state, county = cty,year=2018,output = "wide")
u18 <- u18 %>% mutate(pU18 = (B01001_003E + B01001_004E +
                                B01001_005E + B01001_006E + B01001_027E +
                                B01001_028E + B01001_029E + B01001_030E)/B01001_001E*100)
u18 <- u18 %>% replace(is.na(.), 0) %>% select(GEOID,pU18)

##get means
disabled %>% summarise(mean(pDis))
zveh %>% summarise(mean(pZero))
old %>% summarise(mean(pOld))
u18 %>% summarise(mean(pU18))

###change first letter of obj for each county and re-run
kd<-disabled %>% 
  mutate(dis = ifelse(pDis > 13.0, "Above Average", "Below Average")) 
kz<-zveh %>% 
  mutate(zero = ifelse(pZero > 9.41, "Above Average", "Below Average"))
ko<-old %>% 
  mutate(old = ifelse(pOld > 17.8, "Above Average", "Below Average"))
ku<-u18 %>% 
  mutate(u18 = ifelse(pU18 > 19.2, "Above Average", "Below Average"))

######################################################combine counties by factor
#dis4<-rbind(kd,sd,spd,cd)
#zer4<-rbind(kz,sz,spz,cz)
#old4<-rbind(ko,so,spo,co)
#und4<-rbind(ku,su,spu,cu)
#######################################################add geometry
#cty <- c("king george","spotsylvania","caroline","stafford")
va <- get_acs(year = 2018, survey = "acs5", geography = "block group", state = "hi", county = "honolulu",variables = "B01003_001",geometry = TRUE) %>% select(GEOID)
#####################################################join factors to geometry
disg<-inner_join(kd,va) %>% st_as_sf() %>% st_transform(4326)
zerg<-inner_join(kz,va)%>% st_as_sf() %>% st_transform(4326)
oldg<-inner_join(ko,va)%>% st_as_sf() %>% st_transform(4326)
undg<-inner_join(ku,va)%>% st_as_sf() %>% st_transform(4326)
##########################plot
pal1 <- colorFactor(c("#99CCFF", "#CCECFF"), domain = disg$dis)
pal2 <- colorFactor(c("#99CCFF", "#CCECFF"), domain = zerg$zero)
pal3 <- colorFactor(c("#99CCFF", "#CCECFF"), domain = oldg$old)
pal4 <- colorFactor(c("#99CCFF", "#CCECFF"), domain = undg$u18)

leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=disg,stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7,fillColor =pal1(disg$dis),group = "Disability") %>%
  addPolygons(data=zerg,stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7,fillColor =pal2(zerg$zero),group = "Carless") %>%  
  addPolygons(data=oldg,stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7,fillColor =pal3(oldg$old),group = "Elderly") %>%
  addPolygons(data=undg,stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7,fillColor =pal4(undg$u18),group = "Youth") %>%  
  
  addLegend("topright", pal = pal1,values = disg$dis,title = "EJ Threshold") %>%
  addLayersControl(overlayGroups =c("Disability","Carless","Elderly","Youth"), options = layersControlOptions(collapsed=FALSE))  %>%
  hideGroup(c("Carless","Elderly","Youth")) %>%
  htmlwidgets::onRender("
        function() {$('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\">EJ Measures</label>');}")  %>%
  fitBounds(-158.274219, 21.228911, -157.657611,21.736234) %>% 
  htmlwidgets::saveWidget(file = "t6ej.html")


