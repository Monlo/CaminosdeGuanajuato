rm(list=ls())
setwd("~")

# Cargar Paquetes 
require(foreign)
require(readxl)
require(tidyverse)
require(stringr)
require(mxmaps)
require(dplyr)
require(plyr)
require(ggplot2)

#################################
###   CAMINOS DE GUANAJUATO   ###
#################################

# 1. ¿Como ha evolucionado la tasa de investigaciones por homicidios dolosos 
#    en relación a la nacional?
#    Fuente de datos: Incidencia delictiva del fuero común (SESNSP)
#    Periodo: 1997-2019

#### Cargar las Bases de Datos ####

# Incidencia delictiva del Fuero Común, SESNSP (1997-2017)
inp = "C://Users//lopez//OneDrive//Documentos//Bases de datos//SESNSP//Nacional" 
out = "C://Users//lopez//OneDrive//Documentos//Artículos//caminos_de_guanajuato//out"
estatal1997_2017<- read_xlsx(paste(inp, "IncidenciaDelictiva_FueroComun_Estatal_1997_2017.xlsx", sep="/"))

# Incidencia delictiva del Fuero Común, SESNSP (2015-2019)
inp2 = "C://Users//lopez//OneDrive//Documentos//Bases de datos//SESNSP//Incidencia delictiva//Fuero común" 
estatal2015_2019<- read_xlsx(paste(inp2, "Estatal Delitos - enero 2019.xlsx", sep="/"))

# Proyecciones de población a mitad de año de CONAPO
# Disponible en https://datos.gob.mx/busca/dataset/proyecciones-de-la-poblacion-de-mexico-y-de-las-entidades-federativas-2016-2050
inp3 = "C://Users//lopez//OneDrive//Documentos//Artículos//Caminos de Guanajuato//input" 
pob_mit<- read.csv(paste(inp3, "proyecciones_pob.csv", sep="/"))
### Limpiar las tablas ###

# Emparejo los nombres
nuevos_nombres <- c("AÑO", "INEGI", "ENTIDAD", "bien_juri", "tipo_delito", "subtipo_delito",
                    "modalidad", "enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto",
                    "septiempre", "octubre", "noviembre", "diciembre", "total")

names(estatal2015_2019)<- nuevos_nombres

# Pasar a Mayúsculas los estados
estatal2015_2019$ENTIDAD<- toupper(estatal2015_2019$ENTIDAD) 

### Calcular tasas de homicidios dolosos ###

# Filtrar por homicidios, agrupar y calcular tasas para el periodo1 (1997-2017)
hom_period1<- filter(estatal1997_2017, TIPO=="DOLOSOS")
hom_period1<- filter(hom_period1, AÑO!="2017" & AÑO!="2016" & AÑO!="2015") 

hom_period1 = group_by(hom_period1, AÑO, ENTIDAD, INEGI) %>%
              summarize(tot = sum(TOTAL, na.rm=T))

# Filtrar por homicidios, agrupar y calcular tasas para el periodo 2 (2015-2018)
hom_period2 = filter(estatal2015_2019, subtipo_delito=="Feminicidio" | 
                    subtipo_delito=="Homicidio doloso") %>% 
                    group_by(AÑO, ENTIDAD, INEGI) %>%
                    summarize(tot = sum(total, na.rm=T))

# Junto las tablas de Periodo 1 y Periodo 2
periodo_final <- rbind.fill(hom_period1,hom_period2)

# Le pego la población estatal para calcular las tasas
pob_mit<- select(pob_mit, "AÑO", "ENTIDAD", "CVE_GEO", "POB_MIT_AÑO")
pob_mit$ENTIDAD <- as.character(pob_mit$ENTIDAD)
periodo_final <- left_join(periodo_final, pob_mit, by=c("AÑO"="AÑO", "INEGI"="CVE_GEO")) %>% 
                  mutate(tasa_total = round(tot / POB_MIT_AÑO * 100000,1))

# Le cambio los nombres y acomodo la base final
names(periodo_final)<- c("year", "ENTIDAD", "INEGI", "TOTAL", "Entidad", "POB", "TASA_TOTAL")
periodo_final<- select(periodo_final, year, INEGI, ENTIDAD, POB, TOTAL, TASA_TOTAL)

# Quito 2019 para solo graficar anuales
periodo_final<- filter(periodo_final, year!="2019")

## Comparar la tasa nacional con la del estado

## Tasa de investigaciones iniciadas por homicidios dolosos y feminicidios Nacional
nacional = group_by(periodo_final, year) %>% 
          summarize(tot = sum(TOTAL, na.rm=T))

# Junto la población
pob_mit_nac<- filter(pob_mit, ENTIDAD=="República Mexicana") %>%
              filter(AÑO>= 1997 & AÑO< 2019) %>%
              left_join(nacional, by=c("AÑO"="year"))  

# Calculo tasa nacional
nacional <- mutate(pob_mit_nac, tasa_total = round(tot / POB_MIT_AÑO * 100000,1))

## Tasa de investigaciones iniciadas por homicidios dolosos y feminicidios en Guanajuato

# Filtro periodo final para el estado 
Guanajuato<- filter(periodo_final, INEGI=="11")

# Emparejo las tablas y nombres
nacional<- select(nacional, AÑO, ENTIDAD, tasa_total)
nacional$ENTIDAD = gsub("República Mexicana", "Nacional", nacional$ENTIDAD) 
Guanajuato<- select(Guanajuato, year, ENTIDAD, TASA_TOTAL)
names(Guanajuato) <- c("AÑO", "ENTIDAD", "tasa_total")  
Guanajuato$ENTIDAD = gsub("GUANAJUATO", "Guanajuato", Guanajuato$ENTIDAD) 

# Junto Guanajuato y nacional
junta <- rbind.fill(nacional,Guanajuato)

### Graficar ###

ggplot(junta) +
  geom_line(aes(x=AÑO, y=tasa_total, color=ENTIDAD), size=1) +
  scale_color_manual(values=c("#a50f15","#225ea8")) +
  labs(title ="Tasa de Investigaciones Iniciadas por Homicidios dolosos", 
       subtitle="Por cada cien mil habitantes", x = "", 
       y = "Tasa de homicidios dolosos", caption = "Fuente: SESNSP", color ="Tasa") +
  theme_minimal() +
  scale_y_continuous(breaks=seq(from=0, to=100, by=10)) + 
  scale_x_continuous(breaks=seq(from=1997, to=2018, by=1)) + 
  theme(axis.text.x = element_text(face="bold", size = 10, angle=90, hjust=1),
        title  = element_text(face="bold", size=14)) 
ggsave(paste(out, "tasa_homicidios.png", sep="/"), width=12, height=12) 

# Guardo las tablas
write.csv(periodo_final, paste(out, "tasa_homicidios_estatal.csv", sep="//"), row.names = F, fileEncoding ="UTF-8")
write.csv(junta, paste(out, "tasa_homicidios_nacional.csv", sep="//"), row.names = F, fileEncoding ="UTF-8")

# Limpio la consola
rm(list=ls())
setwd("~")

# 2. ¿Como ha evolucionado la tasa de investigaciones por homicidios dolosos 
#    en a nivel municipal?
#    Fuente de datos: Incidencia delictiva del fuero común Municipal (SESNSP)
#    Periodo: 2015-2019
#    Link de descarga: 

# Cargo los nuevos directorios
inp = "C://Users//lopez//OneDrive//Documentos//Artículos//Caminos de Guanajuato//input" 
out = "C://Users//lopez//OneDrive//Documentos//Artículos//caminos_de_guanajuato//out"

# Leo la base de Incidencia Delictiva Municipal
# Disponible en: https://www.gob.mx/sesnsp/acciones-y-programas/incidencia-delictiva-87005?idiom=es 
muni<- read.csv(paste(inp, "mun_deli.csv", sep="/"), as.is=T, stringsAsFactors = F, sep=";")

# Cambio nombres
names(muni) <- c("year", "cve_ent", "ent", "cve_muni", "muni", "bien_juri", "tipo_delito", "subtipo_delito",
                    "modalidad", "enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto",
                    "septiempre", "octubre", "noviembre", "diciembre", "total")

# Filtro por Homicidios dolosos y feminicidios
hom = filter(muni, subtipo_delito=="Feminicidio" | subtipo_delito=="Homicidio doloso") 

# Homicidios dolosos y feminicidios por año y por municipio
hom_muni = group_by(hom, year, cve_ent, ent, cve_muni, muni) %>%
           summarize(tot = sum(total, na.rm=T))

# Le pego la población municipal para calcular las tasas

# Utilicé la información de población de Diego del Valle Jones
# Disponible en: https://github.com/diegovalle/conapo-2010/tree/master/clean-data
muni_pob<- read.csv(paste(inp, "municipio-population1990-2030.csv", sep="/"))

# Filtro por Total
muni_pob<- filter(muni_pob, Sex=="Total")

# Le juntamos homicidios y población
junta = full_join(hom_muni, muni_pob, by=c("year"="Year", "cve_muni"="Code"))

# Dejamos sólo las variables que nos interesan
homicidios_municipal<- select(junta, "year", "cve_ent", "ent", 
                              "cve_muni", "muni", "tot", "Population")  

# Calculamos las tasas municipales
homicidios_municipal <- mutate(homicidios_municipal, tasa_total = tot / Population * 100000)

# Filtro por Estado de Guanajuato
gto = filter(homicidios_municipal, cve_ent=="11") 
names(gto)<- c("year", "cve_ent", "ent", "region", "muni", "tot", "Population", "value")

# Guardo la tabla
write.csv(gto, paste(out, "tasa_homicidios_municipal.csv", sep="//"), row.names = F, fileEncoding ="UTF-8")

## Calcular los cambios porcentuales (2015-2018)
gto<-ungroup(gto)
gto<- select(gto, "year", "cve_ent", "ent", "region", "muni", "value") %>%
      spread(year, value)  

# Guardamos la tabla
write.csv(gto, paste(out, "muni_homi_year.csv", sep="//"), row.names = F, fileEncoding ="UTF-8")

# La abrimos (Hice esto porque por alguna razon no puedo calcular el cambio porcentual despues de hacer el spread)
gto <- read.csv(paste(out, "muni_homi_year.csv", sep="/")) 
gto<- mutate(gto, per_change= ((X2018- X2015)/ X2015) *100) 

#Filtro Nas 
gto<- filter(gto, per_change !="NaN")
gto<- filter(gto, per_change !="Inf")

# Para hacer los mapas, utilicé el paquete mxmaps de Diego del Valle Jones
# Disponible en https://github.com/diegovalle/mxmaps

# Me quedo con la región y el valor del mapa
tempo<-ungroup(tempo)
tempo<- select(gto, region, per_change) 
names(tempo)<- c("region", "value")

#mapeamos
mxmunicipio_choropleth(tempo, 
                       num_colors = 5,
                       zoom = c(11001:11046),
                       show_states = FALSE,
                       legend = "Tasas",
                       title = "Cambio porcentual en las tasas de Investigaciones Iniciadas por Homicidios Dolosos (2015-2018)") +
  scale_fill_brewer(palette="OrRd") +
  labs(caption = "Fuente: SESNSP")
