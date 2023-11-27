#paquetes a instalar
library(ggplot2)
library(rjson)
library(readxl)
library(tidyverse)
library(jsonlite)
library(readr)

#CARGA DE DATOS DE DEFUNCIONES DE LAS CCAA QUE EMPIEZAN POR [A,B,C]:

#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN ANDALUCÍA DEL 2007 AL 2021:(NO SE ENCUJENTRAN DATOS JSON):(https://www.ine.es/consul/serie.do?d=true&s=ECM25824)

defunciones_andalucia<- read_delim("DATOS/series-616844354sc_andalucia.csv", 
                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)


#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN ARAGÓN DEL 2007 AL 2021:(https://www.ine.es/consul/serie.do?d=true&s=ECM25823 )
defunciones_aragon<- read_delim("DATOS/series-569481116sc_aragon.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN EL PRINCIPADO DE ASTURIAS DEL 2007 AL 2021:( https://www.ine.es/consul/serie.do?d=true&s=ECM25822 )
defunciones_asturias <- read_delim("DATOS/series-1904166149sc_asturias.csv", 
                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN LAS ISLAS BALEARES DEL 2007 AL 2021:(https://www.ine.es/consul/serie.do?d=true&s=ECM25821 )
defunciones_baleares<- read_delim("DATOS/series1289952461sc_baleares.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)
#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN LAS ISLAS CANARIAS DEL 2007 AL 2021:(https://www.ine.es/consul/serie.do?d=true&s=ECM25820)
defunciones_canarias<- read_delim("DATOS/series1808648068sc_canarias.csv", 
                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)
#DATOS DE DEFUNCIONES POR ENFERMEMEDADES RESPIRATORIAS EN CANTABRIA DEL 2007 AL 2021:(https://www.ine.es/consuserie.do?d=true&s=ECM25819)
defunciones_cantabria<- read_delim("DATOS/series771112492sc_cantabria.csv", 
                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)


#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN CASTILLA Y LEON DEL 2007 AL 2021: (https://www.ine.es/consul/serie.do?d=true&s=ECM25818)
defunciones_cyl<-read_delim("DATOS/series702246534sc_cyl.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN CASTILLA LA MANCHA DEL 2007 AL 2021:(https://www.ine.es/consul/serie.do?d=true&s=ECM25817)
defunciones_clm<-read_delim("DATOS/series-915120596sc_clm.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN CATALUÑA DEL 2007 AL 2021:(https://www.ine.es/consul/serie.do?d=true&s=ECM25816 )
defunciones_cat<-read_delim("DATOS/series-1419254191sc_cat.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN LA COMUNIDAD VALENCIANA DEL 2007 AL 2021:(https://www.ine.es/consul/serie.do?d=true&s=ECM25815 )
defunciones_vlc<-read_delim("DATOS/series-869225815sc_vlc.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN EXTREMADURA DEL 2007 AL 2021:( https://www.ine.es/consul/serie.do?d=true&s=ECM25814)
defunciones_extremadura<-read_delim("DATOS/series-1186384838t_extremadura.csv", delim = "\t", escape_double = FALSE, trim_ws = TRUE)


#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN GALICIA DEL 2007 AL 2021:(https://www.ine.es/consul/serie.do?d=true&s=ECM25813)
defunciones_gal<-read_delim("DATOS/series386793258sc_gal.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN LA COMUNIDAD DE MADRID DEL 2007 AL 2021:(https://www.ine.es/consul/serie.do?d=true&s=ECM25812 )
defunciones_mad<- read_delim("DATOS/series119180366sc_mad.csv", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)

#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN LA REGIÓN DE MURCIA DEL 2007 AL 2021:(https://www.ine.es/consul/serie.do?d=true&s=ECM25811)
defunciones_mrc<-read_delim("DATOS/series-1650015035sc_murcia.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN LA COMUNIDAD FORAL DE NAVARRA DEL 2007 AL 2021:(https://www.ine.es/consul/serie.do?d=true&s=ECM25810)
defunciones_nvr<-read_delim("DATOS/series-1724737377sc_nvr.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
#DATOS DE DEFEUNCIONES POR ENFERMEDADES RESPIRATORIAS EN EL PAÍS VASCO DEL 2007 AL 2021:(https://www.ine.es/consul/serie.do?d=true&s=ECM25809)
defunciones_euskadi<- read_delim("DATOS/series137774152sc_euskadi.csv", 
                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)

#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN LA RIOJA DEL 2007 AL 2021:(https://www.ine.es/consul/serie.do?d=true&s=ECM25808)
defunciones_rioja<-read_delim("DATOS/series-573723813sc_rioja.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)

  
  
#HASTA AQUI SE HAN CARGADO TODOS LOS DATOS DE DEFUNCIONES EN FORMATO CSV POR ENF.RESP EN TODAS LAS CCAA DE ESPAÑA SALVO LAS 2 CUIDADES AUTÓNOMAS DE CEUTA Y MELILLA
#AQUI SE EMPIEZAN A CARGAR LOS DOCUMENTOS RELACIONADOS A CALIDAD DEL AIRE POR ESPAÑA:

<<<<<<< HEAD
#Datos diarios calidad aire valencia
library(readr)
rvvcca <- read_delim("DATOS/rvvcca.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)


=======
library(readr)
c_aire_andalucia <- read_delim("DATOS CALIDAD DEL AIRE/calidad aire andalucia.csv",
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)

c_aire_baleares <- read_delim("DATOS CALIDAD DEL AIRE/calidad aire baleares.csv",
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)

c_aire_castilla_la_mancha <- read_delim("DATOS CALIDAD DEL AIRE/calidad aire castilla la mancha.csv",
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)

c_aire_cataluña <- read_delim("DATOS CALIDAD DEL AIRE/calidad aire cataluña.csv",
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)

c_aire_madrid <- read_delim("DATOS CALIDAD DEL AIRE/calidad aire madrid.csv",
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)

c_aire_murcia <- read_delim("DATOS CALIDAD DEL AIRE/calidad aire murcia.csv",
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)

c_aire_pais_vasco <- read_delim("DATOS CALIDAD DEL AIRE/calidad aire pais vasco.csv",
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)

c_aire_valencia <- read_delim("DATOS CALIDAD DEL AIRE/calidad aire valencia.csv",
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)

c_aire_aragon <- read_delim("DATOS CALIDAD DEL AIRE/calidad de aire aragon.csv",
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)

c_aire_asturias <- read_delim("DATOS CALIDAD DEL AIRE/calidad del aire baleares.csv",
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)

c_aire_castilla_leon <- read_delim("DATOS CALIDAD DEL AIRE/calidad del aire castilla y león.csv",
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)
>>>>>>> bdea6b1f95fe22fd92494e88d86937adc5551f96

library(readxl)
oms_datos_mundiales <- read_excel("DATOS/who_aap_2021_v9_11august2022.xlsx", 
                                                         sheet = "AAP_2022_city_v9")
library(dplyr)
tabla_españa<-oms_datos_mundiales %>% filter(`WHO Country Name` == "Spain")
tabla_españa

 
#PRUEBA PIPELINE :
#IDM stands for INDICE DE MORTALIDAD:
andalucia_data<-
  defunciones_andalucia %>% 
  select(.data=.,Valor3,Valor5,PERIODO,VALOR) %>% 
  rename(.data=.,CCAA=Valor3,CausadeMuerte=Valor5,Year=PERIODO,IDM=VALOR)

andalucia_data
#DATOS LIMPIADOS PARA ARÁGON:
aragon_data<-
  defunciones_aragon %>% 
  select(.data=.,Valor3,Valor5,PERIODO,VALOR) %>% 
  rename(.data=.,CCAA=Valor3,CausadeMuerte=Valor5,Year=PERIODO,IDM=VALOR)

#DATOS LIMPIADOS PARA ASTURIAS:
asturias_data<-
  defunciones_asturias %>% 
  select(.data=.,Valor3,Valor5,PERIODO,VALOR) %>% 
  rename(.data=.,CCAA=Valor3,CausadeMuerte=Valor5,Year=PERIODO,IDM=VALOR)

#DATOS LIMPIADOS PARA BALEARES
baleares_data<-
  defunciones_baleares %>%
  select(.data=.,Valor3,Valor5,PERIODO,VALOR) %>% 
  rename(.data=.,CCAA=Valor3,CausadeMuerte=Valor5,Year=PERIODO,IDM=VALOR)
  
#DATOS LIMPIADOS PARA CANARIAS
canarias_data<-
  defunciones_canarias %>% 
  select(.data=.,Valor3,Valor5,PERIODO,VALOR) %>% 
  rename(.data=.,CCAA=Valor3,CausadeMuerte=Valor5,Year=PERIODO,IDM=VALOR)

#DATOS LIMPIADOS PARA CANTABRIA
cantabria_data<-
  defunciones_cantabria %>% 
  select(.data=.,Valor3,Valor5,PERIODO,VALOR) %>% 
  rename(.data=.,CCAA=Valor3,CausadeMuerte=Valor5,Year=PERIODO,IDM=VALOR)

#DATOS LIMPIADOS PARA CATALUÑA
cat_data<-
  defunciones_cat %>% 
  select(.data=.,Valor3,Valor5,PERIODO,VALOR) %>% 
  rename(.data=.,CCAA=Valor3,CausadeMuerte=Valor5,Year=PERIODO,IDM=VALOR)

#DATOS LIMPIADOS PARA CASTILLA LA MANCHA
clm_data<-
  defunciones_clm %>% 
  select(.data=.,Valor3,Valor5,PERIODO,VALOR) %>% 
  rename(.data=.,CCAA=Valor3,CausadeMuerte=Valor5,Year=PERIODO,IDM=VALOR)

#DATOS LIMPIADOS PARA CYL
cyl_data<-
  defunciones_cyl %>% 
  select(.data=.,Valor3,Valor5,PERIODO,VALOR) %>% 
  rename(.data=.,CCAA=Valor3,CausadeMuerte=Valor5,Year=PERIODO,IDM=VALOR)

#DATOS LIMPIADOS PARA PAÍS VASCO
euskadi_data<-
  defunciones_euskadi %>% 
  select(.data=.,Valor3,Valor5,PERIODO,VALOR) %>% 
  rename(.data=.,CCAA=Valor3,CausadeMuerte=Valor5,Year=PERIODO,IDM=VALOR)

#DATOS LIMPIADOS EXTREMADURA
extremadura_data<-
  defunciones_extremadura %>% 
  select(.data=.,Valor3,Valor5,PERIODO,VALOR) %>% 
  rename(.data=.,CCAA=Valor3,CausadeMuerte=Valor5,Year=PERIODO,IDM=VALOR)

#DATOS LIMPIADOS GALICIA
gal_data<-
  defunciones_gal %>% 
  select(.data=.,Valor3,Valor5,PERIODO,VALOR) %>% 
  rename(.data=.,CCAA=Valor3,CausadeMuerte=Valor5,Year=PERIODO,IDM=VALOR)

#DATOS LIMPIADOS MADRID
mad_data<-
  defunciones_mad %>% 
  select(.data=.,Valor3,Valor5,PERIODO,VALOR) %>% 
  rename(.data=.,CCAA=Valor3,CausadeMuerte=Valor5,Year=PERIODO,IDM=VALOR)

#DATOS LIMPIADOS REGION DE MURCIA
mrc_data<-
  defunciones_mrc %>% 
  select(.data=.,Valor3,Valor5,PERIODO,VALOR) %>% 
  rename(.data=.,CCAA=Valor3,CausadeMuerte=Valor5,Year=PERIODO,IDM=VALOR)


#DATOS LIMPIADOS NAVARRA
nvr_data<-
  defunciones_nvr %>% 
  select(.data=.,Valor3,Valor5,PERIODO,VALOR) %>% 
  rename(.data=.,CCAA=Valor3,CausadeMuerte=Valor5,Year=PERIODO,IDM=VALOR)

#DATOS LIMPIADOS RIOJA
rioja_data<-
  defunciones_rioja %>% 
  select(.data=.,Valor3,Valor5,PERIODO,VALOR) %>% 
  rename(.data=.,CCAA=Valor3,CausadeMuerte=Valor5,Year=PERIODO,IDM=VALOR)

#DATOS LIMPIADOS C.VALENCIANA

vlc_datos<-
  defunciones_vlc %>% 
  select(.data=.,Valor3,Valor5,PERIODO,VALOR) %>% 
  rename(.data=.,CCAA=Valor3,CausadeMuerte=Valor5,Year=PERIODO,IDM=VALOR)

#HASTA AQUI SE HAN LIMPIADO TODAS LAS TABLAS DE DEFUNCIONES QUE VIENEN PARA CADA CCAA EXCEPTO LAS 2 CUIDADES AUTONOMAS DE CEUTA Y MELILLA

#SE EMPIEZA A HACER UN JOIN DE LAS TABLAS DE DEFUNCIONES PARA TODAS LAS CCAA PARA DEMOSTRARLAS EN UN UNICA TABLA:
#SE DEBEN REVISAR LOS DATOS PARA QUITAR LOS SEPARADORES DE MILES DE LOS MILES PORQUE CREARAN PROBLEMAS A LA HORA DE JUNTARLOS 

library(tidyverse)
vlc_rec<-
  vlc_datos %>%
  mutate(IDM=as.numeric(gsub("\\.","",IDM)))
STR
#tabla andalucia sin separador de miles:

andalucia_rec<-
  andalucia_data %>%
  mutate(IDM=as.numeric(gsub("\\.","",IDM)))

#tabla aragon sin separador de miles:

aragon_rec<-
  aragon_data %>% 
  mutate(IDM=as.numeric(gsub("\\.","",IDM)))

#tabla asturias sin separador de miles

asturias_rec<-
  asturias_data %>% 
  mutate(IDM=as.numeric(gsub("\\.","",IDM)))


#tabla canarias sin separador de miles:

canarias_rec<-
canarias_data %>% 
  mutate(IDM=as.numeric(gsub("\\.","",IDM)))

#tabla cataluña sin separador de miles:
cat_rec<-
cat_data %>% 
  mutate(IDM=as.numeric(gsub("\\.","",IDM)))

#tabla la mancha sin separador de miles:
clm_rec<-
  clm_data %>% 
  mutate(IDM=as.numeric(gsub("\\.","",IDM)))

#tabla cyl sin separador de miles:
cyl_rec<-
  cyl_data %>%
  mutate(IDM=as.numeric(gsub("\\.","",IDM)))

#tabla euskadi sin separador de miles:
euskadi_rec<-
  euskadi_data %>%
  mutate(IDM=as.numeric(gsub("\\.","",IDM)))


#tabla extremadura sin separador de miles:
extremadura_rec<-
  extremadura_data %>% 
  mutate(IDM=as.numeric(gsub("\\.","",IDM)))
  
  
#tabla galicia sin separador de miles:
gal_rec<-
  gal_data %>%
  mutate(IDM=as.numeric(gsub("\\.","",IDM)))

#tabla madrid sin separador de miles:

mad_rec<-
  mad_data %>%
  mutate(IDM=as.numeric(gsub("\\.","",IDM)))

#tabla murcia sin separador de miles:

mrc_rec<-
  mrc_data %>%
  mutate(IDM=as.numeric(gsub("\\.","",IDM)))

#FIN DE RECTIFICACION DE LAS TABLAS DE DEFUNCIONES  (ELIMINACION DE SEPARADOR DE MILES POR LAS TABLAS QUE LAS CONTENGAN) :

asturias_rec  
canarias_rec
rbind(asturias_rec,canarias_rec) 
  





  
