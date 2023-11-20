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

#Datos diarios calidad aire valencia
library(readr)
rvvcca <- read_delim("DATOS/rvvcca.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)



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

  








  
