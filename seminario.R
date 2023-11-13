#paquetes a instalar
library(ggplot2)
library(rjson)
library(readxl)
library(tidyverse)
library(jsonlite)
library(readr)

#CARGA DE DATOS DE DEFUNCIONES DE LAS CCAA QUE EMPIEZAN POR [A,B,C]:

#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN ANDALUCÍA DEL 2007 AL 2021:(NO SE ENCUJENTRAN DATOS JSON):(https://www.ine.es/consul/serie.do?d=true&s=ECM25824)

defunciones_andalucia<- read_delim("C:/Users/34665/Downloads/SEMESTRE 5/FUENTE DE DATOS/MATERIAL SEMINARIO/defunciones por ccaa/series-616844354sc_andalucia.csv", 
                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)


#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN ARAGÓN DEL 2007 AL 2021:(https://www.ine.es/consul/serie.do?d=true&s=ECM25823 )
defunciones_aragon<- read_delim("C:/Users/34665/Downloads/SEMESTRE 5/FUENTE DE DATOS/MATERIAL SEMINARIO/defunciones por ccaa/series-569481116sc_aragon.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN ARAGÓN DEL 2007 AL 2021:( https://www.ine.es/consul/serie.do?d=true&s=ECM25822 )
defunciones_asturias <- read_delim("C:/Users/34665/Downloads/SEMESTRE 5/FUENTE DE DATOS/MATERIAL SEMINARIO/defunciones por ccaa/series-1904166149sc_asturias.csv", 
                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN LAS ISLAS BALEARES DEL 2007 AL 2021:(https://www.ine.es/consul/serie.do?d=true&s=ECM25821 )
defunciones_baleares<- read_delim("C:/Users/34665/Downloads/SEMESTRE 5/FUENTE DE DATOS/MATERIAL SEMINARIO/defunciones por ccaa/series1289952461sc_baleares.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)
#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN LAS ISLAS CANARIAS DEL 2007 AL 2021:(https://www.ine.es/consul/serie.do?d=true&s=ECM25820)
defunciones_canarias<- read_delim("C:/Users/34665/Downloads/SEMESTRE 5/FUENTE DE DATOS/MATERIAL SEMINARIO/defunciones por ccaa/series1808648068sc_canarias.csv", 
                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)
#DATOS DE DEFUNCIONES POR ENFERMEMEDADES RESPIRATORIAS EN CANTABRIA DEL 2007 AL 2021:(https://www.ine.es/consul/serie.do?d=true&s=ECM25819)
defunciones_cantabria<- read_delim("C:/Users/34665/Downloads/series-2108428772sc.csv", 
                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)


#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN CASTILLA Y LEON DEL 2007 AL 2021: (https://www.ine.es/consul/serie.do?d=true&s=ECM25818)
defunciones_cyl<-read_delim("C:/Users/34665/Downloads/SEMESTRE 5/FUENTE DE DATOS/MATERIAL SEMINARIO/defunciones por ccaa/series702246534sc_cyl.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN CASTILLA LA MANCHA DEL 2007 AL 2021:(https://www.ine.es/consul/serie.do?d=true&s=ECM25817)
defunciones_clm<-read_delim("C:/Users/34665/Downloads/SEMESTRE 5/FUENTE DE DATOS/MATERIAL SEMINARIO/defunciones por ccaa/series-915120596sc_clm.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN CATALUÑA DEL 2007 AL 2021:(https://www.ine.es/consul/serie.do?d=true&s=ECM25816 )
defunciones_cat<-read_delim("C:/Users/34665/Downloads/SEMESTRE 5/FUENTE DE DATOS/MATERIAL SEMINARIO/defunciones por ccaa/series-1419254191sc_cat.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN LA COMUNIDAD VALENCIANA DEL 2007 AL 2021:(https://www.ine.es/consul/serie.do?d=true&s=ECM25815 )
defunciones_vlc<-read_delim("C:/Users/34665/Downloads/SEMESTRE 5/FUENTE DE DATOS/MATERIAL SEMINARIO/series-869225815sc_vlc.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN EXTREMADURA DEL 2007 AL 2021:( https://www.ine.es/consul/serie.do?d=true&s=ECM25814)
defunciones_extremadura<-read_delim("C:/Users/34665/Downloads/SEMESTRE 5/FUENTE DE DATOS/MATERIAL SEMINARIO/defunciones por ccaa/series-1186384838t_extremadura.csv", delim = "\t", escape_double = FALSE, trim_ws = TRUE)


#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN GALICIA DEL 2007 AL 2021:(https://www.ine.es/consul/serie.do?d=true&s=ECM25813)
defunciones_gal<-read_delim("C:/Users/34665/Downloads/SEMESTRE 5/FUENTE DE DATOS/MATERIAL SEMINARIO/defunciones por ccaa/series386793258sc_gal.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN LA COMUNIDAD DE MADRID DEL 2007 AL 2021:(https://www.ine.es/consul/serie.do?d=true&s=ECM25812 )
defunciones_mad<- read_delim("C:/Users/34665/Downloads/SEMESTRE 5/FUENTE DE DATOS/MATERIAL SEMINARIO/defunciones por ccaa/series119180366sc_mad.csv", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)

#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN LA REGIÓN DE MURCIA DEL 2007 AL 2021:(https://www.ine.es/consul/serie.do?d=true&s=ECM25811)
defunciones_mur<-read_delim("C:/Users/34665/Downloads/SEMESTRE 5/FUENTE DE DATOS/MATERIAL SEMINARIO/defunciones por ccaa/series-1650015035sc_murcia.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN LA COMUNIDAD FORAL DE NAVARRA DEL 2007 AL 2021:(https://www.ine.es/consul/serie.do?d=true&s=ECM25810)
defunciones_nvr<-read_delim("C:/Users/34665/Downloads/series-1724737377sc_nvr.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
#DATOS DE DEFEUNCIONES POR ENFERMEDADES RESPIRATORIAS EN EL PAÍS VASCO DEL 2007 AL 2021:(https://www.ine.es/consul/serie.do?d=true&s=ECM25809)
defunciones_euskadi<- read_delim("C:/Users/34665/Downloads/SEMESTRE 5/FUENTE DE DATOS/MATERIAL SEMINARIO/defunciones por ccaa/series137774152sc_euskadi.csv", 
                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)

#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN LA RIOJA DEL 2007 AL 2021:(https://www.ine.es/consul/serie.do?d=true&s=ECM25808)
defunciones_rioja<-read_delim("C:/Users/34665/Downloads/SEMESTRE 5/FUENTE DE DATOS/MATERIAL SEMINARIO/defunciones por ccaa/series-573723813sc_rioja.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)

  
  
#HASTA AQUI SE HAN CARGADO TODOS LOS DATOS DE DEFUNCIONES EN FORMATO CSV POR ENF.RESP EN TODAS LAS CCAA DE ESPAÑA SALVO LAS 2 CUIDADES AUTÓNOMAS DE CEUTA Y MELILLA
