#paquetes a instalar
library(ggplot2)
library(rjson)
library(readxl)
library(tidyverse)
library(jsonlite)
library(readr)

#CARGA DE DATOS DE DEFUNCIONES DE LAS CCAA QUE EMPIEZAN POR [A,B,C]:

#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN ANDALUCÍA DEL 2007 AL 2021:(NO SE ENCUJENTRAN DATOS JSON)

defunciones_andalucia<- read_delim("C:/Users/34665/Downloads/SEMESTRE 5/FUENTE DE DATOS/MATERIAL SEMINARIO/defunciones por ccaa/series-616844354sc_andalucia.csv", 
                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)


#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN ARAGÓN DEL 2007 AL 2021:(NO SE ENCUJENTRAN DATOS JSON)
defunciones_aragon<- read_delim("C:/Users/34665/Downloads/SEMESTRE 5/FUENTE DE DATOS/MATERIAL SEMINARIO/defunciones por ccaa/series-569481116sc_aragon.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN ARAGÓN DEL 2007 AL 2021:(NO SE ENCUJENTRAN DATOS JSON)
defunciones_asturias <- read_delim("C:/Users/34665/Downloads/SEMESTRE 5/FUENTE DE DATOS/MATERIAL SEMINARIO/defunciones por ccaa/series-1904166149sc_asturias.csv", 
                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN LAS ISLAS BALEARES DEL 2007 AL 2021:
defunciones_baleares<- read_delim("C:/Users/34665/Downloads/SEMESTRE 5/FUENTE DE DATOS/MATERIAL SEMINARIO/defunciones por ccaa/series1289952461sc_baleares.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)
#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN LAS ISLAS CANARIAS DEL 2007 AL 2021:
defunciones_canarias<- read_delim("C:/Users/34665/Downloads/SEMESTRE 5/FUENTE DE DATOS/MATERIAL SEMINARIO/defunciones por ccaa/series1808648068sc_canarias.csv", 
                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)
#DATOS DE DEFUNCIONES POR ENFERMEMEDADES RESPIRATORIAS EN CANTABRIA DEL 2007 AL 2021:
defunciones_cantabria<- read_delim("C:/Users/34665/Downloads/series-2108428772sc.csv", 
                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN CASTILLA Y LEON DEL 2007 AL 2021:
defunciones_cyl<-read_delim("C:/Users/34665/Downloads/SEMESTRE 5/FUENTE DE DATOS/MATERIAL SEMINARIO/defunciones por ccaa/series702246534sc_cyl.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN CASTILLA LA MANCHA DEL 2007 AL 2021:
defunciones_clm<-read_delim("C:/Users/34665/Downloads/SEMESTRE 5/FUENTE DE DATOS/MATERIAL SEMINARIO/defunciones por ccaa/series-915120596sc_clm.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN CATALUÑA DEL 2007 AL 2021:
defunciones_cat<-read_delim("C:/Users/34665/Downloads/SEMESTRE 5/FUENTE DE DATOS/MATERIAL SEMINARIO/defunciones por ccaa/series-1419254191sc_cat.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN LA COMUNIDAD VALENCIANA DEL 2007 AL 2021:
defunciones_vlc<-read_delim("C:/Users/34665/Downloads/SEMESTRE 5/FUENTE DE DATOS/MATERIAL SEMINARIO/series-869225815sc_vlc.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

defunciones_andalucia
defunciones_aragon
defunciones_asturias
defunciones_baleares
defunciones_canarias
defunciones_cantabria
defunciones_cyl

defunciones_clm
defunciones_cat
defunciones_vlc
