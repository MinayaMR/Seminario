#paquetes a instalar
library(ggplot2)
library(rjson)
library(readxl)
library(tidyverse)
library(jsonlite)
library(readr)

#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN ANDALUCÍA DEL 2007 AL 2021:(NO SE ENCUJENTRAN DATOS JSON)

defunciones_andalucia<- read_delim("C:/Users/34665/Downloads/SEMESTRE 5/FUENTE DE DATOS/MATERIAL SEMINARIO/series-2107089158sc_andalucia.csv", 
                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)


#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN ARAGÓN DEL 2007 AL 2021:(NO SE ENCUJENTRAN DATOS JSON)
defunciones_aragon<- read_delim("C:/Users/34665/Downloads/SEMESTRE 5/FUENTE DE DATOS/MATERIAL SEMINARIO/series956392681sc_aragon.csv", 
                                                           delim = ";", escape_double = FALSE, trim_ws = TRUE)

#DATOS DE DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS EN ARAGÓN DEL 2007 AL 2021:(NO SE ENCUJENTRAN DATOS JSON)
defunciones_asturias <- read_delim("C:/Users/34665/Downloads/SEMESTRE 5/FUENTE DE DATOS/MATERIAL SEMINARIO/series645472709sc_asturias.csv", 
                                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
#DATOS DE DEFUNCIONES POR ENFERMEDADES REPIRATORIAS EN EL PRINCIPADO DE ASTURIAS DEL 2007 AL 2021:
