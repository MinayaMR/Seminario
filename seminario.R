#paquetes a instalar
library(ggplot2)
library(rjson)
library(readxl)
library(tidyverse)
library(jsonlite)
library(readr)
library(dplyr)
library(readODS)

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
#TRATAMIENTO DE LAS TABLAS Y GRÁFICAS:
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


#DATOS LIMPIADOS NAVARRA:
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

#SE VAN A JUNTAR TODAS LA TABLAS (UNION)  DE DEFUNCIONES PARA TODAS LAS CCAA PARA MOSTRARLAS EN UN UNICA TABLA:
#SE DEBEN REVISAR LOS DATOS PARA QUITAR LOS SEPARADORES DE MILES DE LOS MILES PORQUE CREARAN PROBLEMAS A LA HORA DE JUNTARLOS 
#PARA QUE LOS DATOS SEAN COHERENTES,TENEMOS QUE CAMBIAR LOS DATOS QUE VIENEN SEPARADOS POR PUNTOS,LO CONSIDERAREMOS COMO MILES Y NO COMO INDICES COMO NO TENEMOS INDICADORES DEL CONTRARIO:

library(tidyverse)
vlc_rec<-
  vlc_datos %>%
  mutate(IDM=as.numeric(gsub("\\.","",IDM)))
str
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

#LA TABLA SIGUIENTE CONTIENE TODOS LOS DATOS DE DEFUNCIONES PARA CADA CCAA EN EL PERIODO DE 2007 HASTA 2021:(UNION DE LAS TABLAS):
total_defunciones_CCAA<-
  rbind(andalucia_rec,aragon_rec,asturias_rec,baleares_data,canarias_rec,cantabria_data,cat_rec,clm_rec,cyl_rec,euskadi_rec,extremadura_rec,gal_rec,mad_rec,mrc_rec,nvr_data,rioja_data,vlc_rec) %>% 
  arrange(.data=.,desc(IDM))

#POR AÑOS:
#2007:
mortalidad_07<-total_defunciones_CCAA %>% 
  filter(.data=.,Year==2007) %>% 
  arrange(.data=.,desc(IDM)) %>% 
  mutate(.data=.,IDM_nacional=sum(IDM))
#2008:
mortalidad_08<-total_defunciones_CCAA %>% 
  filter(.data=.,Year==2008) %>% 
  arrange(.data=.,desc(IDM)) %>% 
  mutate(.data=.,IDM_nacional=sum(IDM))
#2009:
mortalidad_09<-total_defunciones_CCAA %>% 
  filter(.data=.,Year==2009) %>% 
  arrange(.data=.,desc(IDM)) %>% 
  mutate(.data=.,IDM_nacional=sum(IDM))
#2010:
mortalidad_10<-total_defunciones_CCAA %>% 
  filter(.data=.,Year==2010) %>% 
  arrange(.data=.,desc(IDM)) %>% 
  mutate(.data=.,IDM_nacional=sum(IDM))
#2011:
mortalidad_11<-total_defunciones_CCAA %>% 
  filter(.data=.,Year==2011) %>% 
  arrange(.data=.,desc(IDM)) %>% 
  mutate(.data=.,IDM_nacional=sum(IDM))

#2012:
mortalidad_12<-total_defunciones_CCAA %>% 
  filter(.data=.,Year==2012) %>% 
  arrange(.data=.,desc(IDM)) %>% 
  mutate(.data=.,IDM_nacional=sum(IDM))
#2013
mortalidad_13<-total_defunciones_CCAA %>% 
  filter(.data=.,Year==2013) %>% 
  arrange(.data=.,desc(IDM)) %>% 
  mutate(.data=.,IDM_nacional=sum(IDM))
#2014:
mortalidad_14<-total_defunciones_CCAA %>% 
  filter(.data=.,Year==2014) %>% 
  arrange(.data=.,desc(IDM)) %>% 
  mutate(.data=.,IDM_nacional=sum(IDM))

#2015:
mortalidad_15<-total_defunciones_CCAA %>% 
  filter(.data=.,Year==2015) %>% 
  arrange(.data=.,desc(IDM)) %>% 
  mutate(.data=.,IDM_nacional=sum(IDM))
#2016:
mortalidad_16<-total_defunciones_CCAA %>% 
  filter(.data=.,Year==2016) %>% 
  arrange(.data=.,desc(IDM)) %>% 
  mutate(.data=.,IDM_nacional=sum(IDM))

#2017:
mortalidad_17<-total_defunciones_CCAA %>% 
  filter(.data=.,Year==2017) %>% 
  arrange(.data=.,desc(IDM)) %>% 
  mutate(.data=.,IDM_nacional=sum(IDM))

#2018:
mortalidad_18<-total_defunciones_CCAA %>% 
  filter(.data=.,Year==2018) %>% 
  arrange(.data=.,desc(IDM)) %>% 
  mutate(.data=.,IDM_nacional=sum(IDM))
#2019:
mortalidad_19<-total_defunciones_CCAA %>% 
  filter(.data=.,Year==2019) %>% 
  arrange(.data=.,desc(IDM)) %>% 
  mutate(.data=.,IDM_nacional=sum(IDM))

#2020:
mortalidad_20<-total_defunciones_CCAA %>% 
  filter(.data=.,Year==2020) %>% 
  arrange(.data=.,desc(IDM)) %>% 
  mutate(.data=.,IDM_nacional=sum(IDM))
#2021:
mortalidad_21<-total_defunciones_CCAA %>% 
  filter(.data=.,Year==2021) %>% 
  arrange(.data=.,desc(IDM)) %>% 
  mutate(.data=.,IDM_nacional=sum(IDM))


#CCAA POR AÑOS:
#DEFUNCIONES ANDALUCIA:
#2007:
andalucia_def_2007<-
  andalucia_rec %>% filter(.data=.,Year==2007)

#2008:
andalucia_def_2008<-
  andalucia_rec %>% filter(.data=.,Year==2008)

#2009:
andalucia_def_2009<-
  andalucia_rec %>% filter(.data=.,Year==2009)

#2010:
andalucia_def_2010<-
  andalucia_rec %>% filter(.data=.,Year==2010)

#2011:
andalucia_def_2011<-
  andalucia_rec %>% filter(.data=.,Year==2011)
#2012:
andalucia_def_2012<-
  andalucia_rec %>% filter(.data=.,Year==2012)
#2013
andalucia_def_2013<-
  andalucia_rec %>% filter(.data=.,Year==2013)

#2014:
andalucia_def_2014<-
  andalucia_rec %>% filter(.data=.,Year==2014)

#2015
andalucia_def_2015<-
  andalucia_rec %>% filter(.data=.,Year==2015)

#2016:
andalucia_def_2016<-
  andalucia_rec %>% filter(.data=.,Year==2016)

#2017:
andalucia_def_2017<-
  andalucia_rec %>% filter(.data=.,Year==2017)

#2018:
andalucia_def_2018<-
  andalucia_rec %>% filter(.data=.,Year==2018)

#2019:
andalucia_def_2019<-
  andalucia_rec %>% filter(.data=.,Year==2019)

#2020:
andalucia_def_2020<-
  andalucia_rec %>% filter(.data=.,Year==2020)

#2021:
andalucia_def_2021<-
  andalucia_rec %>% filter(.data=.,Year==2021)

library(ggplot2)

library(dplyr)


#GRÁFICOS POR AÑOS :

graph_2007<-ggplot(data=mortalidad_07, aes(x = factor(Year), y = IDM, fill = CCAA)) +
  geom_bar(stat = "identity",color="black", position = "dodge") +
  labs(title = "Comparación de Índice de Mortalidad por CCAA en 2007",
       x = "Año",
       y = "Índice de Mortalidad",
       fill = "CCAA") +
  theme_classic()

graph_2008<- ggplot(data=mortalidad_08, aes(x = factor(Year), y = IDM, fill = CCAA)) +
  geom_bar(stat = "identity",color="black", position = "dodge") +
  labs(title = "Comparación de Índice de Mortalidad por CCAA en 2008",
       x = "Año",
       y = "Índice de Mortalidad",
       fill = "CCAA") +
  theme_classic()

graph_2009<-ggplot(data=mortalidad_09, aes(x = factor(Year), y = IDM, fill = CCAA)) +
  geom_bar(stat = "identity",color="black", position = "dodge") +
  labs(title = "Comparación de Índice de Mortalidad por CCAA en 2009",
       x = "Año",
       y = "Índice de Mortalidad",
       fill = "CCAA") +
  theme_classic()

graph_2010<-ggplot(data=mortalidad_10, aes(x = factor(Year), y = IDM, fill = CCAA)) +
  geom_bar(stat = "identity",color="black", position = "dodge") +
  labs(title = "Comparación de Índice de Mortalidad por CCAA en 2010",
       x = "Año",
       y = "Índice de Mortalidad",
       fill = "CCAA") +
  theme_classic()

graph_2011<-
  ggplot(data=mortalidad_11, aes(x = factor(Year), y = IDM, fill = CCAA)) +
  geom_bar(stat = "identity",color="black", position = "dodge") +
  labs(title = "Comparación de Índice de Mortalidad por CCAA en 2011",
       x = "Año",
       y = "Índice de Mortalidad",
       fill = "CCAA") +
  theme_classic()

graph_2012<-
  ggplot(data=mortalidad_12, aes(x = factor(Year), y = IDM, fill = CCAA)) +
  geom_bar(stat = "identity", color="black",position = "dodge") +
  labs(title = "Comparación de Índice de Mortalidad por CCAA en 2012",
       x = "Año",
       y = "Índice de Mortalidad",
       fill = "CCAA") +
  theme_classic()

graph_2013<-
  ggplot(data=mortalidad_13, aes(x = factor(Year), y = IDM, fill = CCAA)) +
  geom_bar(stat = "identity", color="black",position = "dodge") +
  labs(title = "Comparación de Índice de Mortalidad por CCAA en 2013",
       x = "Año",
       y = "Índice de Mortalidad",
       fill = "CCAA") +
  theme_classic()

graph_2014<-
  ggplot(data=mortalidad_14, aes(x = factor(Year), y = IDM, fill = CCAA)) +
  geom_bar(stat = "identity", color="black",position = "dodge") +
  labs(title = "Comparación de Índice de Mortalidad por CCAA en 2014",
       x = "Año",
       y = "Índice de Mortalidad",
       fill = "CCAA") +
  theme_classic()

graph_2015<-
  ggplot(data=mortalidad_15, aes(x = factor(Year), y = IDM, fill = CCAA)) +
  geom_bar(stat = "identity", color="black",position = "dodge") +
  labs(title = "Comparación de Índice de Mortalidad por CCAA en 2015",
       x = "Año",
       y = "Índice de Mortalidad",
       fill = "CCAA") +
  theme_classic()

graph_2016<-
  ggplot(data=mortalidad_16, aes(x = factor(Year), y = IDM, fill = CCAA)) +
  geom_bar(stat = "identity", color="black",position = "dodge") +
  labs(title = "Comparación de Índice de Mortalidad por CCAA en 2016",
       x = "Año",
       y = "Índice de Mortalidad",
       fill = "CCAA") +
  theme_classic()


graph_2017<-
  ggplot(data=mortalidad_17, aes(x = factor(Year), y = IDM, fill = CCAA)) +
  geom_bar(stat = "identity", color="black",position = "dodge") +
  labs(title = "Comparación de Índice de Mortalidad por CCAA en 2017",
       x = "Año",
       y = "Índice de Mortalidad",
       fill = "CCAA") +
  theme_classic() 


graph_2018<-
  ggplot(data=mortalidad_18, aes(x = factor(Year), y = IDM, fill = CCAA)) +
  geom_bar(stat = "identity", color="black",position = "dodge") +
  labs(title = "Comparación de Índice de Mortalidad por CCAA en 2018",
       x = "Año",
       y = "Índice de Mortalidad",
       fill = "CCAA") +
  theme_classic()

graph_2019<-
  ggplot(data=mortalidad_19, aes(x = factor(Year), y = IDM, fill = CCAA)) +
  geom_bar(stat = "identity", color="black",position = "dodge") +
  labs(title = "Comparación de Índice de Mortalidad por CCAA en 2019",
       x = "Año",
       y = "Índice de Mortalidad",
       fill = "CCAA") +
  theme_classic()

graph_2020<-
  ggplot(data=mortalidad_20, aes(x = factor(Year), y = IDM, fill = CCAA)) +
  geom_bar(stat = "identity", color="black",position = "dodge") +
  labs(title = "Comparación de Índice de Mortalidad por CCAA en 2020",
       x = "Año",
       y = "Índice de Mortalidad",
       fill = "CCAA") +
  theme_classic()


graph_2021<-
  ggplot(data=mortalidad_21, aes(x = factor(Year), y = IDM, fill = CCAA)) +
  geom_bar(stat = "identity", color="black",position = "dodge") +
  labs(title = "Comparación de Índice de Mortalidad por CCAA en 2021",
       x = "Año",
       y = "Índice de Mortalidad",
       fill = "CCAA") +
  theme_classic()
#Modificar los nombres de las CCAA para que sean mas manejables a la hora de trabajar con los gráficos:
#Se cambian los nombres de las CCAA de nuestra tabla con un case when ,se añade otra columna con el promedio de IDM por CCAA a ,lo largo de los años representados:

ccaa_nuevos_nombres <- c("Madrid, Comunidad de" = "C. Madrid",
                         "Comunitat Valenciana" = "C.Valenciana",
                         "Castilla y León" = "CyL",
                         "Castilla - La Mancha" = "CLM",
                         "País Vasco" = "Euskadi",
                         "Asturias, Principado de"="Asturias",
                         "Murcia, Región de"="Murcia",
                         "Balears, Illes"="Baleares",
                         "Navarra, Comunidad Foral de"="Navarra",
                         "Rioja, La"="Rioja")
#  library(tidyverse)

total_defunciones_CCAA <-total_defunciones_CCAA %>%
  mutate(CCAA = case_when(
    CCAA %in% names(ccaa_nuevos_nombres) ~ ccaa_nuevos_nombres[CCAA],
    TRUE ~ CCAA
  ))  %>%
  group_by(CCAA) %>%
  drop_na() %>% 
  mutate(Promedio_IDM = mean(IDM))
#GRÁFICO DE BARRAS:EVOLUCION DE IDM POR CCAA POR EL PERIODO DE TIEMPO DADO[2007,2021]: 

IDM_CCAA_graph<-
  ggplot(data=total_defunciones_CCAA,aes(x=factor(Year),y=IDM,fill=CCAA,width=1.5))+
  geom_bar(stat = "identity",color="black", position = position_dodge(width = 0.7)) +
  labs(title = "Gráfico de Barras:Evolución de IDM por CCAA por el periodo de tiempo 2007 a 2021(ambos inclusivos)",
       x = "Años",
       y = "Índice de Mortalidad",
       fill = "CCAA") +
  theme_minimal()


#FIN TODO LO RELACIONADO CON DEFUNCIONES POR ENFERMEDADES RESPIRATORIAS:


#AQUI SE EMPIEZAN A CARGAR LOS DOCUMENTOS RELACIONADOS A CALIDAD DEL AIRE POR ESPAÑA:
#HEAD
#Datos diarios calidad aire valencia
library(readr)
c_aire_valencia <- read_delim("DATOS/rvvcca.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)



library(readr)
c_aire_andalucia <- read_delim("DATOS CALIDAD DEL AIRE/calidad aire andalucia.csv",
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)


#c_aire_baleares <- read_delim("DATOS CALIDAD DEL AIRE/calidad aire baleares.csv",
                             #delim = ";", escape_double = FALSE, trim_ws = TRUE)

#calidad_aire_baleares <- read_csv("DATOS CALIDAD DEL AIRE/calidad aire baleares.csv")


#c_aire_castilla_la_mancha <- read_delim("DATOS CALIDAD DEL AIRE/calidad aire castilla la mancha.csv",
                             # delim = ";", escape_double = FALSE, trim_ws = TRUE)

#c_aire_cataluña <- read_delim("DATOS CALIDAD DEL AIRE/calidad aire cataluña.csv",
                              #delim = ";", escape_double = FALSE, trim_ws = TRUE)
calidad_aire_cataluña <- read_csv("DATOS CALIDAD DEL AIRE/calidad aire cataluña.csv")


#c_aire_madrid <- read_delim("DATOS CALIDAD DEL AIRE/calidad aire madrid.csv",
                              #delim = ";", escape_double = FALSE, trim_ws = TRUE)


c_aire_murcia <- read_delim("DATOS CALIDAD DEL AIRE/calidad aire murcia.csv",
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)

#c_aire_pais_vasco <- read_delim("DATOS CALIDAD DEL AIRE/calidad aire pais vasco.csv",
                              #delim = ";", escape_double = FALSE, trim_ws = TRUE)

c_aire_valencia <- read_delim("DATOS CALIDAD DEL AIRE/calidad aire valencia.csv",
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)


calidad_de_aire_aragon <- read_csv("DATOS CALIDAD DEL AIRE/calidad de aire aragon.csv")


#c_aire_asturias <- read_delim("DATOS CALIDAD DEL AIRE/calidad del aire baleares.csv",
                              #delim = ";", escape_double = FALSE, trim_ws = TRUE)

c_aire_castilla_leon <- read_delim("DATOS CALIDAD DEL AIRE/calidad del aire castilla y leon.csv",
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)
calidad_del_aire_castilla_y_leon <- read_delim("DATOS CALIDAD DEL AIRE/calidad del aire castilla y leon.csv", 
                                               delim = ";", escape_double = FALSE, trim_ws = TRUE)

library(readr)

library(readxl)
oms_datos_mundiales <- read_excel("DATOS/who_aap_2021_v9_11august2022.xlsx", 
                                                         sheet = "AAP_2022_city_v9")
library(dplyr)
tabla_españa<-
  oms_datos_mundiales %>% 
  filter(`WHO Country Name` == "Spain")
tabla_españa


#Aqui comienza la limpieza de tablas relacionadas con la calidad del aire.
library(tidyverse)
andalucia_data_calidad<-
  c_aire_andalucia %>%
  drop_na() %>% 
  mutate(CCAA="Andalucia") %>% 
  select(.data=.,CCAA, F_FECHA, `'PM10'`, `'PM25'`, `'NO2'`, `'O3'`,`'SO2'`) %>% 
  rename(.data=.,FECHA=F_FECHA) 
  


cataluña_data_calidad<-
  calidad_aire_cataluña %>% 
  drop_na() %>% 
  mutate(CCAA="Cataluña") %>% 
  select(.data=.,CCAA, ANY, MES, MAGNITUD, `NOM CONTAMINANT`, UNITATS) %>% 
  rename(.data=.,AÑO=ANY, NOMBRE_CONTAMINANTE=`NOM CONTAMINANT`, UNIDADES=UNITATS)

#TABLA C.VALENCIANA PARA EL AÑO 2019 TRATADA Y LIMPIADA:
valencia_data_calidad_2019<-
  c_aire_valencia %>% 
  mutate(CCAA= "Valencia") %>% 
  select(.data=.,CCAA, fecha, pm1, pm2_5, pm10, no, no2, nox, o3) %>%
  drop_na() %>% 
  filter(substr(fecha, 1, 4) == "2019") %>% 
  summarise(.data=.,CCAA='Valencia',fecha='2019',across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

#TABLA C.VALENCIANA PARA EL AÑO 2020 TRATADA Y LIMPIADA:
valencia_data_calidad_2020<-
c_aire_valencia %>% 
  mutate(CCAA= "Valencia") %>% 
  select(.data=.,CCAA, fecha, pm1, pm2_5, pm10, no, no2, nox, o3) %>%
  drop_na() %>% 
  filter(substr(fecha, 1, 4) == "2020") %>% 
  summarise(.data=.,CCAA='Valencia',fecha='2020',across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

#TABLA C.VALENCIANA PARA EL AÑO 2021 TRATADA Y LIMPIADA:
valencia_data_calidad_2021<-
  c_aire_valencia %>% 
  mutate(CCAA= "Valencia") %>% 
  select(.data=.,CCAA, fecha, pm1, pm2_5, pm10, no, no2, nox, o3) %>%
  drop_na() %>% 
  filter(substr(fecha, 1, 4) == "2021") %>% 
  summarise(.data=.,CCAA='Valencia',fecha='2021',across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))



#limpieza de la tabla excel por ccaa.
datos_calidad_aire_galicia<-
  tabla_españa %>%
  filter(`City or Locality` %in% c("A Coruna", "As Pontes De Garcia Rodriguez","Cee","Arteixo","Cambre","Cervo","Dumbria","Ferrol","Lousame","Lugo","Muras","Neda","Ordes","Oural","Ourense","Pastoriza","Pontevedra","Santiago De Compostela","Savinao (O)","Traslodeiro","Vigo","Vilalba","Xove")) %>% 
  select(.data=.,`City or Locality`:`NO2 temporal coverage (%)`) %>% 
  drop_na()
  


datos_calidad_aire_andalucia<-
  tabla_españa %>% 
  filter(`City or Locality` %in% c("Alcala De Guadaira","Algar","Algeciras","Almeria","Almonte","Arcos De La Frontera","Bailen","Barriada Rio San Pedro","Bedar","Benahadux","Cadiz","Campillos","Campohermoso","Carboneras","Cordoba","Cuevas Del Almanzora","Dos Hermanas","El Ejido","Espiel","Estacion Ferrea","Granada","Guillena","Huelva","Jaen","Jerez De La Frontera","La Linea De La Concepcion","Los Barrios","Lucena Del Puerto","Mairena Del Aljarafe","Malaga","Marbella","Moguer","Mojacar","Motril","Niebla","Nijar","Obejo","Ogijares","Palmones","Palos De La Frontera","Prado Del Rey","Puerto Real","Punta Umbria","San Fernando","San Juan Del Puerto","San Nicolas Del Puerto","San Roque","Sevilla","Torredonjimeno","Venta Gaspar","Villaharta","Villanueva Del Arzobispo","Viznar","Zona De Los Principes")) %>% 
  select(.data=.,`City or Locality`:`NO2 temporal coverage (%)`)

datos_calidad_aire_aragon<-
  tabla_españa %>% 
  filter(`City or Locality` %in% c("Alagon","Alcaniz","Alcorisa","Bujaraloz","Calanda","Caspe","Cerollera (La)","El Pueyo De Araguas","Escatron","Foz Calanda","Huesca","La Cerollera","Monzon","Puig Moreno","San Jorge","Sarinena","Sastago","Teruel","Torrevelilla","Utrillas","Zaragoza")) %>%
  select(.data=.,`City or Locality`:`NO2 temporal coverage (%)`)
  

datos_calidad_aire_baleares<-
  tabla_españa %>% 
  filter(`City or Locality` %in% c("Alcudia","Bunyola","Ciutadella De Menorca","Eivissa","Fortelasa De Isabel Ii","Mao","Palma De Mallorca","Pollenca","Sa Pobla","Sant Antoni De Portmany","Santa Eulalia Del Rio","Trepuco")) %>%
  select(.data=.,`City or Locality`:`NO2 temporal coverage (%)`)

datos_calidad_aire_canarias<-
  tabla_españa %>% 
  filter(`City or Locality` %in% c("Aguimes","Arafo","Arico","Arona","Arrecife","Arucas","Balcon De Telde","Brena Baja","Buzanada","Candelaria","Castillo Del Romeral","El Medano","Igueste De Candelaria","Las Palmas","Las Palmas De Gran Canaria","Los Realejos","Los Sauces","Palma","Puerto Del Rosario","San Bartolome De Tirajana","San Sebastian De La Gomera","Santa Cruz De La Palma","Santa Cruz De Tenerife","Teguise","Telde","Urbanizacion Tajao","Villa De Valverde")) %>% 
  select(.data=.,`City or Locality`:`NO2 temporal coverage (%)`)

datos_calidad_aire_cantabria<-
  tabla_españa %>% 
  filter(`City or Locality` %in% c("Astillero","Barreda","Camargo","Castro Urdiales","Los Corrales De Buelna","Los Tojos","Reinosa","Santander","Torrelavega")) %>% 
  select(.data=.,`City or Locality`:`NO2 temporal coverage (%)`)

datos_calidad_aire_castillamancha<-
  tabla_españa %>% 
  filter(`City or Locality` %in% c("Albacete","Azuqueca De Henares","Campisabalos","Cuenca","Guadalajara","Illescas","Puertollano","San Pablo De Los Montes","Talavera De La Reina","Toledo")) %>% 
  select(.data=.,`City or Locality`:`NO2 temporal coverage (%)`)

datos_calidad_aire_cyl<-
  tabla_españa %>% 
  filter(`City or Locality` %in% c("Aranda De Duero","Avila","Burgos","Buron","Cardenajimeno","Carracedelo","Congosto","Cuadros","El Maillo","Fabero","Guardo","La Robla","Leon","Lillo Del Bierzo","Medina De Pomar","Medina Del Campo","Miranda De Ebro","Muriel De La Fuente","Palencia","Penausende","Ponferrada","Salamanca","Segovia","Soria","Valladolid","Venta De Banos","Villalba De Guardo","Zamora")) %>% 
  select(.data=.,`City or Locality`:`NO2 temporal coverage (%)`)
  

datos_calidad_aire_cataluña<-
  tabla_españa %>% 
  filter(`City or Locality` %in% c("Alcanar","Alcover","Ametlla De Mar (L')","Amposta","Badalona","Barbera Del Valles","Barcelona","Bellver De Cerdanya","Berga","Cadaques","Calafat","Clariana","Constanti","Cornella De Llobregat","Cubelles","El Brull","El Prat De Llobregat","Els Monjos","Els Torms","Figols","Gava","Girona","Granollers","Hospitalet De Llobregat (L')","Igualada","L'ametlla De Mar","L'hospitalet De Llobregat","La Magina","La Rapita","Les Cases D'alcanar","Les Tres Cales","Lleida","Manlleu","Manresa","Martorell","Mataro","Mollet Del Valles","Montcada I Reixac","Perafort","Pont De Vilomara I Rocafort","Reus","Rubi","Sabadell","Sant Adria De Besos
","Sant Andreu De La Barca","Sant Celoni","Sant Cugat Del Valles","Sant Esteve De La Sarga","Sant Feliu De Llobregat","Sant Just Desvern","Sant Salvador","Sant Vicenc Dels Horts","Santa Coloma De Gramenet","Santa Perpetua De Mogoda","Sitges","Tarragona","Terrassa","Tona","Vallcebre","Vandellos","Vila-Seca","Viladecans","Vilafranca Del Penedes")) %>%  
  select(.data=.,`City or Locality`:`NO2 temporal coverage (%)`)
  

datos_calidad_aire_madrid<-
  tabla_españa %>% 
  filter(`City or Locality` %in% c("Alcala De Henares","Alcobendas","Alcorcon","Algete","Aranjuez","Arganda Del Rey","Cerceda","Ciudad Real","Collado Villalba","Coslada","Fuenlabrada","Getafe","Guadalix De La Sierra","Leganes","Madrid","Majadahonda","Mostoles","Orusco De Tajuna","Patones","Rivas-Vaciamadrid","San Martin De Valdeiglesias","Torrejon De Ardoz","Valdemoro","Villa Del Prado","Villarejo De Salvanes")) %>% 
  select(.data=.,`City or Locality`:`NO2 temporal coverage (%)`)

datos_calidad_aire_navarra<-
  tabla_españa %>% 
  filter(`City or Locality` %in% c("Arguedas","Funes","Navarra","Pamplona","Tudela")) %>% 
  select(.data=.,`City or Locality`:`NO2 temporal coverage (%)`)

datos_calidad_aire_valencia<-
  tabla_españa %>% 
  filter(`City or Locality` %in% c("Alacant","Albalat Dels Tarongers","Alcoi","Alcora (L')","Almassora","Alzira","Benicasim","	
Benidorm","Bunol","Burjassot","Burriana","Castello","Castellon De La Plana","Caudete De Las Fuentes","Cirat","Coratxa","El Grao De Castellon","Elche","Elda","Gandia","L'alcora","Morella","Onda","Paterna","Pinoso","Pobla De Benifassa","Port De Sagunt","Quart De Poblet","Sagunt","Salinas","San Isidro","Torrevieja","Valencia","Villafranca Del Cid","Villar Del Arzobispo","Vinaros","Viver","Zarra","Zorita Del Maestrazgo")) %>% 
  select(.data=.,`City or Locality`:`NO2 temporal coverage (%)`)

datos_calidad_aire_extremadura<-
  tabla_españa %>% 
  filter(`City or Locality` %in% c("Badajoz","Caceres","Merida","Oliva De Plasencia","Puebla De Sancho Perez","Toril","Zafra")) %>% 
  select(.data=.,`City or Locality`:`NO2 temporal coverage (%)`)

datos_calidad_aire_euskadi<-
  tabla_españa %>% 
  filter(`City or Locality` %in% c("Abanto Y Ciervana/Abanto Zierb","Aia","Arrasate","Arrigorriaga","Azpeitia","Barakaldo","Basauri","Beasain","Bernedo", "Bilbao","Biscay","Bizkaia","Donostia","Durango","Eltziego","Erandio","Errenteria","Gallarta","Hernani","Lalastra","Lantaron","Laudio","Lemoa","Mimetiz","Mundaka","Muskiz","Portugalete","Sondika","Tolosa","Vitoria-Gasteiz","Zalla")) %>% 
  select(.data=.,`City or Locality`:`NO2 temporal coverage (%)`)

datos_calidad_aire_asturias<-
  tabla_españa %>% 
  filter(`City or Locality` %in% c("Aviles","Cangas Del Narcea","Gijon","Langreo","Llanes","Lugones","Mieres","Oviedo","San Martin Del Rey Aurelio","Siero")) %>% 
  select(.data=.,`City or Locality`:`NO2 temporal coverage (%)`) 

datos_calidad_aire_murcia<-
  tabla_españa %>% 
  filter(`City or Locality` %in% c("Alcantarilla","Caravaca De La Cruz","Cartagena","La Aljorra","Lorca","Murcia")) %>% 
  select(.data=.,`City or Locality`:`NO2 temporal coverage (%)`)

datos_calidad_aire_rioja<-
  tabla_españa %>% 
  filter(`City or Locality` %in% c("Alfaro","Arrubal","El Rio","Galilea","Logrono","Pradejon")) %>%
  select(.data=.,`City or Locality`:`NO2 temporal coverage (%)`) 
 
  


 
print(tabla_españa)
#CALIDAD DE AIRE EN ANDALUCIA POR AÑOS:

## EN 2010:
calidad_aire_andalucia_2010<-
datos_calidad_aire_andalucia %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2010) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Andalucía') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)


##EN 2011:SE DEBEN BUSCAR OTROS DATOS PARA ESTE AÑO PORQUE DEVUELVE UN TIBBLE VACÍO:
calidad_aire_andalucia_2011<-
  datos_calidad_aire_andalucia %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2011) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Andalucía') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

##NUEVA TABLA ANDALUCIA CALIDAD 2011:
calidad_aire_andalucia_2011<-read_ods("DATOS CALIDAD DEL AIRE/andalucia_2003_2019.ods", sheet = "Andalucia_Parametro", skip = 1) %>%rename(CCAA = "Desag.Territorial", Cantidad_Toneladas = "X2011") %>%
  filter(contaminante %in% c("PM2,5 (t)", "PM10 (t)", "NOx (t)")) %>%
  mutate(`Measurement Year` = 2011,Cantidad = Cantidad_Toneladas * case_when(
      contaminante == "PM2,5 (t)" ~ 1e6 / 0.001,  
      contaminante == "PM10 (t)" ~ 1e6 / 0.001,  
      contaminante == "NOx (t)" ~ 1e6 / 1.88  
    )
  ) %>%
  select(CCAA, contaminante, `Measurement Year`, Cantidad) %>%
  pivot_wider(names_from = contaminante,values_from = Cantidad) %>%
  rename(.data=.,`PM2.5 (μg/m3)`=`PM2,5 (t)`,`PM10 (μg/m3)`=`PM10 (t)`,`NO2 (μg/m3)`=`NOx (t)`) %>% 
  relocate(`PM2.5 (μg/m3)`, .after = `Measurement Year`) %>% 
  relocate(`PM10 (μg/m3)`, .before = `NO2 (μg/m3)`)


#Calidad aire andalucia 2012
calidad_aire_andalucia_2012<-
  datos_calidad_aire_andalucia %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2012) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Andalucía') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)
            
#Calidad aire andalucia 2013
calidad_aire_andalucia_2013<-
  datos_calidad_aire_andalucia %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2013) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Andalucía') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)


#Calidad aire andalucia 2014
calidad_aire_andalucia_2014<-
  datos_calidad_aire_andalucia %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2014) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Andalucía') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#Calidad aire andalucia 2015
calidad_aire_andalucia_2015<-
  datos_calidad_aire_andalucia %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2015) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Andalucía') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#Calidad aire andalucia 2016
calidad_aire_andalucia_2016<-
  datos_calidad_aire_andalucia %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2016) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Andalucía') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#Calidad aire andalucia 2017
calidad_aire_andalucia_2017<-
  datos_calidad_aire_andalucia %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2017) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Andalucía') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#Calidad aire andalucia 2018
calidad_aire_andalucia_2018<-
  datos_calidad_aire_andalucia %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2018) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Andalucía') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#Calidad aire andalucia 2019
calidad_aire_andalucia_2019<-
  datos_calidad_aire_andalucia %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2019) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Andalucía') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)



#CALIDAD AIRE GALICIA POR AÑOS:

#calidad aire galicia 2010
calidad_aire_galicia_2010<-
  datos_calidad_aire_galicia %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2010) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Galicia') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#Calidad aire galicia 2011: se deben buscar otros datos ya que no hay variables
calidad_aire_galicia_2011<-
  datos_calidad_aire_galicia %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2011) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Galicia') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`) %>% 
  select(.data=., CCAA, `Measurement Year`, `PM2.5 (μg/m3)`, `PM10 (μg/m3)`, `NO2 (μg/m3)`)

#Calidad aire galicia 2012: se deben buscar otros datos ya que hay variables
calidad_aire_galicia_2012<-
  datos_calidad_aire_galicia %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2012) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Galicia') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#Calidad aire galicia 2013;
calidad_aire_galicia_2013<-
  datos_calidad_aire_galicia %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2013) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Galicia') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#Calidad aire galicia 2014
calidad_aire_galicia_2014<-
  datos_calidad_aire_galicia %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2014) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Galicia') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#Calidad aire galicia 2015
calidad_aire_galicia_2015<-
  datos_calidad_aire_galicia %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2015) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Galicia') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#Calidad aire galicia 2016
calidad_aire_galicia_2016<-
  datos_calidad_aire_galicia %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2016) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Galicia') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#Calidad aire galicia 2017
calidad_aire_galicia_2017<-
  datos_calidad_aire_galicia %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2017) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Galicia') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#Calidad aire galicia 2018
calidad_aire_galicia_2018<-
  datos_calidad_aire_galicia %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2018) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Galicia') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#Calidad aire galicia 2019
calidad_aire_galicia_2019<-
  datos_calidad_aire_galicia %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2019) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Galicia') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)


#CALIDAD AIRE CASTILLA Y LEON POR AÑOS:

#Calidad aire cyl 2010
calidad_aire_cyl_2010<-
  datos_calidad_aire_cyl %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2010) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Castilla y Leon') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire cyl 2011
calidad_aire_cyl_2011<-
  datos_calidad_aire_cyl %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2011) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='CyL') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`) %>% 
  select(.data = ., CCAA, `Measurement Year`, `PM2.5 (μg/m3)`, `PM10 (μg/m3)`, `NO2 (μg/m3)`)

#calidad aire cyl 2012
calidad_aire_cyl_2012<-
  datos_calidad_aire_cyl %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2012) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Castilla y Leon') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire cyl 2013
calidad_aire_cyl_2013<-
  datos_calidad_aire_cyl %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2013) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Castilla y Leon') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire cyl 2014
calidad_aire_cyl_2014<-
  datos_calidad_aire_cyl %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2014) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Castilla y Leon') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#Calidad aire cyl 2015
calidad_aire_cyl_2015<-
  datos_calidad_aire_cyl %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2015) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Castilla y Leon') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire cyl 2016
calidad_aire_cyl_2016<-
  datos_calidad_aire_cyl %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2016) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Castilla y Leon') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire cyl 2017
calidad_aire_cyl_2017<-
  datos_calidad_aire_cyl %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2017) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Castilla y Leon') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire cyl 2018
calidad_aire_cyl_2018<-
  datos_calidad_aire_cyl %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2018) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Castilla y Leon') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire cyl 2019
calidad_aire_cyl_2019<-
  datos_calidad_aire_cyl %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2019) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Castilla y Leon') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)



#CALIDAD DEL AIRE ARAGON POR AÑOS 

#Calidad aire aragon 2010
calidad_aire_aragon_2010<-
  datos_calidad_aire_aragon %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2010) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Aragon') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire aragon 2011
calidad_aire_aragon_2011<-
  datos_calidad_aire_aragon %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2011) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Aragon') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`) %>% 
  select(.data=., CCAA, `Measurement Year`, `PM2.5 (μg/m3)`, `PM10 (μg/m3)`, `NO2 (μg/m3)`)

#Calidad aire aragon 2012
calidad_aire_aragon_2012<-
  datos_calidad_aire_aragon %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2012) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Aragon') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#CALIDAD AIRE ARAGON 2013
calidad_aire_aragon_2013<-
  datos_calidad_aire_aragon %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2013) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Aragon') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#CALIDAD AIRE ARAGON 2014
calidad_aire_aragon_2014<-
  datos_calidad_aire_aragon %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2014) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Aragon') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#CALIDAD AIRE ARAGON 2015
calidad_aire_aragon_2015<-
  datos_calidad_aire_aragon %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2015) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Aragon') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#cALIDAD AIRE ARAGON 2016
calidad_aire_aragon_2016<-
  datos_calidad_aire_aragon %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2016) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Aragon') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#CALIDAD AIRE ARAGON 2017
calidad_aire_aragon_2017<-
  datos_calidad_aire_aragon %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2017) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Aragon') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#cALIDAD AIRE ARAGON 2018
calidad_aire_aragon_2018<-
  datos_calidad_aire_aragon %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2018) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Aragon') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#CALIDAD AIRE ARAGON 2019
calidad_aire_aragon_2019<-
  datos_calidad_aire_aragon %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2019) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Aragon') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)


#CALIDAD DEL AIRE MURCIA PARA TODOS LO AÑOS:

#calidad aire murcia 2010
calidad_aire_murcia_2010<-
  datos_calidad_aire_murcia %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2010) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Murcia') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire murcia 2011: NO HAY DATOS
calidad_aire_murcia_2011<-
  datos_calidad_aire_murcia %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2011) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Murcia') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`) %>% 
  select(.data=., CCAA, `Measurement Year`, `PM2.5 (μg/m3)`, `PM10 (μg/m3)`, `NO2 (μg/m3)`)

#Calidad aire murcia 2012: NO HAY DATOS 
calidad_aire_murcia_2012<-
  datos_calidad_aire_murcia %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2012) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Murcia') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire murcia 2013
calidad_aire_murcia_2013<-
  datos_calidad_aire_murcia %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2013) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Murcia') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire murcia 2014
calidad_aire_murcia_2014<-
  datos_calidad_aire_murcia %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2014) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Murcia') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire murcia 2015
calidad_aire_murcia_2015<-
  datos_calidad_aire_murcia %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2015) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Murcia') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire murcia 2016
calidad_aire_murcia_2016<-
  datos_calidad_aire_murcia %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2016) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Murcia') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire murcia 2017
calidad_aire_murcia_2017<-
  datos_calidad_aire_murcia %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2017) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Murcia') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire murcia 2018
calidad_aire_murcia_2018<-
  datos_calidad_aire_murcia %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2018) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Murcia') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire murcia 2019
calidad_aire_murcia_2019<-
  datos_calidad_aire_murcia %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2019) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Murcia') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)



#CALIDAD DEL AIRE NAVARRA PARA TODOS LOS AÑOS:

#calidad aire navarra 2010
calidad_aire_navarra_2010<-
  datos_calidad_aire_navarra %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2010) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Navarra') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#CALIDAD DEL AIRE 2011 NAVARRA NUEVOS DATOS:
calidad_aire_navarra_2011 <- read_delim("DATOS CALIDAD DEL AIRE/navarra_2011.csv", 
                             delim = ";", escape_double = FALSE, col_names = FALSE, 
                             trim_ws = TRUE,skip=1) %>%
    drop_na() %>% 
    rename(.data=.,"SO2 (µg/m³)"=X1,"NO(µg/m³)"=X2,"NO2(µg/m³)"=X3,"CO (mg/m³)"=X4,"O3 (µg/m³)"=X5,"PM10 (µg/m³)"=X6,"NOx (µg/m³)"=X7) %>% 
    select(.data=.,`SO2 (µg/m³)`:`NOx (µg/m³)`) %>% 
    summarise(.data=.,`SO2 (µg/m³)`=mean(`SO2 (µg/m³)`,na.rm=TRUE),`NO(µg/m³)`=mean(`NO(µg/m³)`,na.rm=TRUE),`NO2 (µg/m3)`=mean(`NO2(µg/m³)`,na.rm=TRUE),`CO (mg/m³)`=mean(`CO (mg/m³)`,na.rm=TRUE),`O3 (µg/m³)`=mean(`O3 (µg/m³)`,na.rm=TRUE),`PM10 (µg/m3)`=mean(`PM10 (µg/m³)`,na.rm=TRUE),`NOx (µg/m³)`=mean(`NOx (µg/m³)`,na.rm=TRUE)) %>%
  mutate(.data=.,CCAA='Navarra',`Measurement Year`=2011,`PM2.5 (μg/m3)`=NA) %>% 
  select(.data=., CCAA, `Measurement Year`,`PM2.5 (μg/m3)`,`PM10 (µg/m3)`,`NO2 (µg/m3)`)
  


#calidad aire navarra 2012: NO HAY DATOS 
calidad_aire_navarra_2012<-
  datos_calidad_aire_navarra %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2012) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Navarra') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

navarra_2012 <- read_delim("DATOS CALIDAD DEL AIRE/navarra_2012.csv", 
                           delim = ";", escape_double = FALSE, col_names = FALSE, 
                           trim_ws = TRUE, skip=1)
  


#calidad aire navarra 2013
calidad_aire_navarra_2013<-
  datos_calidad_aire_navarra %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2013) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Navarra') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire navarra 2014
calidad_aire_navarra_2014<-
  datos_calidad_aire_navarra %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2014) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Navarra') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire navarra 2015
calidad_aire_navarra_2015<-
  datos_calidad_aire_navarra %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2015) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Navarra') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire navarra 2016
calidad_aire_navarra_2016<-
  datos_calidad_aire_navarra %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2016) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Navarra') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire navarra 2017
calidad_aire_navarra_2017<-
  datos_calidad_aire_navarra %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2017) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Navarra') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire navarra 2018
calidad_aire_navarra_2018<-
  datos_calidad_aire_navarra %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2018) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Navarra') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)


#calidad aire navarra 2019
calidad_aire_navarra_2019<-
  datos_calidad_aire_navarra %>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2019) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Navarra') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)








#DATOS CALIDAD AIRE CANTABRIA POR AÑOS

#calidad aire cantabria 2010
calidad_aire_cantabria_2010<-
  datos_calidad_aire_cantabria%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2010) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Cantabria') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire cantabria 2011: NO HAY DATOS
#calidad_aire_cantabria_2011<-
  datos_calidad_aire_cantabria%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2011) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Cantabria') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#NUEVOS DATOS CANTABRIA 2011:


calidad_aire_cantabria_2011 <- read_delim("DATOS CALIDAD DEL AIRE/cantabria_2011.csv", 
                               delim = ";", escape_double = FALSE, col_names = FALSE, 
                               trim_ws = TRUE, skip = 1) %>% 
    select(.data = .,X2:X3) %>% 
    drop_na() %>% 
    rename(.data=.,`PM10 (µg/m3)`=X2,`NO2 (µg/m3)`=X3) %>%
    summarise(.data=.,across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
    mutate(.data=.,CCAA='Cantabria',`Measurement Year`=2011,`PM2.5 (µg/m3)`=NA) %>% 
    relocate(CCAA, `Measurement Year`, .before = 1:2) %>% 
  select(.data=.,CCAA,`Measurement Year`,`PM2.5 (µg/m3)`,`PM10 (µg/m3)`,`NO2 (µg/m3)`)
    
  

<<<<<<< HEAD
#calidad aire cantabria 2012: no hay datos
calidad_aire_cantabria_2012<-
  datos_calidad_aire_cantabria%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2012) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Cantabria') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)
=======

#calidad aire cantabria 2012:nueva tabla:
>>>>>>> 7353df59ca219a29416dea253c146fe33a9173df


calidad_aire_cantabria_2012 <- read_delim("DATOS CALIDAD DEL AIRE/cantabria_2012.csv", 
                             delim = ";", escape_double = FALSE, col_names = FALSE, 
                             trim_ws = TRUE,skip=1) %>% 
  select(.data = .,X2:X3) %>% 
  drop_na() %>% 
  rename(.data=.,`PM10 (µg/m³)`=X2,`NO2 (µg/m³)`=X3) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
mutate(.data=.,CCAA='Cantabria',`Measurement Year`=2012,`PM2.5 (µg/m³)`=NA) %>% 
  relocate(CCAA, `Measurement Year`, .before = 1:2)


#calidad aire cantabria 2013
calidad_aire_cantabria_2013<-
  datos_calidad_aire_cantabria%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2013) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Cantabria') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire cantabria 2014
calidad_aire_cantabria_2014<-
  datos_calidad_aire_cantabria%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2014) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Cantabria') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)


#calidad aire cantabria 2015
calidad_aire_cantabria_2015<-
  datos_calidad_aire_cantabria%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2015) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Cantabria') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire cantabria 2016
calidad_aire_cantabria_2016<-
  datos_calidad_aire_cantabria%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2016) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Cantabria') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire cantabria 2017
calidad_aire_cantabria_2017<-
  datos_calidad_aire_cantabria%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2017) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Cantabria') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire cantabria 2018
calidad_aire_cantabria_2018<-
  datos_calidad_aire_cantabria%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2018) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Cantabria') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire cantabria 2019
calidad_aire_cantabria_2019<-
  datos_calidad_aire_cantabria%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2019) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Cantabria') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)


#CALIDAD DEL AIRE CATALUÑA POR AÑOS

#calidad aire cataluña 2010
calidad_aire_cataluña_2010<-
  datos_calidad_aire_cataluña%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2010) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Cataluña') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire cataluña 2011
calidad_aire_cataluña_2011<-
  datos_calidad_aire_cataluña%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2011) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Cataluña') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`) %>% 
  select(.data=.,CCAA:`NO2 (μg/m3)`)

#calidad aire cataluña 2012
calidad_aire_cataluña_2012<-
  datos_calidad_aire_cataluña%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2012) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Cataluña') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire cataluña 2013
calidad_aire_cataluña_2013<-
  datos_calidad_aire_cataluña%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2013) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Cataluña') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire cataluña 2014
calidad_aire_cataluña_2014<-
  datos_calidad_aire_cataluña%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2014) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Cataluña') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire cataluña 2015
calidad_aire_cataluña_2015<-
  datos_calidad_aire_cataluña%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2015) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Cataluña') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire cataluña 2026
calidad_aire_cataluña_2016<-
  datos_calidad_aire_cataluña%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2016) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Cataluña') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)


#calidad aire cataluña 2017
calidad_aire_cataluña_2017<-
  datos_calidad_aire_cataluña%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2017) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Cataluña') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire cataluña 2018
calidad_aire_cataluña_2018<-
  datos_calidad_aire_cataluña%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2018) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Cataluña') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire cataluña 2019
calidad_aire_cataluña_2019<-
  datos_calidad_aire_cataluña%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2019) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Cataluña') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)


#CALIDAD DEL AIRE MADRID POR AÑOS

#calidad aire madrid 2010
calidad_aire_madrid_2010<-
  datos_calidad_aire_madrid%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2010) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Madrid') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire madrid 2011
calidad_aire_madrid_2011<-
  datos_calidad_aire_madrid%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2011) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Madrid') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`) %>% 
  select(.data=.,CCAA:`NO2 (μg/m3)`)

#calidad aire madrid 2012
calidad_aire_madrid_2012<-
  datos_calidad_aire_madrid%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2012) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Madrid') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire madrid 2013
calidad_aire_madrid_2013<-
  datos_calidad_aire_madrid%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2013) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Madrid') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire madrid 2014
calidad_aire_madrid_2014<-
  datos_calidad_aire_madrid%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2014) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Madrid') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire madrid 2015
calidad_aire_madrid_2015<-
  datos_calidad_aire_madrid%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2015) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Madrid') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire madrid 2016
calidad_aire_madrid_2016<-
  datos_calidad_aire_madrid%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2016) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Madrid') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire madrid 2017
calidad_aire_madrid_2017<-
  datos_calidad_aire_madrid%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2017) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Madrid') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire madrid 2018
calidad_aire_madrid_2018<-
  datos_calidad_aire_madrid%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2018) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Madrid') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire madrid 2019
calidad_aire_madrid_2019<-
  datos_calidad_aire_madrid%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2019) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Madrid') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)


#CALIDAD DEL AIRE CASTILLA LA MANCHA POR AÑOS

#calidad aire mancha 2010
calidad_aire_mancha_2010<-
  datos_calidad_aire_castillamancha%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2010) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Castilla la Mancha') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad del aire mancha 2011
calidad_aire_mancha_2011<-
  datos_calidad_aire_castillamancha%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2011) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Castilla la Mancha') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`) %>% 
  select(.data=.,CCAA:`NO2 (μg/m3)`)


#calidad aire mancha 2012
calidad_aire_mancha_2012<-
  datos_calidad_aire_castillamancha%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2012) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Castilla la Mancha') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad del aire mancha 2013
calidad_aire_mancha_2013<-
  datos_calidad_aire_castillamancha%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2013) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Castilla la Mancha') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad del aire mancha 2014
calidad_aire_mancha_2014<-
  datos_calidad_aire_castillamancha%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2014) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Castilla la Mancha') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calodad del aire mancha 2015
calidad_aire_mancha_2015<-
  datos_calidad_aire_castillamancha%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2015) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Castilla la Mancha') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad del aire mancha 2016
calidad_aire_mancha_2016<-
  datos_calidad_aire_castillamancha%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2016) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Castilla la Mancha') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad del aire mancha 2017
calidad_aire_mancha_2017<-
  datos_calidad_aire_castillamancha%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2017) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Castilla la Mancha') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad del aire mancha 2018
calidad_aire_mancha_2018<-
  datos_calidad_aire_castillamancha%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2018) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Castilla la Mancha') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad del aire mancha 2019
calidad_aire_mancha_2019<-
  datos_calidad_aire_castillamancha%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2019) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Castilla la Mancha') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)


#CALIDAD DEL AIRE VALENCIA POR AÑOS

#calidad aire valencia 2010
calidad_aire_valencia_2010<-
  datos_calidad_aire_valencia%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2010) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Valencia') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad del aire valencia 2011
calidad_aire_valencia_2011<-
  datos_calidad_aire_valencia%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2011) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Valencia') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`) %>% 
  select(.data=.,CCAA:`NO2 (μg/m3)`)

#calidad del aire valencia 2012
calidad_aire_valencia_2012<-
  datos_calidad_aire_valencia%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2012) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Valencia') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire valencia 2013
calidad_aire_valencia_2013<-
  datos_calidad_aire_valencia%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2013) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Valencia') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad del aire valencia 2014
calidad_aire_valencia_2014<-
  datos_calidad_aire_valencia%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2014) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Valencia') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad del aire valencia 2015
calidad_aire_valencia_2015<-
  datos_calidad_aire_valencia%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2015) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Valencia') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad del aire valencia 2016
calidad_aire_valencia_2016<-
  datos_calidad_aire_valencia%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2016) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Valencia') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)


#calidad del aire valencia 2017
calidad_aire_valencia_2017<-
  datos_calidad_aire_valencia%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2017) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Valencia') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad del aire valencia 2018
calidad_aire_valencia_2018<-
  datos_calidad_aire_valencia%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2018) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Valencia') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad del aire valencia 2019
calidad_aire_valencia_2019<-
  datos_calidad_aire_valencia%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2019) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Valencia') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#CALIDAD AIRE VALENCIA 2020:SE DEBEN BUSCAR OTROS DATOS:
calidad_aire_valencia_2020<-
  datos_calidad_aire_valencia%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2020) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Valencia') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#CALIDAD AIRE VALENCIA 2021:SE DEBEN BUSCAR OTROS DATOS 
calidad_aire_valencia_2021<-
  datos_calidad_aire_valencia%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2021) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Valencia') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)




#CALIDAD DEL AIRE EXTREMADURA POR AÑOS

#calidad del aire extremadura 2010
calidad_aire_extremadura_2010<-
  datos_calidad_aire_extremadura%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2010) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Extremadura') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire extremadura 2011: NO HAY DATOS 
calidad_aire_extremadura_2011<-
  datos_calidad_aire_extremadura%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2011) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Extremadura') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`) %>% 
  select(.data=.,CCAA:`NO2 (μg/m3)`)

#clidad del aire extremadura 2012
calidad_aire_extremadura_2012<-
  datos_calidad_aire_extremadura%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2012) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Extremadura') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad del aire extremadura 2013
calidad_aire_extremadura_2013<-
  datos_calidad_aire_extremadura%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2013) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Extremadura') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)




#calidad del aire extremadura 2014
calidad_aire_extremadura_2014<-
  datos_calidad_aire_extremadura%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2014) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Extremadura') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire extremadura 2015
calidad_aire_extremadura_2015<-
  datos_calidad_aire_extremadura%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2015) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Extremadura') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire extremadura 2016
calidad_aire_extremadura_2016<-
  datos_calidad_aire_extremadura%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2016) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Extremadura') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire extremadura 2017
calidad_aire_extremadura_2017<-
  datos_calidad_aire_extremadura%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2017) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Extremadura') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#calidad aire extremadura 2018
calidad_aire_extremadura_2018<-
  datos_calidad_aire_extremadura%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2018)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Extremadura') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#CALIDAD AIRE EXTREMADURA 2019:
calidad_aire_extremadura_2019<-
  datos_calidad_aire_extremadura%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2019)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Extremadura') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#CALIDAD AIRE EXTREMADURA 2020:SE DEBEN BUSCRA OTROS DATOS:
calidad_aire_extremadura_2020<-
  datos_calidad_aire_extremadura%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2020)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Extremadura') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#CALIDAD AIRE EXTREMADURA 2021:SE DEBEN BUSCAR OTROS DATOS:
calidad_aire_extremadura_2021<-
  datos_calidad_aire_extremadura%>% 
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2021)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Extremadura') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#CALIDAD DEL AIRE DE LA RIOJA POR AÑOS:


#CALIDAD DEL AIRE RIOJA 2010:
calidad_aire_rioja_2010<-
  datos_calidad_aire_rioja %>%
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2010)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='La Rioja') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#CALIDAD DEL AIRE RIOJA 2011:SE DEBEN BUSCAR OTROS DATOS /TIBBLE VACÍO:
calidad_aire_rioja_2011<-
  datos_calidad_aire_rioja %>%
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2011)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='La Rioja') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#CALIDAD DEL AIRE RIOJA 2012:SE DEBEN BUSCAR OTROS DATOS /TIBBLE VACÍO:
calidad_aire_rioja_2012<-
datos_calidad_aire_rioja %>%
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2012)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='La Rioja') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#CALIDAD DEL AIRE RIOJA 2013:
calidad_aire_rioja_2013<-
datos_calidad_aire_rioja %>%
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2013)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='La Rioja') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#CALIDAD DEL AIRE RIOJA 2014 :
calidad_aire_rioja_2014<-
datos_calidad_aire_rioja %>%
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2014)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='La Rioja') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#CALIDAD DEL AIRE RIOJA 2015:
calidad_aire_rioja_2015<-
datos_calidad_aire_rioja %>%
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2015)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='La Rioja') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#CALIDAD DEL AIRE RIOJA 2016:
calidad_aire_rioja_2016<-
datos_calidad_aire_rioja %>%
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2016)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='La Rioja') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#CALIDAD DEL AIRE RIOJA 2017:
calidad_aire_rioja_2017<-
  datos_calidad_aire_rioja %>%
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2017)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='La Rioja') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#CALIDAD DEL AIRE RIOJA 2018:
calidad_aire_rioja_2018<-
datos_calidad_aire_rioja %>%
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2018)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='La Rioja') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#CALIDAD DEL AIRE RIOJA 2019:
calidad_aire_rioja_2019<-
datos_calidad_aire_rioja %>%
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2019)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='La Rioja') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#CALIDAD DEL AIRE RIOJA 2020:SE DEBE BUSCAR OTROS DATOS :
calidad_aire_rioja_2020<-
  datos_calidad_aire_rioja %>%
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2020)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='La Rioja') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#CALIDAD DEL AIRE RIOJA 2021:SE DEBEN BUSCAR OTROS DATOS:
calidad_aire_rioja_2021<-
  datos_calidad_aire_rioja %>%
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2021)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='La Rioja') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#TABLAS DE CALIDAD DEL AIRE EN EUSKADI POR AÑOS:

#CALIDAD DEL AIRE EUSKADI 2010:
calidad_aire_euskadi_2010<-
  datos_calidad_aire_euskadi %>%
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2010)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Euskadi') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#CALIDAD DEL AIRE EUSKADI 2011:NO SE ENCUENTRAN DATOS PARA ESTE AÑO:
calidad_aire_euskadi_2011<-
  datos_calidad_aire_euskadi %>%
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2011)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Euskadi') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`) %>% 
  select(.data=.,CCAA:`NO2 (μg/m3)`)


#CALIDAD DEL AIRE EUSKADI 2012:
calidad_aire_euskadi_2012<-
  datos_calidad_aire_euskadi %>%
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2012)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Euskadi') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#CALIDAD DEL AIRE EUSKADI 2013:
calidad_aire_euskadi_2013<-
  datos_calidad_aire_euskadi %>%
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2013)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Euskadi') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)


#CALIDAD DEL AIRE EUSKADI 2014:
calidad_aire_euskadi_2014<-
  datos_calidad_aire_euskadi %>%
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2014)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Euskadi') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#CALIDAD DEL AIRE EUSKADI 2015:
calidad_aire_euskadi_2015<-
  datos_calidad_aire_euskadi %>%
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2015)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Euskadi') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#CALIDAD DEL AIRE EUSKADI 2016:
calidad_aire_euskadi_2016<-
  datos_calidad_aire_euskadi %>%
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2016)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Euskadi') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#CALIDAD DEL AIRE EUSKADI 2017:
calidad_aire_euskadi_2017<-
  datos_calidad_aire_euskadi %>%
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2017)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Euskadi') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#CALIDAD DEL AIRE EUSKADI 2018:
calidad_aire_euskadi_2018<-
  datos_calidad_aire_euskadi %>%
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2018)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Euskadi') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#CALIDAD DEL AIRE EUSKADI 2019:
calidad_aire_euskadi_2019<-
  datos_calidad_aire_euskadi %>%
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2019)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Euskadi') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)

#CALIDAD DEL AIRE EUSKADI 2020:NO SE CONSIDERARA ESTE AÑO EN EL ANALISIS POR FALTA DE DATOS PARA TODAS LAS CCAA PENINSULARES:
calidad_aire_euskadi_2020<-
  datos_calidad_aire_euskadi %>%
  group_by(`Measurement Year`) %>% 
  filter(.data=.,`Measurement Year`==2020)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(.data=.,CCAA='Euskadi') %>% 
  relocate(.data=.,CCAA,.before = `Measurement Year`)



#se cargan aqui datos para la calidad del aire en la rioja por provincias en 2011,2012,2020,2021:

#provincias para rioja 2011:
ESalfaro_2011 <- 
  read_excel("DATOS CALIDAD DEL AIRE/Datos validados La Rioja 2003-2012 (1)/ESalfaro_2003_2012.xls", 
                                 sheet = "2011") %>% 
  drop_na() %>%
  filter(NO2_HI >= 0, PM10_HI >= 0, PM25_HI >= 0, NO_HI >= 0) %>%
  select(NO2_HI, PM10_HI, PM25_HI, NO_HI) %>% 
  summarise(.data=.,across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  rename(.data=., NO2 = NO2_HI, PM10 = PM10_HI, PM25 = PM25_HI, NO = NO_HI) %>% 
  mutate(.data=.,CCAA = "La Rioja", `Measurement Year`= 2011) %>% 
  relocate(.data=.,CCAA, `Measurement Year`,.before = 1)

ESpradejon_2011 <- read_excel("DATOS CALIDAD DEL AIRE/Datos validados La Rioja 2003-2012 (1)/ESpradejon_2005_2012.xls", 
                                   sheet = "2011") %>% 
  drop_na() %>%
  filter(NO2_HI >= 0, PM10_HI >= 0, PM25_HI >= 0, NO_HI >= 0) %>%
  select(NO2_HI, PM10_HI, PM25_HI, NO_HI) %>% 
  summarise(.data=.,across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  rename(.data=., NO2 = NO2_HI, PM10 = PM10_HI, PM25 = PM25_HI, NO = NO_HI) %>% 
  mutate(.data=.,CCAA = "La Rioja", `Measurement Year`= 2011) %>% 
  relocate(.data=.,CCAA, `Measurement Year`,.before = 1)
  
ESgalilea_2011 <- read_excel("DATOS CALIDAD DEL AIRE/Datos validados La Rioja 2003-2012 (1)/ESgalilea_2005_2012.xls", 
                                  sheet = "2011") %>% 
  drop_na() %>%
  filter(NO2_HI >= 0, PM10_HI >= 0, PM25_HI >= 0, NO_HI >= 0) %>%
  select(NO2_HI, PM10_HI, PM25_HI, NO_HI) %>% 
  summarise(.data=.,across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  rename(.data=., NO2 = NO2_HI, PM10 = PM10_HI, PM25 = PM25_HI, NO = NO_HI) %>% 
  mutate(.data=.,CCAA = "La Rioja", `Measurement Year`= 2011) %>% 
  relocate(.data=.,CCAA, `Measurement Year`,.before = 1)

ESciguena_2011<-read_excel("DATOS CALIDAD DEL AIRE/Datos validados La Rioja 2003-2012 (1)/ESciguena_2003_2012.xls",sheet = "2011") %>% 
  drop_na() %>%
  filter(NO2_HI >= 0, PM10_HI >= 0, NO_HI >= 0) %>%
  select(NO2_HI, PM10_HI,NO_HI) %>% 
  summarise(.data=.,across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  rename(.data=., NO2 = NO2_HI, PM10 = PM10_HI,  NO = NO_HI) %>% 
  mutate(.data=.,CCAA = "La Rioja", `Measurement Year`= 2011) %>% 
  relocate(.data=.,CCAA, `Measurement Year`,.before = 1) %>% 
  mutate(.data=.,PM25=NA)
  
ESarrubal_2011<-read_excel("DATOS CALIDAD DEL AIRE/Datos validados La Rioja 2003-2012 (1)/ESarrubal_2005_2012.xls",sheet = "2011") %>% 
  drop_na() %>%
  filter(NO2_HI >= 0, PM10_HI >= 0, PM25_HI >= 0, NO_HI >= 0) %>%
  select(NO2_HI, PM10_HI, PM25_HI, NO_HI) %>% 
  summarise(.data=.,across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  rename(.data=., NO2 = NO2_HI, PM10 = PM10_HI, PM25 = PM25_HI, NO = NO_HI) %>% 
  mutate(.data=.,CCAA = "La Rioja", `Measurement Year`= 2011) %>% 
  relocate(.data=.,CCAA, `Measurement Year`,.before = 1)



#NUEVA TABLA CALIDAD RIOJA 2011:TABLA FINAL:

calidad_aire_rioja_2011<-rbind(ESarrubal_2011, ESalfaro_2011, ESgalilea_2011, ESpradejon_2011, ESciguena_2011)  %>%
  reframe(.data=.,`NO2 (μg/m3)`=mean(NO2,na.rm = TRUE),`PM10 (μg/m3)`=mean(PM10,na.rm = TRUE),`PM2.5 (μg/m3)`=mean(PM25,na.rm=TRUE)) %>% 
  mutate(.data=.,CCAA = "La Rioja", `Measurement Year`= 2011) %>% 
  relocate(.data=.,CCAA, `Measurement Year`,.before = 1) %>% 
  select(.data=.,CCAA,`Measurement Year`,`PM2.5 (μg/m3)`,`PM10 (μg/m3)`,`NO2 (μg/m3)`)




#CALIDAD AIRE RIOJA 2012 POR PROVINCIAS:

ESalfaro_2012 <- read_excel("DATOS CALIDAD DEL AIRE/Datos validados La Rioja 2003-2012 (1)/ESalfaro_2003_2012.xls", 
                                 sheet = "2012") %>% 
  drop_na() %>%
  filter(NO2_HI >= 0, PM10_HI >= 0, PM25_HI >= 0, NO_HI >= 0) %>%
  select(NO2_HI, PM10_HI, PM25_HI, NO_HI) %>% 
  summarise(.data=.,across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  rename(.data=., NO2 = NO2_HI, PM10 = PM10_HI, PM25 = PM25_HI, NO = NO_HI) %>% 
  mutate(.data=.,CCAA = "La Rioja", `Measurement Year`= 2012) %>% 
  relocate(.data=.,CCAA, `Measurement Year`,.before = 1)

ESpradejon_2012 <- read_excel("DATOS CALIDAD DEL AIRE/Datos validados La Rioja 2003-2012 (1)/ESpradejon_2005_2012.xls", 
                              sheet = "2012") %>% 
  drop_na() %>%
  filter(NO2_HI >= 0, PM10_HI >= 0, PM25_HI >= 0, NO_HI >= 0) %>%
  select(NO2_HI, PM10_HI, PM25_HI, NO_HI) %>% 
  summarise(.data=.,across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  rename(.data=., NO2 = NO2_HI, PM10 = PM10_HI, PM25 = PM25_HI, NO = NO_HI) %>% 
  mutate(.data=.,CCAA = "La Rioja", `Measurement Year`= 2012) %>% 
  relocate(.data=.,CCAA, `Measurement Year`,.before = 1)

ESgalilea_2012 <- read_excel("DATOS CALIDAD DEL AIRE/Datos validados La Rioja 2003-2012 (1)/ESgalilea_2005_2012.xls", 
                             sheet = "2012") %>% 
  drop_na() %>%
  filter(NO2_HI >= 0, PM10_HI >= 0, PM25_HI >= 0, NO_HI >= 0) %>%
  select(NO2_HI, PM10_HI, PM25_HI, NO_HI) %>% 
  summarise(.data=.,across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  rename(.data=., NO2 = NO2_HI, PM10 = PM10_HI, PM25 = PM25_HI, NO = NO_HI) %>% 
  mutate(.data=.,CCAA = "La Rioja", `Measurement Year`= 2012) %>% 
  relocate(.data=.,CCAA, `Measurement Year`,.before = 1)

ESciguena_2012<-read_excel("DATOS CALIDAD DEL AIRE/Datos validados La Rioja 2003-2012 (1)/ESciguena_2003_2012.xls",sheet = "2012") %>% 
  drop_na() %>%
  filter(NO2_HI >= 0, PM10_HI >= 0, NO_HI >= 0) %>%
  select(NO2_HI, PM10_HI,NO_HI) %>% 
  summarise(.data=.,across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  rename(.data=., NO2 = NO2_HI, PM10 = PM10_HI,  NO = NO_HI) %>% 
  mutate(.data=.,CCAA = "La Rioja", `Measurement Year`= 2012) %>% 
  relocate(.data=.,CCAA, `Measurement Year`,.before = 1) %>% 
  mutate(.data=.,PM25=NA)

ESarrubal_2012<-read_excel("DATOS CALIDAD DEL AIRE/Datos validados La Rioja 2003-2012 (1)/ESarrubal_2005_2012.xls",sheet = "2012") %>% 
  drop_na() %>%
  filter(NO2_HI >= 0, PM10_HI >= 0, PM25_HI >= 0, NO_HI >= 0) %>%
  select(NO2_HI, PM10_HI, PM25_HI, NO_HI) %>% 
  summarise(.data=.,across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  rename(.data=., NO2 = NO2_HI, PM10 = PM10_HI, PM25 = PM25_HI, NO = NO_HI) %>% 
  mutate(.data=.,CCAA = "La Rioja", `Measurement Year`= 2012) %>% 
  relocate(.data=.,CCAA, `Measurement Year`,.before = 1)



#NUEVA TABLA CALIDAD RIOJA 2011:TABLA FINAL:

calidad_aire_rioja_2012<-
  rbind(ESarrubal_2012, ESalfaro_2012, ESgalilea_2012, ESpradejon_2012, ESciguena_2012)  %>%
  reframe(.data=.,NO2=mean(NO2,na.rm = TRUE),PM10=mean(PM10,na.rm = TRUE),PM25=mean(PM25,na.rm=TRUE),NO=mean(NO,na.rm=TRUE)) %>% 
  mutate(.data=.,CCAA = "La Rioja", `Measurement Year`= 2012) %>% 
  relocate(.data=.,CCAA, `Measurement Year`,.before = 1)


  
#union de todas las ccaa por años 
#2010
ccaa_2010_calidad<-
  rbind(calidad_aire_andalucia_2010, calidad_aire_galicia_2010, calidad_aire_cyl_2010, calidad_aire_aragon_2010, calidad_aire_murcia_2010, calidad_aire_navarra_2010, calidad_aire_cantabria_2010, calidad_aire_cataluña_2010, calidad_aire_madrid_2010, calidad_aire_mancha_2010, calidad_aire_valencia_2010, calidad_aire_extremadura_2010, calidad_aire_rioja_2010,calidad_aire_euskadi_2010) %>% 
  select(CCAA:`NO2 (μg/m3)`) 


calidad_nacional_2010<-
  ccaa_2010_calidad%>%
  filter_all(~ !is.nan(.)) %>% 
  summarise(.data=.,across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
  mutate(.data=.,CCAA="Nacional",`Measurement Year`= 2010) %>% relocate(.data=.,CCAA, `Measurement Year`,.before = 1)


# TABLA FINAL CALIDAD DEL AIRE A NIVEL NACIONAL 2011:
colnames(calidad_aire_cantabria_2011) <- c("CCAA", "Measurement Year", "PM2.5 (μg/m3)", "PM10 (μg/m3)", "NO2 (μg/m3)")
colnames(calidad_aire_navarra_2011) <- c("CCAA", "Measurement Year", "PM2.5 (μg/m3)", "PM10 (μg/m3)", "NO2 (μg/m3)")

calidad_nacional_2011_bind<-rbind(calidad_aire_andalucia_2011,calidad_aire_aragon_2011,calidad_aire_cataluña_2011,calidad_aire_cyl_2011,calidad_aire_euskadi_2011,calidad_aire_extremadura_2011,calidad_aire_galicia_2011,calidad_aire_madrid_2011,calidad_aire_mancha_2011,calidad_aire_murcia_2011,calidad_aire_rioja_2011,calidad_aire_valencia_2011,calidad_aire_cantabria_2011,calidad_aire_navarra_2011) 


calidad_nacional_2011<-
  calidad_nacional_2011_bind%>%
  filter_all(~ !is.nan(.)) %>% 
  summarise(.data=.,across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
  mutate(.data=.,CCAA="Nacional",`Measurement Year`= 2011) %>% relocate(.data=.,CCAA, `Measurement Year`,.before = 1)
  
  


#2012

#2013
ccaa_2013_calidad<-
  rbind(calidad_aire_andalucia_2013, calidad_aire_galicia_2013, calidad_aire_cyl_2013, calidad_aire_aragon_2013, calidad_aire_murcia_2013, calidad_aire_navarra_2013, calidad_aire_cantabria_2013, calidad_aire_cataluña_2013, calidad_aire_madrid_2013, calidad_aire_mancha_2013, calidad_aire_valencia_2013, calidad_aire_extremadura_2013, calidad_aire_rioja_2013) %>% 
  select(CCAA:`NO2 (μg/m3)`) 
 

#2014
ccaa_2014_calidad<-
  rbind(calidad_aire_andalucia_2014, calidad_aire_galicia_2014, calidad_aire_cyl_2014, calidad_aire_aragon_2014, calidad_aire_murcia_2014, calidad_aire_navarra_2014, calidad_aire_cantabria_2014, calidad_aire_cataluña_2014, calidad_aire_madrid_2014, calidad_aire_mancha_2014, calidad_aire_valencia_2014, calidad_aire_extremadura_2014, calidad_aire_rioja_2014) %>% 
  select(CCAA:`NO2 (μg/m3)`)
  

#2015
ccaa_2015_calidad<-
  rbind(calidad_aire_andalucia_2015, calidad_aire_galicia_2015, calidad_aire_cyl_2015, calidad_aire_aragon_2015, calidad_aire_murcia_2015, calidad_aire_navarra_2015, calidad_aire_cantabria_2015, calidad_aire_cataluña_2015, calidad_aire_madrid_2015, calidad_aire_mancha_2015, calidad_aire_valencia_2015, calidad_aire_extremadura_2015, calidad_aire_rioja_2015) %>% 
  select(CCAA:`NO2 (μg/m3)`) 
  

#2016
ccaa_2016_calidad<-
  rbind(calidad_aire_andalucia_2016, calidad_aire_galicia_2016, calidad_aire_cyl_2016, calidad_aire_aragon_2016, calidad_aire_murcia_2016, calidad_aire_navarra_2016, calidad_aire_cantabria_2016, calidad_aire_cataluña_2016, calidad_aire_madrid_2016, calidad_aire_mancha_2016, calidad_aire_valencia_2016, calidad_aire_extremadura_2016, calidad_aire_rioja_2016) %>% 
  select(CCAA:`NO2 (μg/m3)`) 
  

#2017
ccaa_2017_calidad<-
  rbind(calidad_aire_andalucia_2017, calidad_aire_galicia_2017, calidad_aire_cyl_2017, calidad_aire_aragon_2017, calidad_aire_murcia_2017, calidad_aire_navarra_2017, calidad_aire_cantabria_2017, calidad_aire_cataluña_2017, calidad_aire_madrid_2017, calidad_aire_mancha_2017, calidad_aire_valencia_2017, calidad_aire_extremadura_2017, calidad_aire_rioja_2017) %>% 
  select(CCAA:`NO2 (μg/m3)`) 
 

#2018
ccaa_2018_calidad<-
  rbind(calidad_aire_andalucia_2018, calidad_aire_galicia_2018, calidad_aire_cyl_2018, calidad_aire_aragon_2018, calidad_aire_murcia_2018, calidad_aire_navarra_2018, calidad_aire_cantabria_2018, calidad_aire_cataluña_2018, calidad_aire_madrid_2018, calidad_aire_mancha_2018, calidad_aire_valencia_2018, calidad_aire_extremadura_2018, calidad_aire_rioja_2018) %>% 
  select(CCAA:`NO2 (μg/m3)`) 
  

#2019
ccaa_2019_calidad<-
  rbind(calidad_aire_andalucia_2019, calidad_aire_galicia_2019, calidad_aire_cyl_2019, calidad_aire_aragon_2019, calidad_aire_murcia_2019, calidad_aire_navarra_2019, calidad_aire_cantabria_2019, calidad_aire_cataluña_2019, calidad_aire_madrid_2019, calidad_aire_mancha_2019, calidad_aire_valencia_2019, calidad_aire_extremadura_2019, calidad_aire_rioja_2019) %>% 
  select(CCAA:`NO2 (μg/m3)`) 
  


  

#datos calidad del aire por año y gases

#2010

#2010 para no2
ccaa_2010_no2<-ccaa_2010_calidad %>% 
  relocate(`NO2 (μg/m3)`, .before = `PM2.5 (μg/m3)`) %>% 
  select(CCAA:`NO2 (μg/m3)`) %>% 
  arrange(.data=., desc(`NO2 (μg/m3)`))

#2010 para pm2.5
ccaa_2010_pm2.5<-ccaa_2010_calidad %>% 
  select(CCAA:`PM2.5 (μg/m3)`) %>% 
  arrange(.data=., desc(`PM2.5 (μg/m3)`))

#2010 para pm10
ccaa_2010_pm10<-ccaa_2010_calidad %>% 
  relocate(`PM10 (μg/m3)`, .before = `PM2.5 (μg/m3)`) %>% 
  select(CCAA:`PM10 (μg/m3)`) %>% 
  arrange(.data=., desc(`PM10 (μg/m3)`))


#2013

#2013 para no2
ccaa_2013_no2<-ccaa_2013_calidad %>% 
  relocate(`NO2 (μg/m3)`, .before = `PM2.5 (μg/m3)`) %>% 
  select(CCAA:`NO2 (μg/m3)`) %>% 
  arrange(.data=., desc(`NO2 (μg/m3)`))

#2013 para pm2.5
ccaa_2013_pm2.5<-ccaa_2013_calidad %>% 
  select(CCAA:`PM2.5 (μg/m3)`) %>% 
  arrange(.data=., desc(`PM2.5 (μg/m3)`))

#2013 para pm10
ccaa_2013_pm10<-ccaa_2013_calidad %>% 
  relocate(`PM10 (μg/m3)`, .before = `PM2.5 (μg/m3)`) %>% 
  select(CCAA:`PM10 (μg/m3)`) %>% 
  arrange(.data=., desc(`PM10 (μg/m3)`))
  

#2014

#2014 para no2
ccaa_2014_no2<-ccaa_2014_calidad %>% 
  relocate(`NO2 (μg/m3)`, .before = `PM2.5 (μg/m3)`) %>% 
  select(CCAA:`NO2 (μg/m3)`) %>% 
  arrange(.data=., desc(`NO2 (μg/m3)`))

#2014 para pm2.5
ccaa_2014_pm2.5<-ccaa_2014_calidad %>% 
  select(CCAA:`PM2.5 (μg/m3)`) %>% 
  arrange(.data=., desc(`PM2.5 (μg/m3)`))

#2014 para pm10
ccaa_2014_pm10<-ccaa_2014_calidad %>% 
  relocate(`PM10 (μg/m3)`, .before = `PM2.5 (μg/m3)`) %>% 
  select(CCAA:`PM10 (μg/m3)`) %>% 
  arrange(.data=., desc(`PM10 (μg/m3)`))


#2015

#2015 para no2
ccaa_2015_no2<-ccaa_2015_calidad %>% 
  relocate(`NO2 (μg/m3)`, .before = `PM2.5 (μg/m3)`) %>% 
  select(CCAA:`NO2 (μg/m3)`) %>% 
  arrange(.data=., desc(`NO2 (μg/m3)`))

#2015 para pm2.5
ccaa_2015_pm2.5<-ccaa_2015_calidad %>% 
  select(CCAA:`PM2.5 (μg/m3)`) %>% 
  arrange(.data=., desc(`PM2.5 (μg/m3)`))

#2015 para pm10
ccaa_2015_pm10<-ccaa_2015_calidad %>% 
  relocate(`PM10 (μg/m3)`, .before = `PM2.5 (μg/m3)`) %>% 
  select(CCAA:`PM10 (μg/m3)`) %>% 
  arrange(.data=., desc(`PM10 (μg/m3)`))
  

#2016

#2016 para no2
ccaa_2016_no2<-ccaa_2016_calidad %>% 
  relocate(`NO2 (μg/m3)`, .before = `PM2.5 (μg/m3)`) %>% 
  select(CCAA:`NO2 (μg/m3)`) %>% 
  arrange(.data=., desc(`NO2 (μg/m3)`))

#2016 para pm2.5
ccaa_2016_pm2.5<-ccaa_2016_calidad %>% 
  select(CCAA:`PM2.5 (μg/m3)`) %>% 
  arrange(.data=., desc(`PM2.5 (μg/m3)`))

#2016 para pm10
ccaa_2016_pm10<-ccaa_2016_calidad %>% 
  relocate(`PM10 (μg/m3)`, .before = `PM2.5 (μg/m3)`) %>% 
  select(CCAA:`PM10 (μg/m3)`) %>% 
  arrange(.data=., desc(`PM10 (μg/m3)`))


#2017

#2017 para no2
ccaa_2017_no2<-ccaa_2017_calidad %>% 
  relocate(`NO2 (μg/m3)`, .before = `PM2.5 (μg/m3)`) %>% 
  select(CCAA:`NO2 (μg/m3)`) %>% 
  arrange(.data=., desc(`NO2 (μg/m3)`))

#2017 para pm2.5
ccaa_2017_pm2.5<-ccaa_2017_calidad %>% 
  select(CCAA:`PM2.5 (μg/m3)`) %>% 
  arrange(.data=., desc(`PM2.5 (μg/m3)`))

#2017 para pm10
ccaa_2017_pm10<-ccaa_2017_calidad %>% 
  relocate(`PM10 (μg/m3)`, .before = `PM2.5 (μg/m3)`) %>% 
  select(CCAA:`PM10 (μg/m3)`) %>% 
  arrange(.data=., desc(`PM10 (μg/m3)`))


#2018

#2018 para no2
ccaa_2018_no2<-ccaa_2018_calidad %>% 
  relocate(`NO2 (μg/m3)`, .before = `PM2.5 (μg/m3)`) %>% 
  select(CCAA:`NO2 (μg/m3)`) %>% 
  arrange(.data=., desc(`NO2 (μg/m3)`))

#2018 para pm2.5
ccaa_2018_pm2.5<-ccaa_2018_calidad %>% 
  select(CCAA:`PM2.5 (μg/m3)`) %>% 
  arrange(.data=., desc(`PM2.5 (μg/m3)`))

#2018 para pm10
ccaa_2018_pm10<-ccaa_2018_calidad %>% 
  relocate(`PM10 (μg/m3)`, .before = `PM2.5 (μg/m3)`) %>% 
  select(CCAA:`PM10 (μg/m3)`) %>% 
  arrange(.data=., desc(`PM10 (μg/m3)`))


#2019

#2019 para no2
ccaa_2019_no2<-ccaa_2019_calidad %>% 
  relocate(`NO2 (μg/m3)`, .before = `PM2.5 (μg/m3)`) %>% 
  select(CCAA:`NO2 (μg/m3)`) %>% 
  arrange(.data=., desc(`NO2 (μg/m3)`))

#2019 para pm2.5
ccaa_2019_pm2.5<-ccaa_2019_calidad %>% 
  select(CCAA:`PM2.5 (μg/m3)`) %>% 
  arrange(.data=., desc(`PM2.5 (μg/m3)`))

#2019 para pm10
ccaa_2019_pm10<-ccaa_2019_calidad %>% 
  relocate(`PM10 (μg/m3)`, .before = `PM2.5 (μg/m3)`) %>% 
  select(CCAA:`PM10 (μg/m3)`) %>% 
  arrange(.data=., desc(`PM10 (μg/m3)`))



#graficas por años y diferentes gases













