#Todas las formulas presentadas en este script fueron construidas primero en el rmd "TP2_limpieza" para corroborar su correcto funcionamiento
#por eso tienen una estructura bastante parecida a un chunk

#Formula para levantar los txt de siniestros mas recientes

get_siniestros_nuevos <- function() {
  
  setwd("siniestros/nuevos/")
  
  Siniestros_Nuevos <-
    list.files(pattern = "*.txt") %>% 
    map_df(~read.delim(.,header = FALSE,sep = ",",encoding = "UTF-8",quote = ""))
  
  Siniestros_Nuevos <- Siniestros_Nuevos %>%
    select(-V2) %>%
    mutate(V12 = as.numeric(V12)) %>%
    mutate(V11 = as.numeric(V11)) %>%
    filter(V12 != is.na(V12))
  
  Siniestros_Nuevos <- Siniestros_Nuevos %>%
    rename(fecha = V1,
           Calle = V3,
           Tipo_Siniestro = V4,
           Gravedad = V5,
           Dia_semana = V6,
           Hora = V7,
           Departamento = V8,
           Ciudad = V9,
           Fixed = V10,
           X = V11,
           Y = V12)
  
}

#Formula para levantar los txt de siniestros mas antiguos

get_siniestros_antiguos <- function() {
  
  setwd("siniestros/antiguos/")
  
  Siniestros_Antiguos <-
    list.files(pattern = "*.txt") %>% 
    map_df(~read.delim(.,header = FALSE,sep = ",",encoding = "UTF-8",quote = ""))
  
  Siniestros_Antiguos <- Siniestros_Antiguos %>%
    mutate(V10 = as.numeric(V10)) %>%
    mutate(V11 = as.numeric(V11)) %>%
    filter(V11 != is.na(V11))
  
  Siniestros_Antiguos <- Siniestros_Antiguos %>%
    rename(fecha = V1,
           Calle = V2,
           Tipo_Siniestro = V3,
           Gravedad = V4,
           Dia_semana = V5,
           Hora = V6,
           Departamento = V7,
           Ciudad = V8,
           Fixed = V9,
           X = V10,
           Y = V11)
  
}

# formula para levantar, limpiar y unir las dos bases de siniestros en un solo DF
# Primero se deben correr las formulas de get_siniestros_antiguos y get_siniestros_nuevo dado que dentro de esta se pedira especificamente estos dos datasets

get_siniestros <- function() {
  
  Siniestros <- Siniestros_Nuevos %>%
    rbind(Siniestros_Antiguos)
  
  Siniestros <- Siniestros %>%
    mutate(Hora = as.numeric(Hora)) %>%
    mutate(fecha = dmy(fecha)) %>%
    select(-Fixed)
  
  Siniestros <- Siniestros %>%
    mutate(Calle = sub(".*? ", "",Calle)) %>%
    mutate(Tipo_Siniestro = sub(".*? ", "",Tipo_Siniestro)) %>%
    mutate(Gravedad = sub(".*? ", "",Gravedad)) %>%
    mutate(Dia_semana = sub(".*? ", "",Dia_semana)) %>%
    mutate(Departamento = sub(".*? ", "",Departamento)) %>%
    mutate(Ciudad = sub(".*? ", "",Ciudad))
  
  Siniestros <- Siniestros %>%
    mutate(anio = format(floor_date(fecha,"year"),"%Y")) %>%
    mutate(Mes = floor_date(fecha,"month"))
  
  Siniestros <- st_as_sf(Siniestros,coords = c("X","Y"), crs = 32721)
  
  Siniestros <- Siniestros %>%
    st_transform(4326)
}

#Formula para levantar los txt de la base de personas afectadas por los siniestros 

get_personas <- function() {
  setwd("siniestros_personas/")
  
  Siniestros_Personas <-
    list.files(pattern = "*.txt") %>% 
    map_df(~read.delim(.,header = FALSE,sep = ",",encoding = "UTF-8",quote = ""))
  
  Siniestros_Personas <- Siniestros_Personas %>%
    rename(Fecha = V1,
           Edad = V3,
           Rol = V4,
           Calle = V5,
           Zona = V6,
           Gravedad = V7,
           Tipo_Siniestro = V8,
           Usa_Cinturon = V9,
           Usa_Casco = V10,
           Dia_semana = V11,
           Sexo = V12,
           Hora = V13,
           Departamento = V14,
           Localidad = V15,
           Tipo_Vehiculo = V16,
           fixed = V17,
           x = V18,
           y = V19) %>%
    filter(!is.na(y)) %>%
    select(-V2,fixed)
  
  Siniestros_Personas <- Siniestros_Personas %>%
    mutate(Rol = sub(".*? ", "",Rol)) %>%
    mutate(Calle = sub(".*? ", "",Calle)) %>%
    mutate(Zona = sub(".*? ", "",Zona)) %>%
    mutate(Tipo_Siniestro = sub(".*? ", "",Tipo_Siniestro)) %>%
    mutate(Gravedad = sub(".*? ", "",Gravedad)) %>%
    mutate(Usa_Cinturon = sub(".*? ", "",Usa_Cinturon)) %>%
    mutate(Usa_Casco = sub(".*? ", "",Usa_Casco)) %>%
    mutate(Dia_semana = sub(".*? ", "",Dia_semana)) %>%
    mutate(Departamento = sub(".*? ", "",Departamento)) %>%
    mutate(Localidad = sub(".*? ", "",Localidad)) %>%
    mutate(Tipo_Vehiculo = sub(".*? ", "",Tipo_Vehiculo))
  
  Siniestros_Personas <- Siniestros_Personas %>%
    mutate(Edad = as.numeric(Edad)) %>%
    mutate(Hora = as.numeric(Hora)) %>%
    mutate(x = as.numeric(x)) %>%
    mutate(y = as.numeric(y)) %>%
    mutate(Fecha = dmy(Fecha))
  
  Siniestros_Personas <- st_as_sf(Siniestros_Personas,coords = c("x","y"), crs = 32721)
  
  Siniestros_Personas <- Siniestros_Personas %>%
    st_transform(4326)
}
