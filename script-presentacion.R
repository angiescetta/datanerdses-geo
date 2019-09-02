#Librerias
library(tidyverse)
library(sf)
library(data.table)
library(ggmap)
library(leaflet)
library(corrplot)

#Datos
barrios_caba <- st_read("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/barrios/barrios.geojson")

rus2017 <- fread("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/relevamiento-usos-del-suelo/relevamiento-usos-del-suelo-2017.csv",
                  encoding = 'UTF-8',
                 sep = ",",
                 header = TRUE,
                 showProgress=TRUE,
                 data.table=FALSE)

#Agrupación de los usos del suelo en las categorías: Vivienda, Oficinas, Interes y Entretenimiento, Comercial, Educacion, Salud.
rus2017 <- rus2017 %>%
  mutate(CATEGORIA=case_when(SUBRAMA == "VIVIENDA UNICA" | SUBRAMA == "VIVIENDA" | SUBRAMA == "CASA" | SUBRAMA == "DEPARTAMENTOS"~"RESIDENCIAL",
                             RAMA == "CONSTRUCCION" & SUBRAMA == "" | SUBRAMA == "OFICINAS" | SUBRAMA == "PRODUCCION DE MUSICA, FILMES Y VIDEOCINTAS" | SUBRAMA == "SERVICOS EMPRESARIALES PROFESIONALES" | SUBRAMA == "SERVICIOS DE RADIO Y TELEVISION"~"OFICINAS",
                             RAMA == "INSTALACIONES PARA EDIFICIOS Y OBRAS DE INGENIERIA CIVIL" & SUBRAMA == "" | RAMA == "SERVICIOS DE ASOCIACIONES" & SUBRAMA == "" | SUBRAMA == "SUCURSAL BANCARIA" | SUBRAMA == "ADMINISTRACION PUBLICA, DEFENSA Y SEGURIDAD SOCIAL OBLIGATORIA" | SUBRAMA == "SERVICIOS PARA EL ORDEN PUBLICO Y LA SEGURIDAD" | SUBRAMA == "ADMINISTRACION PUBLICA" | SUBRAMA == "SERVICIOS PUBLICOS" | SUBRAMA == "SERVICIOS DE LA ADMINISTRACION PUBLICA" | SUBRAMA=="SERVICIOS DE LA SEGURIDAD SOCIAL OBLIGATORIA" | SUBRAMA ==  "SERVICIOS SOCIALES" | SUBRAMA == "SERVICIOS DE SINDICATOS" | SUBRAMA == "ELIMINACION DE DESPERDICIOS Y AGUAS RESIDUALES, SANEAMIENTO Y SERVICIOS SIMILARES" | SUBRAMA == "GENERACION, TRANSPORTE Y DISTRIBUCION DE ENERGIA ELECTRICA" | SUBRAMA == "SERVICIOS DE ASOCIACIONES" | SUBRAMA == "SERVICIOS DE ASOCIACIONES NCP" | SUBRAMA == "SERVICIOS DE MERCADOS Y CAJA DE VALORES" | SUBRAMA == "SERVICIOS DE ORGANIZACIONES EMPRESARIALES Y DE EMPLEADORES" | SUBRAMA == "SERVICIOS DE ORGANIZACIONES RELIGIOSAS" | SUBRAMA == "SERVICIOS COMPLEMENTARIOS AL TRANSPORTE" | SUBRAMA == "SERVICIOS DE ASUNTOS EXTERIORES" | SUBRAMA ==  "SERVICIO DE TRANSPORTE URBANO DE CARGA" | SUBRAMA == "SERVICIO DE TRANSPORTE" | SUBRAMA == "RECICLAMIENTO DE DESPERDICIOS Y DESECHOS NO METALICOS"~"SERVICIOS",
                             SUBRAMA == "BIBLIOTECA" | SUBRAMA == "MUSEO" | SUBRAMA == "PRODUCCION DE ESPECTACULOS TEATRALES Y MUSICALES" | SUBRAMA == "EXHIBICON DE FILMES Y VIDEOCINTAS" | SUBRAMA ==  "SERVICIOS DE ESPARCIMIENTO RELACIONADOS CON JUEGOS DE AZAR Y APUESTAS" | SUBRAMA == "SERVICIO DE ENTRETENIMIENTO NCP" | SUBRAMA == "PLAZA"~"INTERES Y ENTRETENIMIENTO",
                             SUBRAMA == "GASTRONOMIA" | SUBRAMA == "HOTELERIA"~"GASTRONOMIA Y HOTELERIA",
                             RAMA == "VENTA AL POR MAYOR DE ALIMENTOS Y BEBIDAS" & SUBRAMA == "" | RAMA == "CONFECCION DE PRENDAS DE VESTIR" & SUBRAMA == "" | SUBRAMA == "VENTA POR MAYOR DE METALES Y MINERALES METALIFEROS" | SUBRAMA == "VENTA POR MAYOR DE MERCANCIAS" | SUBRAMA == "VENTA POR MAYOR DE MAQUINAS, EQUIPOS Y MATERIALES CONEXOS" | SUBRAMA == "VENTA DE VEHICULOS AUTOMOTORES" | SUBRAMA == "VENTA DE PARTES Y ACCESORIOS PARA AUTOMOTORES" | SUBRAMA == "VENTA DE MOTOS, PARTES Y ACCESORIOS" | SUBRAMA == "VENTA DE LOCALES ESPECIALIZADOS" | SUBRAMA == "VENTA DE COMBUSTIBLES Y LUBRICANTE PARA AUTOMOTORES" | SUBRAMA == "VENTA AL POR MENOR EXEPTO LA ESPECIALIZADA" |  SUBRAMA == "VENTA AL POR MENOR ESPECIALIZADO NCP" | SUBRAMA == "VENTA AL POR MENOR DE PRODUCTOS ALIMENTICIOS ESPECIALIZADOS" | SUBRAMA == "VENTA AL POR MENOR DE ARTICULOS USADOS" | SUBRAMA == "VENTA AL POR MAYOR EN COMISION O CONSIGNACION DE MERCANCIAS NCP"  | SUBRAMA == "VENTA AL POR MAYOR DE MUEBLES E INSTALACIONES PARA LA INDUSTRIA, EL COMERCIO Y LOS SERVICIOS" | SUBRAMA == "VENTA AL POR MAYOR DE MERCANCIAS NCP" | SUBRAMA == "VENTA AL POR MAYOR DE MAQUINAS, EQUIPOS Y MATERIALES CONEXOS NCP" | SUBRAMA == "VENTA AL POR MAYOR DE MADERA, MATERIALES PARA LA CONSTRUCCION. PLOMERIA E INSTALACIONES DE GAS" | SUBRAMA == "VENTA AL POR MAYOR DE ARTICULOS DE USO DOMESTICO O PERSONAL NCP" | SUBRAMA == "LOTERIA" | SUBRAMA == "CASA DE CAMBIO" | SUBRAMA == "DISTRIBUCION DE FILMES Y VIDEOCINTAS" | SUBRAMA == "PELUQUERIA Y TRATAMIENTOS DE BELLEZA" | SUBRAMA == "SERVICIOS INMOBILIARIOS" | SUBRAMA == "SERVICIOS INMOBILIARIOS\n" | SUBRAMA == "LAVADO Y LIMPIEZA" | SUBRAMA == "REPARACION DE EFECTOS PERSONALES Y ENSERES DOMESTICOS" | SUBRAMA == "REPARACION DE MOTOS" | SUBRAMA == "EDICION E IMPRESIÃ“N" | SUBRAMA == "SERVICIO DE CORREO Y TELECOMUNICACIONES" | SUBRAMA == "SERVICIO DE SALONES DE JUEGO" | SUBRAMA == "POMPAS FUNEBRES" | SUBRAMA == "PRODUCTOS DE MADERA" | SUBRAMA == "PREPARACION DE TERRENOS PARA OBRAS" | SUBRAMA == "SERVICIOS FINANCIEROS NCP" | SUBRAMA == "SERVICIOS INFORMATICOS Y ACTIVIDADES CONEXAS" | SUBRAMA == "SERVICIOS JURIDICOS" | SUBRAMA == "MANTENIMIENTO Y REPARACION" | SUBRAMA == "ELABORACION DE PRODUCTOS LACTEOS" | SUBRAMA == "ELABORACION DE PRODUCTOS ALIMENTICIOS" | SUBRAMA ==  "ALQUILER DE EQUIPO DE TRANSPORTE" | SUBRAMA == "ALQUILER DE MAQUINARIA Y EQUIPO NCP" | SUBRAMA == "ALQUILER DE ENSERES PERSONALES Y EQUIPOS" | SUBRAMA == "SERVICIOS DE SEGUROS" | SUBRAMA == "SERVICIOS NCP" | SUBRAMA == "SERVICIOS EMPRESARIALES NCP" | SUBRAMA == "GALERIA" | SUBRAMA == "SERVICIO DE TELECOMUNICACIONES" | SUBRAMA == "SERVICIO DE TRANSPORTE AEREO DE CARGA" | SUBRAMA == "SERVICIO DE TRANSPORTE AEREO DE PASAJEROS" | SUBRAMA == "SERVICIOS DE MUDANZA" | SUBRAMA == "SERVICIOS TURISTICOS" | SUBRAMA == "SERVICIOS VETERINARIOS" | SUBRAMA ==  "SERVICOS EMPRESARIALES NO PROFESIONALES" | SUBRAMA == "TV. RADIO, ESPECTACULOS, CULTURALES" | SUBRAMA == "SERVICIOS DE SEGUROS DE SALUD" | SUBRAMA == "UNIFICADO LOCAL" | SUBRAMA == "DEPORTES" | SUBRAMA == "UNIFICADO ESTACION DE SERVICIOS" | SUBRAMA ==  "INSTALACIONES DE GAS, AGUA, SANITARIOS Y DE CLIMATIZACION" | SUBRAMA == "SERVICIO DE TRANSPORTE AUTOMOTOR DE PASAJEROS" | SUBRAMA ==  "SERVICIOS DE ENTIDADES FINANCIERAS NO BANCARIAS" | SUBRAMA ==  "PRODUCCION DE FILMES Y VIDEOCINTAS" | SUBRAMA == "PRODUCCION Y PROCESAMIENTO DE CARNE, FRUTAS, LEGUMBRES, HORTALIZAS, ACEITES Y GRASAS" | SUBRAMA == "INSTALACION DE ASCENSORES, MONTACARGAS Y ESCALERAS MECANICAS" | SUBRAMA == "SERVICIOS DE SEGUROS PATRIMONIALES" | SUBRAMA=="SERVICIOS COMPLEMENTARIOS AL TRANSPORTE TERRESTRE" & TIPO2_16!="GARAGE PRIVADO"~"COMERCIAL",
                             RAMA == "FABRICACION DE PRODUCTOS TEXTILES" & SUBRAMA == "" | RAMA == "FABRICACION DE INSTRUMENTOS MEDICOS, OPTICOS Y DE PRECISION; FABRICACION DE RELOJES" & SUBRAMA == "" | RAMA == "FABRICACION DE EQUIPOS DE COMUNICACION" & SUBRAMA == "" | RAMA == "ELABORACION DE PRODUCTOS ALIMENTICIOS NCP" & SUBRAMA == "" | RAMA == "FABRICACIN DE TEJIDOS DE PUNTO" & SUBRAMA == "" | SUBRAMA == "ELABORACIN DE BEBIDAS NO ALCOHOLICAS; PRODUCCION DE AGUAS MINERALES" | SUBRAMA == "ELABORACION DE BEBIDAS" | SUBRAMA == "ELABORACION DE PRODUCTOS DE MOLINERIA, ALMIDONES Y PRODUCTOS DERIVADOS DEL ALMIDON, ELABORACION DE *" | SUBRAMA == "CONFECCION DE ROPA INTERIOR, PRENDAS PARA DORMIR Y PARA LA PLAYA" | SUBRAMA == "CURTIDO Y TERMINACION DE CUEROS; FABRICACION DE ARTICULOS DE MARROQUINERIA TALABARTERIA" | SUBRAMA == "FABRICACIN DE PINTURAS, BARNICES Y PRODUCTOS DE REVESTIMIENTO, TINTAS DE IMPRENTA Y MASILLAS" | SUBRAMA == "FABRICACIÃ“N DE PRODUCTOS ELABORADOS DE METAL NCP" | SUBRAMA == "FABRICACION DE APARATOS DE USO DOMESTICO NCP" | SUBRAMA == "FABRICACION DE ARTICULOS DE MARROQUINERIA, TALABARTERIA Y CALZADO Y DE SUS PARTES" | SUBRAMA == "FABRICACION DE AUTOPARTES, PIEZAS Y ACCESORIOS PARA VEHICULOS AUTOMOTORES Y SUS MOTORES" | SUBRAMA == "FABRICACION DE CALZADO Y DE SUS PARTES" | SUBRAMA == "FABRICACION DE COSMETICOS, PERFUMES Y PRODUCTOS DE HIGIENE Y TOCADOR" | SUBRAMA == "FABRICACION DE EQUIPO ELECTRICO" | SUBRAMA == "FABRICACION DE HILADOS Y TEJIDOS" | SUBRAMA == "FABRICACION DE JABONES Y DETERGENTES, PREPARADOS PARA LIMPIAR Y PULIR, PERFUMES Y PREPARADOS DE TOC*" | SUBRAMA == "FABRICACION DE LAMPARAS ELECTRICAS Y EQUIPO DE ILUMINACION" | SUBRAMA == "FABRICACION DE MALETAS, BOLSOS, Y SIMILARES, ARTICULOS DE TALABARTERIA Y ARTICULOS DE CUERO NCP" | SUBRAMA == "FABRICACION DE MAQUINARIA DE OFICINA, CONTABILIDAD E INFORMATICA" | SUBRAMA == "FABRICACION DE MAQUINARIA DE USO GENERAL" | SUBRAMA == "FABRICACION DE MAQUINARIA DE USO GENERAL NCP" | SUBRAMA == "FABRICACION DE MAQUINARIA DE USO GENERAL Y ESPECIAL NCP" | SUBRAMA == "FABRICACION DE MEDICAMENTOS" | SUBRAMA == "FABRICACION DE MOTORES, GENERADORES Y TRANSFORMADORES ELECTRICOS" | SUBRAMA == "FABRICACION DE MUEBLES Y COLCHONES" | SUBRAMA == "FABRICACION DE PAPEL Y PRODUCTOS DE PAPEL" | SUBRAMA == "FABRICACION DE PARTES Y PIEZAS DE CARPINTERIA PARA CONSTRUCCIONES" | SUBRAMA == "FABRICACION DE PRODUCTOS DE CAUCHO" | SUBRAMA == "FABRICACION DE PRODUCTOS DE LA REFINACION DEL PETROLEO" | SUBRAMA == "FABRICACION DE PRODUCTOS METALICOS NCP" | SUBRAMA == "FABRICACION DE PRODUCTOS MINERALES NO METALICOS NCP" | SUBRAMA == "FABRICACION DE PRODUCTOS PLASTICOS" | SUBRAMA == "FABRICACION DE PRODUCTOS QUIMICOS" | SUBRAMA == "FABRICACION DE PRODUCTOS TEXTILES NCP" | SUBRAMA == "FABRICACION DE VIDRIO Y PRODUCTOS DE VIDRIO" | SUBRAMA == "SERVICIOS AGRICOLAS" | SUBRAMA == "GALPON" | SUBRAMA == "UNIFICADO GALPON" | SUBRAMA == "INDUSTRIAS MANUFACTURERAS NCP" | SUBRAMA == "EDIFICIO PRODUCTIVO" | SUBRAMA == "SERVICIOS DE ALMACENAMIENTO Y DEPOSITO"~"PRODUCTIVO",
                             RAMA == "ENSEÃ‘ANZA" & SUBRAMA == "" | SUBRAMA == "ENSEÃ‘ANZA INICIAL Y PRIMARIA" | SUBRAMA == "ENSEÃ‘ANZA INICIAL, PRIMARIA Y SECUNDARIA" | SUBRAMA == "ENSEÃ‘ANZA PARA ADULTOS Y SERVICIOS DE ENSEÃ‘ANZA NCP" | SUBRAMA == "ENSEÃ‘ANZA SECUNDARIA DE FORMACION PROFESIONAL Y TECNICA" | SUBRAMA == "ENSEÃ‘ANZA SUPERIOR Y FORMACION DE POSGRADO"~"EDUCACION",
                             SUBRAMA == "SERVICIOS DE ATENCION MEDICA"~"SALUD"))

#Configuración del formato de los mapas:
#a.Valores discretos:
theme_caba_d <- theme (plot.margin = margin(0.25, 1, 0.25, 0.1, "cm"), #ajustar los margenes del gráfico
                       panel.background = element_rect(fill = "gray100", colour = "gray100", size = 2, linetype = "solid"), #fondo del gráfico
                       panel.grid.major = element_line(size = 0.5, linetype = "dashed", colour = "gray80"), #lineas del gráfico
                       panel.grid.minor = element_line(size = 0.25, linetype = "dashed", colour = "gray90"), #líneas auxiliares
                       title=element_text(size=10, face = "bold"), #tamaño de titulo del mapa
                       legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
                       legend.key.width = unit(0.4,"cm"), #ancho de cuadrados de referencia 
                       legend.position="right", #ubicacion de leyenda
                       legend.direction = "horizontal", #dirección de la leyenda
                       legend.title=element_text(size=8, face = "bold"), #tamaño de titulo de leyenda
                       legend.text=element_text(size=7), #tamaño de texto de leyenda
                       plot.caption=element_text(face = "italic", colour = "gray35",size=6), #tamaño de nota al pie
                       axis.text = element_blank(), #texto eje X e Y
                       axis.ticks = element_blank())

#b.Valores continuos:
theme_caba_c <- theme (plot.margin = margin(0.25, 1, 0.25, 0.1, "cm"), #ajustar los margenes del gráfico
                       panel.background = element_rect(fill = "gray100", colour = "gray100", size = 2, linetype = "solid"), #fondo del gráfico
                       panel.grid.major = element_line(size = 0.5, linetype = "dashed", colour = "gray80"), #lineas del gráfico
                       panel.grid.minor = element_line(size = 0.25, linetype = "dashed", colour = "gray90"), #líneas auxiliares
                       title=element_text(size=10, face = "bold"), #tamaño de titulo del mapa
                       legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
                       legend.key.width = unit(0.4,"cm"), #ancho de cuadrados de referencia 
                       legend.position="right", #ubicacion de leyenda
                       legend.direction = "vertical", #dirección de la leyenda
                       legend.title=element_text(size=8, face = "bold"), #tamaño de titulo de leyenda
                       legend.text=element_text(size=7), #tamaño de texto de leyenda
                       plot.caption=element_text(face = "italic", colour = "gray35",size=6), #tamaño de nota al pie
                       axis.text = element_blank(), #texto eje X e Y
                       axis.ticks = element_blank())

#A MAPEAR!

#MAPA 1: Los Barrios de CABA
ggplot() +
  geom_sf(data = barrios_caba, aes(fill = barrio), color = NA) +
  labs(title = "Barrios de la Ciudad de Buenos Aires",
       fill = "Barrios",
       caption= "Fuente de datos: https://data.buenosaires.gob.ar/")+
  scale_fill_viridis_d(alpha = 0.9)+
  guides(fill=guide_legend(title.position = "top", ncol=2))+
  theme_caba_d +
  ggsave("01_mapa_barrios.jpg", width = 30, height = 15, units = "cm", dpi = 300, limitsize = TRUE)

#MAPA 2: Las Comunas de CABA
ggplot() +
  geom_sf(data = barrios_caba, aes(fill = as.factor(comuna)), color = NA) +
  labs(title = "Comunas de la Ciudad de Buenos Aires",
       fill = "Comunas",
       caption= "Fuente de datos: https://data.buenosaires.gob.ar/",
       x="",
       y="")+
  scale_fill_viridis_d(option="plasma", alpha = 0.9)+
  guides(fill=guide_legend(title.position = "top", ncol=1))+
  theme_caba_d +
  ggsave("02_mapa_comunas.jpg", width = 30, height = 15, units = "cm", dpi = 300, limitsize = TRUE)

#MAPA 3: La Distribución de los Comercios en la Ciudad
#Extra: Descargar mapa de fondo
bbox <- c(-58.546686, #Asignar coordenadas de los límites del bounding box de la Ciudad
          -34.711145,
          -58.329097,
          -34.529156)

CABA <- get_stamenmap(bbox = bbox, #Descargar mapa
                      maptype = "toner-lite",
                      zoom=13)

ggmap(CABA) +
  geom_point(data = filter(rus2017, CATEGORIA=="COMERCIAL" | CATEGORIA=="GASTRONOMIA Y HOTELERIA"), 
             aes(x = X, y = Y), alpha = 0.5, color="turquoise4", size=0.4) +
  labs(title="Distribución de la Actividad Comercial",
       subtitle="Ciudad Autónoma de Buenos Aires",
       x="",
       y="",
       caption= "Fuente de datos: https://data.buenosaires.gob.ar/")+
  theme_caba_d+
  ggsave("03_mapa_densidad_comercios.jpg", width = 30, height = 15, units = "cm", dpi = 300, limitsize = TRUE)

#MAPA 4: Cantidad de comercios por Barrio
rus2017_geo <- rus2017 %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326) #Conversión del csv de RUS a dato espacial
barrios_caba <- st_transform(barrios_caba, st_crs(rus2017_geo)) #Conversión sistema de coordenadas de barrios
rus2017_geo <- st_join(rus2017_geo, barrios_caba) #Join espacial: a cada uso del suelo le asigno el barrio al que pertenece

barrios_caba <- barrios_caba %>% #Conteo de comercios por barrio
  mutate(barrio=as.factor(barrio)) %>%
  left_join(filter(rus2017_geo, CATEGORIA=="COMERCIAL" | CATEGORIA=="GASTRONOMIA Y HOTELERIA") %>%
              group_by(barrio) %>%
              summarise(cant_comercios=n()) %>%
              st_set_geometry(NULL),
               by=c("barrio"))

ggplot() +
  geom_sf(data = barrios_caba, aes(fill = cant_comercios), color = NA) +
  labs(title = "Actividad Comercial por Barrio",
       subtitle="Ciudad Autónoma de Buenos Aires",
       fill = "Cantidad",
       caption= "Fuente de datos: https://data.buenosaires.gob.ar/")+
  scale_fill_viridis_c(alpha = 0.9)+
  theme_caba_c +
  ggsave("04_mapa_barrios_comercios.jpg", width = 30, height = 15, units = "cm", dpi = 300, limitsize = TRUE)

#MAPA 5: Cantidad de comercios por Comuna
barrios_caba %>%
  group_by(comuna) %>%
  summarise(cant_comercios=sum(cant_comercios)) %>%
  ggplot() +
  geom_sf(aes(fill = cant_comercios), color = NA) +
  labs(title = "Actividad Comercial por Comuna",
       subtitle="Ciudad Autónoma de Buenos Aires",
       fill = "Cantidad",
       caption= "Fuente de datos: https://data.buenosaires.gob.ar/",
       x="",
       y="")+
  scale_fill_viridis_c(option="plasma")+
  theme_caba_c +
  ggsave("05_mapa_comunas_comercios.jpg", width = 30, height = 15, units = "cm", dpi = 300, limitsize = TRUE)


#MAPA 6: Densidad de comercios en la Ciudad (matriz)
ggmap(CABA) +
  geom_bin2d(data = filter(rus2017, CATEGORIA=="COMERCIAL" | CATEGORIA=="GASTRONOMIA Y HOTELERIA"),
             aes(x = X, y = Y), bins = 100, alpha = 0.8) +
  labs(title="Distribución de la Actividad Comercial",
       subtitle="Ciudad Autónoma de Buenos Aires",
       x="",
       y="",
       caption= "Fuente de datos: https://data.buenosaires.gob.ar/",
       fill="Cantidad")+
  scale_fill_distiller(palette = "Spectral")+
  theme_caba_c+
  ggsave("06_mapa_dens_comerc_matriz.jpg", width = 30, height = 15, units = "cm", dpi = 300, limitsize = TRUE)

#MAPA 7: Densidad de comercios en la Ciudad (curvas-lineas)
ggmap(CABA) +
  geom_density2d(data = filter(rus2017, CATEGORIA=="COMERCIAL" | CATEGORIA=="GASTRONOMIA Y HOTELERIA"), 
                 aes(x = X, y = Y, 
                     color = stat(level)), alpha = 0.8, size=1) +
  labs(title="Distribución de la Actividad Comercial",
       subtitle="Ciudad Autónoma de Buenos Aires",
       x="",
       y="",
       caption= "Fuente de datos: https://data.buenosaires.gob.ar/",
       color="Nivel")+
  scale_color_distiller(palette = "Spectral")+
  theme_caba_c+
  ggsave("07_mapa_dens_comerc_curvas.jpg", width = 30, height = 15, units = "cm", dpi = 300, limitsize = TRUE)

#MAPA 8: Densidad de comercios en la Ciudad (curvas-poligonos)
ggmap(CABA) +
  stat_density_2d(data = filter(rus2017, CATEGORIA=="COMERCIAL" | CATEGORIA=="GASTRONOMIA Y HOTELERIA"), 
                  aes(x = X, y = Y, 
                      fill = stat(level)), alpha = 0.6, geom = "polygon") +
  labs(title="Distribución de la Actividad Comercial",
       subtitle="Ciudad Autónoma de Buenos Aires",
       x="",
       y="",
       caption= "Fuente de datos: https://data.buenosaires.gob.ar/",
       fill="Nivel")+
  scale_fill_distiller(palette = "Spectral")+
  theme_caba_c+ #eje X e Y
  ggsave("08_mapa_dens_comerc_curvas_poly.jpg", width = 30, height = 15, units = "cm", dpi = 300, limitsize = TRUE)

#MAPA 9: Densidad de otros usos del suelo en la Ciudad (curvas-poligonos)
ggmap(CABA) +
  stat_density_2d(data = filter(rus2017, CATEGORIA=="COMERCIAL" | CATEGORIA=="GASTRONOMIA Y HOTELERIA"|
                                  CATEGORIA=="RESIDENCIAL"| CATEGORIA=="OFICINAS"), 
                  aes(x = X, y = Y, 
                      fill = stat(level)), alpha = 0.6, geom = "polygon") +
  labs(title="Distribución de los Usos del Suelo",
       subtitle="Ciudad Autónoma de Buenos Aires",
       x="",
       y="",
       caption= "Fuente de datos: https://data.buenosaires.gob.ar/",
       fill="Nivel")+
  scale_fill_distiller(palette = "Spectral")+
  theme_caba_c+
  facet_wrap(~CATEGORIA)+
  ggsave("09_mapa_dens_otros_curvas_poly.jpg", width = 30, height = 15, units = "cm", dpi = 300, limitsize = TRUE)

#MAPA 10: Matriz de la Ciudad

caba_matriz <- barrios_caba %>% #Generar matriz
  st_make_grid(n=c(60,60)) %>% 
  st_sf() %>%
  st_intersection(barrios_caba) %>% 
  mutate(matriz=row_number())

rus2017_geo <- st_join(rus2017_geo, caba_matriz) #Asignar a cada uso del suelo la matriz a la que pertenece

rus2017_geo_matriz <- rus2017_geo %>%
  filter(CATEGORIA=="COMERCIAL" | CATEGORIA=="GASTRONOMIA Y HOTELERIA") %>%
  group_by(matriz, CATEGORIA) %>%
  summarise(cant_matriz=n()) %>%
  st_set_geometry(NULL)

rus2017_geo_matriz <- left_join(caba_matriz, rus2017_geo_matriz) #Asignar cantidad de comercios a cada cuadricula

ggplot()+
  geom_sf(data=filter(rus2017_geo_matriz),
          aes(fill=cant_matriz), color=NA) +
  labs(title="Distribución de la Actividad Comercial",
       subtitle="Ciudad Autónoma de Buenos Aires",
       x="",
       y="",
       caption= "Fuente de datos: https://data.buenosaires.gob.ar/",
       fill="Cantidad")+
  scale_fill_distiller(palette = "Spectral")+
  theme_caba_c+
  ggsave("10_mapa_comercios_matriz.jpg", width = 30, height = 15, units = "cm", dpi = 300, limitsize = TRUE)
