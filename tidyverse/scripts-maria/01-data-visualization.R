# Visualización de datos - 14 Sept 2019

# Importando librerías necesarias
library(tidyverse)





# OBJETIVOS
# Los coches con motor más grande consumen más combustible que los coches con motor más pequeño
# La relación consumo-tamaño es lineal? Es no lineal? Es exponencial?
# Es positiva? Es negativa? 



# Visualizando y obteniendo información de los datos
View(mpg)
help(mpg)
?mpg
# displ: engine displacement, in litres
# hwy: highway miles per gallon





# GGPLOT
# Plot básico
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))



# Modificando la estética
# Clasificando con color
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

# Clasificando con tamaño
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

# Clasificando con tranparencia
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

# Clasificando con forma (solo permite como mucho 6 formas a la vez)
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, shape = drv))



# Elección manual de estéticas
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), color = "purple")



# Facetas
# La variable debe ser discreta
# facet_wrap
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~class, nrow = 2)

# facet_grid
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv~cyl)
# El primero está en las filas (drv) y el segundo, en las columnas (cyl)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(.~cyl)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(~cyl)
# No hay diferencia alguna entre este y el plot inmediatamente anterior

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv~.)
# Si en este caso no ponemos el punto, el plot no se ejecuta



# Diferentes geometrías
# Scatterplot
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

# Smooth
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

# Smooth con 3 tipos distintos de línea en función de drv
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

# Smooth con 3 tipos distintos de color en función de drv
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, color = drv))

# Smooth con 3 tipos distintos de color y línea en función de drv
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv, color = drv))

# Combinación del anterior con Scatterplot
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv, color = drv))



# Agrupando datos 
# Sin leyenda
# Parámetro group agrupa los datos, pero no los diferencia
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

# Con leyenda (hay que diferenciar los grupos de algún modo)
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv, color = drv))

# Diferenciando SIN leyenda
# Explicitamos que no se muestre leyenda alguna
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv, color = drv),
              show.legend = F)



# Evitando repeticiones innecesarias
# Ajustes locales prevalecen ante ajustes globales. Locales ganan a globales
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth(mapping = aes(linetype = drv), color = "black")



# Filtrando datos
# Con corredor
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth(data = filter(mpg, class == "suv"), color = "black")

# Sin corredor
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth(data = filter(mpg, class == "suv"), color = "black", se = F)





# Dataset diamantes -------------------------------------------------------

# Diagrama de barras
View(diamonds)

# Gráfico de barras. Eje vertical se corresponde con la frecuencia absoluta, que no está presente en el dataset
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

# Esta transformación estadística hace exactamente lo mismo que barplot
ggplot(data = diamonds) +
  stat_count(mapping = aes(x = cut))



# Subconjunto dataset diamantes
demo_diamonds = tribble(
  ~cut,       ~freqs,
  "Fair",       1610,
  "Good",       4906,
  "Very Good", 12082, 
  "Premium",   13791,
  "Ideal",     21551
)



# Cambiando las transformaciones estadísticas
# Identidad
# stat = "identity" debe estar presente porque hemos indicado variable y = freqs y queremos que
#   en el eje vertical se representen las frecuencias absolutas
ggplot(data = demo_diamonds) +
  geom_bar(mapping = aes(x = cut, y = freqs),
              stat = "identity")

# Proporción (Agrupando por filas, agrupando las x)
# Al usar el parámetro y = ..prop.., necesitamos indicar group = 1, para hacer que todas las filas
#   sumen 1
ggplot(data = diamonds) +
  stat_count(mapping = aes(x = cut, y = ..prop.., group = 1))



# Resumen de los datos
ggplot(data = diamonds) +
  stat_summary(mapping = aes(x = cut, y = depth),
               fun.ymin = min,
               fun.ymax = max,
               fun.y = median)



# Colores y formas de los gráficos
# Borde coloreado
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, color = cut))

# Relleno
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = cut))

# Relleno combinando categorías
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = color))



# Parámetro position
# position = "identity" 
# Hace overlapping, conviene no poner relleno. 
# Todas las barras empiezan abajo
ggplot(data = diamonds, mapping = aes(x = cut, color = clarity)) +
  geom_bar(fill = NA, position = "identity")

# position = "fill"
# Sirve para comparar proporciones
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) +
  geom_bar(position = "fill")

# position = "dodge"
# Evita overlapping
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) +
  geom_bar(position = "dodge")

# position = "jitter" con ejemplo de mpg
# Hay overlapping, no se dibujan todos los puntos por redondeo
# Añade ruido que dispersa los puntos que no se veían
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(position = "jitter")

# Ya existe como función
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_jitter()



# Sistemas de coordenadas
# coord_flip() cambia los papeles de x e y
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot() +
  coord_flip()

# coord_quickmap() configura el aspect ratio para mapas
mundi = map_data("world") #Crea el mapa del mundo
ggplot(mundi, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", color = "purple") +
  coord_quickmap()

mundi2 = map_data("world2") #Crea el mapa del mundo
ggplot(mundi2, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", color = "purple") +
  coord_quickmap()

# coord_polar() utiliza coordenadas polares
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = cut),
           show.legend = F,
           width = 1) +
  scale_fill_brewer(palette = "GnBu") +
  theme(aspect.ratio = 1) + #Me hace el plot cuadrado
  labs(x = NULL, y = NULL,
       title = "Gráfico completito",
       subtitle = ":)",
       caption = "By Mery") +
  coord_polar() + 
  facet_wrap(~clarity)



# Grámatica por capas de ggplot2


