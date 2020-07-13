library(tidyverse)

# Análisis exploratorio de los datos
# * Modelar
# * Representación gráfica
# * Transformar datos

# * ¿Qué tipo de variaciones sufren las variables?
# * ¿Qué tipo de covariación sufren las variables?





### VARIACIÓN
## Variables categóricas
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

diamonds %>%
  count(cut)

## Variables continuas
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5) #binwidth para indicar tamaño del intervalo

diamonds %>% 
  count(cut_width(carat, 0.5)) #cut_width() para indicar el tamañl del intervalo

diamonds_filter <- diamonds %>%
  filter(carat < 3)

ggplot(data = diamonds_filter) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.05) #Hemos refinado los intervalos

library(wesanderson)
ggplot(data = diamonds_filter, mapping = aes(x = carat, color = cut)) +
  geom_freqpoly(binwidth = 0.1) 

# * ¿Qué valores son más comunes? ¿Por qué?
# * ¿Qué valores son más raros? ¿Por qué? ¿Cumple con lo que esperábamos?
# * ¿Vemos algún patrón característico o inusual?

#Refinando en el histograma el intervalo a 0.01, vemos el patrón de que después de un valor alto, decae el valor: alto-decae-alto-decae-...
ggplot(data = diamonds_filter) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.01)

# * ¿Qué determina que los elementos de un cluster sean similares entre sí?
# * ¿Qué determina que clusters separados sean diferentes entre sí?
# * Describir y explicar cada uno de los clusters
# * ¿Por qué una observación puede ser clasificada en el cluster erróneo?



### GEYSER
# * eruptions: tiempo de erupción en minutos
# * waiting: tiempo en minutos hasta la siguiente erupción
ggplot(data = faithful, mapping = aes(x = eruptions)) +
  geom_histogram(binwidth = 0.2)
#Hay dos grupos, separados en el propio histograma: erupciones cortas y largas



#outliers
ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0,100))

unusual_diamonds <- diamonds %>%
  filter(y < 2 | y > 30) %>%
  select(price, x, y, z) %>%
  arrange(y)

#2 opciones para solventar outliers:
# * Cargarnos la fila pertinente. Opciones poco recomendable
good_diamonds <- diamonds %>%
  filter(between(y, 2.5, 29.5))

# * Reemplazar los valores inusuales por NA
good_diamonds <- diamonds %>%
  mutate(y = ifelse((y<2 | y>30), NA, y))

ggplot(data = good_diamonds, mapping = aes(x = x, y = y)) +
  geom_point(na.rm = T, alpha = 0.2)



library(nycflights13)
flights %>%
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_minute = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_minute/60
  ) %>%
  ggplot(mapping = aes(sched_dep_time)) +
    geom_freqpoly(mapping = aes(color = cancelled), binwidth = 1/4)





### COVARIACIÓN
# Categoría vs Var. Continua
ggplot(data = diamonds, mapping = aes(x = price)) +
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)

ggplot(diamonds) +
  geom_bar(mapping = aes(x = cut))

ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) +
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot() #Nos hace intuir que el corte no está relacionado con el precio



ggplot(mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

ggplot(mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) #Ordena por medianas

ggplot(mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip() #Para girar 90 grados el gráfico



# Categoría vs Categoría
ggplot(diamonds) +
  geom_count(mapping = aes(x = cut, y = color)) #Visualización de tabla de contingencia

diamonds %>%
  count(color,cut) #Tabla de contingencia de estas dos variables

diamonds %>%
  count(color, cut) %>%
  ggplot(mapping = aes(x = cut, y = color)) +
  geom_tile(mapping = aes(fill = n)) +
  scale_fill_gradientn(colours=rainbow(5))



# Continua vs Continua
ggplot(diamonds) +
  geom_point(mapping = aes(x = carat, y = price), alpha = 0.01)

library(hexbin)
ggplot(diamonds) +
  geom_hex(mapping = aes(x = carat, y = price))

diamonds %>%
  filter(carat < 3) %>%
  ggplot(mapping = aes(x = carat, y = price)) +
    geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)), varwidth = T)

diamonds %>%
  filter(carat < 3) %>%
  ggplot(mapping = aes(x = carat, y = price)) +
    geom_boxplot(mapping = aes(group = cut_number(carat, 20)))



## Búsqueda de patrones
# * ¿Coincidencias? -> Patrón debido al azar?¿
# * Relaciones que implica el patrón
# * Fuera de la relación
# * ¿Otras variables afectadas?
# * ¿Subgrupos?

ggplot(faithful) +
  geom_point(mapping = aes(x = eruptions, y = waiting))

library(modelr)
mod <- lm(log(price)~log(carat), data = diamonds)

diamonds_pred <- diamonds %>%
  add_residuals(mod) %>%
  mutate(res = exp(resid))


ggplot(data = diamonds_pred) +
  geom_point(mapping = aes(x = carat, y = resid))

ggplot(data = diamonds_pred) +
  geom_boxplot(mapping = aes(x = cut, y = resid))





