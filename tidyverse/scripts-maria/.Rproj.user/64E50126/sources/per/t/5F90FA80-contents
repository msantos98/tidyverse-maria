library(tidyverse)
library(nycflights13)

View(flights)
head(flights)
tail(flights)

?tibble

#tibble es un data frame mejorado para tidyverse
## * int -> números enteros
## * dbl -> números reales (double)
## * chr -> vector de caracteres o strings
## * dttm -> date + time 
## * lgl -> logical, contiene valores booleanos (T o F)
## * fctr -> factor, variables categóricas
## * date -> fecha (día, mes y año)

## * filter() -> filtrar observaciones a partir de valores concretos
## * arrange() -> reordenar las filas
## * select() -> seleccionar variables por sus nombres
## * mutate() -> crea nuevas variables con funciones a partir de las existentes
## * summarise() -> colapsar varios valores para dar un resumen de los mismos

## * group_by() -> opera la función a la que acompaña grupo a grupo





# FILTER ------------------------------------------------------------------

jan1 <- filter(flights, month == 1, day == 1) #Vuelos del 1 de Enero

flights %>% #Para concatenar filters
  filter(month == 1, day == 1) %>%
  filter(dep_delay>0)

may19 <- filter(flights, month == 5, day == 19) #Vuelos del 19 de Mayo

jul26 <- filter(flights, month == 7, day == 26) #Vuelos el día de mi cumple

#Vuelos del 25 de Diciembre
#Además de guardarse el subdataset en la variable, se imprime por consola
(dec25 <- filter(flights, month == 12, day == 25)) 

filter(flights, month == 5) #Vuelos en Mayo



#Para calcular igualdades cuando hay problemas con la finitud de los números con los que trabajan las máquinas
near(sqrt(2)^2, 2)



filter(flights, month == 5 | month == 6) #Vuelos en Mayo o Junio

may_june <- filter(flights, month %in% c(5,6)) #Vuelos en Mayo o Junio





#LEYES DE MORGAN
#!(x&y) == (!x)|(!y)
#!(x|y) == (!x)&(!y)
filter(flights, !(arr_delay > 60 | dep_delay >60))
filter(flights, arr_delay <= 60, dep_delay <= 60)





# Trabajando con NA
age.mery <- NA
is.na(age.mery)

# La función filter no tiene problemas con los NA
df <- tibble(x = c(1,2,NA,4,5))
df
filter(df, x > 2)
filter(df, is.na(x) | x > 2)





# ARRANGE -----------------------------------------------------------------

sorted_date <- arrange(flights, year, month, day) #Ordena por año, mes y, por último, día
flights %>% 
  arrange(year, month, day) #Ordena por año, mes y, por último, día

tail(flights)
tail(sorted_date)

arrange(flights, desc(arr_delay)) #Ordena,en orden descendiente, por retraso en llegadas

arrange(flights, desc(dep_delay)) #Ordena,en orden descendiente, por retraso en salidas

# A la función arrange() tampoco le afectan los NA, los deja al final
arrange(df, x)
arrange(df, desc(x))





# SELECT ------------------------------------------------------------------

View(sorted_date[1024:1068, ])

#De las filas mostradas anteriormente, solamente mostramos dos columnas (las de los retrasos)
View(select(sorted_date[1024:1068, ], dep_delay, arr_delay)) 

select(flights, year, month, day) #Solo mostramos el año, mes y día

select(flights, dep_time:arr_delay) #Solo mostramos las columnas entre dep_time y arr_delay, ambas inclusive

select(flights, -(year:day)) #Mostramos todas las columnas, menos las que hay entre year y day, ambas inclusive

select(flights, starts_with("dep")) #Mostramos todas las columnas cuyo nombre empieza por dep

select(flights, ends_with("delay")) #Mostramos todas las columnas cuyo nombre acaba por delay

select(flights, contains("st")) #Mostramos todas las columnas cuyo nombre contiene por st

select(flights, matches("(.)\\1")) #Mostramos todas las columnas cuyo nombre tiene alguna doble letra (rr, como es el caso)

select(flights, num_range("x",1:5))# #Mostramos todas las columnas cuyo nombre  es x1, x2, x3, x4, x5



#Cambiando las columnas de nombre (primero el cambio, luego el nombre antiguo)
rename(flights, deptime = dep_time, 
       anio = year, mes = month, dia = day)

#Lo bueno que tiene rename es que no omite ninguna columna. 
#En cambio, las siguiente instrucción muestra solo la columna a las que le hemos modificado el nombre
select(flights, deptime = dep_time)

#Esta instrucción lo que hace es cambiar el orden de las columnas, porque primero hemos llamado a tres, y luego al resto
select(flights, time_hour, distance, air_time, everything())





# MUTATE Y TRANSMUTE ------------------------------------------------------

#Seleccionamos el subdataset con el que trabajaremos ahora, para dejar intacto el original
flights_new <- select(flights,
                      year:day, #Todo lo que tenga que ver con tiempo
                      ends_with("delay"), #Todo lo que tenga que ver con retrasos 
                      distance,
                      air_time) 

mutate(flights_new,
       time_gain = arr_delay - dep_delay, #Diferencia de tiempo de retraso (será el retraso total) en minutos
       air_time_hour = air_time/60, #Tiempo de vuelo en horas
       time_gain_per_hour = time_gain/air_time_hour, #Retraso total por hora en minutos
       flight_speed = distance/air_time_hour #Velocidad en Km/h
) -> flights_new #Le asignamos el mutate al subdataset creado anteriormente para no perder las nuevas variables



#Por si nos queremos quedar solamente con las nuevas variables creadas
transmute(flights_new,
          time_gain = arr_delay - dep_delay, 
          air_time_hour = air_time/60, 
          time_gain_per_hour = time_gain/air_time_hour, 
          flight_speed = distance/air_time_hour) -> data_from_flights



# Las transformaciones que podemos utilizar son

# * Operaciones aritméticas: +, -, *, /, ^  

# * Agregados de funciones: x/sum(x) : proporición sobre el total
#                           x - mean(x): distancia respecto de media
#                           (x - mean(x))/sd(x): tipificación
#                           (x - min(x))/(max(x) - min(x)): estandarizar entre [0,1]

# * Aritmética modular: %/% -> cociente de la división entera, %% -> resto de la división entera
#                       x == y * (x%/%y) + (x%%y) 

transmute(flights, 
          air_time,
          hour_air = air_time %/% 60,
          minute_air = air_time %% 60
) # Se queda solamente con las nuevas variables creadas

# * Logaritmos: log() -> logaritmo en base e, log2(), log10()

# * Offsets: lead() -> mueve hacia la izquierda, lag() -> mueve hacia la derecha

df <- 1:12
lag(df) #Mueve una posición a la derecha, los nuevos elementos son NA
lead(df) #Mueve una posición a la izquierda

# * Funciones acumulativas: cumsum(), cumprod(), cummin(), cummax(), cummean()

cumsum(df)
cumprod(df)
cummin(df)
cummax(df)
cummean(df)

# * Comparaciones lógicas: <, <=, >, >=, ==, !=
transmute(flights,
          dep_delay,
          has_been_delayed = (dep_delay > 0))

# * Rankings: min_rank(), row_number(), dense_rank(), percent_rank(), cume_dist(), ntile()

df <- c(7, 1, 2, 5, 3, 3, 8, NA, 3, 4, -2)
min_rank(df) #Nos devuelve las posiciones una vez ordenados de menor a mayor los datos. Si hay empates
             #  se salta tantas posiciones después como números repetidos en dicho empate
min_rank(desc(df)) #Nos devuelve las posiciones una vez ordenados de mayor a menor los datos
row_number(df) #Nos devuelve las posiciones una vez ordenados de menor a mayor los datos SIN empates
dense_rank(df) #El resultado es muy parecido a min_rank(). Considera empates y sigue el orden
percent_rank(df) #Tanto por uno de los datos. Se dividen los datos entre el máximo de estos. Sigue la misma
                 # forma que min_rank
cume_dist(df) #Nos dice los percentiles de los datos. Es el percent_rank() acumulado
ntile(df, n = 4) #Nos dice a qué cuartil pertenecen los datos
ntile(df, n = 100) #Nos dice a qué percentil pertenecen los datos





# SUMMARISE ---------------------------------------------------------------

summarise(flights, 
          mean_delay = mean(dep_delay, na.rm = T))

by_month_group <- group_by(flights, year, month)
summarise(by_month_group, 
          mean_delay = mean(dep_delay, na.rm = T)) # Calculamos retraso mensual

by_day_group <- group_by(flights, year, month, day)
summarise(by_day_group, 
          mean_delay = mean(dep_delay, na.rm = T)) # Calculamos el retraso diario

mutate(summarise(group_by(flights, carrier), 
          mean_delay = mean(dep_delay, na.rm = T)), # Retraso medio por compañía
          sorted = min_rank(mean_delay)) # Ordenamos el retraso medio por compañía

#na.rm significa NA remove





# PIPES -------------------------------------------------------------------

group_by_dest <- group_by(flights, dest) # Agrupamos por destino
delay <- summarise(group_by_dest,
                   count = n(), # Cuenta número de vuelos a cada uno de los destinos
                   dist = mean(distance, na.rm = T), # Media de las distancias
                   delay = mean(arr_delay, na.rm = T) # Media del retraso de llegada
                   )
View(delay)
delay <- filter(delay, 
                count > 100, 
                dest != "HNL") 
# Para ver solamente los destinos que superan los 100 vuelos y destino diferente a HNL

ggplot(delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 0.2) +
  geom_smooth(se = F) +
  geom_text(aes(label = dest, alpha = count))



delays <- flights %>%
  group_by(dest) %>%
  summarise(
    count = n(),
    dist = mean(distance, na.rm = T),
    delay = mean(arr_delay, na.rm = T)
  ) %>%
  filter(count > 100, dest != "HNL")

#En nuestro dataset de vuelos, NA significa vuelo cancelado
not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

#Los vuelos no cancelados y su resumen:
flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
  group_by(year,month,day) %>%
  summarise(mean = mean(dep_delay), # No es necesario parámetro na.rm pues ya los hemos omitido en el filter
            median = median(dep_delay),
            sd = sd(dep_delay),
            count = n()
            ) 

delay_numtail <- not_cancelled %>%
  group_by(tailnum) %>%
  summarise(delay = mean(arr_delay))

ggplot(data = delay_numtail, mapping = aes(x = delay)) +
  geom_freqpoly(binwidth = 5) # Polígono de frecuencias

ggplot(data = delay_numtail, mapping = aes(x = delay)) +
  geom_histogram(binwidth = 2) # Histograma

delay_numtail <- not_cancelled %>%
  group_by(tailnum) %>%
  summarise(delay = mean(arr_delay),
            count = n())

#Retraso en función de número de vuelos
ggplot(data = delay_numtail, mapping = aes(x = count, y = delay)) +
  geom_point(alpha = 0.2) 

delay_numtail %>%
  filter(count > 50) %>% #Nos quedamos solo con los vuelos que han volado más de 50 veces
  ggplot(mapping = aes(x = count, y = delay)) +
  geom_point(alpha = 0.2) 

#command + shift + P -> ejecuta la última línea de código





#### BASEBALL
library(Lahman)

batting <- as_tibble(Batting)

batters <- batting %>%
  group_by(playerID) %>%
  summarise(hits = sum(H, na.rm = T), #Número de veces que le ha dado
            bats = sum(AB, na.rm = T), #Número de oportunidades de bateo
            bat_average = hits/bats)
  
batters %>%
  filter(bats > 100) %>%
  ggplot(mapping = aes(x = bats, y = bat_average)) +
    geom_point(alpha = 0.2) +
    geom_smooth(se = F)

batters %>%
  filter(bats > 100) %>%
  arrange(desc(bat_average))





# * Medidas de Centralización 

not_cancelled %>% #Recordemos que en este dataset no hay NAs en delays
  group_by(carrier) %>% #Agrupamos por compañía aérea
  summarise(mean = mean(arr_delay),
            mean2 = mean(arr_delay[arr_delay > 0]), #Sabiendo que ha sido retrasado, la media de retraso
            median = median(arr_delay)
            )

# * Medidas de Dispersión
not_cancelled %>% #Recordemos que en este dataset no hay NAs en delays
  group_by(carrier) %>% #Agrupamos por compañía aérea
  summarise(sd = sd(arr_delay),
            iqr = IQR(arr_delay), #Rango intercuantílico
            mad = mad(arr_delay) #Desviación absoluta media
            ) %>%
  arrange(desc(sd))

# * Medidas de Orden
not_cancelled %>% # Recordemos que en este dataset no hay NAs en delays
  group_by(carrier) %>% # Agrupamos por compañía aérea
  summarise(first = min(arr_delay),
            q1 = quantile(arr_delay, 0.25), #Primer cuartil
            median = quantile(arr_delay, 0.5),
            q3 = quantile(arr_delay, 0.75), #Tercer cuartil
            last = max(arr_delay)
  ) 
  
# * Medidas de Posición
not_cancelled %>% #Recordemos que en este dataset no hay NAs en delays
  group_by(carrier) %>% #Agrupamos por compañía aérea
  summarise(first_dep  = first(dep_time),
            second_dep = nth(dep_time, 2),
            third_dep = nth(dep_time, 3),
            last_dep = last(dep_time))
  
# * Medidas de Posición
not_cancelled %>% # Recordemos que en este dataset no hay NAs en delays
  group_by(carrier) %>%
  mutate(rank = min_rank(dep_time)) %>%
  filter(rank %in% range(rank)) # Ranking por compañías

# * Funciones de Conteo
flights %>%
  group_by(dest) %>%
  summarise(
    count = n(),
    carriers = n_distinct(carrier),
    arrivals = sum(!is.na(arr_delay))
  ) %>%
  arrange(desc(carriers))

not_cancelled %>%
  count(dest)

not_cancelled %>% # Vuelos ponderados por la distancia que recorren
  count(tailnum, wt = distance)

not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(
    n_before_5am = sum(dep_time < 500) # Total de vuelos  al día que han salido antes de las 5:00am
  )

not_cancelled %>%
  group_by(carrier) %>%
  summarise(
    morethan_hour_delay = mean(arr_delay > 60) #Vuelos retrasados más de una hora en tanto por 1
  ) %>%
  arrange(desc(morethan_hour_delay))





# Agrupaciones múltiples
daily <- group_by(flights, year, month, day)
(flights_per_day <- summarise(daily,
                             n_flights = n())) # Número total de vuelos al día
(flights_per_month <- summarise(flights_per_day,
                               n_flights = sum(n_flights))) #Se pierde el nivel diario
(flights_per_month <- summarise(flights_per_month,
                                n_flights = sum(n_flights))) #Se pierde el nivel mensual

business <- group_by(flights, carrier, dest, origin) 
summarise(business, n_flights = n()) %>%
  summarise(n_flights = sum(n_flights)) %>%
  summarise(n_flights = sum(n_flights))

daily %>% 
  ungroup() %>% # Desagrupa la agrupación
  summarise(n_flights = n())

business %>%
  ungroup() %>%
  summarise(n_flights = n())



flights %>%
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10) # Al final tenemos los 10 peores vuelos de cada día en cuanto a retraso de llegada
  
popular_dest <- flights %>%
  group_by(dest) %>%
  filter(n() > 365) #Al final tendremos los vuelos visitados más de una vez al día

popular_dest %>%
  filter(arr_delay > 0) %>%
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>%
  select(year:day, dest, arr_delay, prop_delay)
  
  
  
  
