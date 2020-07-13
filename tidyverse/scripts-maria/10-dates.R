library(tidyverse)
library(lubridate)
library(nycflights13)

#<date> -> fecha
#<time> -> hora
#<dttm> -> fecha y hora
#library(hms) -> sirve para crear nuestros propios formatos de fecha y hora

today() #día de hoy
now() #tiempo ahora

ymd("2015-06-13") #año mes día
mdy("Enero 30th, 2018") #mes día año
dmy("8-Jun-2018") #día mes año
ymd(20180608) #estas funciones aceptan también números y no solo strings, pero hay que
              #meterlos en formato completo

ymd_hms("2018-06-08 19:35:28") #año mes día hora minuto segundo
mdy_hm("06/30/2017 05:30", tz = "GMT") #podemos aportar la time zone





flights %>% 
  select(year, month, day, hour, minute) %>%
  mutate(departure = make_datetime(year, month, day, hour, minute))
#creamos una nueva columna de formato datetime

make_date_time_100 <- function(year, month, day, time){
  make_datetime(year, month, day, time %/% 100, time %% 100)
} #función que dado año, mes, día y objeto time me lo devuelve en formato datetime

flights_dt <- flights %>%
  filter(!is.na(dep_time), !is.na(arr_time)) %>% #quitamos NA de en medio
  mutate(
    dep_time = make_date_time_100(year, month, day, dep_time),
    arr_time = make_date_time_100(year, month, day, arr_time),
    sched_dep_time = make_date_time_100(year, month, day, sched_dep_time),
    sched_arr_time = make_date_time_100(year, month, day, sched_arr_time)
  ) %>%
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt %>% 
  View()

flights_dt %>%
  ggplot(aes(dep_time)) + 
    geom_freqpoly(binwidth = 24*60*60) #mismo número de divisiones que segundos tiene 1 día

flights_dt %>%
  filter(dep_time < ymd(20130102)) %>% #nos quedamos con el primer día del dataset
  ggplot(aes(dep_time)) +
    geom_freqpoly(binwidth = 600) #600 s = 10 min (bloques de 10 minutos)





as_datetime(today())
as_date(now())

#para dates el 1 es un día
#para datetimes el 1 es un segundo

#EPOCH -> 1970-01-01
as_datetime(60*60*2018) # Cada unidad representa un segundo
as_date(365*10 + 2)     # Cada unidad representa un día





nowwww <- now()
nowwww

year(nowwww)
month(nowwww, label = TRUE, abbr = FALSE) #label es para la etiqueta, y abbr para la abreviación
mday(nowwww)
yday(nowwww)
wday(nowwww, label = TRUE, abbr = FALSE)
hour(nowwww)
minute(nowwww)
second(nowwww)





#¿Qué día de la semana se viaja más?
flights_dt %>%
  mutate(wday = wday(dep_time, label = TRUE, abbr = FALSE)) %>%
  ggplot(aes(x = wday)) +
  geom_bar()



#Promedio de retraso de un avión por minuto dentro de una hora
flights_dt %>%
  mutate(minute = minute(dep_time)) %>%
  group_by(minute) %>%
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  ) %>%
  ggplot(aes(minute, avg_delay)) + 
  geom_line()



#Programación del horario tiene patrón?
flights_dt %>%
  mutate(minute = minute(sched_dep_time)) %>%
  group_by(minute) %>%
  summarise(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    n = n()
  ) %>%
  ggplot(aes(minute, avg_delay))+
  geom_line()
  


#Programación de salida de vuelos tiene un patrón: salen más vuelos en horas "bonitas"
flights_dt %>%
  mutate(minute = minute(sched_dep_time)) %>%
  group_by(minute) %>%
  summarise(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    n = n()
  ) %>%
  ggplot(aes(minute, n))+
  geom_line()
  




#floor_date()
#round_date()
#ceiling_date()

#Número de vuelos por semana
flights_dt %>%
  count(week = floor_date(dep_time, "week")) %>%
  ggplot(aes(week, n)) + 
  geom_line()



d <- now()
d  

year(d) <- 2030
month(d) <- 11
hour(d) <- hour(d) + 3
 
#Podemos cambiar la fecha creando una nueva a partir de la original
update(d, year = 2020, month = 10, mday = 7, hour = 4)

dmy("01-02-2018") %>%
  update(mday = 30)

dmy("01-02-2018") %>%
  update(hour = 54321)

#Todos los vuelos reescalados al dep_time primer día del año
flights_dt %>%
  mutate(dep_hour = update(dep_time, yday = 1)) %>%
  ggplot(aes(dep_hour)) + 
  geom_freqpoly(binwidth = 300)





#LAPSOS DE TIEMPO
# *duraciónes -> número exacto de segundos
# *periodos -> unidades humanas (semana, mes, año)
# *intervalos -> punto de partida y de final


 
##Duración

jb_age <- today() - ymd(19880519)
mery_age <- today() - ymd(19980726)

class(jb_age) #Clase de diferencia horaria

as.duration(jb_age)
as.duration(mery_age)

dseconds(3250) #duración en segundos
dminutes(1024) #duración en minutos
dhours(c(12, 24)) #duración en horas
ddays(5:10) #duración en días
dweeks(4)  #duración en semanas
dyears(1) #duración en años

#Podemos utilizar operaciones aritméticas
2*dyears(1)
dyears(1) + dweeks(13) + dhours(22)

tomorrow <- today() + ddays(1) #obtenemos el día de mañana sumando a hoy un día
tomorrow

last_year <- today() - dyears(1) #Día de hoy el año pasado
last_year

one_pm <- ymd_hms("2016-03-12 13:00:00", tz = "America/New_York")
one_pm
one_pm + ddays(1) #Las zonas horarias no se mantienen, porque es el día en que se cambia la hora



#Periodos

one_pm
one_pm + days(1) #Con periodos resolvemos el problema del cambio de hora anterior

seconds(35)
minutes(15)
hours(c(12,24))
days(7)
weeks(4)
months(1:6)
years(1)

#Podemos hacer operaciones aritméticas
10*months(6) + days(8)
days(20)+ hours(32) + minutes(45)
ymd("2016-05-19") + years(2)



flights_dt %>%
  filter(arr_time < dep_time) %>% 
  View() #Esto pasa porque cambia la zona horaria

#Para solucionar el problema anterior, realizamos lo siguiente
flights_dt %>%
  mutate(
    overnight = arr_time < dep_time,
    arr_time = arr_time + days(overnight*1),
    sched_arr_time = sched_arr_time + days(overnight*1)
  ) -> flights_dt

flights_dt %>%
  filter(overnight, arr_time < dep_time)



#Intervalos

dyears(1)/ddays(365) #Esto funciona porque trabajamos con segundos

years(1)/days(1) #No funciona, necesitamos intervalos

next_year <- today() + years(1)
(today() %--% next_year) %/% ddays(1)




 
#Time Zones 
# IANA -> <continente>/<city>
# Europe/Madrid, Europe/Paris, America/New_York

Sys.timezone() #Nos dice nuestra zona horaria

length(OlsonNames()) #Número total de zonas horarias
head(OlsonNames())



#Mismo instante de tiempo en 3 lugares diferentes del mundo
(x1 <- ymd_hms("2018-06-08 12:00:00", tz = "America/New_York"))
(x2 <- ymd_hms("2018-06-08 18:00:00", tz = "Europe/Copenhagen"))
(x3 <- ymd_hms("2018-06-09 04:00:00", tz = "Pacific/Auckland"))

x1-x2
x2-x3



#UTC <-> GMT
x4 <- c(x1,x2,x3) #R internacionaliza internamente
x4
x4a <- with_tz(x4, tzone = "Europe/Madrid")
x4a

x4a - x4

x4b <- force_tz(x4, tzone = "Australia/Lord_Howe")
x4b
x4b - x4
