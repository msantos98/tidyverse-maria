library(tidyverse)
library(forcats)

day_levels <- c("Lun", "Mar", "Mie", "Jue", "Vie", "Sab", "Dom")

x1 <- c("Vie", "Lun", "Mar", "Dom")
sort(x1)

y1 <- factor(x1, levels = day_levels)
sort(y1)

x2 <- c("Vim", "Lun", "Mar", "Dom")
y2 <- factor(x2, levels = day_levels)
y2 #El valor que no es uno de los factores ha quedado reemplazado por un NA
y2 <- parse_factor(x2, levels = day_levels) #Si queremos Warning, utilizamos parse_factor

factor(x1) #Si no indicamos los niveles, se crean a partir de las entradas del vector 
           #y los ordena automáticamente
f1 <- factor(x1, levels = unique(x1)) #Si indicamos los niveles con la función unique,
                                      #se crean los niveles a partir de las entradas del
                                      #vector y se ordenan por orden de aparición
f2 <- x1 %>% 
  factor() %>% 
  fct_inorder() #Define los factores de los niveles en el orden de aparición

levels(f2) #Para acceder a los niveles de un factor





gss_cat %>% 
  View()

gss_cat %>% 
  count(marital) #Contar las ocurrencias de un nivel

gss_cat %>% 
  ggplot(aes(marital)) + 
    geom_bar() #Por defecto, ggplot se deshace de los niveles que no tinen ningún valor

gss_cat %>% 
  ggplot(aes(race)) + 
    geom_bar() + 
    scale_x_discrete(drop = FALSE) #Para que ggplot no se deshaga de los niveles vacíos
    #Not applicable significa que no contestó a la pregunta referente a la raza





#horas de tele en función de la religión
religion_summary <- gss_cat %>%
  group_by(relig) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

religion_summary %>% View()

ggplot(religion_summary, aes(tvhours, relig)) + 
  geom_point() #No nos sirve de mucho

#La función de ordenación por defecto de fct_reorder es la mediana (de menor a mayor)
ggplot(religion_summary, aes(tvhours, fct_reorder(relig, tvhours))) + 
  geom_point()
#fct_reorder(relig, tvhours) lo que hace es ordenar el factor relig en fución de tvhours
#donde este último es ordenado de menor a mayor

religion_summary %>%
  mutate(relig = fct_reorder(relig, tvhours)) %>%
  ggplot(aes(tvhours, relig)) + 
  geom_point() #Hace lo mismo que la instrucción anterior, lo que hemos llevado antes a cabo
               #la reordenación del factor relig





#income en función de horas de tele
gss_cat %>%
  group_by(rincome) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  ) %>%
  mutate(rincome = fct_reorder(rincome, age)) %>%
  ggplot(aes(age,rincome)) +
    geom_point()
#Lo que ocurre es que rincome ya estaba ordenado de forma coherente

gss_cat %>%
  group_by(rincome) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  ) %>%
  #mutate(rincome = fct_reorder(rincome, age)) %>%
  mutate(rincome = fct_relevel(rincome, "Not applicable")) %>% #recolocamos Not applicable al final
  ggplot(aes(age, rincome)) +
    geom_point()



by_age <- gss_cat %>%
  filter(!is.na(age)) %>% #quitamos los NA
  group_by(age, marital) %>%
  count()

ggplot(by_age, aes(age, n, color = marital)) + 
  geom_line(na.rm = TRUE)

#Ordenamos los niveles de la leyenda para que todo sea más visual
ggplot(by_age, aes(age, n, color = fct_reorder2(marital, age, n)))+
  geom_line(na.rm = TRUE)+
  labs(color = "Marital") 

gss_cat %>%
  mutate(marital = marital %>% 
           fct_infreq() %>% #reordena los factores por primera aparición o frecuencia
           fct_rev()) %>% #orden creciente
  ggplot(aes(marital)) + 
    geom_bar()





#Redefiniendo niveles con fct_recode
gss_cat %>% 
  count(partyid)

gss_cat %>%
  mutate(partyid = fct_recode(partyid,
    "Republican, strong"      = "Strong republican",
    "Republican, weak"        = "Not str republican",
    "Independent, near rep"   = "Ind,near rep",
    "Independent, near dem"   = "Ind,near dem",
    "Democrat, weak"          = "Not str democrat",
    "Democrat, strong"        = "Strong democrat",
    "Other"                   = "No answer",
    "Other"                   = "Don't know",
    "Other"                   = "Other party"
  )
  ) %>% count(partyid)



#aglutinamos los niveles
gss_cat %>%
  mutate(partyid = fct_collapse(partyid,
          other = c("No answer", "Don't know", "Other party"),
          republican   = c("Strong republican", "Not str republican"),
          independent   = c("Ind,near rep", "Independent", "Ind,near dem"),
          democrat   = c("Not str democrat", "Strong democrat")
        )
  ) %>%
  count(partyid)



#juntamos los factores menos comunes en la categoría otros
gss_cat %>%
  mutate(relig = fct_lump(relig, n = 5)) %>% #Hacemos un top n = 5
  count(relig, sort = TRUE) %>%
  print(n = 3) #Imprimimos el top n=3

