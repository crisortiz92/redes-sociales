# Clear plots
if(!is.null(dev.list())) dev.off()
#Tamanio de Memoria
memory.size(max=F)
# Garbage collection
gc()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())
# Restart R
.rs.restartR()

### Carga de Librerías ####
library(rtweet)
library(tidyverse)
library(lubridate)

##API Key & Secret ##
consumer_key = ''
consumer_secret = ''
access_token = ''
access_secret = ''

### Autenticación vía browser ####
token <- create_token(
  app = "",
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret)

n = 3200 #número maximo de tweets

### Get_timeline se usa para extraer los post de una cuenta ####

guillermo <- get_timeline(user = "@lassoguillermo", n = n, parse = TRUE,
                          check = TRUE, include_rts = FALSE)
### Ajustamos la fecha para que sea la de Ecuador ###
guillermo <- guillermo %>% mutate(created_at = with_tz(created_at,
                                                         tz = "America/Bogota") )


correa <- get_timeline(user = "@mashirafael", n = n, parse = TRUE,
                          check = TRUE, include_rts = FALSE)
### Ajustamos la fecha para que sea la de Ecuador ###
correa <- correa %>% mutate(created_at = with_tz(created_at,
                                                       tz = "America/Bogota") )

  
abdala <- get_timeline(user = "@abdalabucaram", n = n, parse = TRUE,
                           check = TRUE, include_rts = FALSE)
### Ajustamos la fecha para que sea la de Ecuador ###
abdala <- abdala %>% mutate(created_at = with_tz(created_at,
                                                         tz = "America/Bogota") )


write_rds(guillermo, "guillermo.rds")
write_rds(correa, "correa.rds")
write_rds(abdala, "abdala.rds")


# Se unen todos los tweets en un único dataframe
tweets <- bind_rows(guillermo, correa, abdala)
write_rds(tweets, "tweets.rds")

#agrupo por nombre de usuario y totalizo
tweets %>% group_by(screen_name) %>% summarise(numero_tweets = n()) 


#Selección de variables
tweets <- tweets %>% dplyr::select(screen_name, created_at, status_id, text)

#Se renombran las variables con nombres más prácticos
tweets <- tweets %>% rename(autor = screen_name, fecha = created_at,
                            texto = text, tweet_id = status_id)

limpiar_tokenizar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minúsculas
  nuevo_texto <- tolower(texto)
  # Eliminación de páginas web (palabras que empiezan por "http." seguidas 
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # Eliminación de signos de puntuación
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # Eliminación de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  # Tokenización por palabras individuales
  nuevo_texto <- str_split(nuevo_texto, " ")[[1]]
  # Eliminación de tokens con una longitud < 2
  nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 1})
  return(nuevo_texto)
}

# Se aplica la función de limpieza y tokenización a cada tweet
tweets <- tweets %>% dplyr::mutate(texto_tokenizado = purrr::map(.x = texto,
                                                   .f = limpiar_tokenizar))

tweets_tidy <- tweets %>% dplyr::select(-texto) %>% unnest()
tweets_tidy <- tweets_tidy %>% rename(token = texto_tokenizado)
head(tweets_tidy) 


### Distribución por separado ###
tweets %>% filter(fecha > '2020/07/01') %>% 
  ggplot(aes(x = as.Date(fecha), fill = autor)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  scale_x_date(date_labels = "%d-%m-%Y", date_breaks = "1 month") +
  labs(x = "fecha de publicación", y = "número de tweets") +
  facet_wrap(~ autor, ncol = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

#En el mismo gráfico líneas
tweets_mes_anyo <- tweets %>% filter(fecha > '2020/07/01') %>% mutate(mes_anyo = format(fecha, "%m-%d"))
tweets_mes_anyo %>% group_by(autor, mes_anyo) %>% summarise(n = n()) %>%
  ggplot(aes(x = mes_anyo, y = n, color = autor)) +
  geom_line(aes(group = autor)) +
  labs(title = "Número de tweets publicados", x = "",
       y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 8),
        legend.position = "bottom") +
  annotate(
    geom = "curve", x = 7, y = 35, xend = 5, yend = 45, 
    curvature = .4, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = 7.2, y = 35, label = "Abdalá realizó un monólogo en Twitter", hjust = "left")

#### Frecuencia de palabras ####

## Total de palabras escritas por cada usuario ##
tweets_tidy %>% filter(fecha > '2020/07/01') %>% group_by(autor) %>% summarise(n = n()) 
tweets_tidy %>% filter(fecha > '2020/07/01') %>% ggplot(aes(x = autor)) +
  geom_bar() + coord_flip() + theme_bw() +
  labs(
    x = NULL, y = NULL,
    title = "Total de palabras escritas",
    subtitle = "Meses de Julio - Agosto",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )


### Palabras distintas utilizadas por cada usuario ###
tweets_tidy %>% filter(fecha > '2020/07/01')  %>% select(autor, token) %>% 
  distinct() %>%  group_by(autor) %>% summarise(palabras_distintas = n()) 

tweets_tidy %>% filter(fecha > '2020/07/01') %>% dplyr::select(autor, token) %>%
  distinct() %>%   ggplot(aes(x = autor)) + geom_bar() + coord_flip() +
  theme_bw() +
  labs(
    x = NULL, y = NULL,
    title = "Total de palabras diferentes escritas",
    subtitle = "Meses de Julio - Agosto",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

### Longitud media de los tweets por usuario ###
tweets_tidy %>% filter(fecha > '2020/07/01') %>% group_by(autor, tweet_id) %>% summarise(longitud = n()) %>%
  group_by(autor) %>% summarise(media_longitud = mean(longitud),sd_longitud = sd(longitud))

tweets_tidy %>% filter(fecha > '2020/07/01') %>%  group_by(autor, tweet_id) %>% summarise(longitud = n()) %>%group_by(autor) %>%
  summarise(media_longitud = mean(longitud),
            sd_longitud = sd(longitud)) %>%
  ggplot(aes(x = autor, y = media_longitud)) +
  geom_col() +
  geom_errorbar(aes(ymin = media_longitud - sd_longitud,
                    ymax = media_longitud + sd_longitud)) +
  coord_flip() + theme_bw() +
  labs(
    x = NULL, y = NULL,
    title = "Longitud media de los tweets por usuario",
    subtitle = "Y su desviación estándar",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

### Palabras más utilizadas por usuario ###
tweets_tidy %>% filter(fecha > '2020/07/01') %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
  top_n(10, n) %>% arrange(autor, desc(n)) %>% print(n=30)

### Filtramos StopWords ###
tweets_tidy <- tweets_tidy %>% filter(!(token %in% tm::stopwords(kind="es")))

### Representación gráfica de las frecuencias ###
tweets_tidy %>% filter(fecha > '2020/07/01') %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
  top_n(10, n) %>% arrange(autor, desc(n)) %>%
  ggplot(aes(x = reorder(token,n), y = n, fill = autor)) +
  geom_col() +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  facet_wrap(~autor,scales = "free", ncol = 1, drop = TRUE)

### Correlación entre usuarios por palabras utilizadas ###
library(gridExtra)
library(scales)

tweets_spread <- tweets_tidy %>% filter(fecha > '2020/07/01') %>% group_by(autor, token) %>% count(token) %>%
  spread(key = autor, value = n, fill = NA, drop = TRUE)

cor.test(~ LassoGuillermo + MashiRafael, method = "pearson", data = tweets_spread)

cor.test(~ LassoGuillermo + abdalabucaram, data = tweets_spread)

p1 <- ggplot(tweets_spread, aes(MashiRafael,LassoGuillermo)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

p2 <- ggplot(tweets_spread, aes(abdalabucaram,LassoGuillermo)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

grid.arrange(p1, p2, nrow = 1)

## Para poder valorar adecuadamente el nivel de correlación es interesante 
## conocer el número de palabras comunes entre cada par de autores.

palabras_comunes <- dplyr::intersect(tweets_tidy %>% filter(fecha > '2020/07/01') %>% 
                                       filter(autor=="LassoGuillermo") %>%
                                       dplyr::select(token), tweets_tidy %>% 
                                       filter(autor=="MashiRafael") %>%
                                       dplyr::select(token)) %>% nrow()
paste("Número de palabras comunes entre Guillermo Lasso y Rafael correa", palabras_comunes)

palabras_comunes <- dplyr::intersect(tweets_tidy %>% filter(fecha > '2020/07/01') %>%
                                       filter(autor=="LassoGuillermo") %>%
                                       dplyr::select(token), tweets_tidy %>% 
                                       filter(autor=="abdalabucaram") %>%
                                       dplyr::select(token)) %>% nrow()
paste("Número de palabras comunes entre Guillermo Lasso y Abdalá Bucaram", palabras_comunes)