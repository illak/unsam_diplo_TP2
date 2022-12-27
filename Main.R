library(tidyverse)
library(tidytext)

raw_data <- read_csv("data/M5_corpus_medios.csv")


# EDA ----
# chequeo variables del dataset
names(raw_data)

# vistazo rápido del dataset
raw_data %>% 
  head(10) %>% 
  View()

# chequeo resumen del dataset
summary(raw_data)





# Tokenización / Limpieza ----

# Metodo clásico ----
# PRIMERO USAMOS MÉTODO "CLÁSICO o TRADICIONAL": tokenización + stopwords
# chequeo dimensiones (cantidad de filas luego de la tokenización)
raw_data %>% 
  unnest_tokens(palabra, texto) %>% 
  nrow()

# chequeo primeras filas
raw_data %>% 
  unnest_tokens(palabra, texto) %>% 
  head(20) %>% 
  View()

# tokenizo
data_tokenizado <- raw_data %>% 
  unnest_tokens(word, texto)

# cargamos las "stopwords"
stop_words <- read_csv('https://raw.githubusercontent.com/Alir3z4/stop-words/master/spanish.txt', col_names=FALSE) %>%
  rename(word = X1) %>%
  mutate(word = stringi::stri_trans_general(word, "Latin-ASCII"))

# removemos "stopwords"
data_no_stopwords <- data_tokenizado %>% 
  anti_join(stop_words)

# chequeamos palabras más utilizadas por medio (TOP 10)
top10_words_by_medio <- data_no_stopwords %>% 
  group_by(medio, word) %>% 
  summarise(
    count = n()
  ) %>% 
  slice_max(order_by = count, n = 10)


# vemos la información de manera visual
top10_words_by_medio %>% 
  mutate(medio = as.factor(medio)) %>% 
  ggplot(aes(x = count, y = reorder_within(word, count, medio))) +
  geom_col(aes(fill = medio), show.legend = FALSE) +
  geom_text(aes(label = count), hjust = -.1) +
  # para ordenar las barras al interior de cada facet (se usa en conjunto con "reorder_within")
  scale_y_reordered() +
  # expandimos un poco a la derecha (40%) para que entre el texto de la barra
  scale_x_continuous(expand = expansion(mult = c(0, .4))) +
  guides(fill = "none") +
  facet_wrap(vars(medio), scales = "free_y") +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank()
  )
