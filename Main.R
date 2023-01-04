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

# cargamos las "stopwords" de las clases
stop_words <- read_csv('https://raw.githubusercontent.com/Alir3z4/stop-words/master/spanish.txt', col_names=FALSE) %>%
  rename(word = X1) %>%
  mutate(word = stringi::stri_trans_general(word, "Latin-ASCII"))

# removemos "stopwords"
# data_no_stopwords <- data_tokenizado %>% 
#   anti_join(stop_words)



# vemos la información de manera visual
plot_top_words <- function(dataset) {
  # chequeamos palabras más utilizadas por medio (TOP 10)
  top10_words_by_medio <- dataset %>% 
    group_by(medio, word) %>% 
    summarise(
      count = n()
    ) %>% 
    slice_max(order_by = count, n = 10)
  
  plot <- top10_words_by_medio %>% 
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
      axis.text.x = element_blank(),
      panel.grid = element_blank()
    )
  
  plot
}

#plot_top_words(data_no_stopwords)


# usando otro dataset de "stopwords"
vacias <- read_csv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/vacias.txt",
                   locale = default_locale())

# removemos "stopwords"
data_no_stopwords2 <- data_tokenizado %>% 
  anti_join(vacias %>% rename(word = palabra))

plot_top_words(data_no_stopwords2)


# usando el dataset de "stopwords" de tidytext
# sw_tidytext <- get_stopwords("es")
# 
# data_no_stopwords3 <- data_tokenizado %>% 
#   anti_join(sw_tidytext)
# 
# plot_top_words(data_no_stopwords3)



# Enfoque de bigramas (extra pero puede ser interesante)
bigramas <- raw_data %>%
  unnest_tokens(bigrama,
                texto,
                token = "ngrams",
                n = 2)

bigramas_separados <- bigramas %>%
  separate(bigrama,
           c("palabra1", "palabra2"),
           sep = " ")

bigramas_filtrados <- bigramas_separados %>%
  filter(!palabra1 %in% vacias$palabra,
         !palabra2 %in% vacias$palabra)

top_bigramas_x_medio <- bigramas_filtrados %>% 
  unite(bigrama, palabra1, palabra2, sep = " ")
  count(medio, bigrama, sort = TRUE)


top_bigramas_x_medio %>% 
  mutate(medio = as.factor(medio)) %>% 
  group_by(medio) %>% 
  top_n(10) %>% 
  ggplot(aes(x = n, y = reorder_within(bigrama, n, medio))) +
  geom_col() +
  geom_text(aes(label = n), hjust = -.1) +
  scale_y_reordered() +
  scale_x_continuous(expand = expansion(mult = c(0, .4))) +
  facet_wrap(vars(medio), scales = "free_y")
  
# Análisis de grafos ----
library(igraph)
library(ggraph)
library(grid)

generar_grafo <- function(medio, freq) {
  
  grafo <- bigramas_filtrados %>%
    filter(medio == {medio}) %>% 
    count(palabra1, palabra2, sort = TRUE) %>% 
    filter(n > {freq}) %>%
    graph_from_data_frame()
  
  
  ggraph(grafo, layout = "nicely") +
    geom_edge_link(aes(edge_alpha = n),
                   show.legend = FALSE,
                   arrow = arrow(type = "closed",
                                 length = unit(3, "mm"))) +
    geom_node_point(color = "lightblue", size = 3) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

# grafo para clarín
generar_grafo("pagina12", 100)


# Enfoque tf-idf ----

mystopwords <- tibble(word = c("embed","jpg","loading","hd","protected",
                               "comentar","guardar","leé","minutouno","páginai","lxs",
                               "mirá"))

palabras_medio <- data_tokenizado %>%
  # stopwords (revisar con y sin)
  anti_join(vacias %>% rename(word = palabra)) %>% 
  # "elimino" números
  mutate(word = str_extract(word, "[[:alpha:]]+")) %>%
  anti_join(mystopwords) %>%  
  count(medio, word, sort = TRUE)

palabras_medio <- palabras_medio %>% 
  bind_tf_idf(word, medio, n)


palabras_medio %>% 
  group_by(medio) %>% 
  slice_max(order_by = tf_idf, n = 10) %>% 
  ggplot(aes(x = tf_idf, y = reorder_within(word, tf_idf, medio))) +
  geom_col(aes(fill = medio), show.legend = FALSE) +
  #geom_text(aes(label = tf_idf), hjust = -.1) +
  # para ordenar las barras al interior de cada facet (se usa en conjunto con "reorder_within")
  scale_y_reordered() +
  # expandimos un poco a la derecha (40%) para que entre el texto de la barra
  scale_x_continuous(expand = expansion(mult = c(0, .4))) +
  guides(fill = "none") +
  facet_wrap(vars(medio), scales = "free_y") +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    panel.grid = element_blank()
  )

# análisis por fecha (mes)

top_words_medio_mes <- function(mes_filter, n_top = 10){
  df <- data_tokenizado %>%
    filter(mes == mes_filter) %>% 
    # stopwords (revisar con y sin)
    anti_join(vacias %>% rename(word = palabra)) %>% 
    # "elimino" números
    mutate(word = str_extract(word, "[[:alpha:]]+")) %>%
    anti_join(mystopwords) %>%  
    count(medio, word, sort = TRUE) %>% 
    ungroup() %>% 
    bind_tf_idf(word, medio, n) %>% 
    group_by(medio) %>% 
    slice_max(order_by = tf_idf, n = n_top)
  
  df
}


top_words_medio_mes(7) %>% 
  ggplot(aes(x = tf_idf, y = reorder_within(word, tf_idf, medio))) +
  geom_col(aes(fill = medio), show.legend = FALSE) +
  #geom_text(aes(label = tf_idf), hjust = -.1) +
  # para ordenar las barras al interior de cada facet (se usa en conjunto con "reorder_within")
  scale_y_reordered() +
  # expandimos un poco a la derecha (40%) para que entre el texto de la barra
  scale_x_continuous(expand = expansion(mult = c(0, .4))) +
  guides(fill = "none") +
  facet_wrap(vars(medio), scales = "free_y") +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    panel.grid = element_blank()
  )




top_words_medio_dia_mes <- function(dia_f, mes_f, top_n = 10){
  df <- data_tokenizado %>%
    filter(dia == dia_f, mes == mes_f) %>% 
    # stopwords (revisar con y sin)
    anti_join(vacias %>% rename(word = palabra)) %>% 
    # "elimino" números
    mutate(word = str_extract(word, "[[:alpha:]]+")) %>%
    anti_join(mystopwords) %>%  
    count(medio, word) %>% 
    ungroup() %>% 
    bind_tf_idf(word, medio, n) %>% 
    group_by(medio) %>% 
    slice_max(order_by = tf_idf, n = top_n)
  
  df
}

top_words_medio_dia_mes(12,9) %>% 
  ggplot(aes(x = tf_idf, y = reorder_within(word, tf_idf, medio))) +
  geom_col(aes(fill = medio), show.legend = FALSE) +
  #geom_text(aes(label = tf_idf), hjust = -.1) +
  # para ordenar las barras al interior de cada facet (se usa en conjunto con "reorder_within")
  scale_y_reordered() +
  # expandimos un poco a la derecha (40%) para que entre el texto de la barra
  scale_x_continuous(expand = expansion(mult = c(0, .4))) +
  guides(fill = "none") +
  facet_wrap(vars(medio), scales = "free") +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    panel.grid = element_blank()
  )




# TOPIC MODELING ----
# limpiamos el dataset
# más stopwords!!
stopwords_extra <- read.delim("https://raw.githubusercontent.com/Alir3z4/stop-words/master/spanish.txt",
                              col.names = c("word"))

data_clean <- data_tokenizado %>%
  # removemos stopwords
  anti_join(vacias %>% rename(word = palabra)) %>% 
  # removemos stopwords extra
  anti_join(stopwords_extra) %>% 
  # "elimino" números
  mutate(word = str_extract(word, "[[:alpha:]]+")) %>%
  filter(!is.na(word)) %>% 
  # removemos algunas palabras detectadas "a mano"
  anti_join(mystopwords)


# Topic Modeling por noticia ----
library(topicmodels)

cuenta_palabras2 <- data_clean %>% 
  unite(document, medio, id) %>% 
  count(document, word, sort = TRUE)

medios_tdm <- cuenta_palabras2 %>% 
  cast_dtm(document, word, n)

# 10 tópicos
medios_lda <- LDA(medios_tdm, k = 10, control = list(seed = 42))
medios_lda

# guardamos el modelo (pesado!)
saveRDS(medios_lda, "model_LDA.rda")

medios_temas <- tidy(medios_lda, matrix = "beta")

# top palabras temas en cada topico
top_temas <- medios_temas %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 5) %>% 
  ungroup() %>% 
  arrange(topic, -beta)

top_temas %>% View()

top_temas %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# Una forma de ver los términos más utilizados en cada tópico
as.data.frame(terms(medios_lda, 10)) %>% View()


# GAMMA
medios_gamma <- tidy(medios_lda, matrix = "gamma")
medios_gamma


#TODO debemos separar los documentos por MEDIO y luego promediar el tópico de cada uno,
# quizás luego se puede volver a traer el dato de la fecha usando ID y
# luego se puede hacer un análisis temporal de tópicos
