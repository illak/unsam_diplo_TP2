library(tidyverse)

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
