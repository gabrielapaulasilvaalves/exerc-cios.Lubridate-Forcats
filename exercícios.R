#Nos exercícios a seguir, vamos utilizar a base lakers, que contém estatísticas
#jogo a jogo do Los Angeles Lakers na temporada 2008-2009.

# 1. Repare que a coluna date no data.frame é um vetor de inteiros. Transforme
# essa coluna em um vetor de valores com classe date.

"1."
#Instalar pacotes
install.packages("magrittr")
library("magrittr")
install.packages("tibble")
library(tibble)
install.packages("lubridate")
library(lubridate)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
lakers %>% as_tibble()
#Transformando em coluna
lakers%>%
  mutate(date = ymd(date))

#2. Crie uma coluna que junte as informações de data e tempo de jogo (colunas
#date e time) em objetos da classe date.

"2."
lakers%>%
  mutate(date_time = paste0(date, "00:", time)%>% ymd_hms) %>%
  select(date_time)

#3. Crie as colunas dia, mes e ano com as respectivas informações sobre a
# data do jogo.

"3."
lakers %>%
  mutate(
    date = ymd(date),
    dia  = day(date),
    mes  = month(date),
    ano  = year(date)
    )%>%
  select(date, dia, mes, ano)

#4. Em média, quanto tempo o Lakers demora para arremessar a primeira bola no
#primeiro período?
"4."
lakers %>%
  dplyr::filter(etype == "shot", period == 1, team == "LAL") %>% #filtro
  dplyr::mutate( #mudar a classe
    time = hms(paste0(time, "00;", time)),
    cronometro = 12*60 - minute(time)*60 - second(time)
  ) %>%
  dplyr::group_by(date) %>% #agrupar
  dplyr::filter(cronometro == min(cronometro)) %>%
  dplyr::ungroup() %>%
  dplyr::summarise(media = mean(cronometro))

install.packages("usethis")
usethis::use_git()
usethis::use_github()
