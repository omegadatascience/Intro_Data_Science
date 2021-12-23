#-----------------------------------------------------------------------
# Introdução à Ciência de Dados
#
#                                            Prof. Dr. Walmes M. Zeviani
#                                                     Ômega Data Science
#                                                            2021-out-28
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------

library(tidyverse)
search()

# Caminho para o arquivo na WEB.
url <- "http://leg.ufpr.br/~walmes/data/euro_football_players.txt"

# Leitura com `readr`.
tb <- read_tsv(file = url,
               col_names = TRUE,
               quote = "",
               comment = "#")
class(tb)
str(tb, give.attr = FALSE)

ncol(tb)
nrow(tb)
dim(tb)

unique(tb$team)

# tb %>%
#     distinct(team) %>%
#     View()

tb_distin <- tb %>%
    distinct(team)
tb_distin

View(tb),

tb %>% 
    count(team, sort = TRUE)

tb %>% 
    count(country, sort = TRUE)

tb %>% 
    count(, sort = TRUE)

tb %>%
    group_by(team) %>%
    summarise(Gols = sum(goal, na.rm = TRUE),
              Amarelos = sum(yel, na.rm = TRUE),
              Vermelhos = sum(red, na.rm = TRUE))

tb %>%
    group_by(team) %>%
    summarise_at(c("goal", "yel", "red"), c("mean", "sd"), na.rm = TRUE)

tb %>%
    group_by(team) %>%
    summarise_if(is.numeric, mean, na.rm = TRUE)

tb %>%
    replace_na(replace = list(goal = 0,
                              yel = 0,
                              red = 0,
                              cm = mean(tb$cm, na.rm = TRUE),
                              kg = 80)) %>%
    count(cm, sort = TRUE)

tb %>%
    group_by(team) %>%
    summarise_at(c("cm", "kg", "age"), c("mean", "median", "sd"), na.rm = TRUE)

tb %>%
    group_by(team) %>%
    summarise(correlacao = cor(cm, kg, use = "pairwise.complete.obs"))

tb <- tb %>%
    mutate(imc = kg/(cm/100)^2)

tb %>%
    group_by(team) %>%
    summarise_at("imc", mean, na.rm = TRUE) %>%
    arrange(desc(imc))

tb %>%
    slice_min(order_by = imc, n = 10) %>%
    # slice_max(order_by = imc, n = 10) %>%
    select(name, cm, kg, imc)

# < 16,5 	Peso severamente abaixo do normal
# < 18,5 	Peso abaixo do normal
# 18,5 – 24,99 	Normal
# 25 – 29,99 	Pré-obeso
# 30 – 34,99 	Obesidade classe I
# 35 – 39,99 	Obesidade classe II
# > 40 	Obesidade classe III 

# Classificação dos valores em níveis de obesidade.
lim <- c(0, 16.5, 18.5, 25, 30, 35, 40, Inf)
lab <- c("sev", "aba", "normal", "preobe", "obe1", "obe2", "obe3")
tb <- tb %>%
    mutate(imc_classe = cut(imc, breaks = lim, labels = lab))

tb %>%
    count(imc_classe)

names(tb)

tb %>%
    filter(imc_classe == "preobe") %>%
    select(name, kg, cm, imc, imc_classe, team) %>%
    count(team, sort = TRUE)

range(tb$cm, na.rm = TRUE)

lim <- seq(160, 210, by = 5)

tb <- tb %>%
    mutate(cm_classe = cut(cm, breaks = lim, right = FALSE))

tb %>%
    group_by(cm_classe) %>%
    summarise_at("aw", mean, na.rm = TRUE)

names(tb)

tb %>%
    count(pos) %>%
    View()

tb %>%
    mutate(position = str_sub(pos, 0, 1)) %>%
    count(position)

tb <- tb %>%
    mutate(position = str_replace(pos, "^([A-Z]+).*", "\\1")) 

tb %>%
    count(cm_classe, position) %>%
    pivot_wider(names_from = position, values_from = n)

#-----------------------------------------------------------------------
# HB 20.

# Caminho para os dados.
url <- "http://leg.ufpr.br/~walmes/data/hb20_venda_webmotors_280314.txt"

# Leitura com `readr`.
tb <- read_tsv(file = url,
               col_names = TRUE)
class(tb)
str(tb, give.attr = FALSE)

tb %>%
    count(carro) %>%
    mutate(freq = n/sum(n))

tb %>%
    count(cor, sort = TRUE)
    
tb %>%
    count(cor, carro) %>%
    pivot_wider(names_from = "carro", values_from = "n")

xtabs(~cor + carro, data = tb)

tb %>%
    mutate(cambio = ifelse(str_detect(especificacao, "MANUAL"),
                           yes = "MANUAL",
                           no = "AUTO")) %>%
    count(cambio)

tb %>%
    mutate(cambio = ifelse(str_detect(especificacao, "MANUAL"),
                           yes = "MANUAL",
                           no = "AUTO")) %>%
    count(cambio)

tb %>%
    mutate(cambio = case_when(str_detect(especificacao, "MANUAL") ~ "MANUAL",
                              TRUE ~ "AUTO")) %>%
    count(cambio)

with(tb, cor(km, preco))

tb %>%
    group_by(carro) %>%
    summarise(correlacao = cor(km, preco))

cut_number(1:12, 3)

tb %>%
    mutate_at(c("preco", "km"), ~cut_number(., n = 3)) %>%
    # mutate_at(c("preco", "km"), ~log(., 5)) %>%
    count(preco, km)
    
tb %>%
    mutate(preco = cut_number(preco, 4),
           km = cut(km, c(0, 1, 5000, 30000, Inf), right = FALSE)) %>%
    count(preco, km) %>%
    pivot_wider(names_from = "km", values_from = "n")

tb %>%
    mutate(km = cut(km, 
                    breaks = c(0, 1, 5000, 30000, Inf),
                    labels = c("novo", "seminovo", "semiusado", "usado"),
                    right = FALSE)) %>%
    group_by(carro, km) %>%
    summarise_at("preco", mean) %>%
    spread(key = "carro", value = "preco")
    # pivot_wider(names_from = "carro", values_from = "preco")

