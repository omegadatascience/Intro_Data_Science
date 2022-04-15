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

tb <- tb %>%
    mutate(position = str_replace(pos, "^([A-Z]+).*", "\\1"))

ggplot(data = tb,
       mapping = aes(x = team)) +
    geom_bar() +
    coord_flip()

ggplot(data = tb,
       mapping = aes(x = position)) +
    geom_bar()

tb_gols <- tb %>%
    group_by(team) %>%
    summarise_at("goal", mean, na.rm = TRUE) %>%
    mutate(team = reorder(team, goal))
tb_gols

ggplot(data = tb_gols,
       mapping = aes(x = team, y = goal)) +
    geom_col() +
    coord_flip()

tb_text <- tibble(position = unique(tb$position),
                  texto = c("Meio de campo",
                            "Lateral",
                            "Zegueiro",
                            "Goleiro",
                            "Atacante",
                            "Volante"))

ggplot(data = tb,
       mapping = aes(x = cm,
                     y = kg,
                     color = position)) +
    geom_jitter()

ggplot(data = tb,
       mapping = aes(x = cm,
                     y = kg,
                     color = age
                     # size = age
                     )) +
    facet_wrap(facets = ~position) +
    # geom_point()
    geom_jitter() +
    # geom_smooth(se = FALSE) +
    geom_smooth(method = "lm",
                formula = y ~ poly(x,  degree = 2),
                se = FALSE) +
    scale_color_distiller(palette = 1, direction = 1) +
    labs(x = "Altura dos jogadores (cm)",
         y = "Peso dos jogadores (kg)",
         color = "Idade (anos)") +
    geom_text(data = tb_text,
              mapping = aes(x = 170, y = 100, label = texto),
              inherit.aes = FALSE)

ggplot(data = tb,
       mapping = aes(x = cm)) +
    geom_histogram(bins = 20,
                   color = "black",
                   fill = "#ab1565") +
    geom_rug()

ggplot(data = tb,
       mapping = aes(x = cm)) +
    geom_density(fill = "#ab1565", alpha = 0.5, bw = 1) +
    geom_rug()

m <- mean(tb$cm, na.rm = TRUE)
s <- sd(tb$cm, na.rm = TRUE)

ggplot(data = tb,
       mapping = aes(x = cm)) +
    geom_histogram(mapping = aes(y = ..density..),
                   bins = 20,
                   color = "black",
                   fill = "gray") +
    geom_density(fill = "#ab1565", alpha = 0.2) +
    stat_function(fun = dnorm,
                  args = list(mean = m, sd = s),
                  color = "magenta") +
    geom_vline(xintercept = m, color = "cyan") +
    geom_rug()

pl_ecdf <-
ggplot(data = tb,
       mapping = aes(x = cm)) +
    stat_ecdf() +
    stat_function(fun = pnorm,
                  args = list(mean = m, sd = s),
                  color = "magenta") +
    geom_rug()

pl_boxplot <-
ggplot(data = tb,
       mapping = aes(x = cm)) +
    stat_boxplot() +
    geom_rug()

gridExtra::grid.arrange(pl_ecdf, pl_boxplot, ncol = 1)


ggplot(data = tb,
       mapping = aes(x = cm)) +
    geom_density(fill = "#ab1565", alpha = 0.5, bw = 1) +
    geom_rug()

ggplot(data = tb,
       mapping = aes(x = kg)) +
    geom_density(fill = "#ab1565", alpha = 0.5, bw = 1) +
    geom_rug()

ggplot(data = tb,
       mapping = aes(x = cm,
                     y = kg)) +
    geom_jitter() +
    geom_density2d(color = "red")

ggplot(data = tb,
       mapping = aes(x = cm,
                     y = kg)) +
    geom_bin2d(color = "red")
    # geom_hex()

ggplot(data = tb,
       mapping = aes(x = position, y = cm)) +
    geom_boxplot(fill = "orange")

ggplot(data = tb,
       mapping = aes(x = position, y = cm)) +
    geom_violin(fill = "orange")

ggplot(data = tb,
       mapping = aes(x = cm)) +
    facet_grid(facets = position ~ .) +
    geom_density(fill = "orange")

ggplot(data = tb,
       mapping = aes(y = cm)) +
    facet_grid(facets = . ~ position) +
    geom_density(fill = "orange")

#-----------------------------------------------------------------------
