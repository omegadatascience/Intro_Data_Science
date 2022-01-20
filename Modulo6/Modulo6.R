#-----------------------------------------------------------------------
#                                            Prof. Dr. Walmes M. Zeviani
#                                leg.ufpr.br/~walmes · github.com/walmes
#                                        walmes@ufpr.br · @walmeszeviani
#                      Laboratory of Statistics and Geoinformation (LEG)
#                Department of Statistics · Federal University of Paraná
#                                       2021-nov-11 · Curitiba/PR/Brazil
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Pacotes.

rm(list = objects())

library(tidyverse)
library(DataExplorer)
library(GGally)

#-----------------------------------------------------------------------
# Aquisição dos dados.

browseURL("http://archive.ics.uci.edu/ml/")

url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
da <- read_csv(url, col_names = FALSE)
attr(da, "spec") <- NULL
str(da)

# Criando o nome das variáveis.
nms <- c("radius", "texture", "perimeter", "area", "smoothness",
         "compactness", "concavity", "concave", "symmetry",
         "fractal")
n <- outer(Y = c("mn", "sd", "lg"),
           X = nms,
           FUN = paste, sep = "_")
names(da) <- c("id", "diagnosis", n)
str(da)

da <- da %>%
    select(-id)

da %>%
    select(starts_with("radius"))

#-----------------------------------------------------------------------

da %>%
    count(diagnosis) %>%
    mutate(freq = n/sum(n))

ls("package:DataExplorer")

plot_boxplot(da, by = "diagnosis", nrow = 6, ncol = 5)

ggpairs(data = select(da, diagnosis, ends_with("mn")),
        ggplot2::aes(colour = diagnosis))

ggpairs(data = select(da, diagnosis, ends_with("sd")),
        ggplot2::aes(colour = diagnosis))

ggpairs(data = select(da, diagnosis, ends_with("lg")),
        ggplot2::aes(colour = diagnosis))

#-----------------------------------------------------------------------

to_unit <- function(x) {
    a <- min(x)
    b <- max(x)
    (x - a)/(b - a)
}

names(da)

# db <- da %>%
#     mutate_at(vars(-diagnosis), scale)

db <- da %>%
    mutate_at(vars(-diagnosis), to_unit)
summary(db)

#-----------------------------------------------------------------------

library(class)

p <- 0.75
n <- nrow(db)

i <- sample(c(TRUE, FALSE),
            size = n,
            replace = TRUE,
            prob = c(p, 1 - p))
table(i)

db_train <- db[i, ]
db_test <- db[!i, ]

db_train %>%
    count(diagnosis) %>%
    mutate(freq = n/sum(n))

db_test %>%
    count(diagnosis) %>%
    mutate(freq = n/sum(n))

k <- 1
fit <- knn(train = db_train[, -1],
           test = db_test[, -1],
           cl = db_train[[1]],
           k = k)

ct <- table(fit, db_test[[1]])
ct

sum(diag(ct))/sum(ct)

#-----------------------------------------------------------------------
# Simplificando para visualizar e entender.

ggplot(data = db_train,
       mapping = aes(x = radius_sd,
                     y = radius_mn,
                     color = diagnosis)) +
    geom_point(pch = 1) +
    geom_point(data = db_test,
               mapping = aes(x = radius_sd,
                             y = radius_mn),
               color = "black",
               inherit.aes = TRUE) +
    coord_fixed()

# Criando um grid fino de valores para traçar a fronteira do
# classificador.
grid <- expand.grid(seq(0, 1, length.out = 200),
                    seq(0, 1, length.out = 200),
                    KEEP.OUT.ATTRS = FALSE)
names(grid) <- c("radius_mn", "radius_sd")
nrow(grid)

# Usando apenas o vizinho mais próximo.
k <- 7
m0_grid <- knn(train = db_train[, names(grid)[1:2]],
               test = grid[, 1:2],
               cl = db_train[[1]],
               k = k)
grid$diagnosis <- as.integer(m0_grid) - 1

ggplot() +
    geom_tile(data = grid,
              mapping = aes(x = radius_sd,
                            y = radius_mn,
                            fill = diagnosis),
              inherit.aes = TRUE,
              alpha = 0.5) +
    geom_point(data = db_train,
               mapping = aes(x = radius_sd,
                             y = radius_mn,
                             color = diagnosis),
               pch = 1) +
    # geom_point(data = db_test,
    #            mapping = aes(x = radius_sd,
    #                          y = radius_mn),
    #            color = "black",
    #            inherit.aes = TRUE) +
    scale_fill_distiller(palette = 5) +
    scale_colour_brewer(palette = "Set1") +
    coord_fixed()

m0 <- knn(train = db_train[, names(grid)[1:2]],
          test = db_test[, names(grid)[1:2]],
          cl = db_train[[1]],
          k = k)

# Matriz de confusão.
ct <- table(db_test[[1]], m0)
ct

# Fração de acertos (acurácia).
sum(diag(ct))/sum(ct)

#-----------------------------------------------------------------------

results <- replicate(10, {
    i <- sample(c(TRUE, FALSE),
                size = n,
                replace = TRUE,
                prob = c(p, 1 - p))
    table(i)
    db_train <- db[i, ]
    db_test <- db[!i, ]
    k_seq <- seq(1, by = 2, length.out = 15)
    acc_seq <-
        sapply(k_seq,
               FUN = function(ki) {
                   m0 <- knn(train = db_train[, names(grid)[1:2]],
                             test = db_test[, names(grid)[1:2]],
                             cl = db_train[[1]],
                             k = ki)
                   ct <- table(db_test[[1]], m0)
                   sum(diag(ct))/sum(ct)
               })
    list(data.frame(k_seq = k_seq, acc_seq = acc_seq))
})

str(results)
results <- do.call(rbind, results)

plot(acc_seq ~ k_seq, data = results)

#-----------------------------------------------------------------------
# Divisão dos dados em treino e teste.

library(caret)

# Proporções das classes no treino e teste usando sample().
rbind(train = prop.table(xtabs(~diagnosis, db_train)),
      test = prop.table(xtabs(~diagnosis, db_test)))

# Criando as partições.
set.seed(789)
intrain <- createDataPartition(y = db$diagnosis,
                               p = 0.75,
                               list = FALSE)

db_train <- db[intrain, ]
db_test <- db[-intrain, ]
nrow(db_train)
nrow(db_test)

# Proporções das classes no treino e teste usando createDataPartition().
rbind(train = prop.table(xtabs(~diagnosis, db_train)),
      test = prop.table(xtabs(~diagnosis, db_test)))

#-----------------------------------------------------------------------
# Submete para o método.

# Parametriza a valiação cruzada.
trctrl <- trainControl(method = "repeatedcv",
                       number = 5,
                       repeats = 7)
tunegrid <- expand.grid(k = seq(1, by = 2, length.out = 15))

# set.seed(159)
knn_fit <- train(diagnosis ~ .,
                 data = db_train,
                 method = "knn",
                 trControl = trctrl,
                 # tuneLength = 15,
                 tuneGrid = tunegrid)

# Classe e métodos.
class(knn_fit)
methods(class = class(knn_fit))

# Usa a função `caret::knn3()` como workhorse. Baseia-se no código em C
# do {class}.
class(knn_fit$finalModel)
methods(class = class(knn_fit$finalModel))
# help(knn3, help_type = "html")

# Acurácia média e sua dispersão.
perf <- knn_fit$resample %>%
    summarise_at("Accuracy", c("mean", "sd"))
perf

# Resultado do procedimento.
knn_fit

names(knn_fit)
knn_fit$bestTune

# Gráfico para escolha do parâmetro de tunning.
plot(knn_fit)

# Cuidado com o domínio da escala.
plot(knn_fit, ylim = c(0.9, 1)) +
    latticeExtra::layer(
        panel.segments(
            x0 = knn_fit$bestTune,
            x1 = knn_fit$bestTune,
            y0 = perf$mean - perf$sd,
            y1 = perf$mean + perf$sd)) +
    latticeExtra::layer(
        panel.points(
            x = knn_fit$bestTune,
            y = perf$mean,
            col = "black",
            pch = 19))

# Predição nos dados deixados de fora.
m0 <- predict(knn_fit, newdata = db_test)

# Matriz de confusão.
confusionMatrix(m0, as.factor(db_test$diagnosis))

#-----------------------------------------------------------------------
