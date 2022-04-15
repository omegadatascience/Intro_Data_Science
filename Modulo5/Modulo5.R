## Fundamentos de estatística: Ômega Fly ---------------------------------------
## Prof. Wagner Hugo Bonat · Ômega Data Science --------------------------------
## Data: 09/11/2021 ------------------------------------------------------------
rm(list=ls())

## Carregando alguns pacotes adicionais
library(readr) # Leitura de arquivos
library(dplyr) # Manipulação de dados
library(lubridate) # Manipulação de datas e horas
library(ggplot2) ## Gráficos

## Carregando a base de dados
url <- "http://leg.ufpr.br/~wagner/data/omegafly.csv"
dados <- read_csv2(url)

## Dar uma olhada na base de dados
View(dados)

## Calculando os atrasos
dados$hora_prevista_chegada <- hm(dados$hora_prevista_chegada )
dados$hora_chegada <- hm(dados$hora_chegada)
dados$atraso <- dados$hora_chegada - dados$hora_prevista_chegada
dados$atraso <- as.numeric(dados$atraso)/60

## Olhando o resultado
View(dados)

## Histograma
ggplot(dados) +
  geom_histogram(aes(atraso))

## Valores negativos, estranho?
dados %>%
  filter(atraso < -100) %>%
  View()

## Vamos somar 60*24 = 1440 minutes apenas nestes casos
dados <- dados %>%
  mutate(atraso = ifelse(atraso < -100, atraso + 1440, atraso))

## Para facilitar a visualização vamos excluir atrasos acima de 500
dados <- dados %>% 
  mutate(atraso = ifelse(atraso > 500, NA, atraso))

## Distribuição de frequências
ggplot(dados) +
  geom_histogram(aes(atraso))

## Por que um voo atrasa?

## Aeroporto de origem ---------------------------------------------------------
ggplot(dados) +
  geom_density(aes(atraso, y = ..density.., fill = origin))

ggplot(dados) +
  geom_boxplot(aes(x = origin, y = atraso))

## Operadora do voo ------------------------------------------------------------
ggplot(dados) +
  geom_density(aes(atraso, y = ..density.., fill =carrier))

ggplot(dados) +
  geom_boxplot(aes(x = carrier, y = atraso))

ggplot(dados) +
  geom_boxplot(aes(x = carrier, y = atraso)) +
  facet_wrap(~ origin)

## Atraso médio por operadora e aeroporto de origem
dados %>%
  group_by(origin, carrier) %>%
  summarize("Media" = mean(atraso, na.rm = TRUE),
            "Desvio_padrao" = sd(atraso, na.rm = TRUE))

## Criando covariáveis baseado na data -----------------------------------------
dados$time_hour <- as.POSIXct(dados$time_hour, format = "%Y/%m/%d %H:%M")
dados <- dados %>%
  mutate(ano = year(time_hour),
         dia_semana = wday(time_hour, label = TRUE),
         dia_ano = yday(time_hour),
         dia_mes = mday(time_hour),
         mes = month(time_hour, label = TRUE), 
         dia = day(time_hour),
         hora = hour(time_hour),
         minuto = minute(time_hour), .)

dados$dia_semana <- factor(dados$dia_semana, levels = c("dom", "seg", "ter", 
                                                        "qua", "qui", "sex", "sáb"))
dados$mes <- factor(dados$mes, levels = c("jan", "fev", "mar", "abr", "mai", "jun",
                                          "jul", "ago", "set", "out", "nov", "dez"))

## Hora prevista da partida-----------------------------------------------------
ggplot(dados) +
  geom_boxplot(aes(x = as.factor(hora), y = atraso))

dados %>%
  group_by(hora) %>%
  summarize("Media" = mean(atraso, na.rm = TRUE),
            "Desvio_padrao" = sd(atraso, na.rm = TRUE))

## Dia da semana ---------------------------------------------------------------
ggplot(dados) +
  geom_boxplot(aes(x = dia_semana, y = atraso))

dados %>%
  group_by(dia_semana) %>%
  summarize("Media" = mean(atraso, na.rm = TRUE),
            "Desvio_padrao" = sd(atraso, na.rm = TRUE))

## Dia do mês ------------------------------------------------------------------
ggplot(dados) +
  geom_boxplot(aes(x = as.factor(dia_mes), y = atraso))

dados %>%
  group_by(dia_mes) %>%
  summarize("Media" = mean(atraso, na.rm = TRUE),
            "Desvio_padrao" = sd(atraso, na.rm = TRUE)) %>%
  View()

## Dia do ano ------------------------------------------------------------------
ggplot(dados) +
  geom_boxplot(aes(x = as.factor(dia_ano), y = atraso))

## Mes do ano ------------------------------------------------------------------
ggplot(dados) +
  geom_boxplot(aes(x = mes, y = atraso))

dados %>%
  group_by(mes) %>%
  summarize("Media" = mean(atraso, na.rm = TRUE),
            "Desvio_padrao" = sd(atraso, na.rm = TRUE))

## Voltar para os slides.

## Especificando um modelo simples para descrever os atrasos --------------------


## Acertando as categorias para mostrar na ordem natural
dados$dia_semana <- as.character(dados$dia_semana)
dados$dia_semana <- as.factor(dados$dia_semana)
dados$dia_semana <- factor(dados$dia_semana, levels = c("dom", "seg", "ter", 
                                                        "qua", "qui", "sex", "sáb"))

dados$mes <- as.character(dados$mes)
dados$mes <- as.factor(dados$mes)
dados$mes <- factor(dados$mes, levels = c("jan", "fev", "mar", "abr", "mai", "jun",
                                          "jul", "ago", "set", "out", "nov", "dez"))


## Regressão linear simples
# Efeito só da origem
fit1 <- lm(atraso ~ origin, data = dados)
summary(fit1)

## Regressão linear múltipla
# Efeito da operadorad
fit2 <- lm(atraso ~ carrier, data = dados)
summary(fit2)

## Hora do dia
fit3 <- lm(atraso ~as.factor(hora), data = dados)
summary(fit3)

## Dia da semana
fit4 <- lm(atraso ~ dia_semana, data = dados)
summary(fit4)

## Dia do mês
fit5 <- lm(atraso ~ as.factor(dia), data = dados)
summary(fit5)

## Mes do ano
fit6 <- lm(atraso ~ mes, data = dados)
summary(fit6)

## Dia do ano
fit7 <- lm(atraso ~ as.factor(dia_ano), data = dados)
summary(fit7)

## Juntando tudo!
fit_completo <- lm(atraso ~ origin + carrier +as.factor(hora) + 
                     dia_semana + mes, data = dados)
summary(fit_completo)

## E dia do ano e dia do mes? 
fit_completo2 <- lm(atraso ~ origin + carrier +as.factor(hora) + dia_semana + mes +
                     as.factor(dia_mes) + as.factor(dia_ano), data = dados)
summary(fit_completo2)

## Muitos parâmetros!! Superparametrização ???
anova(fit_completo2)

## Tem como simplificar?
require(mgcv)
fit_smooth <- gam(atraso ~ origin + carrier + as.factor(hora) + dia_semana + mes +
                   s(dia_mes) + s(dia_ano), data = dados)
summary(fit_smooth)
plot(fit_smooth)

## Podemos criar uma espécie de API em que entramos com as características do
# voo e a API solta o tempo esperado de atraso a incerteza associada. Podemos
# tbm calcular probabilidades de interesse, por exemplo, probabilidade de atrasar
# mais de 60 minutos.


################################################################################

## Regressão logística

# Vamos supor que queremos apenas calcular a probabilidade de atrasar mais de 60 minutos.
# Podemos criar uma nova variável resposta sendo: 0 se Y < 60 e 1 Y >= 60

dados <- dados %>%
  mutate(binaria = ifelse(atraso < 60, 0, 1))
prop.table(table(dados$binaria))


## Modelo completo
fit_logistico <- gam(binaria ~ origin + carrier + as.factor(hora) + 
                       dia_semana + mes + s(dia_mes) + s(dia_ano), 
                     family = binomial, data = dados)
summary(fit_logistico)

################################################################################

## Projeto: Detectando o genero baseado em características da voz

# Nome: Gender Recognition by Voice
# Tipo de resposta: Binária
# Modelo candidato: Regressão logística
# Endereço: Gender Recognition by Voice
# Arquivo: voice.csv
# Nível: Intermediário

