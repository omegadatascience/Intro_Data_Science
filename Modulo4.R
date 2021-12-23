## Fundamentos de estatística: Ômega Fly ---------------------------------------
## Prof. Wagner Hugo Bonat · Ômega Data Science --------------------------------
## Data: 04/11/2021 ------------------------------------------------------------
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

## Distribuição de frequências
ggplot(dados) +
  geom_histogram(aes(atraso))

## Como se comporta a distribuição do atraso?
summary(dados$atraso)

## Dependendo do atraso a Omega Fly vai ter um custo
quantile(dados$atraso, probs = seq(0, 1, 0.05))

# A partir de quanto de atraso começamos a ter um custo?
# Vai depender da sua recomendação!

# Cenário 1: Atraso superior a uma hora vamos pagar
dados %>% 
  filter(atraso > 60) %>%
  count()

prop <- (887/dim(dados)[1])
prop

## Aproximadamente 7% das vendas teremos um custo com essa estratégia!
## Quanto vai custar? 

## Voltamos ao produto! 

# Remarcação custo 300
# Acomodação + custeio custo MÉDIO de 150.

# Suponha que vendemos 10000 passagens ao ano para este trajeto.
# Quanto esperamos pagar?
custo_total <- 450*(10000*prop)
custo_total

# Custo adicional por unidade (sem contar taxas de administração e lucro)
custo_add <- custo_total/10000
custo_add # Aproximadamente 31.25 adicional de preço!

# Parece razoável? Quais os problemas com essa abordagem?

# 1) Estamos olhando apenas para o passado.
# 2) Não sabemos entre todos os voos possíveis onde alocaremos os nossos clientes.
# 3) Estamos admitindo que o fato de atrasar para um cliente não tem associação com 
# o atraso de outros.
# 4) É esse mesmo o valor que vamos pagar? Não esse é apenas o valor esperado! 
# O que isso significa? Precisamos pensar em probabilidades.

## Voltamos para os slides

## Atribuindo pesos que refletem chances!
ggplot(dados) +
  geom_bar(aes(atraso))

# Agrupando em classes de 5 em 5 minutos
ggplot(dados) +
  geom_histogram(aes(atraso), binwidth = 5)

# Padronizando para que a soma abaixo da Figura seja 1
ggplot(dados) +
  geom_histogram(aes(atraso, y = ..density..), binwidth = 5)

# A área abaixo da Figura é uma probabilidade.

# Admitindo que a distribuição de frequências é uma boa representação da
# distribuição de probabilidade do tempo de atraso. 
# Podemos calcular probabilidades.

# Qual a probabilidade de um atraso ser superior a 60 minutos?
dados %>% 
  filter(atraso > 60) %>%
  count()

prob_60 <- 887/dim(dados)[1]
prob_60

# Agora temos a distribuição de probabilidade da qtd de tickets que vamos pagar
# Pago 0 se Y <= 60 e 1 se Y > 60

## Número de tickets que esperamos pagar
10000*prob_60

# Custo esperado de gasto em um ticket
0*(1-prob_60) + 450*prob_60

# Custo esperado em 10000 tickets
10000*(0*(1-prob_60) + 450*prob_60)

## Tudo estaria perfeito SE tivessemos realmente a "população"!!
## Mas o que é população? 

## Vamos voltar para os slides.

## Inferência estatística

## O que aconteceria se coletarmos apenas uma amostra de tamanho 10000?
set.seed(123) # Serve para tornar o resultado igual para todos
N <- dim(dados)[1]
n <- 10000
amostra <- sample(1:N, size = n, replace = TRUE)
dados_amostra <- dados[amostra,]

contagem <- dados_amostra %>% 
  filter(atraso > 60) %>%
  count()
prob_60 <- contagem$n/n
prob_60
# Custo esperado adicional por ticket
(0*(1-prob_60) + 450*prob_60)

# E se repetirmos muitas vezes!
n_rep <- 1000
custo_total <- c()
custo_ticket <- c()
prob_60 <- c()

for(i in 1:n_rep) {
  # Sorteia a amostra
  amostra <- sample(1:N, size = n, replace = TRUE)
  # Seleciona a amostra
  dados_amostra <- dados[amostra,]
  # Conta qts atrasos acima de 60 e estima a probabilidade
  prob_60[i] <- dim(dados_amostra[dados_amostra$atraso > 60,])[1]/n
  # Guardando os resultados
  custo_total[i] <- 10000*(0*(1-prob_60[i]) + 450*prob_60[i])
  custo_ticket[i] <- 450*prob_60[i]
}

## Distribuição de probabilidades da probabilidade de ter atraso maior do que 60
ggplot() +
  geom_histogram(aes(x = prob_60, y = ..density..), binwidth = 0.001)

## Distribuição de probabilidades do custo total
ggplot() +
  geom_histogram(aes(x = custo_total, y = ..density..))

## Distribuição de probabilidades do custo por ticket
ggplot() +
  geom_histogram(aes(x = custo_ticket, y = ..density..))

## Quantis
quantile(prob_60, probs = seq(0, 1, by = 0.05))
quantile(custo_total, probs = seq(0, 1, by = 0.05))
quantile(custo_ticket, probs = seq(0, 1, by = 0.05))

# Intervalo com 95% de confiança
quantile(prob_60, probs = c(0.025, 0.975))
quantile(custo_total, probs = c(0.025, 0.975))
quantile(custo_ticket, probs = c(0.025, 0.975))

## Voltamos para os slides.

## Por que um voo atrasa?

# Vamos começar investigando se o aeroporto de origem é importante.

# Opção 1
ggplot(dados) +
  geom_boxplot(aes(origin, atraso))

# Opção 2
ggplot(dados) +
  geom_histogram(aes(x = atraso, y = ..density.., fill = origin), 
                 alpha = 0.8, binwidth = 5)

# Opção 3
ggplot(dados) +
  geom_density(aes(x = atraso, fill = origin))

## Descritiva básica por grupo
dados %>%
  group_by(origin) %>%
  summarize("quantis" = quantile(atraso))

### Função para calcualar probabilidade
compute_probability <- function(y, corte) {
  mean(y > corte)
}

## Para uma particular amostra o que aconteceria?
set.seed(123) # Serve para tornar o resultado igual para todos
N <- dim(dados)[1]
n <- 10000
amostra <- sample(1:N, size = n, replace = TRUE)
dados_amostra <- dados[amostra,]

dados_amostra %>%
  group_by(origin) %>%
  summarize("quantis" = quantile(atraso))

dados_amostra %>%
  group_by(origin) %>%
  summarize("probabilidade" = compute_probability(atraso, corte = 60))

## Será que essa diferença é relevante a ponto de criar uma politica diferente
# de preço para cada aeroporto?

# E se repetirmos muitas vezes!
n_rep <- 1000
custo_total <- matrix(NA, ncol = 2, nrow = 1000)
custo_ticket <- matrix(NA, ncol = 2, nrow = 1000)
prob_60 <- matrix(NA, ncol = 2, nrow = 1000)
diferenca <- c()

for(i in 1:n_rep) {
  # Sorteia a amostra
  amostra <- sample(1:N, size = n, replace = TRUE)
  # Seleciona a amostra
  dados_amostra <- dados[amostra,]
  # Conta qts atrasos acima de 60 e estima a probabilidade
  probs <- dados_amostra %>%
    group_by(origin) %>%
    summarize("probabilidade" = compute_probability(atraso, corte = 60))
  prob_60[i,] <- probs$probabilidade
  diferenca[i] <- probs$probabilidade[1] - probs$probabilidade[2]
  # Guardando os resultados
  custo_total[i,] <- 10000*(0*(1-prob_60[i,]) + 450*prob_60[i,])
  custo_ticket[i,] <- 450*prob_60[i,]
}

## Resumo
quantile(prob_60[,1], probs = c(0.025, 0.975))
quantile(prob_60[,2], probs = c(0.025, 0.975))

quantile(custo_total[,1], probs = c(0.025, 0.975))
quantile(custo_total[,2], probs = c(0.025, 0.975))

quantile(custo_ticket[,1], probs = c(0.025, 0.975))
quantile(custo_ticket[,2], probs = c(0.025, 0.975))

quantile(diferenca, probs = c(0.025, 0.975))

dados_plot <- tibble("prob_60" = c(prob_60[,1], prob_60[,2]),
                     "total" = c(custo_total[,1], custo_total[,2]),
                     "ticket" = c(custo_ticket[,1], custo_ticket[,2]) ,
                     "aeroporto" = rep(c("EWR", "JFK"), each = 1000))

## Comparando as distribuições amostrais
ggplot(dados_plot) +
  geom_density(aes(x = prob_60, fill = aeroporto), alpha = 0.5)

## Distribuição da diferença
ggplot() +
  geom_density(aes(diferenca)) +
  geom_vline(xintercept = 0)

## Intervalo de confiança pra diferença
quantile(diferenca, probs = c(0.025, 0.975))

## Voltamos para os slides.

## Testes de hipóteses

## Hipótese nula: Não há diferença na probabilidade de atraso entre os aeroportos.
## Hipótese alternativa: Há diferença na probabilidade de atraso entre os aeroportos.

## Como fazer o teste?

# Simulamos da hipótese nula.
# Construímos a distribuição amostral sob a hipótese nula.
# Coletamos a nossa amostra.
# Se o observado for raro o suficiente rejeita H0 caso contrário não rejeitamos.

diferenca_H0 <- c()
for(i in 1:1000) {
  amostra <- sample(1:n, replace = TRUE)
  dados_amostra <- dados[amostra,]
  ## Criando os dados sob a hipótese nula
  dados_sob_h0 <- data.frame("atraso" = dados_amostra$atraso,
                             "Grupo_H0" = sample(c("EWR","JFK"), 
                                                 size = 10000, replace = TRUE))
  probs <- dados_sob_h0 %>%
    group_by(Grupo_H0) %>%
    summarize("probabilidade" = compute_probability(atraso, corte = 60))
  diferenca_H0[i] <- probs$probabilidade[1] - probs$probabilidade[2]
}

## Distribuição da diferença sob a hipótese nula
ggplot() +
  geom_density(aes(diferenca_H0)) +
  geom_vline(xintercept = 0)

# Seleciono a minha amostra
set.seed(123)
amostra <- sample(1:dim(dados)[1], size = 10000, replace = TRUE)

probs <- dados[amostra,] %>%
  group_by(origin) %>%
  summarize("probabilidade" = compute_probability(atraso, corte = 60))
dif_obs <- probs$probabilidade[1] - probs$probabilidade[2]

ggplot() +
  geom_density(aes(diferenca_H0)) +
  geom_vline(xintercept = 0) +
  geom_vline(xintercept = dif_obs)

## O que é o p-valor? Probabilidade de observar uma estatística amostral igual
# ou mais extrema do que a observada sob a hipótese nula.
# Se a hipotese é bilaterial (dierença) multiplica por dois
2*mean(diferenca_H0 < dif_obs)

# Regras do dedão p-valor < 0.05 rejeita H0. Da onde veio isso? Da cabeça de alguém!
# Pq? Só Deus sabe!!

## Leitura recomendada
# https://www.tandfonline.com/doi/full/10.1080/00031305.2016.1154108


## Como podemos facilitar o processo de avaliação diferente estratégias?
calcula_risco <- function(corte, n_vendas, custo_realoc, custo_acomo, dados) {
  custo_total <- c()
  custo_ticket <- c()
  prob <- c()
  N <- dim(dados)[1]
  custo <- custo_realoc + custo_acomo
  for(i in 1:n_rep) {
    # Sorteia a amostra
    amostra <- sample(1:N, size = n_vendas, replace = TRUE)
    # Seleciona a amostra
    dados_amostra <- dados[amostra,]
    # Conta qts atrasos acima de 60 e estima a probabilidade
    prob[i] <- dim(dados_amostra[dados_amostra$atraso > corte,])[1]/n_vendas
    # Guardando os resultados
    custo_total[i] <- n_vendas*(0*(1-prob[i]) + custo*prob[i])
    custo_ticket[i] <- custo*prob[i]
  }
  output <- data.frame("Probabilidade" = prob, 
                       "Total" = custo_total,
                       "Ticket" = custo_ticket)
  return(output)
}

## Estratégia 1: 60 minutos
avalia_60 <- calcula_risco(corte = 60, n_vendas = 10000, 
                           custo_realoc = 300, custo_acomo = 150,
                           dados = dados)

ggplot(avalia_60) +
  geom_density(aes(x = Probabilidade))
ggplot(avalia_60) +
  geom_density(aes(x = Ticket))

# E apenas 15 minutos, como ficaria o preço?
avalia_15 <- calcula_risco(corte = 15, n_vendas = 10000, 
                           custo_realoc = 300, custo_acomo = 150,
                           dados = dados)

ggplot(avalia_15) +
  geom_density(aes(x = Probabilidade))
ggplot(avalia_15) +
  geom_density(aes(x = Ticket))


## Talvez 45 minutos?
avalia_45 <- calcula_risco(corte = 45, n_vendas = 10000, 
                           custo_realoc = 300, custo_acomo = 150,
                           dados = dados)

ggplot(avalia_45) +
  geom_density(aes(x = Probabilidade))
ggplot(avalia_45) +
  geom_density(aes(x = Ticket))


## Podemos fazer para vários. 
# Suponha que a politica é ter 5% de risco ao ano.
## O interesse é no preço minimo do produto

corte <- seq(30, 120, by = 10)
custo <- c()
for(i in 1:length(corte)) {
  temp <- calcula_risco(corte = corte[i], n_vendas = 10000, 
                        custo_realoc = 300, custo_acomo = 150,
                        dados = dados)
  custo[i] <- quantile(temp$Ticket, 0.95)
  print(i)
}


## Quer cobrar qto? $ 25
grafico <- data.frame("corte" = corte, "custo" = custo)
ggplot(grafico) +
  geom_line(aes(corte, custo)) +
  geom_hline(yintercept = 25)

## FIM -------------------------------------------------------------------------