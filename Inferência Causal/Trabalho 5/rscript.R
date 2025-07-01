# Pacotes
pacman::p_load(dplyr, boot)

# Carregand os dados
load(
    "C:/Users/vitor/OneDrive/R/Estatistica-UFPR/Inferência Causal/Trabalho 5/dados.RData"
)
dados %>% group_by(T) %>% summarise(n = n(),
                                    mean_e = mean(escore),
                                    mean_age = mean(idade))

# Questão 1

## AS

# Estimativa
mean(dados$Y[dados$A == 1]) - mean(dados$Y[dados$A == 0])

# Intervalo de Confiança

AS <- function(df, id) {
    data <- df[id, ]
    mean(data$Y[data$A == 1]) - mean(data$Y[data$A == 0])
}

set.seed(1903)
as_boot <- boot(dados, AS, R = 2000)

ic <- boot.ci(as_boot, conf = 0.95, type = 'perc')

# Visualização
hist(as_boot$t,
     col = 'steelblue',
     xlab = 'Estimativas de AS',
     main = 'Histograma de AS e IC')

# Adiciona linhas do IC
abline(
    v = ic$perc[4:5],
    col = 'red',
    lwd = 2,
    lty = 3
)  # limites inferior e superior
