# Pacotes
pacman::p_load(dplyr, boot, Matching)

# Carregand os dados
load(
    "C:/Users/vitor/OneDrive/R/Estatistica-UFPR/Inferência Causal/Trabalho 5/dados.RData"
)


dados %>% group_by(T) %>% summarise(n = n(),
                                    mean_e = mean(escore),
                                    mean_age = mean(idade))

# Questão 1 ####

## AS

# Estimativa
mean(dados$Y[dados$A == 1]) - mean(dados$Y[dados$A == 0])

# Intervalo de Confiança

AS <- function(df, linha) {
    data <- df[linha, ]
    mean(data$Y[data$A == 1]) - mean(data$Y[data$A == 0])
}

set.seed(1903)
as_boot <- boot(dados, AS, R = 2000)

ic_as <- boot.ci(as_boot, conf = 0.95, type = 'perc')

# Visualização
hist(as_boot$t,
     col = 'steelblue',
     xlab = 'Estimativas de AS',
     main = 'Histograma de AS')

# Adiciona linhas do IC
abline(
    v = ic_as$perc[4:5],
    col = 'red',
    lwd = 2,
    lty = 3
)  # limites inferior e superior


# Questão 2 ####

## PP

# Estimativa

### Filtra A=T
pp_dados <- dados %>% filter(A==T)

table(pp_dados$T,pp_dados$A )


# Estimativa 
mean(pp_dados$Y[pp_dados$T == 1]) - mean(pp_dados$Y[pp_dados$T == 0])

# Intervalo de confiança
PP <- function(df, linha) {
    data <- df[linha, ]
    mean(data$Y[data$T == 1]) - mean(data$Y[data$T == 0])
}

set.seed(1903)
pp_boot <- boot(pp_dados, PP, R = 2000)
ic_pp <- boot.ci(pp_boot, conf = 0.95, type = 'perc')

# Estimativa Bootstrap
pp_boot$t %>% mean()

# Intervalo de Confiança
ic_pp

# Visualização
hist(pp_boot$t,
     col = 'darkgreen',
     xlab = 'Estimativas de PP',
     main = 'Histograma de PP')

# Adiciona linhas do IC
abline(
    v = ic_pp$perc[4:5],
    col = 'red',
    lwd = 2,
    lty = 3
)  # limites inferior e superior


# Questão 3 ####

## ITT

# Estimativa
mean(dados$Y[dados$T == 1]) - mean(dados$Y[dados$T == 0])

# Intervalo de Confiança

ITT <- function(df, linha) {
    data <- df[linha, ]
    mean(data$Y[data$T == 1]) - mean(data$Y[data$T == 0])
}

set.seed(1903)
itt_boot <- boot(dados, ITT, R = 2000)

ic_itt <- boot.ci(itt_boot, conf = 0.95, type = 'perc')

# Visualização
hist(itt_boot$t,
     col = 'tomato',
     xlab = 'Estimativas de ITT',
     main = 'Histograma de ITT')

# Adiciona linhas do IC
abline(
    v = ic_itt$perc[4:5],
    col = 'red',
    lwd = 2,
    lty = 3
)  # limites inferior e superior

# Estimativa
itt_boot$t %>% mean() 

# IC
ic_itt

tab <- round(prop.table(table(dados$A, dados$T), margin = 2),2)

# Imprimir a tabela com nomes claros
colnames(tab) <- c("T = 0", "T = 1")
rownames(tab) <- c("A = 0", "A = 1")
tab


# Questão 4 ####

# Estimativa

(mean(dados$Y[dados$T == 1]) - mean(dados$Y[dados$T == 0]))/  
    (mean(dados$A[dados$T == 1] == 1) -  mean(dados$A[dados$T == 0] == 1))


CACE <- function(df, linha) {
    data <- df[linha, ]
    (mean(data$Y[data$T == 1]) - mean(data$Y[data$T == 0])) / # E(Y/T = 1) - E(Y/T = 0)
    (mean(data$A[data$T == 1] == 1) -  mean(data$A[data$T == 0] == 1)) # P(A = 1 /T = 1) - E(A = 1/T = 0)
}


set.seed(1903)
cace_boot <- boot(dados, CACE, R = 2000)

ic_cace <- boot.ci(cace_boot, conf = 0.95, type = 'perc')

# Visualização
hist(cace_boot$t,
     col = 'orange2',
     xlab = 'Estimativas de CACE',
     main = 'Histograma de CACE')

# Adiciona linhas do IC
abline(
    v = ic_cace$perc[4:5],
    col = 'red',
    lwd = 2,
    lty = 3
)  # limites inferior e superior

ic_cace

cace_boot$t %>% mean() #estimativa boot


# Questão 5

## Utilizando tratamento 
Y <- dados$Y
Tr <- dados$T
X <- dados %>% dplyr::select(c(escore, idade))

set.seed(1903)

# utlizando pareamento 
## Estimativa
rr <- Matching::Match(Y = Y, Tr = Tr, X = X, M = 1,  estimand = "ATE")
rr$est


matching <- function(df, linha) {
    data <- df[linha, ]
    Y <- data$Y
    Tr <- data$T
    X <- data %>% dplyr::select(c(escore, idade))
    rr <- Matching::Match(Y = Y, Tr = Tr, X = X, M = 1,  estimand = "ATE", ties = F)
    return(rr$est)
}


set.seed(1903)
matching_boot <- boot(dados, matching, R = 1000)

ic_matching <- boot.ci(cace_boot, conf = 0.95, type = 'perc')

# Visualização
hist(matching_boot$t,
     col = 'orange2',
     xlab = 'Estimativas do ATT',
     main = 'Histograma do ATT via Pareamento')

# Adiciona linhas do IC
abline(
    v = ic_cace$perc[4:5],
    col = 'red',
    lwd = 2,
    lty = 3
)  # limites inferior e superior

ic_matching

matching_boot$t %>% mean() #estimativa boot


