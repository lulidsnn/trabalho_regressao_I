#-------* Limpando o ambiente *-------

rm(list = ls())

#---------------*   *---------------

#-------* Instalando e carregando pacotes necessários *-------

pacotes <- c( "readr", "leaps", "MASS", "car", "ggplot2")

pacotes_nao_instalados <- pacotes[!(pacotes %in% installed.packages()[, "Package"])] # verificar

if (length(pacotes_nao_instalados) > 0) {
  install.packages(pacotes_nao_instalados, dependencies = TRUE)
}

lapply(pacotes, library, character.only = TRUE)

cat("\014") #limpando console

#---------------* Importando dados csv para análise *---------------

dados <- read.csv("winequality-red.csv", header = TRUE, sep = ";")
names(dados)
colnames(dados) <- c('x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7', 'x8', 'x9', 'x10', 'x11', 'y') # renomeando as variáveis para facilitar as regressões

dados <- dados[, c("y", setdiff(names(dados), "y"))] # alterando a posição de y, para ser a primeira coluna 

#---------------* Análise Exploratória *---------------

head(dados) 
str(dados)       
summary(dados)  
pairs(dados)

#---------------* Criação do modelo completo *---------------

modelo_completo <- lm(y ~ ., data = dados)
modelo_completo

#---------------* Testando todas as possíveis regressões  *---------------

subsets <- regsubsets(y ~ ., data = dados, nvmax = ncol(dados) - 1) # testa todas as combinações possíveis de regressão via critério <verificar qual o critério>
summary(subsets) 

summary(subsets)$adjr2 # com base no R2, o modelo com 8 variáveis é o 'melhor', com um R2 de 35.67% (ainda pode melhorar)
summary(subsets)$cp # verificar
summary(subsets)$bic # verificar (e checar se realmente entra nessa etapa)
summary(subsets)$rss # verificar

#---------------*  Métodos forward, backward e stepwise (TESTE)  *---------------

modelo_forward <- step(modelo_completo, direction = 'forward')
modelo_forward
par(mfrow = c(2, 2))
plot(modelo_forward)  

modelo_backward <- step(modelo_completo, direction = 'backward')
modelo_backward
par(mfrow = c(2, 2))
plot(modelo_backward)

modelo_stepwise <- step(modelo_completo, direction = 'both')
modelo_stepwise
plot(modelo_stepwise)

AIC(modelo_completo, modelo_forward, modelo_backward, modelo_stepwise)
BIC(modelo_completo, modelo_forward, modelo_backward, modelo_stepwise)

  