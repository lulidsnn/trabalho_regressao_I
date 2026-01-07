#-------* Limpando o ambiente *-------

rm(list = ls())

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

head(dados) 
str(dados)       
summary(dados)  
pairs(dados)

modelo_completo <- lm(y ~ ., data = dados)
modelo_completo
