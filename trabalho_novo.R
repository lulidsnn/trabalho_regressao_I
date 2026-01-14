#---------------* Limpando o ambiente *---------------

rm(list = ls())

#---------------* Instalando e carregando pacotes necessários *---------------

pacotes <- c( "readr", "leaps", "MASS", "car", "ggplot2", 'GGally')

pacotes_nao_instalados <- pacotes[!(pacotes %in% installed.packages()[, "Package"])] # verificar

if (length(pacotes_nao_instalados) > 0) {
  install.packages(pacotes_nao_instalados, dependencies = TRUE)
}

lapply(pacotes, library, character.only = TRUE)

cat("\014") #limpando console

#---------------* Importando dados csv para análise *---------------

# No R, use 'read.csv2' ou especifique o argumento 'dec'
dados <- read.csv("energy_efficiency.csv", sep = ',')

names(dados)
# garantindo que y seja a primeira coluna

#---------------* Análise Exploratória *---------------

head(dados) 
str(dados)       
summary(dados)  

hist(dados$y, breaks = 20, main = "", xlab = "")

ggpairs(dados)

#---------------------
y <- dados$Y
modelo_completo <- lm(y ~ ., data = dados)
modelo_completo
summary(modelo_completo) # o modelo completo já nos traz uma ideia de quais variáveis podem ser importantes e significativas para o modelo final, mas ainda não nos dá certeza

#---------------* Testando todas as possíveis regressões  *---------------

subsets <- regsubsets( y ~ ., data = dados, nvmax = ncol(dados) - 1)

resumo_subsets <- summary(subsets)
resumo_subsets
resumo_subsets$which # variáveis presentes em cada modelo

resumo_subsets$adjr2 # r2 ajustado de cada um dos modelos
resumo_subsets$cp # cp de mallows

par(mfrow=c(1,1))
plot(subsets, scale='Cp', main='Seleção de Modelos - Critério CP de Mallows')

# com base nos valores de R2 ajustado e Cp de Mallows, os modelos com 6 a 8 variáveis são os melhores

#---------------*  Métodos forward, backward e stepwise  *---------------

y <- dados$y
modelo_nulo<- lm(y ~ 1, data= dados) # modelo nulo, com a presença apenas do intercepto

# FORWARD
modelo_forward <- step(modelo_nulo, direction = 'forward', scope=formula(modelo_completo))
summary(modelo_forward)
car::vif(modelo_forward) 
# plot(modelo_forward)

#BACKWARD
modelo_backward <- step(modelo_completo, direction='backward')
summary(modelo_backward)
car::vif(modelo_backward)

#STEPWISE
modelo_stepwise <- step(modelo_nulo, direction = 'both', scope=formula(modelo_completo))
summary(modelo_stepwise)
car::vif(modelo_stepwise)

#---------------* SELEÇÃO DE MODELOS *---------------

AIC(modelo_forward, modelo_backward, modelo_stepwise)
BIC(modelo_forward, modelo_backward, modelo_stepwise)

# nessas abordagens, ambos os métodos chegaram ao MESMO modelo final!

#Análise de resíduos e diagnóstico (pontos: influente, alavanca, outlier)

par(mfrow=c(2,2))
plot(modelo_forward)