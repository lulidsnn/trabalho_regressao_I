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

dados_orig <- read.csv("winequality-red.csv", header = TRUE, sep = ";")
names(dados_orig)

# Por questões de facilidade, renomeei as variáveis para x1, x2, ..., x11. Antes disso, vou criar um mapa de variáveis, para facilitar a interpretação, quando necessário

# Criando o mapa de variáveis
mapa_variaveis <- data.frame(
  x = paste0("x", 1:11), # novo nome para as variáveis
  original = names(dados_orig)[names(dados_orig) != "quality"] # nomes originais das variáveis independentes
)

mapa_variaveis

# cópia dos dados originais, em que irei renomear as variáveis
dados <- dados_orig 

# renomeando as colunas do dataset cópia (dados)
colnames(dados) <- c( 
  paste0("x", 1:11),
  "y"
)

# garantindo que y seja a primeira coluna
dados <- dados[, c("y", paste0("x", 1:11))]

#---------------* Análise Exploratória *---------------

head(dados) 
str(dados)       
summary(dados)  

hist(dados$y, breaks = 20, main = "Distribuição da Qualidade do Vinho", xlab = "Qualidade")

ggpairs(dados)

# criando função para verificar lineariedade de Y em relação às variáveis X

grafico_y_x <- function(nome_x, data) {
  x <- data[[nome_x]]
  y <- data$y
  
  plot(
    x, data$y,
    xlab = nome_x,
    ylab = "Qualidade",
    main = paste("Qualidade vs", nome_x)
  )
  
  abline(lm(y ~ x), col = "red")
  
}


#verificar lineariedade em relação às seguintes variáveis: x2, x4, x7, x9 ,x11  # explicar porque a escolha dessas variáveis

vars <- c("x2", "x4", "x7", "x11")

par(mfrow = c(2, 2))
invisible(lapply(vars, grafico_y_x, data = dados))



#---------------* Criação do modelo completo *---------------

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
car::vif(modelo_forward) # resultados de VIF indicam ausência de MULTICOLINEARIDADE severa
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
