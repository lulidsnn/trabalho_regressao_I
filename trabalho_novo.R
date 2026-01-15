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

dados_orig <- read.csv("housing.csv", sep = ',')

names(dados_orig)

#---------------* Limpeza de dados *---------------

# removendo a coluna categórica 'ocean_proximity'
dados_orig$ocean_proximity <- NULL 
names(dados_orig)

# quantidade de valores faltantes
sum(is.na(dados_orig)) 

# verificando quais colunas possuem valores faltantes -> nesse caso, coluna total_bedrooms
colSums(is.na(dados_orig)) 

# verificando a porcentagem de valores faltantes
colMeans(is.na(dados_orig)) * 100 

# Como a porcentagem de dados faltantes é aproximadamente 1%, optei por remover os valores faltantes
dados_nao_faltantes <- na.omit(dados_orig)

#----------

# Por questões de facilidade, renomeei as variáveis para x1, x2, ..., x8. 
# Antes disso, vou criar um mapa de variáveis, para facilitar a interpretação, quando necessário.

# Criando o mapa de variáveis
mapa_variaveis <- data.frame(
  x = paste0("x", 1:8), # novo nome para as variáveis
  original = names(dados_nao_faltantes)[names(dados_nao_faltantes) != "median_house_value"] # nomes originais das variáveis independentes
)

mapa_variaveis

# cópia dos dados originais, em que irei renomear as variáveis
dados <- dados_nao_faltantes 


# renomeando as colunas do dataset cópia (dados)
colnames(dados) <- c( 
  paste0("x", 1:8),
  "y"
)

# garantindo que y seja a primeira coluna
dados <- dados[, c("y", paste0("x", 1:8))]

#---------------* Análise Exploratória *---------------

head(dados) 
str(dados)       
summary(dados)  

#------- Distribuição da variável resposta
ggplot(dados, aes(x = y)) +
  geom_histogram(bins = 30, color = 'white') +
  labs(
    title = "Distribuição do valor mediano das residências (variável resposta)",
    x = "Valor mediano",
    y = "Frequência"
  )

# melhorar ★
ggplot(dados, aes(y = y)) +
  geom_boxplot(outlier.color = "red") +
  labs(
    title = "Boxplot do valor mediano das residências",
    y = "Valor mediano"
  )

ggpairs(dados)

#------- Distribuição das variáveis independentes 

dados_long <- dados %>%
  select(x4, x5, x7) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variavel",
    values_to = "valor"
  )

ggplot(dados_long, aes(x = variavel, y = valor)) +
  geom_errorbar(stat = "boxplot", width = 0.2) +
  geom_boxplot(aes(fill = variavel), width = 0.6) +
  labs(
    title = "Boxplot conjunto das variáveis explicativas",
    x = "Variável",
    y = "Valor"
  ) + 
  geom_point(stat = "summary", fun = "mean", shape = 4,
             size = 3) + coord_flip() 


ggplot(dados, aes(x = x8, y = y)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Valor das residências vs renda média",
    x = "Renda média",
    y = "Valor mediano das residências"
  )


#---------------------
y <- dados$y
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

modelo_nulo<- lm(y ~ 1, data= dados) # modelo nulo, com a presença apenas do intercepto

# FORWARD
modelo_forward <- step(modelo_nulo, direction = 'forward', scope=formula(modelo_completo))
summary(modelo_forward)
car::vif(modelo_forward) # checar multicolineariedade
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
