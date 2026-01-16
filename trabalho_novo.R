#---------------* Limpando o ambiente *---------------

rm(list = ls())

#---------------* Instalando e carregando pacotes necessários *---------------

pacotes <- c( "readr", "leaps", "MASS", "car", "ggplot2", 'GGally', 'corrplot')

pacotes_nao_instalados <- pacotes[!(pacotes %in% installed.packages()[, "Package"])]

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
  geom_boxplot(outlier.color = "red", fill='skyblue') +
  labs(
    title = "Boxplot do valor mediano das residências",
    y = "Valor mediano"
  )

# Os dados da variável Y sofreram uma censura no período da coleta, casas com valores acima de $500.000
# foram classificadas como se tivessem o mesmo valor. (informação retirada do livro Hands-On Machine Learning with Scikit-Learn and TensorFlow).
# Dessa forma, se continuarmos com esses valores extremos, o modelo sempre tentará se ajustar aos valores de $500.000

# Assim, optei por fazer um comparativo de dois modelos finais, um com os dados tratados, e o outro com os dados de y 'crus'.

# filtragem dos dados
dados_limpos <- dados %>%
  dplyr::filter(y < 500000)

summary(dados_limpos)

ggplot(dados_limpos, aes(x = y)) +                        
  geom_histogram(bins = 30, color = 'white') +
  labs(
    title = "Distribuição do valor mediano das residências (variável resposta) - DADOS LIMPOS",
    x = "Valor mediano",
    y = "Frequência"
  )

ggpairs(dados) # pairs para dados crus
ggpairs(dados_limpos) # pairs para dados limpos

#------- Distribuição das variáveis independentes 

dados_long <- dados %>%
  dplyr::select(x4, x5, x7) %>%
  tidyr::pivot_longer(
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

# dados 'crus'
cor_mat_cru <- cor(dados)
corrplot(cor_mat_cru, method = "color", addCoef.col = TRUE, type = 'upper', bg='black')

# dados limpos
cor_mat_limpo <- cor(dados_limpos)
corrplot(cor_mat_limpo, method = "color", addCoef.col = TRUE, type = 'upper', bg='black')

# matriz de correlação de ambos os tipos de dados apresenta altos indícios de multicolinearidade 
# no 'bloco' das variáveis x4, x5, x6 e x7. Por isso, optei por tratá-la da seguinte forma

#---------------* Modelo Completo  *---------------

fun_modelo_completo <-function(d){
  y <- d$y
  modelo_completo <- lm(y ~ ., data = dados)
  print(summary(modelo_completo))
  print(vif(modelo_completo))
  return(modelo_completo)
}

modelo_completo_crus <- fun_modelo_completo(dados)

modelo_completo_limpos <- fun_modelo_completo(dados_limpos)

# tratando multicolinearidade

transf_dados <- function(d){
  dados_transformados <- d %>%
    dplyr::mutate(
      x4_x7 = x4 / x7, # média de cômodos por casa (tamanho da casa)
      x5_x4 = x5 / x4, # média de quartos por cômodo (proporção de quartos)
      x6_x7 = x6 / x7 # média de pessoas por casa (densidade populacional)
    ) %>%
    dplyr::select(-x4, -x5, -x6, -x7) # REMOVENDO as variáveis originais 'totais' que tornavam o VIF alto
    return(dados_transformados)
}

dadosc_transf <- transf_dados(dados)
corrplot(cor(dadosc_transf), method = "color", addCoef.col = TRUE, type = 'upper', bg='black', title = 'Matriz de correlação com as novas variáveis (DADOS CRUS)')
dadosL_transf <- transf_dados(dados_limpos)
corrplot(cor(dadosL_transf), method = "color", addCoef.col = TRUE, type = 'upper', bg='black', title = 'Matriz de correlação com as novas variáveis (DADOS LIMPOS)')


#---------------* Testando todas as possíveis regressões  *---------------
  
subs_func <- function(dados){
  subsets <- regsubsets( y ~ ., data = dados, nvmax = ncol(dados) - 1)
  
  resumo_subsets <- summary(subsets)
  print(resumo_subsets)
  
  print(resumo_subsets$adjr2) # r2 ajustado de cada um dos modelos
  print(resumo_subsets$cp) # cp de mallows
  
  plot(subsets, scale='Cp', main='Seleção de Modelos - Critério CP de Mallows')
}

all_regr_crus <- subs_func(dadosc_transf)
all_regr_limpos <- subs_func(dadosL_transf)

#---------------*  Métodos forward, backward e stepwise  *---------------

metodos_func <- function(dados){
  modelo_nulo <- lm(y ~ 1, data= dados) # modelo nulo, com a presença apenas do intercepto
  modelo_completo <- lm(y ~ ., data=dados)
  
  #--- FORWARD
  print('MÉTODO FORWARD')
  modelo_forward <- step(modelo_nulo, direction = 'forward', scope=formula(modelo_completo))
  print(summary(modelo_forward))
  print(car::vif(modelo_forward))
  
  #--- BACKWARD
  print('MÉTODO BACKWARD')
  modelo_backward <- step(modelo_completo, direction='backward')
  print(summary(modelo_backward))
  print(car::vif(modelo_backward))
  
  #--- STEPWISE
  print('MÉTODO STEPWISE')
  modelo_stepwise <- step(modelo_nulo, direction = 'both', scope=formula(modelo_completo))
  print(summary(modelo_stepwise))
  print(car::vif(modelo_stepwise))
  
  modelos <- list(
    Forward = modelo_forward, 
    Backward = modelo_backward, 
    Stepwise = modelo_stepwise
  )
  
  return(modelos)
}

metodos_crus <- metodos_func(dadosc_transf)
metodos_limpos <- metodos_func(dadosL_transf)

#---------------* SELEÇÃO DE MODELOS *---------------

sel_mod <- function(metodo){
  selecao <- data.frame(
    Metodo = names(metodo),
    Qtd_Variaveis = sapply(metodo, function(m) length(coef(m)) - 1),
    R2_Ajustado = sapply(metodo, function(m) summary(m)$adj.r.squared),
    AIC = sapply(metodo, AIC),
    BIC = sapply(metodo, BIC)
  )
  
  tabela_ordenada <- selecao[order(selecao$BIC), ]
  
  return(tabela_ordenada)
}

# resultados modelo final - crus
sel_mod_final_cru <- sel_mod(metodos_crus)
sel_mod_final_cru


# resultados modelo final - limpos
sel_mod_final_limpos <- sel_mod(metodos_limpos)
sel_mod_final_limpos

# modelos finais!

modelo_final_c <- metodos_crus$Forward
modelo_final <- metodos_limpos$Forward

# nessas abordagens, ambos os métodos chegaram ao MESMO modelo final (tanto os dados crus, como os dados limpos)!

# Análise de resíduos e diagnóstico (pontos: influente, alavanca, outlier)

par(mfrow = c(2, 2)) 
plot(modelo_final_c)
plot(modelo_final_l)

par(mfrow = c(1, 1))

residuos_pad <- rstandard(modelo_final)
outliers <- which(abs(residuos_pad) > 3)
print(paste("Total de Outliers:", length(outliers)))

k <- length(coef(modelo_final)) # num parametros
n <- nrow(modelo_final$model)   # num observacoes
corte_leverage <- 3 * (k / n)   # Criterio de corte (3x a média)

leverage <- hatvalues(modelo_final)
pontos_alavanca <- which(leverage > corte_leverage)
print(paste("Total de Pontos de Alavanca:", length(pontos_alavanca)))

cook_dist <- cooks.distance(modelo_final)
corte_cook <- 4 / n

influentes <- which(cook_dist > corte_cook)
print(paste("Total de Pontos Influentes:", length(influentes)))

plot(cook_dist, pch = 20, main = "Distância de Cook: Identificando Pontos Influentes",
     ylab = "Distância de Cook", xlab = "Índice da Observação")
abline(h = corte_cook, col = "red", lty = 2) # Linha de corte
