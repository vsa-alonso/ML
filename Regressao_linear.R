# Lista de Exercícios Parte 2 - Capítulo 11

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("C:/FCD/BigDataRAzure/Cap12")
getwd()


# Regressão Linear
# Definição do Problema: Prever as notas dos alunos com base em diversas métricas
# https://archive.ics.uci.edu/ml/datasets/Student+Performance
# Dataset com dados de estudantes
# Vamos prever a nota final (grade) dos alunos

# Explorando os dados
head(df)
summary(df)
str(df)
any(is.na(df))


install.packages("ggthemes")

library(ggplot2)
library(ggthemes)
library(dplyr)

setwd("C:/Users/vitora17/Documents/DSA")


# Carregando o dataset
df <- read.csv2('estudantes.csv')

View(head(df))


str(df)



table(df$school)
#GP  MS 
#349  46 

table(df$sex)
#GP  MS 
#349  46 

summary(df$age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 15.0    16.0    17.0    16.7    18.0    22.0 

hist(df$age, main = "Histograma", xlab = "Idade")


table(df$address)
#   R   U 
#   88 307

# Variaveis numericas
numericas = c("age", "Medu","Fedu", "traveltime","studytime", "failures", "famrel", "freetime", "goout", "Dalc", "Walc", "health", "absences", "G1", "G2", "G3")        
cor(df[numericas])
pairs(df[numericas])



# Scatterplot Matrix
install.packages("psych")
library(psych)
pairs.panels(df[numericas])

install.packages("corrplot")
library(corrplot)



data_cor <- cor(df[,numericas])
data_cor
head(data_cor)

corrplot(data_cor, method = "color")



# Treinando e Interpretando o Modelo
# Import Library
install.packages("caTools")
library(caTools)

# Criando as amostras de forma randômica
set.seed(101) 
?sample.split
amostra <- sample.split(df$age, SplitRatio = 0.7)

# Criando dados de treino - 70% dos dados
treino = subset(df, amostra == TRUE)

# Criando dados de teste - 30% dos dados
teste = subset(df, amostra == FALSE)


# Gerando o Modelo (Usando todos os atributos)
modelo_v1 <- lm(G3 ~ ., treino)

# Interpretando o Modelo
summary(modelo_v1) # 0.86


# Visualizando o Modelo e Fazendo Previsões

# Obtendo os resíduos
res <- residuals(modelo_v1)

# Convertendo o objeto para um dataframe
res <- as.data.frame(res)
head(res)

# Histograma dos resíduos
ggplot(res, aes(res)) +  
  geom_histogram(fill = 'blue', 
                 alpha = 0.5, 
                 binwidth = 1)

# Plot do Modelo
plot(modelo_v1)






# Fazendo as predições
modelo_v1 <- lm(G3 ~ ., treino)
prevendo_G3 <- predict(modelo_v1, teste)
prevendo_G3



# Visualizando os valores previstos e observados
resultados <- cbind(prevendo_G3, teste$G3) 
colnames(resultados) <- c('Previsto','Real')
resultados <- as.data.frame(resultados)
resultados
min(resultados)


# Tratando os valores negativos
trata_zero <- function(x){
  if  (x < 0){
    return(0)
  }else{
    return(x)
  }
}

# Aplicando a função para tratar valores negativos em nossa previsão
resultados$Previsto <- sapply(resultados$Previsto, trata_zero)
resultados$Previsto

# Calculando o erro médio
# Quão distantes seus valores previstos estão dos valores observados
# MSE
mse <- mean((resultados$Real - resultados$Previsto)^2)
print(mse)

# RMSE
rmse <- mse^0.5
rmse

# Calculando R Squared
SSE = sum((resultados$Previsto - resultados$Real)^2)
SST = sum((mean(df$G3) - resultados$Real)^2)

# R-Squared
# Ajuda a avaliar o nível de precisão do nosso modelo. Quanto maior, melhor, sendo 1 o valor ideal.
R2 = 1 - (SSE/SST)
R2