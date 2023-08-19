# Configurando o diretório de trabalho
setwd("C:/Users/leo4s/Documents/Cursos/DSA/Cientista_de_dados/Big_Data_Analytics_R_M_Azure_Machine_Learning/Projeto_final/Projeto_1")
getwd()

# Carregando o pacote readr
library(readr)
library(caret)


# Carregando o dataset
tiposCarros <- read_csv("FEV-data-Excel.csv")

# Resumo dos dados
View(tiposCarros)

#Explorando e Preparando os Dados
# Visualizando as variáveis
str(tiposCarros)


# Medidas de Tendência Central do consumo médio de energia
summary(tiposCarros$ConsumoEnergiaMedio_100km)

# Construindo um histograma
hist(tiposCarros$Potencia_Motor_Km, main = 'Histograma', xlab = 'ConsumoEnergiaMedio_100km')

# Tabela de contingência do consumo de energia

table(tiposCarros$ConsumoEnergiaMedio_100km)

# Explorando relacionamento entre as variáveis: Matriz de Correlação
cor(tiposCarros[c("TipoCarro", "Bateria", "Peso","Carga_Max", "Potencia_Motor_Km","ConsumoEnergiaMedio_100km", "Alcance", 
                  "Velocidade_Max_Kph")])


# Visualizando relacionamento entre as variáveis: Scatterplot
# O Scatter plot utiliza pontos para representar essa relação, cada ponto representa o valor de uma variável no eixo horizontal e o valor de outra variável no eixo vertical.
pairs(tiposCarros[c("TipoCarro", "Bateria", "Peso","Carga_Max", "Potencia_Motor_Km","ConsumoEnergiaMedio_100km", "Alcance", 
                    "Velocidade_Max_Kph")])

# Scatterplot Matrix
install.packages("psych")
library(psych)

# Gráfico - Relacionamento entre as variáveis
pairs.panels(tiposCarros[c("TipoCarro", "Bateria", "Peso","Carga_Max", "Potencia_Motor_Km","ConsumoEnergiaMedio_100km", "Alcance", 
                           "Velocidade_Max_Kph")])

#Separando os dados de treino dos dados de teste

# Etapa 3:

library(caret)

#Definindo a proporção de dados de teste em 30%

dadosTeste <- 0.3
dadosTreinamento <-0.7

#definindo a proporção para os dados de teste
set.seed(123)

#Usando a função createDataPartition para separar os índices dos dados de teste
indices_teste <- createDataPartition(tiposCarros$ConsumoEnergiaMedio_100km, p = dadosTeste, list = FALSE)

#Criando os conjuntos de treinamento e teste com base nos índices
conjunto_treinamento <- tiposCarros[-indices_teste, ]
conjunto_teste <- tiposCarros[indices_teste, ]


# Etapa 4:

#(usando os dados de treino)
?lm
modelo <- lm(ConsumoEnergiaMedio_100km ~ TipoCarro + Peso +
                             Bateria + Carga_Max + Potencia_Motor_Km + Alcance + Preco, data = tiposCarros)

# Visualizando os coeficientes
modelo


#Prevendo o consumo de energia

?predict
previsao_treinamento <- predict(modelo, previsao_consumo = conjunto_treinamento)
View(previsao_treinamento)

# Prevendo Dados de teste
previsao_teste <- predict(modelo, previsao_consumo = conjunto_teste)
View(conjunto_teste)

previsao_teste2 <- predict(modelo, previsão_consumo = conjunto_teste)
View(previsao_teste2)

summary(modelo)


# Criando o modelo final
modelo_v2 <- lm(ConsumoEnergiaMedio_100km ~ TipoCarro + Peso +
                  Bateria + Carga_Max + Potencia_Motor_Km + Alcance + Preco, data = tiposCarros)


summary(modelo_v2)

# Dados de teste
conjunto_teste_2 <- tiposCarros[indices_teste, ]
View(conjunto_teste_2)

previsao_final <- predict(modelo, conjunto_teste_2)
summary(modelo)

class(previsao_final)

View(previsao_final)



