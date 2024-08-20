install.packages("Metrics")
library(dplyr)
library(readxl)
library(caret)
library(Metrics)

df<-read.csv("C:/Users/euric/Documents/GitHub/Aprendizado de Máquina Chocolate/choclate protfolio project - 11.csv")

names(df) <- tolower(gsub(' ', '_', names(df)))
df <- df %>%
  select(-c(profit, profit.., cost))
df$amount <- as.integer(gsub('\\$|,', '', df$amount))
names(df)[names(df) == 'amount'] <- 'total_sales'
df$total_cost <- df$units * df$cost.per.unit
df$profit <- df$total_sales - df$total_cost
df$profit_percent <- round((df$profit / df$total_cost) * 100, 2)

# Dividir os dados em conjunto de treinamento e teste
set.seed(123)

noTreino = createDataPartition(y=df$profit, p=0.7, groups = 5, list=F)
treino = df[noTreino,]; teste = df[-noTreino,]

modelo = caret::train(profit ~ geography+ total_sales+ units+cost.per.unit+total_cost, data = treino, method = "lm")
predicao = predict(modelo, newdata = teste)
mae <- mae(predicao, teste$profit)
rmse <- rmse(predicao, teste$profit)
rsquared <- R2(predicao, teste$profit)
print(paste("MAE:", mae))
print(paste("RMSE:", rmse))
print(paste("R-squared:", rsquared))
Teste <-teste$profit
Predição <-predicao
comparison_table <- data.frame(Teste, Predição, Teste-Predição)
print(comparison_table)
