Este código implementa um modelo de aprendizado de máquina em R para prever o lucro em vendas de chocolates, utilizando regressão linear. A análise envolve a preparação dos dados, a construção do modelo, e a avaliação de sua performance com base em métricas estatísticas.

1. Instalação e Carregamento das Bibliotecas
As bibliotecas necessárias foram instaladas e carregadas:

Metrics: Fornece funções para calcular métricas de avaliação de modelos, como MAE e RMSE.
dplyr: Utilizada para manipulação de dados, facilitando a transformação e limpeza dos mesmos.
readxl: Permite a leitura de arquivos Excel.
caret: Uma biblioteca abrangente para treinamento e validação de modelos de aprendizado de máquina.

2. Carregamento e Preparação dos Dados
Os dados foram carregados a partir de um arquivo CSV (o caminho do arquivo está vazio no exemplo, preencha com a tabela choclate protfolio project - 11). Em seguida, algumas etapas de pré-processamento foram realizadas:

Padronização dos Nomes das Colunas: Todas as colunas foram convertidas para letras minúsculas, e os espaços foram substituídos por underscores para facilitar o manuseio dos dados.
Seleção de Variáveis: As colunas profit, profit.. e cost foram removidas do conjunto de dados, provavelmente por conterem dados redundantes ou desnecessários.
Conversão de Valores Monetários: A coluna amount, que continha valores monetários, foi convertida para um formato numérico após a remoção de símbolos de dólar e vírgulas. Essa coluna foi renomeada para total_sales.
Cálculo de Novas Variáveis: Duas novas colunas foram criadas:
total_cost: Calculado multiplicando o número de unidades (units) pelo custo por unidade (cost.per.unit).
profit: Calculado subtraindo total_cost de total_sales.
profit_percent: A porcentagem de lucro, calculada como a razão entre profit e total_cost.

3. Divisão dos Dados em Conjunto de Treinamento e Teste
Os dados foram divididos em conjuntos de treinamento (70%) e teste (30%) usando a função createDataPartition. Isso garante que o modelo seja treinado em um subconjunto dos dados e validado em um conjunto separado, evitando overfitting.

4. Treinamento do Modelo de Regressão Linear
Um modelo de regressão linear foi treinado utilizando a função caret::train. As variáveis independentes incluem:

geography: Localização geográfica das vendas.
total_sales: Total de vendas.
units: Número de unidades vendidas.
cost.per.unit: Custo por unidade.
total_cost: Custo total.
O modelo foi ajustado para prever o lucro (profit) com base nessas variáveis.

5. Previsão e Avaliação do Modelo
As previsões foram geradas para o conjunto de teste, e o desempenho do modelo foi avaliado utilizando três métricas principais:

MAE (Mean Absolute Error): Mede o erro absoluto médio entre as previsões e os valores reais. Um valor mais baixo indica maior precisão.
RMSE (Root Mean Square Error): Mede a raiz quadrada do erro quadrático médio. Assim como o MAE, valores mais baixos indicam um modelo mais preciso.
R-squared: Indica a proporção da variabilidade dos dados que é explicada pelo modelo. Um valor mais próximo de 1 indica um modelo que explica bem os dados.
6. Comparação das Previsões
Por fim, uma tabela de comparação foi criada, mostrando os valores reais de lucro (Teste), as previsões (Predição), e a diferença entre eles (Teste-Predição). Essa tabela fornece uma visão clara de como o modelo está performando em relação aos dados de teste.

Código:

install.packages("Metrics")
library(dplyr)
library(readxl)
library(caret)
library(Metrics)

df<-read.csv("")

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
