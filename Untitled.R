library(dplyr)
library(nnet)
library(caret)

colnames(Foot_Data)

Foot_Data <- Foot_Data %>%
  select(Date, League, Season, Home, Away, Odd_Over25_FT, Odd_Over15_FT, Odd_Over05_FT, Odd_BTTS_Yes, Odd_Over35_FT, Goals_H_FT, Goals_A_FT)

Foot_Data <- Foot_Data %>% 
  filter(Odd_Over05_FT != 00)

Foot_Data <- Foot_Data %>%
  mutate(Goals_All = Goals_H_FT + Goals_A_FT)

Foot_Data <- Foot_Data %>%
  mutate(Goals_Bin = ifelse(Goals_All > 0.5, 0, 1))

percent_over_05 <- (sum(Foot_Data$Goals_All > 0.5) / nrow(Foot_Data)) * 100
percent_over_15 <- (sum(Foot_Data$Goals_All > 1.5) / nrow(Foot_Data)) * 100
percent_over_25 <- (sum(Foot_Data$Goals_All > 2.5) / nrow(Foot_Data)) * 100

cat("Porcentagem de partidas com mais de 0.5 gols:", round(percent_over_05, 2), "%\n")
cat("Porcentagem de partidas com mais de 1.5 gols:", round(percent_over_15, 2), "%\n")
cat("Porcentagem de partidas com mais de 2.5 gols:", round(percent_over_25, 2), "%\n")

# Porcentagem de partidas com mais de 0.5 gols: 90.99 %
# Porcentagem de partidas com mais de 1.5 gols: 72.65 %
# Porcentagem de partidas com mais de 2.5 gols: 49.61 %


mean_odds_05 <- (mean(Foot_Data$Odd_Over05_FT))
cat("Média Odds 0,5:", round(mean_odds_05, 5))

mean_odds_15 <- (mean(Foot_Data$Odd_Over15_FT))
cat("Média Odds 1,5:", round(mean_odds_15, 5))

mean_odds_25 <- (mean(Foot_Data$Odd_Over25_FT))
cat("Média Odds 2,5:", round(mean_odds_25, 5))

# Média Odds 0,5: 1.09488
# Média Odds 1,5: 1.29725
# Média Odds 2,5: 1.95846

################################################################################
#Divisão do DataFrame

set.seed(15)
train_index <- sample(1:nrow(Foot_Data), 0.8 * nrow(Foot_Data))

train_data <- Foot_Data[train_index, ]
test_data <- Foot_Data[-train_index, ]

nn_model <- nnet(Goals_Bin ~ Odd_BTTS_Yes + Odd_Over35_FT,
                 data = train_data, 
                 size = 5,   # Número de unidades na camada oculta
                 linout = FALSE,  # Usa ativação sigmoide (para classificação)
                 decay = 0.1,  # Regularização L2 para evitar overfitting
                 maxit = 500,  # Aumenta as iterações para melhor convergência
                 trace = FALSE)  # Evita exibir logs de treinamento

# Exibir o resumo do modelo treinado
summary(nn_model)

predictions_prob <- predict(nn_model, test_data, type = "raw")

threshold <- 0.66  
predictions <- ifelse(predictions_prob > threshold, 1, 0)

# Calcular a acurácia
accuracy <- mean(predictions == test_data$Goals_Bin)
cat("Acurácia do modelo:", round(accuracy * 100, 2), "%\n")

confusionMatrix(as.factor(predictions), as.factor(test_data$Goals_Bin))


