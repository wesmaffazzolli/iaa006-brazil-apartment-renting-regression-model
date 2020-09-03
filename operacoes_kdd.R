#### PARANÁ FEDERAL UNIVERSITY - IAA006 - DATA ARCHITECTURE
#### PRE-PROCESSMENT STAGE - KDD (KNOWLEDGE DISCOVERY IN DATABASES)
### DATASET USED: BRAZILIAN HOUSES TO RENT
## DATASET SOURCE: https://www.kaggle.com/rubenssjr/brasilian-houses-to-rent?select=houses_to_rent_v2.csv
## TEAM MEMBERS: Rafael Eduardo Gomes & Wesley Maffazzollli
## 


#### 
####
####

# Team goals established for this exercise:
# a) Train a regression model to predict the total rent value
# b) Classify which city is based on certain conditions (area, rooms, property tax etc.)

## Column names & meaning:
# [1] city:
# [2] area:
# [3] rooms:
# [4] bathroom:
# [5] parking.spaces:
# [6] floor:
# [7] animal:
# [8] furniture:
# [9] hoa.R:
# [10] rent.amount..R:
# [11] property.tax..R:
# [12] fire.insurance..R:
# [13] total..R:

########################################
#### BEGINNING #########################
########################################

install.packages("tidyverse")
install.packages("e1071")
install.packages("randomForest")
install.packages("kernlab")
install.packages("caret")
install.packages("nnet")

library("e1071")
library("randomForest")
library("kernlab")
library("caret")
library("dbplyr")
library("nnet")

# 0 - Personal Environment set (Diretório padrão pessoal para salvar outputs do R)
getwd()
setwd("C:/Users/wesle/OneDrive - ufpr.br/IAA006 - Arquitetura de Dados/Dataset/iaa006-brazil-apartment-renting-regression-model")
set.seed(7)

# 1 - Dataset loading
houses <- read.csv("houses_to_rent_v2.csv")
colnames(houses)[1] <- "cidade"
colnames(houses)[3] <- "comodos"
colnames(houses)[4] <- "banheiros"
colnames(houses)[5] <- "vagas"
colnames(houses)[6] <- "andares"
colnames(houses)[7] <- "animais"
colnames(houses)[8] <- "mobilia"
colnames(houses)[9] <- "condominio"
colnames(houses)[10] <- "aluguel"
colnames(houses)[11] <- "iptu"
colnames(houses)[12] <- "seguro"
colnames(houses)[13] <- "somatorio"

houses$cod <- 1:nrow(houses)
houses_old <- houses

#### TRATAMENTOS MÍNIMOS PARA RODAR MODELO ######
###############################
# 2 - Coluna Cidades
houses[houses$cidade == "SÃ£o Paulo", which(colnames(houses) == "cidade")] = "São Paulo"

# 3 - Coluna Andares
houses[houses$andares == "-", which(colnames(houses)=="andares")] <- "0"
houses$andares_new <- as.numeric(houses$andares)

# 4 - Coluna Animais
houses$animais_new <- ifelse(houses$animais == "acept", 1, 0)

# 5 - Coluna Mobilia
houses$mobilia_new <- ifelse(houses$mobilia == "furnished", 1, 0)

# 6 - Fatiar 15% da base total para reduzir o número de registros para treino (senão leva mto tempo pro algoritmo)
set.seed(7)
indices_total <- createDataPartition(houses$cidade, p=0.15, list=FALSE)
houses_fatiado <- houses[indices_total, ]

# 7 - Fatiar 80% para treino e 20% para teste
indices <- createDataPartition(houses_fatiado$cidade, p=0.80, list=FALSE)

treino <- houses_fatiado[indices,]

teste <- houses_fatiado[-indices,]

# 8 - Treinar modelo
# Exemplos de algoritmos caret package: https://topepo.github.io/caret/available-models.html
# RNA
modelo_treino_rna <- train(cidade ~ area + comodos + banheiros + vagas + andares_new + animais_new + mobilia_new + condominio +
                      aluguel + iptu + seguro, data = treino, method = "nnet", trace = FALSE)

# ID3: https://www.edureka.co/blog/decision-tree-algorithm/ & https://rpubs.com/JuanBarros/projetoIA2
modelo_treino_id3 <- train(cidade ~ area + comodos + banheiros + vagas + andares_new + animais_new + mobilia_new + condominio +
                            aluguel + iptu + seguro, data = treino, method = "rpart")
# Modelo Estatístico:
# modelo_treino_glm <- train(cidade_new ~ area + comodos + banheiros + vagas + andares_new + animais_new + mobilia_new + condominio +
                      #aluguel + iptu + seguro, data = treino, method = "glm")


# 9 - Usar o modelo treinado, anteriormente, agora com a base de teste para testar a classificação
modelo_predito_rna <- predict(modelo_treino_rna, teste)
modelo_predito_id3 <- predict(modelo_treino_id3, teste)
# modelo_predito_glm <- predict(modelo_treino_glm, teste)

# 10 - Matriz de confusão para visualizar resultado da classificação com 
confusionMatrix(modelo_predito_rna, as.factor(teste$cidade))
confusionMatrix(modelo_predito_id3, as.factor(teste$cidade))
# confusionMatrix(modelo_predito_glm, teste$cidade_new)

# 11 - Salvar modelos gerados para uso posterior
print(modelo_treino_rna)
saveRDS(modelo_treino_rna, "modelo_treino_rna_inicial.rds")

print(modelo_treino_id3)
saveRDS(modelo_treino_id3, "modelo_treino_id3_inicial.rds")

# 12 - Para ler um modelo posteriormente
modelo_treino_rna <- readRDS("modelo_treino_rna_inicial.rds")
modelo_treino_id3 <- readRDS("modelo_treino_id3_inicial.rds")




#####################################################################
#### IMPROVEMENTS START HERE - LET'S DO IT! #########################
#####################################################################

# ... remover outliers, min, max, registros nada a ver, tudo o que não prestar
# 0 - Personal Environment set (Diretório padrão pessoal para salvar outputs do R)
getwd()
setwd("C:/Users/wesle/OneDrive - ufpr.br/IAA006 - Arquitetura de Dados/Dataset/iaa006-brazil-apartment-renting-regression-model")
set.seed(7)

# 1 - Dataset loading
houses <- read.csv("houses_to_rent_v2.csv")
colnames(houses)[1] <- "cidade"
colnames(houses)[3] <- "comodos"
colnames(houses)[4] <- "banheiros"
colnames(houses)[5] <- "vagas"
colnames(houses)[6] <- "andares"
colnames(houses)[7] <- "animais"
colnames(houses)[8] <- "mobilia"
colnames(houses)[9] <- "condominio"
colnames(houses)[10] <- "aluguel"
colnames(houses)[11] <- "iptu"
colnames(houses)[12] <- "seguro"
colnames(houses)[13] <- "somatorio"

houses$cod <- 1:nrow(houses)
houses_old <- houses

#### TRATAMENTOS MÍNIMOS PARA RODAR MODELO ######
###############################
# 2 - Coluna Cidades
houses[houses$cidade == "SÃ£o Paulo", which(colnames(houses) == "cidade")] = "São Paulo"

# 3 - Coluna Andares
houses[houses$andares == "-", which(colnames(houses)=="andares")] <- "0"
houses$andares_new <- as.numeric(houses$andares)

# 4 - Coluna Animais
houses$animais_new <- ifelse(houses$animais == "acept", 1, 0)

# 5 - Coluna Mobilia
houses$mobilia_new <- ifelse(houses$mobilia == "furnished", 1, 0)

# 6 - Base de SP
houses_sp <- houses[houses$cidade == "São Paulo",]
houses <- houses[houses$cidade != "São Paulo",]

# 7 - Fatiar base de SP
set.seed(7)
indices_sp <- createDataPartition(houses_sp$aluguel, p=0.22, list=FALSE)
houses <- rbind(houses, houses_sp[indices_sp,])

# 8 - Fatiar 15% da base total para reduzir o número de registros para treino (senão leva mto tempo pro algoritmo)
# set.seed(7)
# indices_total <- createDataPartition(houses$cidade, p=0.20, list=FALSE)
# houses_fatiado <- houses[indices_total, ]

# 9 - Fatiar 80% para treino e 20% para teste
indices <- createDataPartition(houses$cidade, p=0.80, list=FALSE)

treino <- houses[indices,]

teste <- houses[-indices,]

# 10 - Treinar modelo
# Exemplos de algoritmos caret package: https://topepo.github.io/caret/available-models.html
# RNA
modelo_treino_rna <- train(cidade ~ area + comodos + banheiros + vagas + andares_new + animais_new + mobilia_new + condominio +
                             aluguel + iptu + seguro, data = treino, method = "nnet", trace = FALSE)

# ID3: https://www.edureka.co/blog/decision-tree-algorithm/ & https://rpubs.com/JuanBarros/projetoIA2
modelo_treino_id3 <- train(cidade ~ area + comodos + banheiros + vagas + andares_new + animais_new + mobilia_new + condominio +
                             aluguel + iptu + seguro, data = treino, method = "rpart")


# 10 - Usar o modelo treinado, anteriormente, agora com a base de teste para testar a classificação
modelo_predito_rna <- predict(modelo_treino_rna, teste)
modelo_predito_id3 <- predict(modelo_treino_id3, teste)
# modelo_predito_glm <- predict(modelo_treino_glm, teste)

# 11 - Matriz de confusão para visualizar resultado da classificação com 
confusionMatrix(modelo_predito_rna, as.factor(teste$cidade))
confusionMatrix(modelo_predito_id3, as.factor(teste$cidade))
# confusionMatrix(modelo_predito_glm, teste$cidade_new)

nrow(houses[houses$cidade == "São Paulo", ] )
nrow(houses[houses$cidade == "Rio de Janeiro", ] )
nrow(houses[houses$cidade == "Campinas", ] )
nrow(houses[houses$cidade == "Belo Horizonte", ] )
nrow(houses[houses$cidade == "Porto Alegre", ] )


nrow(houses_fatiado[houses_fatiado$cidade == "São Paulo", ] )
nrow(houses_fatiado[houses_fatiado$cidade == "Rio de Janeiro", ] )
nrow(houses_fatiado[houses_fatiado$cidade == "Campinas", ] )
nrow(houses_fatiado[houses_fatiado$cidade == "Belo Horizonte", ] )
nrow(houses_fatiado[houses_fatiado$cidade == "Porto Alegre", ] )
