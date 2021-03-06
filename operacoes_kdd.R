#### PARAN� FEDERAL UNIVERSITY - IAA006 - DATA ARCHITECTURE
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

# 0 - Personal Environment set (Diret�rio padr�o pessoal para salvar outputs do R)
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

# Carregamento  em mem�ria dos modelos j� existentes
modelo_treino_rna <- readRDS("modelo_treino_rna_inicial.rds")
modelo_treino_id3 <- readRDS("modelo_treino_id3_inicial.rds")


#### TRATAMENTOS M�NIMOS PARA RODAR MODELO ######
###############################
# 2 - Coluna Cidades
houses[houses$cidade == "São Paulo", which(colnames(houses) == "cidade")] = "S�o Paulo"

# 3 - Coluna Andares
houses[houses$andares == "-", which(colnames(houses)=="andares")] <- "0"
houses$andares_new <- as.numeric(houses$andares)

# 4 - Coluna Animais
houses$animais_new <- ifelse(houses$animais == "acept", 1, 0)

# 5 - Coluna Mobilia
houses$mobilia_new <- ifelse(houses$mobilia == "furnished", 1, 0)

# 6 - Fatiar 15% da base total para reduzir o n�mero de registros para treino (sen�o leva mto tempo pro algoritmo)
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
#modelo_treino_rna <- train(cidade ~ area + comodos + banheiros + vagas + andares_new + animais_new + mobilia_new + condominio +
 #                     aluguel + iptu + seguro, data = treino, method = "nnet", trace = FALSE)

# ID3: https://www.edureka.co/blog/decision-tree-algorithm/ & https://rpubs.com/JuanBarros/projetoIA2
# modelo_treino_id3 <- train(cidade ~ area + comodos + banheiros + vagas + andares_new + animais_new + mobilia_new + condominio +
  #                          aluguel + iptu + seguro, data = treino, method = "rpart")
# Modelo Estat�stico:
# modelo_treino_glm <- train(cidade_new ~ area + comodos + banheiros + vagas + andares_new + animais_new + mobilia_new + condominio +
                      #aluguel + iptu + seguro, data = treino, method = "glm")


# 9 - Usar o modelo treinado, anteriormente, agora com a base de teste para testar a classifica��o
modelo_predito_rna <- predict(modelo_treino_rna, teste)
modelo_predito_id3 <- predict(modelo_treino_id3, teste)
# modelo_predito_glm <- predict(modelo_treino_glm, teste)

# 10 - Matriz de confus�o para visualizar resultado da classifica��o com 
confusionMatrix(modelo_predito_rna, as.factor(teste$cidade))
confusionMatrix(modelo_predito_id3, as.factor(teste$cidade))
# confusionMatrix(modelo_predito_glm, teste$cidade_new)

# 11 - Salvar modelos gerados para uso posterior
# print(modelo_treino_rna)
# saveRDS(modelo_treino_rna, "modelo_treino_rna_inicial.rds")

# print(modelo_treino_id3)
# saveRDS(modelo_treino_id3, "modelo_treino_id3_inicial.rds")





#####################################################################
#### C�DIGO DA BASELINE  ############################################
#####################################################################

library("e1071")
library("randomForest")
library("kernlab")
library("caret")
library("dbplyr")
library("nnet")

# 0 - Personal Environment set (Diret�rio padr�o pessoal para salvar outputs do R)
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

# Carregamento  em mem�ria dos modelos j� existentes
modelo_treino_rna <- readRDS("modelo_treino_rna_inicial.rds")
modelo_treino_id3 <- readRDS("modelo_treino_id3_inicial.rds")


#### TRATAMENTOS M�NIMOS PARA RODAR MODELO ######
###############################
# 2 - Coluna Cidades
houses[houses$cidade == "São Paulo", which(colnames(houses) == "cidade")] = "S�o Paulo"

# 3 - Coluna Andares
houses[houses$andares == "-", which(colnames(houses)=="andares")] <- "0"
houses$andares_new <- as.numeric(houses$andares)

# 4 - Coluna Animais
houses$animais_new <- ifelse(houses$animais == "acept", 1, 0)

# 5 - Coluna Mobilia
houses$mobilia_new <- ifelse(houses$mobilia == "furnished", 1, 0)

# 6 - Base de SP
houses_sp <- houses[houses$cidade == "S�o Paulo",]
houses <- houses[houses$cidade != "S�o Paulo",]

# 7 - Fatiar base de SP
set.seed(7)
indices_sp <- createDataPartition(houses_sp$aluguel, p=0.22, list=FALSE)
houses <- rbind(houses, houses_sp[indices_sp,])

# 8 - Base de Campinas
houses_campinas <- houses[houses$cidade == "Campinas",]

# 9 - Fatiar base de Campinas
set.seed(7)
indices_campinas <- createDataPartition(houses_campinas$aluguel, p=0.35, list=FALSE)
houses <- rbind(houses, houses_campinas[indices_campinas,])

table(houses$cidade)

# 10 - Fatiar 15% da base total para reduzir o n�mero de registros para treino (sen�o leva mto tempo pro algoritmo)
set.seed(7)
indices_total <- createDataPartition(houses$cidade, p=0.30, list=FALSE)
houses_fatiado <- houses[indices_total, ]

table(houses_fatiado$cidade)

# 11 - Fatiar 80% para treino e 20% para teste
indices <- createDataPartition(houses_fatiado$cidade, p=0.80, list=FALSE)

treino <- houses_fatiado[indices,]

teste <- houses_fatiado[-indices,]

# 12 - Treinar modelo
# Exemplos de algoritmos caret package: https://topepo.github.io/caret/available-models.html
# RNA
modelo_treino_rna <- train(cidade ~ area + comodos + banheiros + vagas + andares_new + animais_new + mobilia_new + condominio +
                             aluguel + iptu + seguro, data = treino, method = "nnet", trace = FALSE)

# ID3: https://www.edureka.co/blog/decision-tree-algorithm/ & https://rpubs.com/JuanBarros/projetoIA2
modelo_treino_id3 <- train(cidade ~ area + comodos + banheiros + vagas + andares_new + animais_new + mobilia_new + condominio +
                             aluguel + iptu + seguro, data = treino, method = "rpart")

# Modelo Estat�stico:
# modelo_treino_glm <- train(cidade_new ~ area + comodos + banheiros + vagas + andares_new + animais_new + mobilia_new + condominio +
#aluguel + iptu + seguro, data = treino, method = "glm")


# 13 - Usar o modelo treinado, anteriormente, agora com a base de teste para testar a classifica��o
modelo_predito_rna <- predict(modelo_treino_rna, teste)
modelo_predito_id3 <- predict(modelo_treino_id3, teste)
# modelo_predito_glm <- predict(modelo_treino_glm, teste)

# 14 - Matriz de confus�o para visualizar resultado da classifica��o com 
confusionMatrix(modelo_predito_rna, as.factor(teste$cidade))
confusionMatrix(modelo_predito_id3, as.factor(teste$cidade))
# confusionMatrix(modelo_predito_glm, teste$cidade_new)

# 15 - Salvar modelos gerados para uso posterior
# print(modelo_treino_rna)
# saveRDS(modelo_treino_rna, "modelo_treino_rna_inicial.rds")

# print(modelo_treino_id3)
# saveRDS(modelo_treino_id3, "modelo_treino_id3_inicial.rds")

# 16 - Baseline CSV
write.csv(houses, "houses_to_rent_baseline.csv")







#####################################################################
#### C�DIGO DE MELHORIAS  ##########################################
#####################################################################

houses_fatiado$cidade_new <- as.numeric(as.factor(houses_fatiado$cidade))

model <- lm(aluguel ~ cidade_new + comodos + banheiros + vagas + andares_new + animais_new + mobilia_new + condominio +
              iptu + seguro, data = houses_fatiado)

summary(model)

View(houses_fatiado)

# a) Remover outliers pela t�cnica de dist�ncia de Cook

install.packages("outliers")
library("outliers")

houses_fatiado <- within(houses_fatiado, {residuos <- residuals(model)})

cooksd <- cooks.distance(model)

houses_fatiado$cooksd <- cooksd

tam_amostra <- nrow(houses_fatiado)

houses_fatiado$outlier <- with(houses_fatiado, ifelse(cooksd>4/tam_amostra,"yes","no"))

houses_fatiado <- houses_fatiado[houses_fatiado$outlier != "yes", ]


tapply(houses$aluguel, houses$cidade, sd)

####################################################################################

var <- houses

var$mobilia <- NULL

View(var)

var$area_scale <- scale(var$area)

var <- var[var$cod != 2398, ]
var <- var[var$cod != 9242, ]


# ... remover outliers, min, max, registros nada a ver, tudo o que n�o prestar
# 0 - Personal Environment set (Diret�rio padr�o pessoal para salvar outputs do R)
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

# 2 - Coluna Cidades
houses[houses$cidade == "São Paulo", which(colnames(houses) == "cidade")] = "S�o Paulo"

# 3 - Coluna Andares
houses[houses$andares == "-", which(colnames(houses)=="andares")] <- "0"
houses$andares_new <- as.numeric(houses$andares)

# 4 - Coluna Animais
houses$animais_new <- ifelse(houses$animais == "acept", 1, 0)

# 5 - Coluna Mobilia
houses$mobilia_new <- ifelse(houses$mobilia == "furnished", 1, 0)

# 6 - Base de SP
houses_sp <- houses[houses$cidade == "S�o Paulo",]
houses <- houses[houses$cidade != "S�o Paulo",]

# 7 - Fatiar base de SP
set.seed(7)
indices_sp <- createDataPartition(houses_sp$aluguel, p=0.22, list=FALSE)
houses <- rbind(houses, houses_sp[indices_sp,])

# 8 - Fatiar 15% da base total para reduzir o n�mero de registros para treino (sen�o leva mto tempo pro algoritmo)
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


# 10 - Usar o modelo treinado, anteriormente, agora com a base de teste para testar a classifica��o
modelo_predito_rna <- predict(modelo_treino_rna, teste)
modelo_predito_id3 <- predict(modelo_treino_id3, teste)
# modelo_predito_glm <- predict(modelo_treino_glm, teste)

# 11 - Matriz de confus�o para visualizar resultado da classifica��o com 
confusionMatrix(modelo_predito_rna, as.factor(teste$cidade))
confusionMatrix(modelo_predito_id3, as.factor(teste$cidade))
# confusionMatrix(modelo_predito_glm, teste$cidade_new)

nrow(houses[houses$cidade == "S�o Paulo", ] )
nrow(houses[houses$cidade == "Rio de Janeiro", ] )
nrow(houses[houses$cidade == "Campinas", ] )
nrow(houses[houses$cidade == "Belo Horizonte", ] )
nrow(houses[houses$cidade == "Porto Alegre", ] )


nrow(houses_fatiado[houses_fatiado$cidade == "S�o Paulo", ] )
nrow(houses_fatiado[houses_fatiado$cidade == "Rio de Janeiro", ] )
nrow(houses_fatiado[houses_fatiado$cidade == "Campinas", ] )
nrow(houses_fatiado[houses_fatiado$cidade == "Belo Horizonte", ] )
nrow(houses_fatiado[houses_fatiado$cidade == "Porto Alegre", ] )
