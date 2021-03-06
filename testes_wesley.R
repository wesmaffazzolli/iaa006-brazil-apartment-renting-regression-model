install.packages("tidyverse")
install.packages("e1071")
install.packages("randomForest")
install.packages("kernlab")
install.packages("caret")

library("e1071")
library("randomForest")
library("kernlab")
library("caret")
library("dbplyr")

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

summary(houses)

View(houses)


#### TRATAMENTOS M�NIMOS PARA RODAR MODELO ######
###############################
# 2 - Coluna Cidades
houses$cidade_new <- as.factor(houses$cidade)

# 3 - Coluna Andares
houses[houses$andares == "-", which(colnames(houses)=="andares")] <- "0"
houses$andares_new <- as.numeric(houses$andares)

# 4 - Coluna Animais
houses$animais_new <- ifelse(houses$animais == "acept", 1, 0)

# 5 - Coluna Mobilia
houses$mobilia_new <- ifelse(houses$mobilia == "furnished", 1, 0)

# 6 - Fatiar 15% da base total para reduzir o n�mero de registros para treino (sen�o leva mto tempo pro algoritmo)
indices_total <- createDataPartition(houses$cidade, p=0.15, list=FALSE)
houses_fatiado <- houses[indices_total, ]

# 7 - Fatiar 80% para treino e 20% para teste
indices <- createDataPartition(houses_fatiado$cidade, p=0.80, list=FALSE)

treino <- houses_fatiado[indices,]

teste <- houses_fatiado[-indices,]

# 8 - Treinar um modelo Random Forest com a base de treino (coloquei um SVMRadial para teste e como exemplo)
set.seed(7)

modelo_treino_rf <- train(cidade_new ~ area + comodos + banheiros + vagas + andares_new + animais_new + mobilia_new + condominio +
                   aluguel + iptu + seguro, data = treino, method = "rf")

# modelo_treino_svm <- train(cidade_new ~ area + comodos + banheiros + vagas + andares_new + animais_new + mobilia_new + condominio +
#                      aluguel + iptu + seguro, method = "svmRadial")

# modelo_treino_rna <- train(cidade_new ~ area + comodos + banheiros + vagas + andares_new + animais_new + mobilia_new + condominio +
#                      aluguel + iptu + seguro, data = treino, method = "nnet", trace = FALSE)

# 9 - Usar o modelo treinado, anteriormente, agora com a base de teste para testar a classifica��o
modelo_predito_rf <- predict(modelo_treino_rf, teste)
# modelo_predito_svm <- predict(modelo_treino_svm, teste)
# modelo_predito_rna <- predict(modelo_treino_rna, teste)

# 10 - Matriz de confus�o para visualizar resultado da classifica��o com 
confusionMatrix(modelo_predito_rf, teste$cidade_new)
# confusionMatrix(modelo_predito_svm, teste$cidade)
# confusionMatrix(modelo_predito_rna, teste$cidade)









# 2 -  Trocando andares para n�meros e removendo valores grotescos de andares (Ex: 301)
unique(houses$andares) # retorna tipos �nicos de andares

houses[houses$andares == "-", which(colnames(houses)=="andares")] <- "0"

houses <- houses[houses$andares != "301", ]

houses$andares_new <- as.numeric(houses$andares)


# 3 - Corrigindo cidade S�o Paulo que est� com caractere n�o UTF-8
unique(houses$cidade)

houses[houses$cidade == "São Paulo", which(colnames(houses) == "cidade")] = "S�o Paulo"

# 4 - Trocando cidades para n�meros
houses$cidade_new <- as.numeric(as.factor(houses$cidade))

unique(houses$cidade_new)

# 5 - Removendo IPTU muito caro = "313700"
houses <- houses[houses$iptu != "313700", ]

# 6 - Removendo Condom�nio muito caro = "1117000"
houses <- houses[houses$condominio != "1117000", ]
houses <- houses[houses$condominio != "220000", ]
#houses[houses$condominio == "200000", ]

# 7 - Removendo apartamento com �reas distorcidas em m2
# Exemplo custo �rea 2000m2 em BH: https://www.vivareal.com.br/imovel/predio-comercial-luxemburgo-bairros-belo-horizonte-2000m2-aluguel-RS85000-id-1041286922/
houses <- houses[houses$area != "46335", ]
houses <- houses[houses$area != "24606", ]
houses <- houses[houses$area != "12732", ]
houses <- houses[houses$area != "2000", ]



houses[houses$aluguel == 500, ]


houses[order(-houses$area), ]


summary(houses)


View(houses)









# 5 - Estimando modelo preliminar anterior com CIDADES transformadas em n�meros - R-QUADRADO = 0.9858
model <- lm (aluguel ~ cidade_new + area + comodos + banheiros + vagas + andares + animais + mobilia + 
               condominio + iptu + seguro, data = houses)

summary(model)

# 6 - Trocando andares para n�meros e andar = "-" para zero
str(houses$andares)

houses[houses$andares == "-", ] <- "0"
houses$andares <- as.numeric(houses$andares)


# 7 - Estimando modelo preliminar anterior com ANDARES transformados em n�meros - R-QUADRADO = 0.997
model <- lm (aluguel ~ cidade_new + area + comodos + banheiros + vagas + andares_new + animais + mobilia + 
               condominio + iptu + seguro, data = houses)

summary(model)

# 8 - Trocando todos os atributos de texto com valores num�ricos para numeric (Area, comodos, banheiros, vagas,
# condominio, aluguel, iptu, seguro, somatorio)
houses$area <- as.numeric(houses$area)
houses$comodos <- as.numeric(houses$comodos)
houses$banheiros <- as.numeric(houses$banheiros)
houses$vagas <- as.numeric(houses$vagas)
houses$condominio <- as.numeric(houses$condominio)
houses$aluguel <- as.numeric(houses$aluguel)
houses$iptu <- as.numeric(houses$iptu)
houses$seguro <- as.numeric(houses$seguro)
houses$somatorio_new <- as.numeric(houses$somatorio)

# 9 - Estimando modelo preliminar anterior com campos do passo 8 transformados em numeric  - R-QUADRADO = 0.9935
model <- lm (aluguel ~ cidade_new + area_new + comodos_new + banheiros_new + vagas_new + 
               andares_new + animais + mobilia + condominio_new + iptu_new + seguro_new, data = houses)

summary(model)

### Como encontrar valores duplicados:
# https://www.datanovia.com/en/lessons/identify-and-remove-duplicate-data-in-r/#:~:text=The%20function%20distinct()%20%5Bdplyr,R%20base%20function%20unique()%20.&text=The%20option%20.
install.packages("tidyverse")
library("dbplyr")
# Mostra os valores duplicados
houses[duplicated(houses$cidade), ]
# Mostra somente valores �nicos ou 1 de cada existente
unique(houses$cidade)
unique(houses$animais)

# 10 - Descobri valores 0 no meio dos campos textos (animais) que estava transformando para bin�rio, portanto, preciso remover
# essas linhas para n�o se misturarem nos bin�rios novos.
num_linhas <- nrow(houses)
houses$cod <- 1:num_linhas # N�mero inicial de linhas da base = 10692 linhas
summary(houses) # veja neste comando que campos como banheiros, vagas, comodos, area possuem valor min = 0 (zero)

houses[houses$aluguel == 0 && houses$andares == 0, ]

houses_new <- houses[houses$cidade != "0" && houses$area != 0 && houses$comodos != 0 && houses$banheiros != 0 &&
                     houses$vagas != 0 && houses$condominio != 0 && houses$aluguel != 0 && houses$iptu != 0 &&
                       houses$seguro != 0 && houses$somatorio != 0, ]

houses_new <- houses[houses$andares == 0, ]

houses_new

summary(houses_new)

houses_new[houses_new$andares == "0", ]

# 11 - Transformando Animais e mobilia para bin�rio
houses$animais_bin <- ifelse(houses$animais == "acept", 1, 0)
houses$mobilia_bin <- ifelse(houses$mobilia == "furnished", 1, 0)
