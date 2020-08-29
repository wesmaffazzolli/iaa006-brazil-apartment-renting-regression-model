# install.packages("tidyverse")
library("dbplyr")

# 0 - Personal Environment set (Diretório padrão pessoal para salvar outputs do R)
getwd()
setwd("C:/Users/wesle/OneDrive - ufpr.br/IAA006 - Arquitetura de Dados/Dataset/iaa006-brazil-apartment-renting-regression-model")

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
set.seed(7)
houses$cod <- 1:nrow(houses)

summary(houses)

# 2 -  Trocando andares para números e removendo valores grotescos de andares (Ex: 301)
unique(houses$andares) # retorna tipos únicos de andares

houses[houses$andares == "-", which(colnames(houses)=="andares")] <- "0"

houses$andares_new <- as.numeric(houses$andares)

houses <- houses[houses$andares != 301, ]

# 3 - Corrigindo cidade São Paulo que está com 
unique(houses$cidade)

houses_old <- houses


# 4 - Trocando cidades para números
houses$cidade_new <- as.numeric(as.factor(houses$cidade))

# 5 - Estimando modelo preliminar anterior com CIDADES transformadas em números - R-QUADRADO = 0.9858
model <- lm (aluguel ~ cidade_new + area + comodos + banheiros + vagas + andares + animais + mobilia + 
               condominio + iptu + seguro, data = houses)

summary(model)

# 6 - Trocando andares para números e andar = "-" para zero
str(houses$andares)

houses[houses$andares == "-", ] <- "0"
houses$andares <- as.numeric(houses$andares)


# 7 - Estimando modelo preliminar anterior com ANDARES transformados em números - R-QUADRADO = 0.997
model <- lm (aluguel ~ cidade_new + area + comodos + banheiros + vagas + andares_new + animais + mobilia + 
               condominio + iptu + seguro, data = houses)

summary(model)

# 8 - Trocando todos os atributos de texto com valores numéricos para numeric (Area, comodos, banheiros, vagas,
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
# Mostra somente valores únicos ou 1 de cada existente
unique(houses$cidade)
unique(houses$animais)

# 10 - Descobri valores 0 no meio dos campos textos (animais) que estava transformando para binário, portanto, preciso remover
# essas linhas para não se misturarem nos binários novos.
num_linhas <- nrow(houses)
houses$cod <- 1:num_linhas # Número inicial de linhas da base = 10692 linhas
summary(houses) # veja neste comando que campos como banheiros, vagas, comodos, area possuem valor min = 0 (zero)

houses[houses$aluguel == 0 && houses$andares == 0, ]

houses_new <- houses[houses$cidade != "0" && houses$area != 0 && houses$comodos != 0 && houses$banheiros != 0 &&
                     houses$vagas != 0 && houses$condominio != 0 && houses$aluguel != 0 && houses$iptu != 0 &&
                       houses$seguro != 0 && houses$somatorio != 0, ]

houses_new <- houses[houses$andares == 0, ]

houses_new

summary(houses_new)

houses_new[houses_new$andares == "0", ]

# 11 - Transformando Animais e mobilia para binário
houses$animais_bin <- ifelse(houses$animais == "acept", 1, 0)
houses$mobilia_bin <- ifelse(houses$mobilia == "furnished", 1, 0)
