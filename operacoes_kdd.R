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

# 0 - Personal Environment set (Diretório padrão pessoal para salvar outputs do R)
getwd()
setwd("C:/Users/wesle/OneDrive - ufpr.br/IAA006 - Arquitetura de Dados/Dataset/iaa006-brazil-apartment-renting-regression-model")


# 1 - Dataset loading
houses <- read.csv("houses_to_rent_v2.csv")

View(houses)

colnames(houses)

summary(houses)


# 2 - Data Partition/Selection
library("caret")

index <- createDataPartition(houses$total..R.., p = 0.80, list = FALSE)
train_partition <- houses[index, ]
test_partition <- houses[-index, ]


# 3 - Model Trainning
statistics_model <- lm(houses$total..R.. ~ area + rooms + bathroom + parking.spaces + floor
                       + animal_new + furniture_new + hoa..R.. + rent.amount..R.. + property.tax..R.. 
                       + fire.insurance..R.. + total..R.., data = train_partition)
# rf <- train(VOL~., data = treino, method = "rf")
# svm <- train(VOL~., data = treino, method = "svmRadial")
# rna <- train(VOL~., data = treino, method = "nnet", trace = FALSE)


# 3 - Data adjusts
set.seed(7)

row_numbers <- nrow(houses)
houses$cod <- 1:row_numbers

houses$furniture_new <- ifelse(houses$furniture == "furnished", 1, 0)
houses$animal_new <- ifelse(houses$animal == "acept", 1, 0)

houses


