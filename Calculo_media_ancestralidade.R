#======================================================
# Calculo da media de ancestralidade por populaçao
#=======================================================
#
# (C) Copyright 2023, by GP-PGx-UFTM and Contributors.
#
# 
#-----------------
#  ADMIXTURE
#-----------------
#
# Original Author: Caique Manochio
# Contributor(s):  
# Updated by (and date): Caique Manochio 04/09/2023
#
# Dependencies:   ADMIXTURE, R
#
# Command line:	admixture arquivo.bed 8 -j5
#			  				

library(readxl)
library(dplyr)



k8 <- read_xlsx('ancestralidade_k8.xlsx', 
                sheet = 'ANCESTRALIDADE_K8') #Importando o arquivo porcentagem individual de ancestralidade

tabela_resultados <- data.frame()
lista_pop <- c ('LWK', 'ESN', 'YRI', 'MSL', 'GWD', 
                'ACB', 'ASW', 'CLM', 'MXL', 'PUR', 'PEL',
                'TSI', 'IBS', 'GBR', 'CEU', 'FIN',
                'PJL', 'GIH', 'ITU', 'STU', 'BEB',
                'CDX', 'KHV', 'CHS', 'CHB', 'JPT') #lista com o nome da populaçoes 
#For para calcular o media de ancestralidade de cada população
for (i in lista_pop) {
  a <- k8 %>%
    select(POP,EUR,AFR,NAT,EAS,EUR2,AFR2,EAS2,SAS) %>%
    filter(POP == i)
  
  a$EUR <- as.numeric(a$EUR)
  EUR <- mean(a$EUR)
  
  a$AFR <- as.numeric(a$AFR)
  AFR <- mean(a$AFR)
  
  a$NAT <- as.numeric(a$NAT)
  NAT <- mean(a$NAT)
  
  a$EAS <- as.numeric(a$EAS)
  EAS <- mean(a$EAS)
  
  a$EUR2 <- as.numeric(a$EUR2)
  EUR2 <- mean(a$EUR2)
  
  a$AFR2 <- as.numeric(a$AFR2)
  AFR2 <- mean(a$AFR2)
  
  a$EAS2 <- as.numeric(a$EAS2)
  EAS2 <- mean(a$EAS2)
  
  a$SAS <- as.numeric(a$SAS)
  SAS <- mean(a$SAS)
  
  linha_resultado <- data.frame(POP = i, EUR = EUR, AFR = AFR, NAT = NAT,
                                EAS = EAS, EUR2 = EUR2, AFR2 = AFR2, EAS2 = EAS2,
                                SAS = SAS, stringsAsFactors = FALSE)
  tabela_resultados <- rbind(tabela_resultados, linha_resultado)
}


df <- data.frame(POP = character(),
                 EUR = numeric(),AFR = numeric(),NAT = numeric(),EAS = numeric(),EUR2 = numeric(),AFR2 = numeric(),EAS2 = numeric(),SAS = numeric(), stringsAsFactors = FALSE)


# Remover a coluna "POP" antes de arredondar os valores
tabela_sem_pop <- select(tabela_resultados, -POP)

# Funçao para arredondar os valores para 6 casas decimais
arredondar_decimal <- function(valor) {
  round(valor, digits = 6)
}

# Aplicar a funçao em todas as colunas numericas
tabela_decimal <- tabela_sem_pop %>%
  mutate(across(everything(), arredondar_decimal))

# Adicionar novamente a coluna "POP" à tabela decimal
tabela_decimal <- cbind(tabela_resultados$POP, tabela_decimal)

#copiando para uma tabela do excel
media_k8 <- tabela_decimal
install.packages("clipr")
library(clipr)
write_clip(media_k8)

# Resultado
print(tabela_decimal)
