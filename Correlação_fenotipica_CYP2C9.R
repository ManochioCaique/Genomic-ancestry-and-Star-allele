#======================================================
# Calculo de correlação entre frequencia do fenótipo e media de ancestralidade 
#=======================================================
#
# (C) Copyright 2023, by GP-PGx-UFTM and Contributors.
#
# 
#-----------------
#  
#-----------------
#
# Original Author: Caique Manochio
# Contributor(s):  
# Updated by (and date): Caique Manochio 24/10/2023
#
# Dependencies: R
#
# Command line:	

library(dplyr)
library(readxl)

FREQ_PHENO_CYP2C9 <- read_xlsx(choose.files(), sheet = "FREQUENCIA_FENOTI")

media_k8 <- read_xlsx(choose.files(), 
                      sheet = 'MEDIA_POP')

phenotype <- c("phenotype.intermediate_metabolizer",
               "phenotype.normal_metabolizer",
               "phenotype.poor_metabolizer",
               "phenotype.unknown")

colunas <- c("EAS", "EUR", "AFR", 
             "NAT", "EUR2", "AFR2", 
             "EAS2", "SAS")

# Vetores para armazenar os resultados

resultado_df_cyp2c9 <- data.frame(
  Frequencia = character(),
  Coluna = character(),
  Valor_p = numeric(),
  Valor_r = numeric(),
  stringsAsFactors = FALSE
)

# Loop para  colocar frequencia em numerica 
for (frequencia in phenotype) {
  Freq_column <- as.numeric(FREQ_PHENO_CYP2C9[[frequencia]])
  
  
  for (coluna in colunas) {
    media_k8_column <- as.numeric(media_k8[[coluna]])
    
    # Realiza o teste de correlação para Freq[[frequencia]] e media_k8[[coluna]]
    cor_test_2 <- cor.test(Freq_column, media_k8_column, method = "pearson")
    
    #Armazena os resultados com o nome da coluna correspondente
    
    
    resultado_df_cyp2c9 <- rbind(resultado_df_cyp2c9, data.frame(
      Frequencia = frequencia,
      Coluna = coluna,
      Valor_p = cor_test_2$p.value,
      Valor_r = cor_test_2$estimate ))
  }
}

write_clip(resultado_df_cyp2c9, dec = ",")

#Teste correlação com o score de atividade.
FREQ_SCORE_CYP2C9 <- read_xlsx(choose.files(), sheet = "FREQUENCIA_SCORE")

score <- c("dip_score.0_0",
           "dip_score.0_5",
           "dip_score.1_0",
           "dip_score.1_5", 
           "dip_score.2_0")

colunas <- c("EAS", "EUR", "AFR", 
             "NAT", "EUR2", "AFR2", 
             "EAS2", "SAS")



resultado_df_score_C9 <- data.frame(
  Frequencia = character(),
  Coluna = character(),
  Valor_p = numeric(),
  Valor_r = numeric(),
  stringsAsFactors = FALSE
)

# Loop para  colocar frequencia em numerica 
for (frequencia in score) {
  Freq_column <- as.numeric(FREQ_SCORE_CYP2C9[[frequencia]])
  
  
  for (coluna in colunas) {
    media_k8_column <- as.numeric(media_k8[[coluna]])
    
    # Realiza o teste de correlação para Freq[[frequencia]] e media_k8[[coluna]]
    cor_test_2 <- cor.test(Freq_column, media_k8_column, method = "pearson")
    
    #Armazena os resultados com o nome da coluna correspondente
    resultado_df_score_C9 <- rbind(resultado_df_score_C9, data.frame(
      Frequencia = frequencia,
      Coluna = coluna,
      Valor_p = cor_test_2$p.value,
      Valor_r = cor_test_2$estimate ))
  }
}
library(clipr)
write_clip(resultado_df_score_C9, dec = ",")
