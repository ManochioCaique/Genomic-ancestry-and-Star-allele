#======================================================
# Calculo de correlação entre frequencia e media de ancestralidade 
#=======================================================
#
# (C) Copyright 2023, by LGH and Contributors.
#
# 
#-----------------
#  
#-----------------
#
# Original Author: Caique Manochio
# Contributor(s):  
# Updated by (and date): Caique Manochio 04/09/2023
#
# Dependencies: R
#
# Command line:	




library(readxl)

media_k8 <- read_xlsx('ancestralidade_k8.xlsx', 
                sheet = 'MEDIA_POP')                      
Freq <- read_xlsx('tabela_frequencia_cyp2c19.xlsx', 
                  sheet = 'FREQUENCIA_POR_POPULACAO')


alelos <- c('geno.02', 'geno.03', 'geno.04',
            'geno.08', 'geno.09', 'geno.10',
            'geno.13', 'geno.15', 'geno.16', 
            'geno.17', 'geno.22','geno.24', 
            'geno.30', 'geno.34', 'geno.35')

colunas <- c("EAS", "EUR", "AFR", 
             "NAT", "EUR2", "AFR2", 
             "EAS2", "SAS")

# Vetores para armazenar os resultados
valor_p <- list()
valor_r <- list()

# Loop para  colocar frequencia em numerica 
for (frequencia in alelos) {
  Freq_column <- as.numeric(Freq[[frequencia]])
  
  
  for (coluna in colunas) {
    media_k8_column <- as.numeric(media_k8[[coluna]])
    
    # Realiza o teste de correlação para Freq[[frequencia]] e media_k8[[coluna]]
    cor_test_2 <- cor.test(Freq_column, media_k8_column, method = "pearson")
    
    # Armazena os resultados com o nome da coluna correspondente
    nome_coluna <- paste("Cor", coluna, frequencia, sep = "_")
    valor_p[[nome_coluna]] <- cor_test_2$p.value
    valor_r[[nome_coluna]] <- cor_test_2$estimate
  }
}
   








