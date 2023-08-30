library(readxl)

media_k8 <- read_xlsx('ancestralidade_k8.xlsx', 
                sheet = 'MEDIA_POP')                      
Freq <- read_xlsx('tabela_frequencia_cyp2c9.xlsx', 
                  sheet = 'FREQUENCIA_POR_POPULACAO')


alelos <- c("geno.01","geno.02", "geno.03",
            'geno.05', 'geno.06', 'geno.07', 
            "geno.08", "geno.09", "geno.11",
            'geno.12', 'geno.13', 'geno.14', 
            'geno.16', 'geno.29', 'geno.31', 
            'geno.33', 'geno.36', 'geno.44',
            'geno.45')

colunas <- c("EAS", "EUR", "AFR", 
             "NAT", "EUR2", "AFR2", 
             "EAS2", "SAS")

resultado_df <- data.frame(
  Frequencia = character(),
  Coluna = character(),
  Valor_p = numeric(),
  Valor_r = numeric(),
  stringsAsFactors = FALSE
)


# Loop para iterar sobre as frequências
for (frequencia in alelos) {
  Freq_column <- as.numeric(Freq[[frequencia]])
  
  # Loop para iterar sobre as colunas
  for (coluna in colunas) {
    media_k8_column <- as.numeric(media_k8[[coluna]])
    
    # Realiza o teste de correlação para Freq[[frequencia]] e media_k8[[coluna]]
    cor_test_2 <- cor.test(Freq_column, media_k8_column, method = "pearson")
    
   
    resultado_df <- rbind(resultado_df, data.frame(
      Frequencia = frequencia,
      Coluna = coluna,
      Valor_p = cor_test_2$p.value,
      Valor_r = cor_test_2$estimate
    ))
  }
}
   
library(clipr)
write_clip(resultado_df, dec = ",")








