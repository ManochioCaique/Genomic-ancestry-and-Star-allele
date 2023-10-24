#======================================================
# Calculo de frequencia fenotipica para cada população 
#=======================================================
#
# (C) Copyright 2023, by GP-PGx-UFTM and Contributors.
#
# 
#-----------------
#  Stargazer
#-----------------
#
# Original Author: Caique Manochio
# Contributor(s):  
# Updated by (and date): Caique Manochio 24/10/2023
#
# Dependencies: R
#
# Command line:	time nohup python3 /home/Stargazer_v1.0.8/stargazer.py genotype -o nome_output -d tipo_de_dado -t nome_do_gene --vcf arquivo_com_variantes

library(adegenet)
library(dplyr)
library(readxl)


stargazer_output <- read.delim(choose.files())  #arquivo e output do stargazer
ID_POP <- read_xlsx(choose.files(), sheet = "ID_POP") #id e qual a populaçao cada indivíduo e. esta no arquivo de ancestralidade_k8

stargazer_output["POP"] <- ID_POP$cod
star <- stargazer_output[ , c(1, 9, 10, 24)]


#Calcular a frequencia dos dos fenotipos (phenotype)
lista_pop <- c ('LWK', 'ESN', 'YRI',  'GWD', 
                'ACB', 'ASW', 'CLM',  'PUR', 
                'TSI', 'IBS', 'GBR', 'CEU', 'FIN',
                'PJL', 'GIH', 'ITU', 'STU', 'BEB',
                'CDX', 'KHV', 'CHS', 'CHB')
#Populações com 100% de metabolizador normal
#'MSL', 'MXL', 'PEL', 'JPT'


for (i in lista_pop) {
  df <- star %>%
    select(POP, phenotype) %>% #Seleciona as colunas
    filter(POP == i)#Seleciona as linhas
  print(i)
  pop_genind <- df2genind(X = df, sep = ' ', ploidy = 2, ncode = 2) #converte o bando de dados que temos em genind 
  pop_genpop <- genind2genpop(pop_genind) #converte o genind em genpop 
  freq <- makefreq (pop_genpop, quiet = FALSE, missing = NA, truenames = TRUE) #calcula a frequencia da populaçao
  #Cria um objeto que armazena a frequencia e recebe o nome da pop
  nome_df <- paste("freq_", i, sep = "") 
  assign(nome_df, freq)
  print(nome_df)
}


# Defina a lista de nomes de matrizes
lista_dataframes <- c('freq_LWK', 'freq_ESN', 'freq_YRI',  'freq_GWD',
                      'freq_ACB', 'freq_ASW', 'freq_CLM',  'freq_PUR', 
                      'freq_TSI', 'freq_IBS', 'freq_GBR', 'freq_CEU', 'freq_FIN',
                      'freq_PJL', 'freq_GIH', 'freq_ITU', 'freq_STU', 'freq_BEB',
                      'freq_CDX', 'freq_KHV', 'freq_CHS', 'freq_CHB' )


#CONSEGUI GRAÇAS AO SENHOR 
for (nome_matriz in lista_dataframes) {
  assign(nome_matriz, as.data.frame(get(nome_matriz)))
}



df_final <- data.frame(teste = character(length(lista_dataframes)), geno.01 = numeric(length(lista_dataframes)))
#Preeche o o dataframe com os nomes das frequencia
df_final$nome_df <- lista_dataframes


geno_columns <- c("phenotype.intermediate_metabolizer",
                  "phenotype.normal_metabolizer",
                  "phenotype.poor_metabolizer",
                  "phenotype.rapid_metabolizer", 
                  "phenotype.ultrarapid_metabolizer", 
                  "phenotype.unknown")



#Loop para percorer cada dataframe olhar as colunas e salvar a frequencia em uma tabela só.
#Primeiro Loop: Percorre dataframes que estao a listadataframe
#Segundo Loop: Percorre as colunas,(colunas estão na geno_columns),  dentro do dataframe. 
for (i in 1:length(lista_dataframes)) {
  print(i)
  for (col in geno_columns) {
    print(col)
    if (col %in% colnames(get(lista_dataframes[i]))) {  #If usado pra verificar se a dataframe possui aquela coluna.
      df_final[[col]][i] <- get(lista_dataframes[i])[[col]]#Coluna existe se atribui o valor dessa coluna no df_final
    } else { #Se não tiver a coluna atribui o valor de 0 
      df_final[[col]][i] <- 0
    }
  }
}



#Calcular o  frequencia dos score de atividade (dip_score)

for (i in lista_pop) {
  df <- star %>%
    select(POP, dip_score) %>% 
    filter(POP == i)
  print(i)
  pop_genind <- df2genind(X = df, sep = ' ', ploidy = 2, ncode = 2) 
  pop_genpop <- genind2genpop(pop_genind)  
  freq <- makefreq (pop_genpop, quiet = FALSE, missing = NA, truenames = TRUE) 
  
  nome_df <- paste("freq_", i, sep = "") 
  assign(nome_df, freq)
  print(nome_df)
}


for (nome_matriz in lista_dataframes) {
  assign(nome_matriz, as.data.frame(get(nome_matriz)))
}


geno_columns2 <- c("dip_score.0_0",
                   "dip_score.0_5",
                   "dip_score.1_0",
                   "dip_score.1_5", 
                   "dip_score.2_0", 
                   "dip_score.2_5",
                   "dip_score.3_0")


for (i in 1:length(lista_dataframes)) {
  print(i)
  for (col in geno_columns2) {
    print(col)
    if (col %in% colnames(get(lista_dataframes[i]))) {  #If usado pra verificar se a dataframe possui aquela coluna.
      df_final[[col]][i] <- get(lista_dataframes[i])[[col]]#Coluna existe se atribui o valor dessa coluna no df_final
    } else { #Se não tiver a coluna atribui o valor de 0 
      df_final[[col]][i] <- 0
    }
  }
}
#Copiar o df_final para uma planilha do excel. 
library(clipr)
write_clip(df_final)