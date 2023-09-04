#======================================================
# Calculo da frequencia dos alelos em cada populaçao
#=======================================================
#
# (C) Copyright 2023, by LGH and Contributors.
#
# 
#-----------------
#  Stargazer 
#-----------------
#
# Original Author: Fernanda Soares
# Contributor(s):  Caique Manóchio 
# Updated by (and date): Caique Manochio 04/09/2023
#
# Dependencies:   Stargazer, R
#
# Command line:	time nohup python3 /home/Stargazer_v1.0.8/stargazer.py genotype -o nome_output -d tipo_de_dado -t nome_do_gene --vcf arquivo_com_variantes
#			  				




library(adegenet)
library(dplyr)
library(readxl)

#importando os arquivos
stargazer_output <- read.delim(choose.files())  #arquivo e output do stargazer
ID_POP <- read_xlsx(choose.files(), sheet = "ID_POP") #id e qual a populaçao cada indivíduo e. esta no arquivo de ancestralidade_k8

#acrescentando coluna com a populaçao no output do stargazer.
stargazer_output["POP"] <- ID_POP$cod
star <- stargazer_output #criando um dataframe de segurança

#Substituindo o * por 0 quando o numero é igual a 9 ou menor  e apagando o * quando e igual ou maior que 10
#funçao gsub(pattern, replacement, strig_que_vc_quer_mudança)
star$hap1_main <- ifelse(as.numeric(gsub("\\*", "", star$hap1_main)) >= 1 & as.numeric(gsub("\\*", "", star$hap1_main)) <= 9,
                         gsub("\\*", "0", star$hap1_main), gsub("\\*", "", star$hap1_main))
star$hap2_main <- ifelse(as.numeric(gsub("\\*", "", star$hap2_main)) >= 1 & as.numeric(gsub("\\*", "", star$hap2_main)) <= 9, 
                         gsub("\\*", "0", star$hap2_main), gsub("\\*", "", star$hap2_main))
#Criar uma nova coluna com os valores do das colunas 3 e 4. separados por espaço
star$geno <- apply(star[,3:4], 1, paste, collapse = ' ')

lista_pop <- c ('LWK', 'ESN', 'YRI', 'MSL', 'GWD', 
                'ACB', 'ASW', 'CLM', 'MXL', 'PUR', 'PEL',
                'TSI', 'IBS', 'GBR', 'CEU', 'FIN',
                'PJL', 'GIH', 'ITU', 'STU', 'BEB',
                'CDX', 'KHV', 'CHS', 'CHB', 'JPT')

#Loop  para filtrar os individuos de um pop e depois calcular a frequencia e salvar em objeto com nome da população
for (i in lista_pop) {
  df <- star %>%
    select(POP, geno) %>% #Seleciona as colunas
    filter(POP == i)#Seleciona as linhas
  
  pop_genind <- df2genind(X = df, sep = ' ', ploidy = 2, ncode = 2)#converte o bando de dados que temos em genind 
  pop_genpop <- genind2genpop(pop_genind)#converte o genind em genpop 
  freq <- makefreq (pop_genpop, quiet = FALSE, missing = NA, truenames = TRUE)#calcula a frequencia da populaçao
  #Cria um objeto que armazena a frequencia e recebe o nome da pop
  nome_df <- paste("freq_", i, sep = "") 
  assign(nome_df, freq)
  
}

objects <- list(freq_ACB, freq_LWK, freq_ESN, 
                freq_YRI, freq_MSL, freq_GWD, 
                freq_ASW, freq_CLM, freq_MXL, 
                freq_PUR, freq_PEL, freq_TSI, 
                freq_IBS, freq_GBR, freq_CEU, 
                freq_FIN, freq_PJL, freq_GIH, 
                freq_ITU, freq_STU, freq_BEB, 
                freq_CDX, freq_KHV, freq_CHS, 
                freq_CHB, freq_JPT)

#Loop para converter os objetos para dataframes
for (i in 1:length(objects)) {
  objects[[i]] <- as.data.frame(objects[[i]])
}

#Cria um vetor com os nomes dos dataframe (que estao a frequenia)
lista_dataframes <- c('freq_LWK','freq_ESN', 'freq_YRI', 'freq_MSL','freq_GWD',
                      'freq_ACB', 'freq_ASW','freq_CLM', 'freq_MXL','freq_PUR', 'freq_PEL', 
                      'freq_TSI', 'freq_IBS', 'freq_GBR', 'freq_CEU', 'freq_FIN',
                      'freq_PJL', 'freq_GIH', 'freq_ITU', 'freq_STU', 'freq_BEB',
                      'freq_CDX', 'freq_KHV', 'freq_CHS', 'freq_CHB', 'freq_JPT')

#Cria um dataframe pra armazenar os resultados
df_final <- data.frame(teste = character(length(lista_dataframes)), 
                       geno.01 = numeric(length(lista_dataframes)))
#Preeche o o dataframe com os nomes das frequencia
df_final$nome_df <- lista_dataframes

geno_columns <- c("geno.01","geno.02", "geno.03",
                  'geno.05', 'geno.06', 'geno.07', 
                  "geno.08", "geno.09", "geno.11",
                  'geno.12', 'geno.13', 'geno.14', 
                  'geno.16', 'geno.29', 'geno.31', 
                  'geno.33', 'geno.36', 'geno.44',
                  'geno.45')


#Loop para percorer cada dataframe olhar as colunas e salvar a frequencia em uma tabela só.
#Primeiro Loop: Percorre dataframes que estao a listadataframe
#Segundo Loop: Percorre as colunas,(colunas estão na geno_columns),  dentro do dataframe. 
for (i in 1:length(lista_dataframes)) {
  for (col in geno_columns) {
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


