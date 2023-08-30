#Script para filtrar cod de cada amostra do 1KGP

library(dplyr)
library(readxl)

dir()

ID <- read.table("snp_ancestralidade4.fam") #Importanto o arquivo com a ordem e identificaçao de cada amostra 
ID <- ID[ , c(-2,-3,-4,-5,-6)]

KGP <- read_xlsx("ancestralidade_k8.xlsx", sheet = "1KGP", col_names = TRUE) #importanto informaçoes dos dados do 1KGP 

names(KGP)[1:9] <- c('id', 'sex', 'biosample', 'cod', 'name', 
                     'supercod', 'supername', 'popid', 'data') #renomeando as colunas
ID <- as.numeric(ID)


df <- data.frame(id = character(),
                 cod = character()) #Criando um dataframe novo e vazio para armazenar os dados do for 

#For para verificar as amostra que tinha no arquivo e no arquivo 1KGP
for (i in ID$V1) {
  a <- KGP %>%
    select(id, cod) %>%
    filter(id == i)
  
  novalinha <- a
  df <- rbind(df, novalinha)
}

library(clipr)
write_clip(df)
