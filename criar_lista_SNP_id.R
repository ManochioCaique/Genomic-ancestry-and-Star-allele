#Script para criar um lista de com os SNP ID (rs).



dir()
LD <- read.table("snp_LD_filtrado_08.ld") #Importar arquivo .ld (formato que sai do plink depois do de fazer o equilibrio de ligação)
names(LD)[1:7] <- c('chr', 'loc', 'name', 'chr2', 'loc2', 'name2', 'squared') #Mudando os nomes das colunas.
library(dplyr)



snp1 <- data.frame( LD$chr, LD$loc, LD$name) #Separando a primeira coluna em um dataframe.
names(snp1)[1:3] <- c('chr', 'loc', 'name') #Mudando o nome das colunas

snp2 <- data.frame( LD$chr2, LD$loc2, LD$name2)#Separando a segundo coluna em outro dataframe
names(snp2)[1:3] <- c('chr1', 'loc2', 'name') 

snp3 <- snp1 %>% inner_join(snp2, by = 'name') #Juntando os dois dataframe, usando a coluna  que tem os ID dos SNPs 
snp3 <- snp3[, c(-1, -2, -5)] #exlcluindo as  colunas que não vamos usar. 

snp_fil <- unique(snp3)# Excluidos os ID dos SNPs que estão duplicados. 
snp4 <- snp_fil[ , -2] #exlcluindo as  colunas que não vamos usar.

write.table(snp4, file = "lista_snp.txt", sep = "\t",   row.names = FALSE,col.names = FALSE,  quote = TRUE) #Salvando a lista em formato txt, sem nome do linhas e colunas com aspas


library(clipr)
write_clip(snp3)#função usada para copiar o arquivo e colar no excel. 
