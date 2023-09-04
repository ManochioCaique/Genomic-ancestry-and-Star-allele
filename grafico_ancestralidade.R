#==========================================
# GRAFICO DE ANCESTRALIDADE K=8
#==========================================
#
# (C) Copyright 2012, by LGH and Contributors.
#
# 
#-----------------
#  ADMIXTURE
#-----------------
#
# Original Author: Fernanda Soares
# Contributor(s): Wagner Magalhaes, Gilderlanio Araujo, Mateus Gouveia e  Caique Manochio 
# Updated by (and date): Caique Manochio 01/09/2023
#
# Dependencies:  PLINK, ADMIXTURE, R
#
# Command line:	time nohup plink --bfile arquivo.bed --extract lista_snp.txt --make-bed --threads 10 --out snp_para_acestralidade
#			  				admixture arquivo.bed 8 -j5



library(readxl)

dir()
k8 <- read_xlsx(file.choose(), 
                sheet = 'INPUT_GRAFICO_1KGP',
                col_names = T)
#tornando as colunas como numericas
k8$EUR <-as.numeric(k8$EUR)
k8$AFR <-as.numeric(k8$AFR)
k8$NAT <-as.numeric(k8$NAT)
k8$EAS <-as.numeric(k8$EAS)
k8$EUR2 <-as.numeric(k8$EUR2)
k8$AFR2 <-as.numeric(k8$AFR2)
k8$EAS2 <-as.numeric(k8$EAS2)
k8$SAS <-as.numeric(k8$SAS)

#excluindo as colunas tipo caracter 
tbl1 <- k8[-grep('ID', colnames(k8))]
names(tbl1)

tbl2 <- tbl1[-grep('POP', colnames(tbl1))]
names(tbl2)
nrow <- nrow(tbl2)
nrow
#plotando o gráfico 
barplot(t(as.matrix(tbl2)), #matrix usada para criação do garafico
        col = c("red1", "orange", 'hotpink', "green",  
                "seagreen1", "deepskyblue2", "blue", 
                "purple", 'white'), #cores usadas para colorir as anecestralidade
        space = 0, #controla os espaços entre cada barra
        beside = F, #deixa as colunas do grafico como empilhadas (tem que estar como FALSE para isso)
        border = NA, #cor usada para a borda das barras
        main = "Gráfico de Ancestralidade Individual", #Coloca o titulo 
        cex.lab = 0.6) #tamanho do nome dos exios do grafico
#função para colocar o nomes de cada pop
axis(side=1, at = c (35, 165, 285, 400, 525,
                     645, 745, 835, 940, 1050, 1160,
                     1275, 1405, 1520, 1635, 1760,
                     1870, 1990, 2120, 2235, 2360,
                    2470, 2580, 2700, 2825, 2950), #localização no eixo x que os nomes irão aprecer
     labels = c ('LWK', 'ESN', 'YRI', 'MSL', 'GWD', 
                 'ACB', 'ASW', 'CLM', 'MXL', 'PUR', 'PEL',
                 'TSI', 'IBS', 'GBR', 'CEU', 'FIN',
                 'PJL', 'GIH', 'ITU', 'STU', 'BEB',
                 'CDX', 'KHV', 'CHS', 'CHB', 'JPT'), #quais são nomes
     tick = FALSE, #Para colocar as escalas no eixo 
     cex.axis =1, #grossura do eixos 
     las = 2)
#função para colocar legenda dos componenetes  parentais
legend (3010, 0.9, #localização da legenda no gráfico
        pch = 1, #formato do desenho que usando para criar a legenda ..retangulo, boliha
        legend = c ('AFR', 'AFR2', 'EUR2', 'EUR', 
                               'NAT', 'SAS', 'EAS', 'EAS2' ), #legenda que vamos usar 
        text.font = 2, #tamanho da letra da legenda.
        fill = c("red1", "seagreen1", "purple", "deepskyblue2", 
                "blue", 'hotpink', "orange",  "green", 'white'), #cores usadas no gráfico e que representa a legenda
        bty = "n")# não deixa criar  um caixa envolta da legenda 





