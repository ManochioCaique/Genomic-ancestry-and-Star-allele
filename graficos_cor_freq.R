#==========================================
# GRAFICO DE FREQUENCIA POR ANCESTRALIDADE 
#==========================================
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
# Updated by (and date): Caique Manochio 01/09/2023
#
# Dependencies:  R
#


library(ggplot2)
library(readxl)

cyp2c19 <- read_xlsx("tabela_frequencia_cyp2c19.xlsx", sheet = "INPUT_PARA_GRAFICO") 

paleta_pop <- c('#330000', '#660000', '#FB6A4A', '#FF0000', "#A50F15",
                'navy', 'cyan', 'deepskyblue','royalblue1', '#08519C', 'cadetblue1',
                'darkmagenta', 'mediumpurple1', 'mediumpurple4', 'mediumorchid1', 'blueviolet',
                'chartreuse1',  'darkgreen',  'springgreen', '#31A354','olivedrab',
                'chocolate', 'darkgoldenrod2', 'darkorange1', 'orange4', '#A63603')
pop <- c('LWK', 'ESN', 'YRI', 'MSL', 'GWD', 
         'ACB', 'ASW', 'CLM', 'MXL', 'PUR', 'PEL',
         'TSI', 'IBS', 'GBR', 'CEU', 'FIN',
         'PJL', 'GIH', 'ITU', 'STU', 'BEB',
         'CDX', 'KHV', 'CHS', 'CHB', 'JPT')


ggplot() +
  geom_point(data = cyp2c19, aes(x=as.numeric(NAT), y=geno.02, color = POP), #gráfico de pontos, color vai colorir as bolinhas de acordo com a coluna POP
             shape = 19, size = 5) + #shape é o tipo do ponto, size é o tamanho
  labs(y = "Frequência Alelo *2", #modificar legendo do eixo y
       x = "Ancestralidade Nativo America", #modificar legendo do eixo x
       color = "População", #vai modificar legenda da legenda
       title = "Ancestralidade Nativo Americana x Frequência do Alelo *2")+ #Coloca um titulo para o grafico
  theme_classic() + #muda o tema grafico
  theme(axis.title.y = element_text(size = 14, #para fazer mudanças nas legendas e eixo y
                                    margin = margin(r = 15)),#funçao que explica como os elementos que nao sao os dados sao representados
        axis.title.x = element_text(size = 15, 
                                    margin = margin(t = 15)),#para fazer mudanças nas legendas e eixo y
        legend.title = element_text(face = "bold", size = 14), #colaca o legenda em negrito e muda o tamanho da fonte
        title = element_text(size = 16, 
                             face = "bold",
                             margin = margin(b = 15)),
        plot.title = element_text(hjust = 0.5)) +#muda a posiçao do titulo 
   scale_color_manual(values = paleta_pop,
                     breaks = pop) + #troca as cores das pontos nos grafico 
   geom_text(aes(x = 0.2, y = 0.4), 
             label = ' R = - 0,459 \n p-value = 0.018') #adicona um texto na posiçao que escolher do grafico
  