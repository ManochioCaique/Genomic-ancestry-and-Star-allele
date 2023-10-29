library(dplyr)
library(readxl)
library(ggplot2)
library(gridExtra)

#Importando o o banco de dados
#CYP2C19
cyp2c19 <- read_xlsx(file.choose(), sheet = "INPUT_PARA_GRAFICO") 

#Vetor com a paleta de cor dos pontos no gráficos
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

#Criação dos graficos indivíduais para o alelo *2
NAT_2_cyp2c19 <- ggplot() +
  geom_point(data = cyp2c19, aes(x=as.numeric(NAT), y=geno.02, color = POP), 
             shape = 19, size = 5) +
  labs(y = "Frequência do *2",
       x = "Ancestralidade Nativo Americana",
       color = "População", 
       title = "Ancestralidade Nativo Americana x Frequência do *2")+
  theme_classic() +
  theme(axis.title.y = element_text(size = 8,
                                    margin = margin(r = 15)),
        axis.title.x = element_text(size = 8, 
                                    margin = margin(t = 15)),
        legend.title = element_text(face = "bold", size = 8),
        title = element_text(size = 8, 
                             face = "bold",
                             margin = margin(b = 15)),
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = paleta_pop,
                     breaks = pop) + 
  geom_text(aes(x = 0.7, y = 0.35), 
            label = 'R = -0,459  \n p-value = 0,018') +
  scale_y_continuous(limits = c(0,max(cyp2c19$geno.02)))





EASE_2_cyp2c19 <- ggplot() +
  geom_point(data = cyp2c19, aes(x=as.numeric(EAS2), y=geno.02, color = POP), 
             shape = 19, size = 5) +
  labs(y = "Frequência do *2",
       x = "Ancestralidade Leste Asiática - Leste",
       color = "População", 
       title = "Ancestralidade Leste Asiática - Leste x Frequência do *2")+
  theme_classic() +
  theme(axis.title.y = element_text(size = 8,
                                    margin = margin(r = 15)),
        axis.title.x = element_text(size = 8, 
                                    margin = margin(t = 15)),
        legend.title = element_text(face = "bold", size = 8),
        title = element_text(size = 8, 
                             face = "bold",
                             margin = margin(b = 15)),
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = paleta_pop,
                     breaks = pop) + 
  geom_text(aes(x = 0.8, y = 0.2), 
            label = 'R = 0,405  \n p-value = 0,040') +
  scale_y_continuous(limits = c(0,max(cyp2c19$geno.02)+0.02))



SEUR_2_cyp2c19 <- ggplot() +
  geom_point(data = cyp2c19, aes(x=as.numeric(EUR2), y=geno.02, color = POP), 
             shape = 19, size = 5) +
  labs(y = "Frequência do *2",
       x = "Ancestralidade Sul Europeia",
       color = "População", 
       title = "Ancestralidade Sul Europeia x Frequência do *2")+
  theme_classic() +
  theme(axis.title.y = element_text(size = 8,
                                    margin = margin(r = 15)),
        axis.title.x = element_text(size = 8, 
                                    margin = margin(t = 15)),
        legend.title = element_text(face = "bold", size = 8),
        title = element_text(size = 8, 
                             face = "bold",
                             margin = margin(b = 15)),
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = paleta_pop,
                     breaks = pop) + 
  geom_text(aes(x = 0.75, y = 0.35), 
            label = 'R = -0,574  \n p-value = 0,002') 
  
SAS_2_cyp2c19 <- ggplot() +
  geom_point(data = cyp2c19, aes(x=as.numeric(SAS), y=geno.02, color = POP), 
             shape = 19, size = 5) +
  labs(y = "Frequência do *2",
       x = "Ancestralidade Sul Asiática",
       color = "População", 
       title = "Ancestralidade Sul Asiática x Frequência do *2")+
  theme_classic() +
  theme(axis.title.y = element_text(size = 8,
                                    margin = margin(r = 15)),
        axis.title.x = element_text(size = 8, 
                                    margin = margin(t = 15)),
        legend.title = element_text(face = "bold", size = 8),
        title = element_text(size = 8, 
                             face = "bold",
                             margin = margin(b = 15)),
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = paleta_pop,
                     breaks = pop) + 
  geom_text(aes(x = 0.75, y = 0.2), 
            label = 'R = 0,677  \n p-value = 0,0001') 

#Combinando o gráficos em um figura só
plot_combinado <- grid.arrange(NAT_2_cyp2c19 + theme(legend.position = "none"), EASE_2_cyp2c19 + theme(legend.position = "none"),
             SEUR_2_cyp2c19 + theme(legend.position = "none"), SAS_2_cyp2c19+ theme(legend.position = "none"))
#Adicionando a legenda
legenda <- ggplot() +
  geom_point(data = cyp2c19, aes(x=as.numeric(SAS), y=geno.03, color = POP), 
             shape = 19, size = 5) +
  scale_color_manual(values =  paleta_pop,
                     breaks = pop) +
  theme_void() +
  theme(legend.position = "bottom")

plot_final <- grid.arrange(plot_combinado, legenda, ncol = 1, heights = c(4, 1))


#Criação dos graficos indivíduais para o alelo *3

EASW_3_cyp2c19 <- ggplot() +
  geom_point(data = cyp2c19, aes(x=as.numeric(EAS), y=geno.03, color = POP), 
             shape = 19, size = 5) +
  labs(y = "Frequência do *3",
       x = "Ancestralidade Leste Asiática - Oeste",
       color = "População", 
       title = "Ancestralidade Leste Asiática - Oeste x Frequência do *3")+
  theme_classic() +
  theme(axis.title.y = element_text(size = 8,
                                    margin = margin(r = 15)),
        axis.title.x = element_text(size = 8, 
                                    margin = margin(t = 15)),
        legend.title = element_text(face = "bold", size = 8),
        title = element_text(size = 8, 
                             face = "bold",
                             margin = margin(b = 15)),
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = paleta_pop,
                     breaks = pop) + 
  geom_text(aes(x = 0.75, y = 0.02), 
            label = 'R = 0,749  \n p-value = <0,001') + 
  scale_y_continuous(limits = c(0,max(cyp2c19$geno.03)))
  
 


EASE_3_cyp2c19 <- ggplot() +
  geom_point(data = cyp2c19, aes(x=as.numeric(EAS2), y=geno.03, color = POP), 
             shape = 19, size = 5) +
  labs(y = "Frequência do *3",
       x = "Ancestralidade Leste Asiática - Leste",
       color = "População", 
       title = "Ancestralidade Leste Asiática - Leste x Frequência do *3")+
  theme_classic() +
  theme(axis.title.y = element_text(size = 8,
                                    margin = margin(r = 15)),
        axis.title.x = element_text(size = 8, 
                                    margin = margin(t = 15)),
        legend.title = element_text(face = "bold", size = 8),
        title = element_text(size = 8, 
                             face = "bold",
                             margin = margin(b = 15)),
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = paleta_pop,
                     breaks = pop) + 
  geom_text(aes(x = 0.75, y = 0.02), 
            label = 'R = 0,709  \n p-value = <0,001') +
  scale_y_continuous(limits = c(0,max(cyp2c19$geno.03)))

 
SEUR_3_cyp2c19 <- ggplot() +
  geom_point(data = cyp2c19, aes(x=as.numeric(EUR2), y=geno.03, color = POP), 
             shape = 19, size = 5) +
  labs(y = "Frequência do *3",
       x = "Ancestralidade Sul Europeia",
       color = "População", 
       title = "Ancestralidade Sul Europeia x Frequência do *3")+
  theme_classic() +
  theme(axis.title.y = element_text(size = 8,
                                    margin = margin(r = 15)),
        axis.title.x = element_text(size = 8, 
                                    margin = margin(t = 15)),
        legend.title = element_text(face = "bold", size = 8),
        title = element_text(size = 8, 
                             face = "bold",
                             margin = margin(b = 15)),
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = paleta_pop,
                     breaks = pop) + 
  geom_text(aes(x = 0.75, y = 0.06), 
            label = 'R = -0,409  \n p-value = 0,037') +
  scale_y_continuous(limits = c(0,max(cyp2c19$geno.03)))


plot_combinado <- grid.arrange(EASW_3_cyp2c19 + theme(legend.position = "none"), EASE_3_cyp2c19 + theme(legend.position = "none"),
                               SEUR_3_cyp2c19 + theme(legend.position = "none"))





##Criação dos graficos indivíduais para o alelo *17

EASW_17_cyp2c19 <- ggplot() +
  geom_point(data = cyp2c19, aes(x=as.numeric(EAS), y=geno.17, color = POP), 
             shape = 19, size = 5) +
  labs(y = "Frequência do *17",
       x = "Ancestralidade Leste Asiática - Oeste",
       color = "População", 
       title = "Ancestralidade Leste Asiática - Oeste x Frequência do *17")+
  theme_classic() +
  theme(axis.title.y = element_text(size = 8,
                                    margin = margin(r = 15)),
        axis.title.x = element_text(size = 8, 
                                    margin = margin(t = 15)),
        legend.title = element_text(face = "bold", size = 8),
        title = element_text(size = 8, 
                             face = "bold",
                             margin = margin(b = 15)),
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = paleta_pop,
                     breaks = pop) + 
  geom_text(aes(x = 0.75, y = 0.2), 
            label = 'R = -0,655  \n p-value = <0,001') +
  scale_y_continuous(limits = c(0,max(cyp2c19$geno.17)))


EASE_17_cyp2c19 <- ggplot() +
  geom_point(data = cyp2c19, aes(x=as.numeric(EAS2), y=geno.17, color = POP), 
             shape = 19, size = 5) +
  labs(y = "Frequência do *17",
       x = "Ancestralidade Leste Asiática - Leste",
       color = "População", 
       title = "Ancestralidade Leste Asiática - Leste x Frequência do *17")+
  theme_classic() +
  theme(axis.title.y = element_text(size = 8,
                                    margin = margin(r = 15)),
        axis.title.x = element_text(size = 8, 
                                    margin = margin(t = 15)),
        legend.title = element_text(face = "bold", size = 8),
        title = element_text(size = 8, 
                             face = "bold",
                             margin = margin(b = 15)),
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = paleta_pop,
                     breaks = pop) + 
  geom_text(aes(x = 0.75, y = 0.2), 
            label = 'R = -0,593  \n p-value = 0,001') +
  scale_y_continuous(limits = c(0,max(cyp2c19$geno.17)))


WAFR_17_cyp2c19 <- ggplot() +
  geom_point(data = cyp2c19, aes(x=as.numeric(AFR), y=geno.17, color = POP), 
             shape = 19, size = 5) +
  labs(y = "Frequência do *17",
       x = "Ancestralidade Oeste Africana",
       color = "População", 
       title = "Ancestralidade Oeste Africana x Frequência do *17")+
  theme_classic() +
  theme(axis.title.y = element_text(size = 8,
                                    margin = margin(r = 15)),
        axis.title.x = element_text(size = 8, 
                                    margin = margin(t = 15)),
        legend.title = element_text(face = "bold", size = 8),
        title = element_text(size = 8, 
                             face = "bold",
                             margin = margin(b = 15)),
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = paleta_pop,
                     breaks = pop) + 
  geom_text(aes(x = 0.75, y = 0.1), 
            label = 'R = 0,544  \n p-value = 0,004') +
  scale_y_continuous(limits = c(0,max(cyp2c19$geno.17)))


EAFR_17_cyp2c19 <- ggplot() +
  geom_point(data = cyp2c19, aes(x=as.numeric(AFR2), y=geno.17, color = POP), 
             shape = 19, size = 5) +
  labs(y = "Frequência do *17",
       x = "Ancestralidade Leste Africana",
       color = "População", 
       title = "Ancestralidade Leste Africana x Frequência do *17")+
  theme_classic() +
  theme(axis.title.y = element_text(size = 8,
                                    margin = margin(r = 15)),
        axis.title.x = element_text(size = 8, 
                                    margin = margin(t = 15)),
        legend.title = element_text(face = "bold", size = 8),
        title = element_text(size = 8, 
                             face = "bold",
                             margin = margin(b = 15)),
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = paleta_pop,
                     breaks = pop) + 
  geom_text(aes(x = 0.75, y = 0.1), 
            label = 'R = 419  \n p-value = 0,032') +
  scale_y_continuous(limits = c(0,max(cyp2c19$geno.17)))


plot_combinado <- grid.arrange(EASW_17_cyp2c19 + theme(legend.position = "none"), EASE_17_cyp2c19 + theme(legend.position = "none"),
                               WAFR_17_cyp2c19 + theme(legend.position = "none"), EAFR_17_cyp2c19 + theme(legend.position = "none"))

#CYP2C9
#Alelo 2 do cyp2c9
cyp2c9 <- read_xlsx(file.choose(), sheet = "INPUT_PARA_GRAFICO") 


SEUR_2_cyp2c9 <- ggplot() +
  geom_point(data = cyp2c9, aes(x=as.numeric(EUR2), y=geno.02, color = POP), 
             shape = 19, size = 5) +
  labs(y = "Frequência do *2",
       x = "Ancestralidade Sul Europeia",
       color = "População", 
       title = "Ancestralidade Sul Europeia x Frequência do *2")+
  theme_classic() +
  theme(axis.title.y = element_text(size = 8,
                                    margin = margin(r = 15)),
        axis.title.x = element_text(size = 8, 
                                    margin = margin(t = 15)),
        legend.title = element_text(face = "bold", size = 8),
        title = element_text(size = 8, 
                             face = "bold",
                             margin = margin(b = 15)),
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = paleta_pop,
                     breaks = pop) + 
  geom_text(aes(x = 0.75, y = 0.07), 
            label = 'R = 0,932  \n p-value = <0,001') +
  scale_y_continuous(limits = c(0,max(cyp2c9$geno.02)+0.02))


#Alelo 3

WAFR_3_cyp2c9 <- ggplot() +
  geom_point(data = cyp2c9, aes(x=as.numeric(AFR), y=geno.03, color = POP), 
             shape = 19, size = 5) +
  labs(y = "Frequência do *3",
       x = "Ancestralidade Oeste Africana",
       color = "População", 
       title = "Ancestralidade Oeste Africana x Frequência do *3")+
  theme_classic() +
  theme(axis.title.y = element_text(size = 8,
                                    margin = margin(r = 15)),
        axis.title.x = element_text(size = 8, 
                                    margin = margin(t = 15)),
        legend.title = element_text(face = "bold", size = 8),
        title = element_text(size = 8, 
                             face = "bold",
                             margin = margin(b = 15)),
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = paleta_pop,
                     breaks = pop) + 
  geom_text(aes(x = 0.75, y = 0.11), 
            label = 'R = -0,580  \n p-value = 0,001') +
  scale_y_continuous(limits = c(0,max(cyp2c9$geno.03)+0.02))

EAFR_3_cyp2c9 <- ggplot() +
  geom_point(data = cyp2c9, aes(x=as.numeric(AFR2), y=geno.03, color = POP), 
             shape = 19, size = 5) +
  labs(y = "Frequência do *3",
       x = "Ancestralidade Leste Africana",
       color = "População", 
       title = "Ancestralidade Leste Africana x Frequência do *3")+
  theme_classic() +
  theme(axis.title.y = element_text(size = 8,
                                    margin = margin(r = 15)),
        axis.title.x = element_text(size = 8, 
                                    margin = margin(t = 15)),
        legend.title = element_text(face = "bold", size = 8),
        title = element_text(size = 8, 
                             face = "bold",
                             margin = margin(b = 15)),
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = paleta_pop,
                     breaks = pop) + 
  geom_text(aes(x = 0.75, y = 0.11), 
            label = 'R = -0,573  \n p-value = 0,002') +
  scale_y_continuous(limits = c(0,max(cyp2c9$geno.03)+0.02))

SAS_3_cyp2c9 <- ggplot() +
  geom_point(data = cyp2c9, aes(x=as.numeric(SAS), y=geno.03, color = POP), 
             shape = 19, size = 5) +
  labs(y = "Frequência do Alelo *3",
       x = "Ancestralidade Sul Asiátcia",
       color = "População", 
       title = "Ancestralidade Sul Asiátcia x Frequência do Alelo *3")+
  theme_classic() +
  theme(axis.title.y = element_text(size = 8,
                                    margin = margin(r = 15)),
        axis.title.x = element_text(size = 8, 
                                    margin = margin(t = 15)),
        legend.title = element_text(face = "bold", size = 8),
        title = element_text(size = 8, 
                             face = "bold",
                             margin = margin(b = 15)),
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = paleta_pop,
                     breaks = pop) + 
  geom_text(aes(x = 0.75, y = 0.05), 
            label = 'R = 0,757  \n p-value = <0,001') +
  scale_y_continuous(limits = c(0,max(cyp2c9$geno.03)+0.02))



plot_combinado <- grid.arrange(WAFR_3_cyp2c9 + theme(legend.position = "none"), EAFR_3_cyp2c9 + theme(legend.position = "none"),
                            SAS_3_cyp2c9 + theme(legend.position = "none"), nrow = 3,  ncol = 1)

