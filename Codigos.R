library(readxl)
library(tidyverse)
theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5)
      ,
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  return(
    list(
      theme,
      scale_fill_manual(values = estat_colors),
      scale_colour_manual(values = estat_colors)
    )
  )
}
#########################################
estat_colors <- c(
  "#A11D21", "#003366", "#CC9900",
  "#663333", "#FF6600", "#CC9966",
  "#999966", "#006606", "#008091",
  "#041835", "#666666" )
#tratamento do banco##################################
P1<-read_excel("C:/Users/marqu/Documents/Estat/Olimpiadas 2000 - 2016.xlsx", sheet=1, col_names=TRUE)
colnames(P1)<-c("Nome","Sexo","Idade","Altura(m)","Peso(kg)","País","Esporte","Modalidade","Medalha")
P2<-read_excel("C:/Users/marqu/Documents/Estat/Olimpiadas 2000 - 2016.xlsx", sheet=2, col_names=TRUE)
colnames(P2)<-c("Nome","Sexo","Idade","Altura(m)","Peso(kg)","País","Esporte","Modalidade","Medalha")
P3<-read_excel("C:/Users/marqu/Documents/Estat/Olimpiadas 2000 - 2016.xlsx", sheet=3, col_names=TRUE)
colnames(P3)<-c("Nome","Sexo","Idade","Altura(m)","Peso(kg)","País","Esporte","Modalidade","Medalha")
P4<-read_excel("C:/Users/marqu/Documents/Estat/Olimpiadas 2000 - 2016.xlsx", sheet=4, col_names=TRUE)
colnames(P4)<-c("Nome","Sexo","Idade","Altura(m)","Peso(kg)","País","Esporte","Modalidade","Medalha")
P5<-read_excel("C:/Users/marqu/Documents/Estat/Olimpiadas 2000 - 2016.xlsx", sheet=5, col_names=TRUE)
colnames(P5)<-c("Nome","Sexo","Idade","Altura(m)","Peso(kg)","País","Esporte","Modalidade","Medalha")
P<-rbind(P1,P2,P3,P4,P5)
P$`Altura(m)`<-P$`Altura(m)`/100
P$`Peso(kg)`<-P$`Peso(kg)`/2.205
#Analise 1########################################################
PF<-P[P[,2]=="F",]
PF2<-PF
PF<-PF[!(is.na(PF[,9])),]
PF<-PF[!duplicated(PF$Nome),]
PF2<-PF2[!duplicated(PF2$Nome),]
sd(classes$n)
length(classes$n)
Psd<-PF2  %>% count(País,Medalha)
Psd<-Psd[!(is.na(Psd[,2])),]
sum(Psd$n)
classes <- PF %>%
  filter(!is.na(País)) %>%
  count(País) %>%
  mutate(
    freq = n,
    relative_freq = round((freq / sum(freq)) * 100, 1),
    freq = gsub("\\.", ",", relative_freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )
c<-classes %>% filter(relative_freq>5)
c2<-classes %>% filter(n>5)
ggplot(c)+aes(x = fct_reorder(País, n, .desc=T), y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7)+geom_text(position = position_dodge(width = .9),vjust = -0.5,size = 3)+theme_estat()+labs(x="Países",y="Frequência")
#####################################################################################
length(unique(P$Nome))
Pd<-P[!duplicated(P$Nome),]
length(Pd$Sexo=="F")
###########Análise 2##############################
PM<-P[!(is.na(P[,9])),]
PMD<-PM[!duplicated(PM$Nome),]
PMD$IMC<-PMD$`Peso(kg)`/(PMD$`Altura(m)`)^2
PA2<-PMD[PMD[,7]==c("Badminton","Judo","Gymnastics","Athletics"),]
ggplot(PA2)+aes(x=reorder(Esporte, IMC, "median"),y=IMC)+geom_boxplot(fill = c("#A11D21"), width = 0.5)+theme_estat()+stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white")+xlab("Esporte")
round(mean(PA2[PA2[,7]=="Judo",]$IMC,na.rm = T),digits = 2)
ggplot(PA2)+aes(x=Esporte,y=IMC)+geom_boxplot(fill = c("#A11D21"), width = 0.5)+theme_estat()+stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white")+xlab("Esporte")

