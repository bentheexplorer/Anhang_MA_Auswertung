setwd("/Users/ben/Documents/01 Studium/01 Semester/10. Semester - SoSe 21/Abschlussarbeiten/TeX/Masterarbeit/Auswertung")
library(tidyverse)

#### Prätest#####
df <- read.csv("survey.csv")
df_neu <- select(df,8:34)
df_neu <- drop_na(df_neu)
save(df_neu,file="df_prae_tidy.RData")
names(df_neu) <- c("Pseudonym","Hochschulsemester","N1","N2","N3","N4","V5","V6","N7","N9","V10","V11","V11.1","S01","S02","S03","E04","E05","E06","E07","E08","E09","E10","E11","S12","S13","E14")
df_nutzung <- df_neu %>% select(contains("Pseudonym")|contains("Hochschulsemester")|contains("N")|contains("V"))

df_prae_einstellung <- df_neu %>% select(contains("Pseudonym")|contains("Hochschulsemester")|contains("E")|contains("S"))


df_nutzung <- df_nutzung %>% mutate(
N1 = recode(N1,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
N2 = recode(N2,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
N3 = recode(N3,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
N4 = recode(N4,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
N7 = recode(N7,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
N9 = recode(N9,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
V5 = recode(V5,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
V6 = recode(V6,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
V10 = recode(V10,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
V11 = recode(V11,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
V11.1 = recode(V11.1,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6))

df_prae_einstellung <- df_prae_einstellung %>% mutate(
S01 = recode(S01,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
S02 = recode(S02,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),                                    
S03 = recode(S03,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
E04 = recode(E04,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),                                    
E05 = recode(E05,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
E06 = recode(E06,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
E07 = recode(E07,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
E08 = recode(E08,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
E09 = recode(E09,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
E10 = recode(E10,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
E11 = recode(E11,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
S12 = recode(S12,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
S13 = recode(S13,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
E14 = recode(E14,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6)
                                    )
df_prae_einstellung <- df_prae_einstellung %>% mutate(S12u = 7 - S12)
df_prae_einstellung <- df_prae_einstellung %>% mutate(S13u = 7 - S13)
df_prae_einstellung <- df_prae_einstellung %>% mutate(E14u = 7 - E14)
df_prae_einstellung<- column_to_rownames(df_prae_einstellung, var = "Pseudonym")
df_prae_einstellung %>% select(-Hochschulsemester,-S12,-S13,-E14,-E09) ->df_prae_einstellung
df_prae_einstellung$SMean.prae <- rowMeans(df_prae_einstellung)
df_prae_einstellung$SMean.prae <- as.numeric(df_prae_einstellung$SMean.prae)
df_prae_einstellung<- rownames_to_column(df_prae_einstellung, var = "Pseudonym")
df_prae_einstellung <- df_prae_einstellung[-c(6,11), ]
df_nutzung %>% select(-Hochschulsemester) ->df_nutzung
save(df_neu,file="df_tidy.RData")
save(df_nutzung,file="df_nutzung.RData")
save(df_prae_einstellung,file="df_prae_einstellung.RData")

#### Posttest ####
df_post <- read.csv("survey2.csv")
df_post <- select(df_post,6:39)
names(df_post) <- c("Pseudonym","S01post","S02post","S03post","E04post","E05post","E06post","E07post","E08post","E09post","E10post","E11post","S12post","S13post","E14post","Handh_ESP","Grafik_ESP","Inbetrieb_ESP","Mobil_ESP","Vielseit_ESP","Handh_Vern","Grafik_Vern","Inbetrieb_Vern","Mobil_Vern","Vielseit_Vern","Feedback_Sen","H1","H2","H3","H4","H5","H6","H7","Feedback_Sem")
df_post <- select(df_post,-Feedback_Sen,-Feedback_Sem)
df_post <- na.omit(df_post)



save(df_neu,file="df_post_tidy.RData")
df_post_SenVergl <- select(df_post,16:25)
save(df_post_SenVergl,file="df_post_Sensorvergleich.RData")
df_post_einstellung <- select(df_post,1:15)
df_post_einstellung <- df_post_einstellung %>% mutate(
S01post = recode(S01post,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
S02post = recode(S02post,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),                                    
S03post = recode(S03post,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
E04post = recode(E04post,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),                                    
E05post = recode(E05post,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
E06post = recode(E06post,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
E07post = recode(E07post,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
E08post = recode(E08post,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
E09post = recode(E09post,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
E10post = recode(E10post,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
E11post = recode(E11post,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
S12post = recode(S12post,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
S13post = recode(S13post,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
E14post = recode(E14post,'trifft gar nicht zu' = 1,'trifft kaum zu' = 2,'trifft kaum bis teilweise zu' = 3,'trifft teilweise bis überwiegend zu' = 4,"trifft überwiegend zu" = 5,"trifft völlig zu" = 6),
)
df_post_einstellung <- df_post_einstellung %>% mutate(S12postu = 7 - S12post)
df_post_einstellung <- df_post_einstellung %>% mutate(S13postu = 7 - S13post)
df_post_einstellung <- df_post_einstellung %>% mutate(E14postu = 7 - E14post)
df_post_einstellung <- df_post_einstellung %>% select(-S12post,-S13post,-E14post,-E09post)

df_post_einstellung <- remove_rownames(df_post_einstellung)
df_post_einstellung<- column_to_rownames(df_post_einstellung, var = "Pseudonym")
df_post_einstellung$SMean.post <- rowMeans(df_post_einstellung)
df_post_einstellung<- rownames_to_column(df_post_einstellung, var = "Pseudonym")
df_post_einstellung$SMean.post <- as.numeric(df_post_einstellung$SMean.post)

save(df_post_einstellung,file="df_post_Einstellung.RData")
df_seminar_feedb <- select(df_post,26:32)
save(df_seminar_feedb,file="df_Seminar_Feedback.RData")


#### Vergleich Prä-Post#####
Einst <- left_join(df_prae_einstellung,df_post_einstellung,by="Pseudonym")
Einst <- na.omit(Einst)
Einst <- t(Einst)
Einst <- data.frame(Einst)

Einst <- rownames_to_column(Einst, var = "Variablen")
Einst <- Einst %>% arrange(Variablen, by.group=T)
Einst <- column_to_rownames(Einst, var = "Variablen")

Einst <- t(Einst)
Einst <- data.frame(Einst)

Einst_op <- Einst %>% select(-19)
Einst_p <- Einst %>% select(19)
Einst <- bind_cols(Einst_p,Einst_op)
Einst <- remove_rownames(Einst)
Einst <- column_to_rownames(Einst,var="Pseudonym")
rm(Einst_op,Einst_p)
Einst$SMean.prae <- as.numeric(Einst$SMean.prae)
Einst$SMean.post <- as.numeric(Einst$SMean.post)

Einst$Diff <- Einst$SMean.post-Einst$SMean.prae
#### Reliabilitätsanalyse ####
library(psych)
describe(Einst$SMean.post)

# Prä Test Selbstwirksamkeit #
df_prae_einstellung_means_s <- Einst %>% select(starts_with("S"))
df_prae_einstellung_means_s <- df_prae_einstellung_means_s %>% select(-SMean.prae)
df_prae_einstellung_means_s <- df_prae_einstellung_means_s %>% select(!contains("post"))
df_prae_einstellung_means_s <- df_prae_einstellung_means_s %>% mutate_if(is.character, as.numeric)
df_prae_einstellung_means_s$means <- rowMeans(df_prae_einstellung_means_s)
# Prä Test Einstellung #
df_prae_einstellung_means_e <- Einst %>% select(starts_with("E"))
df_prae_einstellung_means_e <- df_prae_einstellung_means_e %>% select(!contains("post"))
df_prae_einstellung_means_e <- df_prae_einstellung_means_e %>% mutate_if(is.character, as.numeric)
df_prae_einstellung_means_e$means <- rowMeans(df_prae_einstellung_means_e)
# Post Test Selbstwirksamkeit #
df_post_einstellung_means_s <- Einst %>% select(starts_with("S"))
df_post_einstellung_means_s <- df_post_einstellung_means_s %>% select(-SMean.post,-SMean.prae)
df_post_einstellung_means_s <- df_post_einstellung_means_s %>% select(contains("post"))
df_post_einstellung_means_s <- df_post_einstellung_means_s %>% mutate_if(is.character, as.numeric)
df_post_einstellung_means_s$means <- rowMeans(df_post_einstellung_means_s)
# Post Test Einstellung #
df_post_einstellung_means_e <- Einst %>% select(starts_with("E"))
df_post_einstellung_means_e <- df_post_einstellung_means_e %>% select(contains("post"))
df_post_einstellung_means_e <- df_post_einstellung_means_e %>% mutate_if(is.character, as.numeric)
df_post_einstellung_means_e$means <- rowMeans(df_post_einstellung_means_e)

EinstMean <- data.frame(df_prae_einstellung_means_s$means,df_post_einstellung_means_s$means,df_prae_einstellung_means_e$means,df_post_einstellung_means_e$means)
names(EinstMean) <- c("Prae_S","Post_S","Prae_E","Post_E")

df_post_einstellung_rel <- df_post_einstellung %>% select(-Pseudonym,-SMean.post)
Rel_S <- psych::alpha(df_post_einstellung_rel %>% select(starts_with("S")), check.keys =TRUE)
Rel_S
Rel_S$item.stats$r.drop
Rel_E <- psych::alpha(df_post_einstellung_rel %>% select(contains("E")), check.keys =TRUE)
Rel_E
Rel_E$item.stats$r.drop
Rel_ges <- psych::alpha(df_post_einstellung_rel)
Rel_ges
#### Nonparametrischer Wilcoxon & Trennschärfe #### 
wilcox.test(Einst$SMean.post,Einst$SMean.prae,paired = T,alternative = "greater")

S01 <- c(df_post_einstellung$S01post, df_prae_einstellung$S01)
S02 <- c(df_post_einstellung$S02post, df_prae_einstellung$S02)
S03 <- c(df_post_einstellung$S03post, df_prae_einstellung$S03)
S12u <- c(df_post_einstellung$S12postu, df_prae_einstellung$S12u)
S13u <- c(df_post_einstellung$S13postu, df_prae_einstellung$S13u)

E04 <- c(df_post_einstellung$E04post, df_prae_einstellung$E04)
E05 <- c(df_post_einstellung$E05post, df_prae_einstellung$E05)
E06 <- c(df_post_einstellung$E06post, df_prae_einstellung$E06)
E07 <- c(df_post_einstellung$E07post, df_prae_einstellung$E07)
E08 <- c(df_post_einstellung$E08post, df_prae_einstellung$E08)
E10 <- c(df_post_einstellung$E10post, df_prae_einstellung$E10)
E11 <- c(df_post_einstellung$E11post, df_prae_einstellung$E11)
E14u <- c(df_post_einstellung$E14postu, df_prae_einstellung$E14u)

ls_prae <- rep("prae", 15)
ls_post <- rep("post", 15)
ls_SP <- c(ls_prae, ls_post)

w_df <- data.frame(S01, S02, S03, S12u, S13u, E04, E05, 
                   E06, E07, E08, E10, E11, E14u, ls_SP)
w_df$score <- rowMeans(w_df[,1:13])
w_df$ls_SP <- as.factor(w_df$ls_SP)
w_dfe <- data.frame(E04, E05, 
                    E06, E07, E08, E10, E11, E14u, ls_SP)
w_dfe$score <- rowMeans(w_dfe[,1:8])
w_dfe$ls_SP <- as.factor(w_dfe$ls_SP)
w_dfs <- data.frame(S01, S02, S03, S12u, S13u, ls_SP)
w_dfs$score <- rowMeans(w_dfs[,1:5])
w_dfs$ls_SP <- as.factor(w_dfs$ls_SP)
library(rstatix)
wilcox_effsize(w_df, score~ls_SP)


#### Normalverteilung & Varianz #### 
EinstMean$S_Diff <- EinstMean$Post_S - EinstMean$Prae_S
EinstMean$E_Diff <- EinstMean$Post_E - EinstMean$Prae_E
ggplot(EinstMean, aes(x = S_Diff)) +
  geom_histogram()
ggplot(EinstMean, aes(x = E_Diff)) +
  geom_histogram()

shapiro.test(EinstMean$S_Diff)
shapiro.test(EinstMean$E_Diff)
shapiro.test(Einst$Diff)
# Varianz #
library(car)
leveneTest(w_df$score,w_df$ls_SP) #komplette Items (E+S)
leveneTest(w_dfs$score,w_dfs$ls_SP)
leveneTest(w_dfe$score,w_dfe$ls_SP)
detach("package:car", unload = TRUE)
#### t-Test ####
t.test(Einst$SMean.post, Einst$SMean.prae,
       paired = T,alternative = "greater",var.equal = F)

library(ggsignif)
plot <- data.frame(
  Score = c(Einst$SMean.prae, Einst$SMean.post),
  Messzeitpunkt = c(rep("1. Prätest", length(Einst$SMean.prae)), rep("2. Posttest", length(Einst$SMean.post))))
pl <- ggplot(data = plot, aes(x = Messzeitpunkt, y = Score, fill=Messzeitpunkt))+
  geom_boxplot()+
  scale_x_discrete(labels = c(
    "2. Posttest" = "Posttest","1. Prätest" = "Prätest"))+
  theme_gray() +
  geom_point(shape=16,alpha = .3)+
  scale_fill_manual(values=c(rgb(0,135,178, max=255), rgb(79,184,0,max=255)))+
  ylim(2,6)+
  ylab("Mittelwerte")+
  theme(axis.text=element_text(size=13,face="bold",family = "Frutiger Next LT W1G"),
        axis.title=element_text(size=14,face="bold",family = "Frutiger Next LT W1G"))+
  guides(fill="none")+
  geom_signif(y_position = 5.5, xmin = 1, xmax = 2, annotation = "*", tip_length = 0.0)
pl
ggsave("mittelwerte.svg", plot=pl, width = 9.9, height = 5.9)

#### Effektstärke ####
effsize::cohen.d(d = Einst$SMean.post, Einst$SMean.prae,
                 paired = TRUE, na.rm = TRUE)
save(Einst,file="Vergleich_Einstellung.RData")
#### Vergleich Sensoren #####
df_post_SenVergl <- select(df_post,16:25)
df_post_SenVergl <- t(df_post_SenVergl)
df_post_SenVergl <- data.frame(df_post_SenVergl)
Mittelwerte <- rowMeans(df_post_SenVergl)
Kategorien <- rep(c("Handhabung","Grafik","Inbetriebnahme","Mobilität","Vielseitigkeit"),2)
ESP <- rep("ESP",5)
Vernier <- rep("Vernier",5)
Sensoren <- c(ESP,Vernier)
GesamtmittelwertESP <- c(mean(Mittelwerte[1:5]),"Gesamtmittelwert","ESP")
GesamtmittelwertVernier <- c(mean(Mittelwerte[5:10]),"Gesamtmittelwert","Vernier")
Vgl <- data.frame(Mittelwerte,Kategorien,Sensoren)
Vgl <- rbind(Vgl,GesamtmittelwertESP,GesamtmittelwertVernier)
Vgl$Mittelwerte <- as.numeric(Vgl$Mittelwerte)
t.test(Mittelwerte ~ Sensoren, data = Vgl, var.equal = F)

Sensoren <- ggplot(Vgl, aes(fill=Sensoren, y=Mittelwerte, x=Kategorien)) + 
  geom_bar(position="dodge", stat="identity")+
  theme_gray()+
  theme(axis.text=element_text(size=12,face="bold",family = "Frutiger Next LT W1G"),
        axis.title=element_text(size=14,face="bold",family = "Frutiger Next LT W1G"),legend.position = "bottom",legend.text=element_text(size=12))+
  ylim(0,10)+
  xlab("")+
  ylab("Bepunktung")+
  guides(fill=guide_legend(title="Sensor:"), text.font = "Frutiger Next LT W1G")+
  scale_fill_manual(values=c(rgb(0,135,178, max=255), rgb(79,184,0,max=255)))+
  geom_signif(y_position = 9.5, xmin = 0.55, xmax = 1.45, annotation = "n.s.", tip_length = 0.0)+
  geom_signif(y_position = 9, xmin = 1.55, xmax = 2.45, annotation = "**", tip_length = 0.0)+
  geom_signif(y_position = 9, xmin = 2.55, xmax = 3.45, annotation = "**", tip_length = 0.0)+
  geom_signif(y_position = 9, xmin = 3.55, xmax = 4.45, annotation = "*", tip_length = 0.0)+
  geom_signif(y_position = 9, xmin = 4.55, xmax = 5.45, annotation = "n.s.", tip_length = 0.0)+
  geom_signif(y_position = 9, xmin = 5.55, xmax = 6.45, annotation = "n.s.", tip_length = 0.0)
  
Sensoren

ggsave(file="Sensoren.svg", plot=Sensoren, width=9, height=5.9)
# Handhabung #

Sensorenvergleich <- t(df_post_SenVergl)
Sensorenvergleich <- data.frame(Sensorenvergleich)
Handh_E <-Sensorenvergleich %>% select(contains("Handh_E"))
names(Handh_E) <- c("Handh")
Handh_V <-Sensorenvergleich %>% select(contains("Handh_V"))
names(Handh_V) <- c("Handh")
Handh_G <- rbind(Handh_E,Handh_V)
ESP_H <- c(rep("ESP",15))
Vernier_H <- c(rep("Vernier",15))
Sensoren_ttest2 <- c(ESP_H,Vernier_H)
ttest_H <- cbind(Handh_G,Sensoren_ttest2)
leveneTest(ttest_H$Handh,ttest_H$Sensoren_ttest2)
t.test(Handh ~ Sensoren_ttest2, data = ttest_H, var.equal = F)

# Grafik #

Sensorenvergleich <- t(df_post_SenVergl)
Sensorenvergleich <- data.frame(Sensorenvergleich)
Grafik_E <-Sensorenvergleich %>% select(contains("Grafik_E"))
names(Grafik_E) <- c("Grafik")
Grafik_V <-Sensorenvergleich %>% select(contains("Grafik_V"))
names(Grafik_V) <- c("Grafik")
Grafik_G <- rbind(Grafik_E,Grafik_V)
ESP_H <- c(rep("ESP",15))
Vernier_H <- c(rep("Vernier",15))
Sensoren_ttest2 <- c(ESP_H,Vernier_H)
ttest_G <- cbind(Grafik_G,Sensoren_ttest2)
leveneTest(ttest_H$Handh,ttest_H$Sensoren_ttest2)
t.test(Grafik ~ Sensoren_ttest2, data = ttest_G, var.equal = F)


# Inbetriebnahme #

Sensorenvergleich <- t(df_post_SenVergl)
Sensorenvergleich <- data.frame(Sensorenvergleich)
Inbetrieb_E <-Sensorenvergleich %>% select(contains("Inbetrieb_E"))
names(Inbetrieb_E) <- c("Inbetrieb")
Inbetrieb_V <-Sensorenvergleich %>% select(contains("Inbetrieb_V"))
names(Inbetrieb_V) <- c("Inbetrieb")
Inbetrieb_G <- rbind(Inbetrieb_E,Inbetrieb_V)
ESP_H <- c(rep("ESP",15))
Vernier_H <- c(rep("Vernier",15))
Sensoren_ttest2 <- c(ESP_H,Vernier_H)
ttest_I <- cbind(Inbetrieb_G,Sensoren_ttest2)
leveneTest(ttest_H$Handh,ttest_H$Sensoren_ttest2)
t.test(Inbetrieb ~ Sensoren_ttest2, data = ttest_I, var.equal = F)



# Mobilität #

Sensorenvergleich <- t(df_post_SenVergl)
Sensorenvergleich <- data.frame(Sensorenvergleich)
Mobil_E <-Sensorenvergleich %>% select(contains("Mobil_E"))
names(Mobil_E) <- c("Mobil")
Mobil_V <-Sensorenvergleich %>% select(contains("Mobil_V"))
names(Mobil_V) <- c("Mobil")
Mobil_G <- rbind(Mobil_E,Mobil_V)
ESP_H <- c(rep("ESP",15))
Vernier_H <- c(rep("Vernier",15))
Sensoren_ttest2 <- c(ESP_H,Vernier_H)
ttest_M <- cbind(Mobil_G,Sensoren_ttest2)
leveneTest(ttest_H$Handh,ttest_H$Sensoren_ttest2)
t.test(Mobil ~ Sensoren_ttest2, data = ttest_M, var.equal = F)

# Vielseitigkeit #

Sensorenvergleich <- t(df_post_SenVergl)
Sensorenvergleich <- data.frame(Sensorenvergleich)
Vielseit_E <-Sensorenvergleich %>% select(contains("Vielseit_E"))
names(Vielseit_E) <- c("Vielseit")
Vielseit_V <-Sensorenvergleich %>% select(contains("Vielseit_V"))
names(Vielseit_V) <- c("Vielseit")
Vielseit_G <- rbind(Vielseit_E,Vielseit_V)
ESP_H <- c(rep("ESP",15))
Vernier_H <- c(rep("Vernier",15))
Sensoren_ttest2 <- c(ESP_H,Vernier_H)
ttest_V <- cbind(Vielseit_G,Sensoren_ttest2)
leveneTest(ttest_H$Handh,ttest_H$Sensoren_ttest2)
t.test(Vielseit ~ Sensoren_ttest2, data = ttest_V, var.equal = F)

#### Teilnehmendenverteilung, Nutzungsverhalten und Seminarfeedback####
# Teilnehmendenverteilung
  library(ggplot2)
g <- ggplot(df_neu) + aes(Hochschulsemester, xlim = "Hochschulsemester") + 
  geom_bar(col = rgb(0,135,178, max=255),fill = rgb(0,135,178, max=255)) + 
  scale_y_continuous(name = "absolute Häufigkeit",breaks = c(1:15)) + 
  scale_x_discrete(limits = c("1.","2.","3.","4.","5.","6."))+ 
  theme(axis.text=element_text(size=12,face="bold",family = "Frutiger Next LT W1G"),
        axis.title=element_text(size=14,face="bold",family = "Frutiger Next LT W1G"))
g
ggsave(file="Hochschulsemester.svg", plot=g, width=7, height=3.5)

# Graph Nutzung Medien
df_nutzung_G <- df_neu %>% select(3:13)
N1 <- data.frame(prop.table(table(df_nutzung_G[[(1)]])))
N1$Frag <- rep("N01", 2)
N2 <- data.frame(prop.table(table(df_nutzung_G[[(2)]])))
N2$Frag <- rep("N02", 3)
N3 <- data.frame(prop.table(table(df_nutzung_G[[(3)]])))
N3$Frag <- rep("N03", 5)
N4 <- data.frame(prop.table(table(df_nutzung_G[[(4)]])))
N4$Frag <- rep("N04", 5)
V5 <- data.frame(prop.table(table(df_nutzung_G[[(5)]])))
V5$Frag <- rep("N05", 4)
V6 <- data.frame(prop.table(table(df_nutzung_G[[(6)]])))
V6$Frag <- rep("N06", 3)
N7 <- data.frame(prop.table(table(df_nutzung_G[[(7)]])))
N7$Frag <- rep("N07", 2)
N9 <- data.frame(prop.table(table(df_nutzung_G[[(8)]])))
N9$Frag <- rep("N09", 4)
V10 <- data.frame(prop.table(table(df_nutzung_G[[(9)]])))
V10$Frag <- rep("N10", 5)
V11 <- data.frame(prop.table(table(df_nutzung_G[[(10)]])))
V11$Frag <- rep("N11", 3)
V11.1 <- data.frame(prop.table(table(df_nutzung_G[[(11)]])))
V11.1$Frag <- rep("N11.1", 1)
Gesamt_Neu_N <- rbind(N1,N2,N3,N4,V5,V6,N7,N9,V10,V11,V11.1)
Gesamt_Neu_N$Var1 <- factor(Gesamt_Neu_N$Var1,
                            levels = c("trifft gar nicht zu", 
                                       "trifft kaum zu", 
                                       "trifft kaum bis teilweise zu", 
                                       "trifft teilweise bis überwiegend zu", 
                                       "trifft überwiegend zu",
                                       "trifft völlig zu"),
                            labels = c("1 trifft gar nicht zu", 
                                       "2 trifft kaum zu", 
                                       "3 trifft kaum bis teilweise zu", 
                                       "4 trifft teilweise bis überwiegend zu", 
                                       "5 trifft überwiegend zu",
                                       "6 trifft völlig zu"))


Gesamt_Neu_N$Frag <- as.factor(Gesamt_Neu_N$Frag)
p <- ggplot(Gesamt_Neu_N) + aes(x=Frag, y=Freq, fill=Var1) + 
  geom_bar(position = "fill", stat="identity", width = 0.75) +
  scale_y_continuous(labels = label_percent(accuracy = NULL)) +
  scale_x_discrete(labels = c("Ich nutze regelmäßig digitale Medien","Ich nutze digitale Medien,damit ich Zugang zu 
  sozialen Netzwerken oder Messenger-Diensten 
  erhalte (z.B. Facebook, WhatsApp,...)", "Ich nutze digitale Medien, um im Lehramtsstudium Aufgaben 
  mit Hilfe von Programmen (z.B. Excel,...) zu erledigen","Ich nutze digitale Medien zur Erstellung von Erklär- & Lernvideos", "Ich nutze digitale Medien zur Erstellung von 
  Podcasts (Audio, Video) in meinem Lehramtsstudium", "Ich nutze digitale Medien zur Foto- & Videobearbeitung", "Ich habe bereits während meiner Schulzeit /
  universitären Ausbildung in naturwissenschaftlichen Fächern 
  mit digitalen Messwerterfassungssystemen gearbeitet","Ich habe bereits mit Microcontrollern 
  und dazugehörigen Sensoren während meiner 
  Schulzeit/meinem Studium gearbeitet","Ich habe bereits Erfahrung im Programmieren","Während meiner Schulzeit habe ich das Fach
Informatik in der Oberstufe belegt", "Während meines Lehramtsstudiums 
habe ich das Fach Informatik belegt"))+
  coord_flip() +
  geom_text(position = "fill", label = scales::percent(Gesamt_Neu_N$Freq,2), hjust = 1, check_overlap = T, size=5, colour="grey65")+
  guides(fill=guide_legend(title="Ausprägungen",reverse = T), text.font = "Frutiger Next LT W1G")+
  ylab("")+
  xlab("")+
  theme_minimal()+
  scale_fill_viridis(discrete = TRUE)+
  theme(axis.text=element_text(size=15,face="bold",family = "Frutiger Next LT W1G"),
        axis.title=element_text(size=15,face="bold",family = "Frutiger Next LT W1G"),legend.position = "bottom",legend.text=element_text(size=14))

p
ggsave(file="Nutzung.svg", plot=p, width=16, height=9)


# Graph Seminar Feedback
df_seminar_feedb <- t(df_seminar_feedb)
df_seminar_feedb <- data.frame(df_seminar_feedb)
df_seminar_feedb <- df_seminar_feedb %>% select(1:13|15)
df_seminar_feedb <- t(df_seminar_feedb)
df_seminar_feedb <- data.frame(df_seminar_feedb)


H1 <- data.frame(prop.table(table(df_seminar_feedb[[(1)]])))
H1$Frag <- rep("H1", 3)
H2 <- data.frame(prop.table(table(df_seminar_feedb[[(2)]])))
H2$Frag <- rep("H2", 3)
H3 <- data.frame(prop.table(table(df_seminar_feedb[[(3)]])))
H3$Frag <- rep("H3", 5)
H4 <- data.frame(prop.table(table(df_seminar_feedb[[(4)]])))
H4$Frag <- rep("H4", 4)
H5 <- data.frame(prop.table(table(df_seminar_feedb[[(5)]])))
H5$Frag <- rep("H5", 3)
H6 <- data.frame(prop.table(table(df_seminar_feedb[[(6)]])))
H6$Frag <- rep("H6", 3)
H7 <- data.frame(prop.table(table(df_seminar_feedb[[(7)]])))
H7$Frag <- rep("H7", 5)
Gesamt_Neu <- rbind(H1,H2,H3,H4,H5,H6,H7)
Gesamt_Neu$Var1 <- factor(Gesamt_Neu$Var1,
                            levels = c("trifft gar nicht zu", 
                                       "trifft kaum zu", 
                                       "trifft kaum bis teilweise zu", 
                                       "trifft teilweise bis überwiegend zu", 
                                       "trifft überwiegend zu",
                                       "trifft völlig zu"),
                            labels = c("1 Trifft gar nicht zu", 
                                       "2 Trifft kaum zu", 
                                       "3 Trifft kaum bis teilweise zu", 
                                       "4 Trifft teilweise bis überwiegend zu", 
                                       "5 Trifft überwiegend zu",
                                       "6 Trifft völlig zu"))
Gesamt_Neu$Frag <- as.factor(Gesamt_Neu$Frag)
p <- ggplot(Gesamt_Neu) + aes(x=Frag, y=Freq, fill=Var1) + 
  geom_bar(position = "fill", stat="identity", width = 0.75) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c("Ich fand die Gestaltung des Seminars passend", " Ich habe durch das Seminar etwas 
  über den Bau von Microcontrollern gelernt","Ich fand das Thema des Seminars interessant", "Ich traue mir nach dem Seminar auch den
  Bau eines anderen Microcontroller-Sensors zu", "Die Erklärungen des Dozenten waren verständlich", "Der Dozent erschien mir kompetent","Hätte ich die Wahl zwischen einem gekauften 
und einem selbstgebauten Sensor, würde ich 
letzteren für den Einsatz im Unterricht bevorzugen"))+
  coord_flip() +
  geom_text(position = "fill", label = scales::percent(Gesamt_Neu$Freq,2), hjust = 1, check_overlap = T, size=5, colour="grey65")+
  guides(fill=guide_legend(title="Ausprägungen",reverse = T), text.font = "Frutiger Next LT W1G")+
  ylab("")+
  xlab("")+
  theme_minimal()+
  scale_fill_viridis(discrete = TRUE)+
  theme(axis.text=element_text(size=15,face="bold",family = "Frutiger Next LT W1G"),
        axis.title=element_text(size=15,face="bold",family = "Frutiger Next LT W1G"),
        legend.position = "bottom",legend.text=element_text(size=14))


p
ggsave(file="Seminar.svg", plot=p, width=12.7, height=4.9)
