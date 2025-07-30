library(reshape2)
library(data.table)
library(ggplot2)
library(lme4)
library(emmeans)
library(lmerTest)
library(stringi)
library(stringr)
library(dplyr)
library(purrr)
library(tidyverse)
library(LMERConvenienceFunctions)
library(rstatix)
library(plotrix)
library(ggpubr)

path<- "E:/hse/omg/df_LZ"
read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}
df_compl <-list.files(path,pattern = "*.csv", full.names = T) %>% 
  map_df(~read_plus(.))
df_compl <- df_compl[, !colnames(df_compl) %in% c('...1', 'filename')]
df_compl <- separate(df_compl, name, into = c("time","subject", "type", "color"), sep = "_")

df_compl <- df_compl %>%
  mutate(color = case_when(
    color == 'GreenPurple'  ~ "PurpleGreen",
    color == 'BlueRed'  ~ "RedBlue",
    color == 'PinkYellow'  ~ "OrangePink",
    color == 'PinkBlue'  ~ "BluePink",
    TRUE ~ as.character(color)
  ))





###
###
###
df_compl <- df_compl %>%
  mutate(set = if_else(type %in% c( "HoneyComb","CubesControl"), 'hyperbolic', 'mystic'))

plot <- ggplot(df_compl, aes(x = factor(type, levels = c("HoneyComb","CubesControl",'Fractal', 'kaleidoscope')), 
                             y = lz, 
                             group = type, 
                             #color = 'black', 
                             fill = type)) +
  geom_boxplot(color = 'black')+
  theme_classic() +
  labs(
    title = "",
    #fill = 'Geometry: ',
    x = "",
    y = "Complexity",
    caption = "",
    
  ) +
  scale_fill_manual(breaks = c("HoneyComb","CubesControl",'Fractal', 'kaleidoscope'),
                    values=c("magenta2","olivedrab3", "magenta2","olivedrab3")) +
  #facet_grid( ~ time) +  # Facet by group
  
  geom_point(size = 1, position = position_dodge(width = 0.2)) +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20),
        text = element_text(size = 20), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"), 
        plot.title = element_text(hjust = 0.5),
        
  )

plot
###
###
###


data3 <- subset(df_compl, type %in% c("CubesControl", "HoneyComb"))
data4 <- subset(df_compl, type %in% c("Fractal", "kaleidoscope"))


m <- lmer(lz ~ type*color + (1|subject), data = data4)
an <- anova(m)
an <- data.table(an,keep.rownames = TRUE)

marginal_em <- emmeans(m, ~ as.factor(type|color),level = 0.95,lmer.df = "satterthwaite")
marginal_em<-as.data.frame(marginal_em)

setwd('E:/hse/omg/')
write.csv(marginal_em, "fk_means_compl_interaction.csv")
write.csv(an, "FK_an_compl.csv")



Tuk1<- NULL
Tuk1<-data.table(summary(emmeans(m, pairwise ~ type|color, adjust = 'Tukey',lmer.df = "satterthwaite"))$contrasts)
Tuk1 <- Tuk1[, group1:=gsub(' -.*', '', contrast)][, group2:=gsub('.*- ', '', contrast)]
Tuk1 <- Tuk1[p.value<0.1, p_significant:=format(p.value, digits = 3)]

n <- Tuk1[!is.na(p_significant), .N]

Tuk1[p.value<0.001, stars:='***']
Tuk1[p.value<0.01 & p.value>0.001 , stars:='**']
Tuk1[p.value<0.05 & p.value>0.01 , stars:='*']
Tuk1[p.value>0.05 & p.value<0.1 , stars:='#']


marginal_em <- emmeans(m, ~ as.factor(hyper),level = 0.95,lmer.df = "satterthwaite")
marginal_em<-as.data.frame(marginal_em)
write.csv(marginal_em, "ppg_hyper_emmeans_color.csv")
write.csv(Tuk1, "complexity_fk_Tukes_interaction.csv")







E:/hse/omg/df_LZ

plot <- ggplot(marginal_em, aes(x = factor(type, levels = c( 'Fractal', 'kaleidoscope')), y = emmean, group = type, color = type, fill = type)) +
  
  
  geom_hline(yintercept = -0.0, linetype = 'dashed', col = 'black', size = 1.0)  +
  #geom_col(position = position_dodge(width=0.7), linewidth = 2, width = 0.6)+
  #scale_x_discrete(labels = c( "HoneyCombs", "ControlCubes")) +
  
  geom_line(size = 2,  position = position_dodge(width = 0.2)) +
  geom_point(size = 2.5, shape = 23, position = position_dodge(width = 0.2)) +
  
  scale_color_manual(breaks = c('Fractal', 'kaleidoscope'),
                     values=c("magenta2","olivedrab3")) +
  scale_fill_manual(breaks = c('Fractal', 'kaleidoscope'),
                    values=c("magenta2","olivedrab3")) +
  geom_errorbar(aes(ymin = emmean -  SE, ymax = emmean +  SE), 
                width = 0.2, position = position_dodge(width = 0.2), size = 2, color = 'black') +
  
  theme_classic() +
  labs(
    title = "",
    #fill = 'Geometry: ',
    x = "",
    y = "Complexity",
    caption = "",
    
  ) +
  geom_point(size = 2.5, shape = 23, position = position_dodge(width = 0.2),color = 'black') +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20),
        text = element_text(size = 20), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"), 
        plot.title = element_text(hjust = 0.5),
        
  )


plot <- ggpar(plot,
              ylim = c(0.825, 0.875),
              font.ytickslab = 30,
              font.xtickslab = 27,
              font.main = 25,
              font.submain = 25,
              font.x = 27,
              font.y = 27,
              #font.title = font_title
              
)
# Print the plot
print(plot)



m <- lmer(lz ~ type*color + (1|subject), data = data4)
an <- anova(m)
an <- data.table(an,keep.rownames = TRUE)

marginal_em <- emmeans(m, ~ as.factor(type|color),level = 0.95,lmer.df = "satterthwaite")
marginal_em<-as.data.frame(marginal_em)
Tuk<-data.table(summary(emmeans(m, pairwise ~ type|color, adjust = 'tukey',lmer.df = "satterthwaite"))$contrasts)
Tuk <- Tuk[, group1:=gsub(' -.*', '', contrast)][, group2:=gsub('.*- ', '', contrast)]
Tuk <- Tuk[p.value<0.1, p_significant:=format(p.value, digits = 3)]
n <- Tuk[!is.na(p_significant), .N]
Tuk[p.value<0.001, stars:='***']
Tuk[p.value<0.01 & p.value>0.001 , stars:='**']
Tuk[p.value<0.05 & p.value>0.01 , stars:='*']
Tuk[p.value>0.05 & p.value<0.1 , stars:='#']


setwd('E:/hse/omg/')
write.csv(marginal_em, "fk_means_compl_colors.csv")
#write.csv(an, "FK_an_compl.csv")

plot <- ggplot(marginal_em, aes(x = factor(color), 
                                y = emmean, 
                                group = type, 
                                color = type, 
                                fill = type)) +
  
  
  geom_hline(yintercept = -0.0, linetype = 'dashed', col = 'black', size = 1.0)  +
  #geom_col(position = position_dodge(width=0.7), linewidth = 2, width = 0.6)+
  #scale_x_discrete(labels = c( "HoneyCombs", "ControlCubes")) +
  
  #geom_line(size = 2,  position = position_dodge(width = 0.2)) +

  scale_color_manual(breaks = c('Fractal', 'kaleidoscope'),
                     values=c("magenta2","olivedrab3")) +
  scale_fill_manual(breaks = c('Fractal', 'kaleidoscope'),
                    values=c("magenta2","olivedrab3")) +
  geom_errorbar(aes(ymin = emmean -  SE, ymax = emmean +  SE), 
                width = 0.5, position = position_dodge(width = 0.5), size = 2, color = 'black') +
  
  theme_classic() +
  labs(
    title = "",
    #fill = 'Geometry: ',
    x = "",
    y = "Complexity",
    caption = "",
    
  ) +
  geom_point(size = 2.5, shape = 23, position = position_dodge(width = 0.5),color = 'black') +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20),
        text = element_text(size = 20), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"), 
        plot.title = element_text(hjust = 0.5),
        
  )


plot <- ggpar(plot,
              ylim = c(0.825, 0.875),
              font.ytickslab = 30,
              font.xtickslab = 27,
              font.main = 25,
              font.submain = 25,
              font.x = 27,
              font.y = 27,
              #font.title = font_title
              
)+
  annotate("text", x = 1:5, y = 0.875, label = Tuk$stars, size = 7)
# Print the plot
print(plot)














m1 <- lmer(lz ~ type*color + (1|subject), data = data3)
d<-romr.fnc(m1, data3, trim = 3) ##### remove outliers
data<- d$data 
m <- lmer(lz ~ type*color + (1|subject), data = data3)
an <- anova(m)
an <- data.table(an,keep.rownames = TRUE)

marginal_em <- emmeans(m, ~ as.factor(type),level = 0.95,lmer.df = "satterthwaite")
marginal_em<-as.data.frame(marginal_em)

setwd('E:/hse/omg/')
write.csv(marginal_em, "hccc_means_compl.csv")
write.csv(an, "hccc_an_compl.csv")


plot <- ggplot(marginal_em, aes(x = factor(type, levels = c("HoneyComb", "CubesControl")), y = emmean, group = type, color = type, fill = type)) +
  
  geom_hline(yintercept = -0.0, linetype = 'dashed', col = 'black', size = 1.0)  +
  #geom_col(position = position_dodge(width=0.7), linewidth = 2, width = 0.6)+
  #scale_x_discrete(labels = c( "HoneyCombs", "ControlCubes")) +
  
  geom_line(size = 2,  position = position_dodge(width = 0.2)) +

  scale_color_manual(breaks = c(   "HoneyComb", "CubesControl" ),
                     values=c("magenta2","olivedrab3")) +
  scale_fill_manual(breaks = c( "HoneyComb", "CubesControl"),
                    values=c("magenta2","olivedrab3")) +
  geom_errorbar(aes(ymin = emmean -  SE, ymax = emmean +  SE), 
                width = 0.2, position = position_dodge(width = 0.2), size = 2, color = 'black') +
  
  theme_classic() +
  labs(
    title = "",
    #fill = 'Geometry: ',
    x = "",
    y = "Complexity",
    caption = "",
    
  ) +
  geom_point(size = 2.5, shape = 23, position = position_dodge(width = 0.2),color = 'black') +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20),
        text = element_text(size = 20), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"), 
        plot.title = element_text(hjust = 0.5),
        
  )


plot <- ggpar(plot,
              ylim = c(0.825, 0.875),
              font.ytickslab = 30,
              font.xtickslab = 27,
              font.main = 25,
              font.submain = 25,
              font.x = 27,
              font.y = 27,
              #font.title = font_title
              
)
# Print the plot
print(plot)







path<- "E:/hse/omg/df_LZ_sens"
read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}
df_compl <-list.files(path,pattern = "*.csv", full.names = T) %>% 
  map_df(~read_plus(.))
df_compl <- df_compl[, !colnames(df_compl) %in% c('...1', 'filename')]
df_compl <- separate(df_compl, name, into = c("time","subject", "type", "color", "sens"), sep = "_")

df_compl <- df_compl %>%
  mutate(color = case_when(
    color == 'GreenPurple'  ~ "PurpleGreen",
    color == 'BlueRed'  ~ "RedBlue",
    color == 'PinkYellow'  ~ "OrangePink",
    color == 'PinkBlue'  ~ "BluePink",
    TRUE ~ as.character(color)
  ))




result3 <- aggregate(df_compl$lz, 
                     by = list(df_compl$type, df_compl$sens ), 
                     FUN=function(x) c(mean=mean(x)))
colnames(result3) = c('type','sensor', 'x')


CC <- subset(result3, type == "CubesControl" )
CC <- CC[, !colnames(CC) %in% c('type')]
CC  <- t(CC)
colnames(CC) <- CC[1, ]
CC <- CC[-1, ]


HC<- subset(result3, type == "HoneyComb" )
HC <- HC[, !colnames(HC) %in% c( 'type')]
HC  <- t(HC)
colnames(HC) <- HC[1, ]
HC <- HC[-1, ]


Fr<- subset(result3, type == "Fractal" )
Fr <- Fr[, !colnames(Fr) %in% c( 'type')]
Fr  <- t(Fr)
colnames(Fr) <- Fr[1, ]
Fr <- Fr[-1, ]


k<- subset(result3, type == "kaleidoscope" )
k <- k[, !colnames(k) %in% c('type')]
k  <- t(k)
colnames(k) <- k[1, ]
k <- k[-1, ]


numeric_cols <- sapply(HC, is.numeric)

HCCC <- HC - CC
Fk <- Fr[, numeric_cols] - k[, numeric_cols]

#HCCC$freq <- CC$freq
#Fk$freq <- k$freq
write.csv(CC, "E:/hse/omg/lz_to_brains/CC.csv")
write.csv(HC, "E:/hse/omg/lz_to_brains/HC.csv")
write.csv(Fr, "E:/hse/omg/lz_to_brains/Fr.csv")
write.csv(k, "E:/hse/omg/lz_to_brains/k.csv")

















































result3$type <- factor(result3$type, levels = c("CubesControl",    "HoneyComb", "Fractal" ,     "kaleidoscope")) 

plot <- ggplot(result3, aes(x = group, y = x[,"mean"], color = hyper)) +
  geom_point(size = 3, position = position_dodge(width = 0.2), shape = 23) +  
  scale_color_manual(values = c("hyperbolic" = "magenta2", "nonhyperbolic" = "olivedrab3")) +
  geom_errorbar(aes(ymin = x[,"mean"] - x[,"std"], ymax = x[,"mean"] + x[,"std"], color = factor(hyper, levels = c("hyperbolic", "nonhyperbolic"))), width = 0.2, position = position_dodge(width = 0.2), size = 2) +
  #facet_grid( ~ hyper) +  # Facet by group
  labs(title = "Periodic component of alpha-band", x = "Type", y = "Scale mean of peak power ", caption = 'Errorbars represent SE') +  # Labels
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),  # Adjust x-axis title size
    axis.title.y = element_text(size = 16),  # Adjust y-axis title size
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    #panel.grid.major.x = element_blank(),
    legend.position = "bottom"  # Adjust as needed
  )

# Show the plot
print(plot)

table1 <- table1 %>%
  group_by(subj) %>% ###########???
  mutate(z_score = scale(a2))

table1 <- table1 %>%
  mutate(hyper = if_else(type %in% c("Fractal", "HoneyComb"), 'hyperbolic', 'nonhyperbolic'))

data3 <- subset(table1, type %in% c("CubesControl", "HoneyComb"))
data4 <- subset(table1, type %in% c("Fractal", "kaleidoscope"))

m1 <- lmer(z_score ~ hyper + (1|subj), data = data4)
d<-romr.fnc(m1, data3, trim = 3) ##### remove outliers
data<- d$data 
m <- lmer(z_score ~ hyper + (1|subj), data = data4)
an <- anova(m)
an <- data.table(an,keep.rownames = TRUE)
an_cols <- c('rn','Pr(>F)') 
an <- an[, ..an_cols]
an$p_value <- format(an$`Pr(>F)`, digits = 3)


data<- read.csv("D:/hse/psychodelic_like_experience/data_processing/psd/ap_analysis.csv")
data1 <- subset(data, er < 0.11)

data2 <- separate(data1, condition, into = c("type", "color"), sep = "/")
table1 <- data2[, !colnames(data2) %in% c('X1', 'X')]

table1 <- subset(table1, a2 != 'NaN') #exponent

result3 <- aggregate(table1$a2, 
                     by = list(table1$type), 
                     FUN=function(x) c(mean=mean(x), std=std.error(x)))
colnames(result3) = c('type', 'x')
result3 <- result3 %>%
  mutate(hyper = if_else(type %in% c("Fractal", "HoneyComb"), 'hyperbolic', 'nonhyperbolic'))%>%
  mutate(group = if_else(type %in% c("Fractal", "kaleidoscope"), 'Fractal/kaleidoscope', 'HoneyComb/CubesControl'))

result3$type <- factor(result3$type, levels = c("CubesControl",    "HoneyComb", "Fractal" ,     "kaleidoscope")) 

plot <- ggplot(result3, aes(x = group, y = x[,"mean"], color = hyper)) +
  geom_point(size = 3, position = position_dodge(width = 0.2), shape = 23) +  
  scale_color_manual(values = c("hyperbolic" = "magenta2", "nonhyperbolic" = "olivedrab3")) +
  geom_errorbar(aes(ymin = x[,"mean"] - x[,"std"], ymax = x[,"mean"] + x[,"std"], color = factor(hyper, levels = c("hyperbolic", "nonhyperbolic"))), width = 0.2, position = position_dodge(width = 0.2), size = 2) +
  #facet_grid( ~ hyper) +  # Facet by group
  labs(title = "Aperiodic component (exponent)", x = "Type", y = "Mean", caption = 'Errorbars represent SE') +  # Labels
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),  # Adjust x-axis title size
    axis.title.y = element_text(size = 16),  # Adjust y-axis title size
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    #panel.grid.major.x = element_blank(),
    legend.position = "bottom"  # Adjust as needed
  )

# Show the plot
print(plot)



library(reshape2)
library(data.table)
library(ggplot2)
library(lme4)
library(emmeans)
library(lmerTest)
library(stringi)
library(stringr)
library(dplyr)
library(purrr)
library(tidyverse)
library(LMERConvenienceFunctions)
library(rstatix)
library(plotrix)
library(ggpubr)

marginal_em_hyp <- subset(result3, group == 'HoneyComb/CubesControl' )

marginal_em <- marginal_em_hyp %>%
  mutate(Condition = if_else(hyper %in% c("hyperbolic"), 'HoneyCombs', 'ControlCubes'))


plot <- ggplot(marginal_em, aes(x = hyper, y = x[,"mean"], group = Condition, color = Condition, fill = Condition)) +
  
  geom_hline(yintercept = -0.0, linetype = 'dashed', col = 'black', size = 1.0)  +
  geom_col(position = position_dodge(width=0.7), linewidth = 2, width = 0.6)+
  scale_x_discrete(labels = c( "HoneyCombs", "ControlCubes")) +
  geom_line(size = 2,  position = position_dodge(width = 0.2)) +
  geom_point(size = 2.5, shape = 23, position = position_dodge(width = 0.2)) +
  
  scale_color_manual(breaks = c('HoneyCombs', 'ControlCubes'),
                     values=c("magenta2","olivedrab3")) +
  scale_fill_manual(breaks = c('HoneyCombs', 'ControlCubes'),
                    values=c("magenta2","olivedrab3")) +
  geom_errorbar(aes(ymin = x[,"mean"] -  x[,"std"], ymax = x[,"mean"] +  x[,"std"]), 
                width = 0.2, position = position_dodge(width = 0.2), size = 2, color = 'black') +
  
  # Facet the plot
  #facet_wrap(~set, ncol = 2) +
  
  #  stat_pvalue_manual(Tuk, label = 'stars', size = 7, bracket.size = 2, tip.length = 0.04,
  #                     y.position =c(max(marginal_em$emmean)+max(marginal_em$SE) + 0.03),inherit.aes = FALSE) +
  # Customize the appearance of the plot
  theme_classic() +
  labs(
    title = "",
    #fill = 'Geometry: ',
    x = "",
    y = "Aperiodic component, dB",
    caption = "",
    
  ) +
  geom_point(size = 2.5, shape = 23, position = position_dodge(width = 0.2),color = 'black', fill = 'white') +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20),
        text = element_text(size = 20), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"), 
        plot.title = element_text(hjust = 0.5),
        
  )


plot <- ggpar(plot,
              ylim = c(0.5, 0.7),
              font.ytickslab = 30,
              font.xtickslab = 27,
              font.main = 25,
              font.submain = 25,
              font.x = 27,
              font.y = 27,
              #font.title = font_title
              
)
# Print the plot
print(plot)








table1 <- table1 %>%
  group_by(subj) %>% ###########???
  mutate(z_score = scale(a2))

table1 <- table1 %>%
  mutate(hyper = if_else(type %in% c("Fractal", "HoneyComb"), 'hyperbolic', 'nonhyperbolic'))

data3 <- subset(table1, type %in% c("CubesControl", "HoneyComb"))
data4 <- subset(table1, type %in% c("Fractal", "kaleidoscope"))

m <- lmer(z_score ~ hyper + (1|subj), data = data4)
an <- anova(m)
an <- data.table(an,keep.rownames = TRUE)

an['Pr(>F)'<0.001, stars:='***']
an['Pr(>F)'<0.01 & 'Pr(>F)'>0.001 , stars:='**']
an['Pr(>F)'<0.05 & 'Pr(>F)'>0.01 , stars:='*']
an['Pr(>F)'>0.05 & 'Pr(>F)'<0.1 , stars:='#']
an$group1 = 'Fractal'
an$group2 = 'kaleidoscope'
#an$stars = "%%"
marginal_em_hyp <- subset(result3, group == 'Fractal/kaleidoscope' )

marginal_em <- marginal_em_hyp %>%
  mutate(Condition = if_else(hyper %in% c("hyperbolic"), 'Fractal', 'kaleidoscope'))


plot <- ggplot(marginal_em, aes(x = Condition, y = x[,"mean"], group = Condition, color = Condition, fill = Condition)) +
  
  geom_hline(yintercept = -0.0, linetype = 'dashed', col = 'black', size = 1.0)  +
  geom_col(position = position_dodge(width=0.7), linewidth = 2, width = 0.6)+
  #scale_x_discrete(labels = c( "Fractal", "kaleidoscope")) +
  geom_line(size = 2,  position = position_dodge(width = 0.2)) +
  geom_point(size = 2.5, shape = 23, position = position_dodge(width = 0.2)) +
  
  scale_color_manual(breaks = c('Fractal', 'kaleidoscope'),
                     values=c("magenta2","olivedrab3")) +
  scale_fill_manual(breaks = c('Fractal', 'kaleidoscope'),
                    values=c("magenta2","olivedrab3")) +
  geom_errorbar(aes(ymin = x[,"mean"] -  x[,"std"], ymax = x[,"mean"] +  x[,"std"]), 
                width = 0.2, position = position_dodge(width = 0.2), size = 2, color = 'black') +
  
  # Facet the plot
  #facet_wrap(~set, ncol = 2) +
  
  #  stat_pvalue_manual(Tuk, label = 'stars', size = 7, bracket.size = 2, tip.length = 0.04,
  #                     y.position =c(max(marginal_em$emmean)+max(marginal_em$SE) + 0.03),inherit.aes = FALSE) +
  # Customize the appearance of the plot
  theme_classic() +
  labs(
    title = "",
    #fill = 'Geometry: ',
    x = "",
    y = "Aperiodic component, dB",
    y = "Periodic alpha, dB",
    caption = "",
    
  ) +
  geom_point(size = 2.5, shape = 23, position = position_dodge(width = 0.2),color = 'black', fill = 'white') +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20),
        text = element_text(size = 20), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"), 
        plot.title = element_text(hjust = 0.5),
        
  )+
  stat_pvalue_manual(an, label = 'stars', size = 7, bracket.size = 2, tip.length = 0.04,
                     y.position =c(max(marginal_em$x[,"mean"])+max(marginal_em$x[,"std"]) + 0.003),inherit.aes = FALSE) 



plot <- ggpar(plot,
              ylim = c(0.5, 0.7),
              font.ytickslab = 30,
              font.xtickslab = 27,
              font.main = 25,
              font.submain = 25,
              font.x = 27,
              font.y = 27,
              #font.title = font_title
              
)
# Print the plot
print(plot)







table1 <- subset(table1, ap3 != 'NaN')

table1 <- table1 %>%
  group_by(subj) %>% ###########???
  mutate(z_score = scale(ap3))

table1 <- table1 %>%
  mutate(hyper = if_else(type %in% c("Fractal", "HoneyComb"), 'hyperbolic', 'nonhyperbolic'))

data3 <- subset(table1, type %in% c("CubesControl", "HoneyComb"))
data4 <- subset(table1, type %in% c("Fractal", "kaleidoscope"))

m <- lmer(z_score ~ hyper + (1|subj), data = data3)
an <- anova(m)
an <- data.table(an,keep.rownames = TRUE)

an['Pr(>F)'<0.001, stars:='***']
an['Pr(>F)'<0.01 & 'Pr(>F)'>0.001 , stars:='**']
an['Pr(>F)'<0.05 & 'Pr(>F)'>0.01 , stars:='*']
an['Pr(>F)'>0.05 & 'Pr(>F)'<0.1 , stars:='#']
an$group1 = 'Fractal'
an$group2 = 'kaleidoscope'
#an$stars = "%%"
marginal_em_hyp <- subset(result3, group == 'Fractal/kaleidoscope' )

marginal_em <- marginal_em_hyp %>%
  mutate(Condition = if_else(hyper %in% c("hyperbolic"), 'Fractal', 'kaleidoscope'))


plot <- ggplot(marginal_em, aes(x = Condition, y = x[,"mean"], group = Condition, color = Condition, fill = Condition)) +
  
  geom_hline(yintercept = -0.0, linetype = 'dashed', col = 'black', size = 1.0)  +
  geom_col(position = position_dodge(width=0.7), linewidth = 2, width = 0.6)+
  #scale_x_discrete(labels = c( "Fractal", "kaleidoscope")) +
  geom_line(size = 2,  position = position_dodge(width = 0.2)) +
  geom_point(size = 2.5, shape = 23, position = position_dodge(width = 0.2)) +
  
  scale_color_manual(breaks = c('Fractal', 'kaleidoscope'),
                     values=c("magenta2","olivedrab3")) +
  scale_fill_manual(breaks = c('Fractal', 'kaleidoscope'),
                    values=c("magenta2","olivedrab3")) +
  geom_errorbar(aes(ymin = x[,"mean"] -  x[,"std"], ymax = x[,"mean"] +  x[,"std"]), 
                width = 0.2, position = position_dodge(width = 0.2), size = 2, color = 'black') +
  
  # Facet the plot
  #facet_wrap(~set, ncol = 2) +
  
  #  stat_pvalue_manual(Tuk, label = 'stars', size = 7, bracket.size = 2, tip.length = 0.04,
  #                     y.position =c(max(marginal_em$emmean)+max(marginal_em$SE) + 0.03),inherit.aes = FALSE) +
  # Customize the appearance of the plot
  theme_classic() +
  labs(
    title = "",
    #fill = 'Geometry: ',
    x = "",
    y = "Aperiodic component, dB",
    y = "Periodic alpha, dB",
    caption = "",
    
  ) +
  geom_point(size = 2.5, shape = 23, position = position_dodge(width = 0.2),color = 'black', fill = 'white') +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20),
        text = element_text(size = 20), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"), 
        plot.title = element_text(hjust = 0.5),
        
  )+
  stat_pvalue_manual(an, label = 'stars', size = 7, bracket.size = 2, tip.length = 0.04,
                     y.position =c(max(marginal_em$x[,"mean"])+max(marginal_em$x[,"std"]) + 0.003),inherit.aes = FALSE) 



plot <- ggpar(plot,
              #ylim = c(1, 1.3),
              font.ytickslab = 30,
              font.xtickslab = 27,
              font.main = 25,
              font.submain = 25,
              font.x = 27,
              font.y = 27,
              #font.title = font_title
              
)
# Print the plot
print(plot)
















#################AP_on_sensors
data<- read.csv("D:/hse/psychodelic_like_experience/data_processing/psd/ap_analysis_sensors.csv")
data1 <- subset(data, er < 0.15)

data2 <- separate(data1, condition, into = c("type", "color"), sep = "/")
table1 <- data2[, !colnames(data2) %in% c('X1', 'X')]
#numeric_cols <- sapply(table1, is.numeric)
table1 <- subset(table1, a2 != 'NaN')

result3 <- aggregate(table1$a2, 
                     by = list(table1$type, table1$sensor ), 
                     FUN=function(x) c(mean=mean(x)))
colnames(result3) = c('type','sensor', 'x')


CC <- subset(result3, type == "CubesControl" )
CC <- CC[, !colnames(CC) %in% c('type')]
CC  <- t(CC)
colnames(CC) <- CC[1, ]
CC <- CC[-1, ]


HC<- subset(result3, type == "HoneyComb" )
HC <- HC[, !colnames(HC) %in% c( 'type')]
HC  <- t(HC)
colnames(HC) <- HC[1, ]
HC <- HC[-1, ]


Fr<- subset(result3, type == "Fractal" )
Fr <- Fr[, !colnames(Fr) %in% c( 'type')]
Fr  <- t(Fr)
colnames(Fr) <- Fr[1, ]
Fr <- Fr[-1, ]


k<- subset(result3, type == "kaleidoscope" )
k <- k[, !colnames(k) %in% c('type')]
k  <- t(k)
colnames(k) <- k[1, ]
k <- k[-1, ]


numeric_cols <- sapply(HC, is.numeric)

#HCCC <- HC[, numeric_cols] - CC[, numeric_cols]
#Fk <- Fr[, numeric_cols] - k[, numeric_cols]

#HCCC$freq <- CC$freq
#Fk$freq <- k$freq
write.csv(CC, "D:/hse/psychodelic_like_experience/data_processing/CC_alpha.csv")
write.csv(HC, "D:/hse/psychodelic_like_experience/data_processing/HC_alpha.csv")
write.csv(Fr, "D:/hse/psychodelic_like_experience/data_processing/Fr_alpha.csv")
write.csv(k, "D:/hse/psychodelic_like_experience/data_processing/k_alpha.csv")








result3 <- result3 %>%
  mutate(hyper = if_else(type %in% c("Fractal", "HoneyComb"), 'hyperbolic', 'nonhyperbolic'))%>%
  mutate(group = if_else(type %in% c("Fractal", "kaleidoscope"), 'Fractal/kaleidoscope', 'HoneyComb/CubesControl'))

result3$type <- factor(result3$type, levels = c("CubesControl",    "HoneyComb", "Fractal" ,     "kaleidoscope")) 

plot <- ggplot(result3, aes(x = group, y = x[,"mean"], color = hyper)) +
  geom_point(size = 3, position = position_dodge(width = 0.2), shape = 23) +  
  scale_color_manual(values = c("hyperbolic" = "magenta2", "nonhyperbolic" = "olivedrab3")) +
  geom_errorbar(aes(ymin = x[,"mean"] - x[,"std"], ymax = x[,"mean"] + x[,"std"], color = factor(hyper, levels = c("hyperbolic", "nonhyperbolic"))), width = 0.2, position = position_dodge(width = 0.2), size = 2) +
  #facet_grid( ~ hyper) +  # Facet by group
  labs(title = "Periodic component of alpha-band", x = "Type", y = "Mean of peak power", caption = 'Errorbars represent SE') +  # Labels
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),  # Adjust x-axis title size
    axis.title.y = element_text(size = 16),  # Adjust y-axis title size
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    #panel.grid.major.x = element_blank(),
    legend.position = "bottom"  # Adjust as needed
  )

# Show the plot
print(plot)

table1 <- table1 %>%
  mutate(color = case_when(
    color == 'GreenPurple'  ~ "PurpleGreen",
    color == 'BlueRed'  ~ "RedBlue",
    color == 'PinkYellow'  ~ "OrangePink",
    color == 'PinkBlue'  ~ "BluePink",
    TRUE ~ as.character(color) # Keep unchanged if none of the conditions match
  ))
sensors<- unique(data2$sensor)
table1 <- table1 %>%
  group_by(subj) %>% ###########???
  mutate(z_score = scale(a2))

table1 <- table1 %>%
  mutate(hyper = if_else(type %in% c("Fractal", "HoneyComb"), 'hyperbolic', 'nonhyperbolic'))

data3 <- subset(table1, type %in% c("CubesControl", "HoneyComb"))
data4 <- subset(table1, type %in% c("Fractal", "kaleidoscope"))


p_vals <- data.table()
#cols <- c("psd")
############### for green heads (main_effects) ##############
for (i in 1:length(sensors)) {
  temp <-subset(data3, sensor == sensors[i])
  m1 <- lmer(z_score ~ hyper + (1|subj), data = temp)
  d<-romr.fnc(m1, temp, trim = 3) ##### remove outliers
  data<- d$data 
  m <- lmer(z_score ~ hyper + (1|subj), data = data)
  an <- anova(m)
  an <- data.table(an,keep.rownames = TRUE)
  an_cols <- c('rn','Pr(>F)') 
  an <- an[, ..an_cols]
  an$p_value <- format(an$`Pr(>F)`, digits = 3)
  #an$interval <- j
  #an$interval <- gsub('beta power','',an$interval)
  #an <- dcast(an,formula = interval~rn,value.var = 'Pr(>F)')
  an$sensor <- sensors[i] 
  #an$sensor_name <- files[sensor==i]$Name
  p_vals <- rbind(p_vals,an)
  
}

data<- read.csv("D:/hse/psychodelic_like_experience/data_processing/psd/ap_analysis_sensors.csv")
data1 <- subset(data, er < 0.11)

data2 <- separate(data1, condition, into = c("type", "color"), sep = "/")
table1 <- data2[, !colnames(data2) %in% c('X1', 'X')]

table1 <- subset(table1, ap3 != 'NaN') #exponent

result3 <- aggregate(table1$ap3, 
                     by = list(table1$type), 
                     FUN=function(x) c(mean=mean(x), std=std.error(x)))
colnames(result3) = c('type', 'x')
result3 <- result3 %>%
  mutate(hyper = if_else(type %in% c("Fractal", "HoneyComb"), 'hyperbolic', 'nonhyperbolic'))%>%
  mutate(group = if_else(type %in% c("Fractal", "kaleidoscope"), 'Fractal/kaleidoscope', 'HoneyComb/CubesControl'))

result3$type <- factor(result3$type, levels = c("CubesControl",    "HoneyComb", "Fractal" ,     "kaleidoscope")) 

plot <- ggplot(result3, aes(x = group, y = x[,"mean"], color = hyper)) +
  geom_point(size = 3, position = position_dodge(width = 0.2), shape = 23) +  
  scale_color_manual(values = c("hyperbolic" = "magenta2", "nonhyperbolic" = "olivedrab3")) +
  geom_errorbar(aes(ymin = x[,"mean"] - x[,"std"], ymax = x[,"mean"] + x[,"std"], color = factor(hyper, levels = c("hyperbolic", "nonhyperbolic"))), width = 0.2, position = position_dodge(width = 0.2), size = 2) +
  #facet_grid( ~ hyper) +  # Facet by group
  labs(title = "Aperiodic component (exponent)", x = "Type", y = "Mean", caption = 'Errorbars represent SE') +  # Labels
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),  # Adjust x-axis title size
    axis.title.y = element_text(size = 16),  # Adjust y-axis title size
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    #panel.grid.major.x = element_blank(),
    legend.position = "bottom"  # Adjust as needed
  )

# Show the plot
print(plot)

table1 <- table1 %>%
  mutate(color = case_when(
    color == 'GreenPurple'  ~ "PurpleGreen",
    color == 'BlueRed'  ~ "RedBlue",
    color == 'PinkYellow'  ~ "OrangePink",
    color == 'PinkBlue'  ~ "BluePink",
    TRUE ~ as.character(color) # Keep unchanged if none of the conditions match
  ))
sensors<- unique(table1$sensor)
table1 <- table1 %>%
  group_by(subj) %>% ###########???
  mutate(z_score = scale(ap3))

table1 <- table1 %>%
  mutate(hyper = if_else(type %in% c("Fractal", "HoneyComb"), 'hyperbolic', 'nonhyperbolic'))

data3 <- subset(table1, type %in% c("CubesControl", "HoneyComb"))
data4 <- subset(table1, type %in% c("Fractal", "kaleidoscope"))


p_vals <- data.table()
#cols <- c("psd")
############### for green heads (main_effects) ##############
for (i in 1:length(sensors)) {
  temp <-subset(data4, sensor == sensors[i])
  m1 <- lmer(z_score ~ hyper + (1|subj), data = temp)
  d<-romr.fnc(m1, temp, trim = 3) ##### remove outliers
  data<- d$data 
  m <- lmer(z_score ~ hyper + (1|subj), data = data)
  an <- anova(m)
  an <- data.table(an,keep.rownames = TRUE)
  an_cols <- c('rn','Sum Sq', 'Mean Sq', 'NumDF',  'DenDF', 'F value', 'Pr(>F)') 
  an <- an[, ..an_cols]
  an$p_value <- format(an$`Pr(>F)`, digits = 3)
  #an$interval <- j
  #an$interval <- gsub('beta power','',an$interval)
  #an <- dcast(an,formula = interval~rn,value.var = 'Pr(>F)')
  an$sensor <- sensors[i] 
  #an$sensor_name <- files[sensor==i]$Name
  p_vals <- rbind(p_vals,an)
  
}
write.csv(p_vals, "E:/hse/psychodelic_like_experience/data_processing/stats/final_stat_ap_fk.csv")




p_vals <- data.table()
#cols <- c("psd")
############### for green heads (main_effects) ##############
#for (i in 1:length(sensors)) {
#temp <-subset(data4, sensor == sensors[i])

m1 <- lmer(z_score ~ hyper + (1|subj), data = data3)
d<-romr.fnc(m1, data3, trim = 3) ##### remove outliers
data<- d$data 
m <- lmer(z_score ~ hyper + (1|subj), data = data)
an <- anova(m)
an <- data.table(an,keep.rownames = TRUE)
an_cols <- c('rn','Sum Sq', 'Mean Sq', 'NumDF',  'DenDF', 'F value', 'Pr(>F)') 
an <- an[, ..an_cols]
an$p_value <- format(an$`Pr(>F)`, digits = 3)
#an$interval <- j
#an$interval <- gsub('beta power','',an$interval)
#an <- dcast(an,formula = interval~rn,value.var = 'Pr(>F)')
#an$sensor <- sensors[i] 
#an$sensor_name <- files[sensor==i]$Name
p_vals <- rbind(p_vals,an)

#}


data<- read.csv("E:/hse/psychodelic_like_experience/data_processing/complexity/c_baselined_alpha.csv")
table1 <- data %>%
  mutate(hyper = if_else(cond %in% c("Fractal", "HoneyComb"), 'hyperbolic', 'nonhyperbolic'))
#table1 <- table1 %>%
# group_by(subject) %>% ###########???
#mutate(z_score = scale(comp_bl))

data3 <- subset(table1, cond %in% c("CubesControl", "HoneyComb"))
data4 <- subset(table1, cond %in% c("Fractal", "kaleidoscope"))

sensors<- unique(table1$sensor)

#result3 <- aggregate(table1$comp_bl, 
#                    by = list(table1$cond), 
#                   FUN=function(x) c(mean=mean(x), std=std.error(x)))
#colnames(result3) = c('type', 'x')




p_vals <- data.table()
#cols <- c("psd")
############### for green heads (main_effects) ##############
for (i in 1:length(sensors)) {
  temp <-subset(data4, sensor == sensors[i])
  m1 <- lmer(comp_bl ~ hyper + (1|subject), data = temp)
  d<-romr.fnc(m1, temp, trim = 3) ##### remove outliers
  data<- d$data 
  m <- lmer(comp_bl ~ hyper + (1|subject), data = data)
  an <- anova(m)
  an <- data.table(an,keep.rownames = TRUE)
  an_cols <- c('rn','Sum Sq', 'Mean Sq', 'NumDF',  'DenDF', 'F value', 'Pr(>F)') 
  an <- an[, ..an_cols]
  an$p_value <- format(an$`Pr(>F)`, digits = 3)
  #an$interval <- j
  #an$interval <- gsub('beta power','',an$interval)
  #an <- dcast(an,formula = interval~rn,value.var = 'Pr(>F)')
  an$sensor <- sensors[i] 
  #an$sensor_name <- files[sensor==i]$Name
  p_vals <- rbind(p_vals,an)
  
}


