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
library(ggthemes)


data<- read.csv("E:/hse/2207_ap_output_color_fin.csv")


data1 <- subset(data, er < 0.11)
data <- data[, !colnames(data) %in% c('X1', 'X', 'sens')]

table1 <- subset(data, a2!= 'NaN') #exponent
table1 <- table1 %>%
  group_by(subj) %>% #normalize data to reduce intersubject differences and balance powers 
  mutate(a2 = scale(a2))



table1 <- table1 %>%
  mutate(hyper = if_else(condition %in% c("Fractal", "HoneyComb"), 'hyperbolic', 'nonhyperbolic'))
table1 <- table1 %>%
  mutate(color = case_when(
    color == 'GreenPurple'  ~ "PurpleGreen",
    color == 'BlueRed'  ~ "RedBlue",
    color == 'PinkYellow'  ~ "OrangePink",
    color == 'PinkBlue'  ~ "BluePink",
    TRUE ~ as.character(color)
  ))


data3 <- subset(table1, condition %in% c("CubesControl", "HoneyComb"))
data4 <- subset(table1, condition %in% c("Fractal", "kaleidoscope"))
colnames(data3)
m <- lmer(a2 ~ hyper*color + (1|subj), data = data4)
an <- anova(m)
an <- data.table(an,keep.rownames = TRUE)

emm_options(lmerTest.limit = 6000)

Tuk1<- NULL
Tuk1<-data.table(summary(emmeans(m, pairwise ~ color|hyper, adjust = 'Tukey',lmer.df = "satterthwaite"))$contrasts)
Tuk1 <- Tuk1[, group1:=gsub(' -.*', '', contrast)][, group2:=gsub('.*- ', '', contrast)]
Tuk1 <- Tuk1[p.value<0.1, p_significant:=format(p.value, digits = 3)]

n <- Tuk1[!is.na(p_significant), .N]

Tuk1[p.value<0.001, stars:='***']
Tuk1[p.value<0.01 & p.value>0.001 , stars:='**']
Tuk1[p.value<0.05 & p.value>0.01 , stars:='*']
Tuk1[p.value>0.05 & p.value<0.1 , stars:='#']
write.csv(Tuk1, "3007_fk_a2_Tuk1.csv")


Tuk1<- NULL
Tuk1<-data.table(summary(emmeans(m, pairwise ~ hyper|color, adjust = 'Tukey',lmer.df = "satterthwaite"))$contrasts)
Tuk1 <- Tuk1[, group1:=gsub(' -.*', '', contrast)][, group2:=gsub('.*- ', '', contrast)]
Tuk1 <- Tuk1[p.value<0.1, p_significant:=format(p.value, digits = 3)]

n <- Tuk1[!is.na(p_significant), .N]

Tuk1[p.value<0.001, stars:='***']
Tuk1[p.value<0.01 & p.value>0.001 , stars:='**']
Tuk1[p.value<0.05 & p.value>0.01 , stars:='*']
Tuk1[p.value>0.05 & p.value<0.1 , stars:='#']
write.csv(Tuk1, "3007_fk_a2_Tuk2.csv")


marginal_em <- emmeans(m, ~ as.factor(hyper|color),level = 0.95,lmer.df = "satterthwaite")
marginal_em<-as.data.frame(marginal_em)
#write.csv(marginal_em, "3007_a2_emm.csv")


marginal_em <- marginal_em %>%
  mutate(Condition = if_else(hyper %in% c("hyperbolic"), 'Fractal', 'kaleidoscope'))





plot <- ggplot(marginal_em, aes(x = factor(color), 
                                y = emmean, 
                                group = Condition, 
                                color = Condition, 
                                fill = Condition)) +
  
  
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
    y = "ap3",
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
              #ylim = c(0.825, 0.875),
              font.ytickslab = 30,
              font.xtickslab = 27,
              font.main = 25,
              font.submain = 25,
              font.x = 27,
              font.y = 27,
              #font.title = font_title
              
)+
  annotate("text", x = 1:5, y = 0.875, label = Tuk1$stars, size = 7)
# Print the plot
print(plot)


Tuk1<- NULL
Tuk1<-data.table(summary(emmeans(m, pairwise ~ color|hyper, adjust = 'Tukey',lmer.df = "satterthwaite"))$contrasts)
Tuk1 <- Tuk1[, group1:=gsub(' -.*', '', contrast)][, group2:=gsub('.*- ', '', contrast)]
Tuk1 <- Tuk1[p.value<0.1, p_significant:=format(p.value, digits = 3)]

n <- Tuk1[!is.na(p_significant), .N]

Tuk1[p.value<0.001, stars:='***']
Tuk1[p.value<0.01 & p.value>0.001 , stars:='**']
Tuk1[p.value<0.05 & p.value>0.01 , stars:='*']
Tuk1[p.value>0.05 & p.value<0.1 , stars:='#']


marginal_em_f<- subset(marginal_em, hyper == 'hyperbolic' )
Tuk1_f<- subset(Tuk1, hyper == 'hyperbolic' )



plot <- ggplot(marginal_em_f, aes(x = factor(color), 
                                y = emmean, 
                                group = Condition, 
                                color = Condition, 
                                fill = Condition)) +
  
  
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
    y = "ap3",
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
        
  )+

  stat_pvalue_manual(Tuk1_f, label = 'stars', size = 7, bracket.size = 2, tip.length = 0.02,
                     y.position =c(max(marginal_em$emmean)+max(marginal_em$SE) + 0.01),step.increase = 0.15,inherit.aes = FALSE) 



plot <- ggpar(plot,
              ylim = c(-1, 1.7),
              font.ytickslab = 30,
              font.xtickslab = 27,
              font.main = 25,
              font.submain = 25,
              font.x = 27,
              font.y = 27,
              #font.title = font_title
              
)#+
  #annotate("text", x = 1:5, y = 0.875, label = Tuk1_f$stars, size = 7)
# Print the plot
print(plot)








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


setwd('C:/Users/User/Documents/pl_project/tables')
write.csv(result3, "a2_means.csv")
write.csv(an, "a2_FK_an.csv")


melted_data <- melt(data, id=c("condition", "color",   "subj"))
melted_data <- subset(melted_data, value != 'NaN') #exponent
result3 <- melted_data %>%
  group_by(variable) %>% #normalize data to reduce intersubject differences and balance powers 
  mutate(value = scale(value))

result3 <- aggregate(result3$value, 
                     by = list(result3$variable, result3$condition), 
                     FUN=function(x) c(mean=mean(x), std=std.error(x)))
colnames(result3)= c('variable', 'condition', 'x')
result3 <- result3 %>%
  mutate(set = if_else(condition %in% c('Fractal', 'kaleidoscope'), 'mystic',  'hyperbolic'))

result3$condition = factor(result3$condition, level = c('HoneyComb', 'CubesControl', 'Fractal', 'kaleidoscope'))
resultd =  subset(result3, variable %in% c( 'a2') )
result_h =  subset(resultd, set %in% c('hyperbolic' ) )
result_h =  subset(resultd, set %in% c('mystic' ) )

plot <- ggplot(result_h, aes(x = factor(result_h$condition, level = c('HoneyComb', 'CubesControl')), y = x[,"mean"], group = condition, color = condition, fill = condition)) +
  
  geom_hline(yintercept = -0.0, linetype = 'dashed', col = 'black', size = 1.0)  +
  geom_col(position = position_dodge(width=0.7), linewidth = 2, width = 0.6)+
  geom_line(size = 2,  position = position_dodge(width = 0.2)) +
  geom_point(size = 2.5, shape = 23, position = position_dodge(width = 0.2)) +
  
  geom_errorbar(aes(ymin = x[,"mean"] -  x[,"std"], ymax = x[,"mean"] +  x[,"std"]), 
                width = 0.2, position = position_dodge(width = 0.2), size = 2, color = 'black') +
  scale_color_manual(breaks = c('HoneyComb', 'CubesControl'),
                     values=c("magenta2","olivedrab3")) +
  scale_fill_manual(breaks = c('HoneyComb', 'CubesControl'),
                    values=c("magenta2","olivedrab3")) +
  
  
  # Facet the plot
  #facet_wrap(~set, ncol = 3) +
  
  #  stat_pvalue_manual(Tuk, label = 'stars', size = 7, bracket.size = 2, tip.length = 0.04,
  #                     y.position =c(max(marginal_em$emmean)+max(marginal_em$SE) + 0.03),inherit.aes = FALSE) +
  # Customize the appearance of the plot
  theme_few() +
  labs(
    title = "",
    #fill = 'Geometry: ',
    x = "",
    y = "",
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
              #ylim = c(0.5, 0.8),
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




result_h =  subset(resultd, set %in% c('mystic' ) )

plot <- ggplot(result_h, aes(x = condition, y = x[,"mean"], group = condition, color = condition, fill = condition)) +
  
  geom_hline(yintercept = -0.0, linetype = 'dashed', col = 'black', size = 1.0)  +
  geom_col(position = position_dodge(width=0.7), linewidth = 2, width = 0.6)+
  geom_line(size = 2,  position = position_dodge(width = 0.2)) +
  geom_point(size = 2.5, shape = 23, position = position_dodge(width = 0.2)) +
  
  geom_errorbar(aes(ymin = x[,"mean"] -  x[,"std"], ymax = x[,"mean"] +  x[,"std"]), 
                width = 0.2, position = position_dodge(width = 0.2), size = 2, color = 'black') +
  
  # Facet the plot
  #facet_wrap(~set, ncol = 3) +
  
  #  stat_pvalue_manual(Tuk, label = 'stars', size = 7, bracket.size = 2, tip.length = 0.04,
  #                     y.position =c(max(marginal_em$emmean)+max(marginal_em$SE) + 0.03),inherit.aes = FALSE) +
  # Customize the appearance of the plot
  theme_few() +
  labs(
    title = "",
    #fill = 'Geometry: ',
    x = "",
    y = "",
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
              #ylim = c(0.5, 0.8),
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
    #y = "Aperiodic component, dB",
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
              #ylim = c(0.5, 0.7),
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



