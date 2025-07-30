library(reshape2)
library(data.table)
library(ggplot2)
library(lme4)
# library("ggpubr")
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
#############################veg
path<- "D:/hse/psychodelic_like_experience/data_processing/pletism_base/"
read_plus <- function(flnm) {
  read_csv(flnm) %>% 
  mutate(filename = flnm)
}
df_kgr <-list.files(path,pattern = "*.csv", full.names = T) %>% 
  map_df(~read_plus(.))

replace_values <- function(x) {
  x <- ifelse(substr(x, 1, 1) == "1", "Fractal", 
              ifelse(substr(x, 1, 1) == "2", "kaleidoscope",
                     ifelse(substr(x, 1, 1) == "3", "CubesControl",
                            ifelse(substr(x, 1, 1) == "4", "HoneyComb", x))))
  return(x)
}

# Apply the function to the values
df_kgr$Condition <- replace_values(df_kgr$Condition)
df_kgr <- df_kgr %>%
  mutate(color = case_when(
    Label == 101 ~ "Blue",
    Label == 102 ~ "Purple",
    Label == 103 ~ "Green",
    Label == 104 ~ "Red",
    Label == 105 ~ "Yellow",
    Label == 201 ~ "Blue",
    Label == 202 ~ "Purple",
    Label == 203 ~ "Green",
    Label == 204 ~ "Red",
    Label == 205 ~ "Yellow",
    Label == 301 ~ "BluePink",
    Label == 302 ~ "GreenPurple",
    Label == 303 ~ "RedBlue",
    Label == 304 ~ "OrangePink",
    Label == 305 ~ "GreenOrange",
    Label == 401 ~ "BluePink",
    Label == 402 ~ "GreenPurple",
    Label == 403 ~ "RedBlue",
    Label == 404 ~ "OrangePink",
    Label == 405 ~ "GreenOrange",
    TRUE ~ as.character(Label) # Handle other cases if needed
  ))

df_kgr <- df_kgr %>%
  mutate(subj = substr(filename, nchar(filename) - 7, nchar(filename) - 4))


#df_kgr <- separate(df_kgr, stim, into = c("type", "color"), sep = "_")
#df_kgr$kgr <- df_kgr$mean - df_kgr$base
table <- aggregate(df_kgr$PPG_Rate_Mean, by=list(df_kgr$Condition, df_kgr$color), FUN=function(x) c(mean=mean(x), std=std.error(x))) # spectral density
colnames(table) <- c( 'type', 'color', 'x')

levels(table$color)
table2 <- table %>%
  mutate(hyper = if_else(type %in% c("Fractal", "HoneyComb"), 'psechedelic_like', 'control'))%>% 
  mutate(set = if_else(type %in% c("Fractal", "kaleidoscope"), 'Mystic', 'Hyperbolic'))
table2$type <- factor(table2$type, levels = c("Fractal", "HoneyComb", "kaleidoscope", "CubesControl"))
table2$color <- factor(table2$color, levels = c("Blue","BluePink","Purple","GreenPurple","Green","GreenOrange", "Red","RedBlue","Yellow","OrangePink"))
setwd('C:/Users/User/Documents/pl_project/tables')
write.csv(table2, "ppg_emmeans.csv")

plot <- ggplot(table2, aes(x = type, y = x[,"mean"], group = color, color = color, fill = hyper)) +
  #geom_point(size = 2.5, shape = 23, position = position_dodge(width = 0.2), aes(fill = color)) +
  geom_errorbar(aes(ymin = x[,"mean"] - x[,"std"], ymax = x[,"mean"] + x[,"std"]), 
                 width = 1, position = position_dodge(width = 0.5), size = 2) +
  geom_line(size = 2, aes(group = color), position = position_dodge(width = 0.5)) +
  geom_point(size = 2.5, shape = 23, position = position_dodge(width = 0.5), aes(fill = hyper)) +
  
  scale_color_manual(breaks = c("Blue","BluePink","Purple","GreenPurple","Green","GreenOrange", "Red","RedBlue","Yellow","OrangePink"),
                    values=c("blue","blue","purple","purple","forestgreen","forestgreen","firebrick1","firebrick1","gold1","gold1")) +
  #scale_fill_manual(breaks = c("Blue","Purple","Green","Red","Yellow","BluePink","GreenPurple","RedBlue","OrangePink","GreenOrange"),
   #                  values=c("blue","purple","green","red","yellow","blue","green","red","magenta","orange")) +
  scale_fill_manual(breaks = c('psechedelic_like', 'control'),
                                      values=c("black","white")) +
                    
  # Facet the plot
  facet_wrap(~set, ncol = 2) +
  

# Customize the appearance of the plot
theme_bw() +
  labs(
    title = "PPG mean values - Baseline (-10 - 0)",
    x = "Stimulus",
    y = "Mean value",
    caption = "Baseline (-10 - 0), SE between subjects"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 15, color = "black"),
    legend.position = "top",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 10),
    plot.caption = element_text(hjust = 0, size = 10)
  )

# Print the plot
print(plot)


table <- aggregate(df_kgr$PPG_Rate_Mean, by=list(df_kgr$Condition), FUN=function(x) c(mean=mean(x), std=std.error(x))) # spectral density
colnames(table) <- c( 'type',  'x')

levels(table$color)
table2 <- table %>%
  mutate(hyper = if_else(type %in% c("Fractal", "HoneyComb"), 'psechedelic_like', 'control'))%>% 
  mutate(set = if_else(type %in% c("Fractal", "kaleidoscope"), 'Mystic', 'Hyperbolic'))
table2$type <- factor(table2$type, levels = c("Fractal", "HoneyComb", "kaleidoscope", "CubesControl"))
#table2$color <- factor(table2$color, levels = c("Blue","BluePink","Purple","GreenPurple","Green","GreenOrange", "Red","RedBlue","Yellow","OrangePink"))


plot <- ggplot(table2, aes(x = type, y = x[,"mean"], group = hyper, color = hyper, fill = hyper)) +
  geom_errorbar(aes(ymin = x[,"mean"] - x[,"std"], ymax = x[,"mean"] + x[,"std"]), 
                width = 0.5, position = position_dodge(width = 0.2), size = 2) +
  geom_line(size = 2, aes(group = hyper), position = position_dodge(width = 0.2)) +
  geom_point(size = 2.5, shape = 23, position = position_dodge(width = 0.2), aes(fill = hyper)) +
  
  scale_color_manual(breaks = c('psechedelic_like', 'control'),
                    values=c("magenta2","olivedrab3")) +
    scale_fill_manual(breaks = c('psechedelic_like', 'control'),
                    values=c("white","white")) +
  
  # Facet the plot
  facet_wrap(~set, ncol = 2) +
  
  
  # Customize the appearance of the plot
  theme_bw() +
  labs(
    title = "PPG mean values - Baseline (-10 - 0)",
    x = "Stimulus",
    y = "Mean value",
    caption = "Baseline (-10 - 0), SE between trials"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 15, color = "black"),
    legend.position = "top",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 10),
    plot.caption = element_text(hjust = 0, size = 10)
  )

# Print the plot
print(plot)

df_kgr <- df_kgr %>%
  mutate(hyper = if_else(Condition %in% c("Fractal", "HoneyComb"), 'psechedelic_like', 'control'))%>% 
  mutate(set = if_else(Condition %in% c("Fractal", "kaleidoscope"), 'Mystic', 'Hyperbolic'))
sets <- unique(df_kgr$set)
p_vals <- data.table()

#cols <- c("psd")
############### for green heads (main_effects) ##############
for (i in 1:length(sets)) {
  temp <-subset(df_kgr, set == sets[i])
  m1 <- lmer(PPG_Rate_Mean ~ hyper*color + (1|subj), data = temp)
  d<-romr.fnc(m1, temp, trim = 3) ##### remove outliers
  data<- d$data 
  m <- lmer(PPG_Rate_Mean ~ hyper*color + (1|subj), data = data)
  an <- anova(m)
  an <- data.table(an,keep.rownames = TRUE)
  an_cols <- c('rn','Sum Sq', 'Mean Sq', 'NumDF',  'DenDF', 'F value' ,'Pr(>F)') 
  an <- an[, ..an_cols]
  an$p_value <- format(an$`Pr(>F)`, digits = 3)
  #an$interval <- j
  #an$interval <- gsub('beta power','',an$interval)
  #an <- dcast(an,formula = interval~rn,value.var = 'Pr(>F)')
  an$set <- sets[i] 
  #an$sensor_name <- files[sensor==i]$Name
  p_vals <- rbind(p_vals,an)
  
}


p_vals[p_value<0.001, stars:='***']
p_vals[p_value<0.01 & p_value>0.001 , stars:='**']
p_vals[p_value<0.05 & p_value>0.01 , stars:='*']
p_vals[p_value>0.05 & p_value<0.1 , stars:='#']

setwd('D:/hse/psychodelic_like_experience/data_processing/stats/')
write.csv(p_vals, "ppg_models.csv")
table1 <- aggregate(df_kgr$PPG_Rate_Mean, by=list( df_kgr$Condition, df_kgr$hyper, df_kgr$set), FUN=function(x) c(mean=mean(x), SE = std.error(x))) # spectral density
colnames(table1)=c('Condition', 'Geometry', 'Set', 'PPG')
table1 <- table1 %>%
mutate(Condition = case_when(
  Condition == 'HoneyComb' ~ "HoneyCombs",
  Condition == 'Fractal' ~ "Fractals",
  Condition == 'kaleidoscope' ~ "Kaleidoscopes",
  Condition == 'CubesControl' ~ "ControlCubes",

  TRUE ~ as.character(Condition) # Handle other cases if needed
))

table1 <- table1 %>%
  mutate(Geometry = case_when(
    Geometry == 'psechedelic_like' ~ "non_Euclidean",
    Geometry == 'control' ~ "Euclidean",

    
    TRUE ~ as.character(Geometry) # Handle other cases if needed
  ))

an_hyp = p_vals[4]
an_hyp$group1 <- 'HoneyCombs'
an_hyp$group2 <- 'ControlCubes'

#############################################
table_hyp = subset(table1, table1$Set == 'Hyperbolic')
plot <- ggplot(table_hyp, aes(x = Condition, y = PPG[,"mean"], group = Condition, color = Condition, fill = Condition)) +
  geom_hline(yintercept = -0.0, linetype = 'dashed', col = 'black', size = 1.0)  +
  geom_col(position = position_dodge(width=0.7), linewidth = 2, width = 0.6)+
  
    geom_errorbar(aes(ymin = PPG[,"mean"] - PPG[,"SE"], ymax = PPG[,"mean"] + PPG[,"SE"]), 
                width = 0.2, position = position_dodge(width = 0.2), size = 2, color ='black') +
  geom_point(size = 2.5, shape = 23, position = position_dodge(width = 0.2), aes(fill = Condition)) +
  
  scale_color_manual(breaks = c('HoneyCombs', 'ControlCubes'),
                     values=c("#ffa600","#bc5090")) +
  scale_fill_manual(breaks = c('HoneyCombs', 'ControlCubes'),
                    values=c("#ffa600","#bc5090")) +
  
  # Facet the plot

  
  # Customize the appearance of the plot
  theme_classic() +
  labs(
    #title = "PPG mean values - Baseline (-10 - 0)",
    x = "Condition",
    y = "Heart rate change, 1/min",
    #caption = "Baseline (-10 - 0), SE between trials"
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
  stat_pvalue_manual(an_hyp, label = 'stars', size =10, bracket.size = 2, tip.length = 0.04,
                     y.position =c(max(table_hyp$PPG[,"mean"])+max(table_hyp$PPG[,"SE"]) + 0.2),
                     inherit.aes = FALSE) 
  


plot <- ggpar(plot,
              ylim = c(-3, 3),
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


table_hyp = subset(table1, table1$Set != 'Hyperbolic')
an_hyp = p_vals[1]
an_hyp$group1 <- 'Fractals'
an_hyp$group2 <- 'Kaleidoscopes'

plot <- ggplot(table_hyp, aes(x = Condition, y = PPG[,"mean"], group = Condition, color = Condition, fill = Condition)) +
  geom_hline(yintercept = -0.0, linetype = 'dashed', col = 'black', size = 1.0)  +
  geom_col(position = position_dodge(width=0.7), linewidth = 2, width = 0.6)+
  
  geom_errorbar(aes(ymin = PPG[,"mean"] - PPG[,"SE"], ymax = PPG[,"mean"] + PPG[,"SE"]), 
                width = 0.2, position = position_dodge(width = 0.2), size = 2, color ='black') +
  geom_point(size = 2.5, shape = 23, position = position_dodge(width = 0.2), aes(fill = Condition)) +
  
  scale_color_manual(breaks = c('Fractals', 'Kaleidoscopes'),
                     values=c("#ff6361","#58508d")) +
  scale_fill_manual(breaks = c('Fractals', 'Kaleidoscopes'),
                    values=c("#ff6361","#58508d")) +
  stat_pvalue_manual(an_hyp, label = 'stars', size =10, bracket.size = 2, tip.length = 0.04,
                     y.position =c(max(table_hyp$PPG[,"mean"])+max(table_hyp$PPG[,"SE"]) + 0.2),
                     inherit.aes = FALSE) +

  # Facet the plot
  
  
  # Customize the appearance of the plot
  theme_classic() +
  labs(
    #title = "PPG mean values - Baseline (-10 - 0)",
    x = "Condition",
    y = "Heart rate change, 1/min",
    #caption = "Baseline (-10 - 0), SE between trials"
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
              ylim = c(-3, 3),
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


# Print the plot
print(plot)

############################################

plot <- ggplot(marginal_em, aes(x = hyper, y = emmean, group = Geometry, color = Geometry, fill = Geometry)) +
  
  geom_hline(yintercept = -0.0, linetype = 'dashed', col = 'black', size = 1.0)  +
  geom_col(position = position_dodge(width=0.7), linewidth = 2, width = 0.6)+
  scale_x_discrete(labels = c( "HoneyCombs", "ControlCubes")) +
  geom_line(size = 2,  position = position_dodge(width = 0.2)) +
  geom_point(size = 2.5, shape = 23, position = position_dodge(width = 0.2)) +
  
  scale_color_manual(breaks = c('HoneyCombs', 'ControlCubes'),
                     values=c("magenta2","olivedrab3")) +
  scale_fill_manual(breaks = c('HoneyCombs', 'ControlCubes'),
                    values=c("magenta2","olivedrab3")) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                width = 0.2, position = position_dodge(width = 0.2), size = 2, color = 'black') +
  
  # Facet the plot
  #facet_wrap(~set, ncol = 2) +
  
  stat_pvalue_manual(Tuk, label = 'stars', size = 7, bracket.size = 2, tip.length = 0.04,
                     y.position =c(max(marginal_em$emmean)+max(marginal_em$SE) + 0.03),inherit.aes = FALSE) +
  # Customize the appearance of the plot
  theme_classic() +
  labs(
    title = "",
    #fill = 'Geometry: ',
    x = "",
    y = "Alpha power, z-score",
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
              ylim = c(-0.18, 0.15),
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


###################################################
emm_options(lmerTest.limit = 6000)

Tuk1<- NULL
temp <-subset(df_kgr, set == 'Mystic')
m <- lmer(PPG_Rate_Mean ~ hyper*color + (1|subj), data = temp)
Tuk1<-data.table(summary(emmeans(m, pairwise ~ color, adjust = 'Tukey',lmer.df = "satterthwaite"))$contrasts)
Tuk1 <- Tuk1[, group1:=gsub(' -.*', '', contrast)][, group2:=gsub('.*- ', '', contrast)]
Tuk1 <- Tuk1[p.value<0.1, p_significant:=format(p.value, digits = 3)]

n <- Tuk1[!is.na(p_significant), .N]

Tuk1[p.value<0.001, stars:='***']
Tuk1[p.value<0.01 & p.value>0.001 , stars:='**']
Tuk1[p.value<0.05 & p.value>0.01 , stars:='*']
Tuk1[p.value>0.05 & p.value<0.1 , stars:='#']
setwd('D:/hse/psychodelic_like_experience/data_processing/stats/')
write.csv(Tuk1, "ppg_mystic_tuks_color.csv")

marginal_em <- emmeans(m, ~ as.factor(color),level = 0.95,lmer.df = "satterthwaite")
marginal_em<-as.data.frame(marginal_em)
write.csv(marginal_em, "ppg_mystic_emmeans_color.csv")


marginal_em$type = 'type'
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
    y = "",
    caption = "",
    
  ) +
  stat_pvalue_manual(Tuk1, label = 'stars', size = 7, bracket.size = 2, tip.length = 0.04,
                     y.position =c(0),
                     step.increase = 0.07,
                     inherit.aes = FALSE) +
  
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
              
)#+
  #annotate("text", x = 1:5, y = 0.875, label = Tuk1$stars, size = 7)
# Print the plot
print(plot)


Tuk1<- NULL
temp <-subset(df_kgr, set == 'Hyperbolic')
m <- lmer(PPG_Rate_Mean ~ hyper*color + (1|subj), data = temp)
Tuk1<-data.table(summary(emmeans(m, pairwise ~ hyper, adjust = 'Tukey',lmer.df = "satterthwaite"))$contrasts)
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
write.csv(Tuk1, "ppg_hyper_Tukes_hyper.csv")


#if (n>1){
#  Tuk <- Tuk[!is.na(p_significant), y.position := seq((thr1+0.01), (thr1+0.3), 0.29/(n-1))]
#} else {
#  Tuk <- Tuk[!is.na(p_significant), y.position := thr1+0.1]
#}
#y.position<-Tuk$y.position
temp <-subset(df_kgr, set == sets[1])
m1 <- lmer(PPG_Rate_Mean ~ hyper*color + (1|subj), data = temp)
d<-romr.fnc(m1, temp, trim = 3) ##### remove outliers
data<- d$data 
m <- lmer(PPG_Rate_Mean ~ hyper*color + (1|subj), data = data)

#Tuk$emmean<-y.position
Tuk2<- NULL
#thr1 <- max(data_test[, mean(data_test) + sterr(data_test), by=c('stimuli', 're')]$V1) 
#thr1 <- thr1+0.02 #for RT

#thr1_min <- min(means[!is.na(mean_beta), mean(mean_beta) - sterr(mean_beta), by=c('trial_type')]$V1) 

Tuk2<-data.table(summary(emmeans(m, pairwise ~ hyper|color, adjust = 'Tukey',lmer.df = "satterthwaite"))$contrasts)
Tuk2 <- Tuk2[, group1:=gsub(' -.*', '', contrast)][, group2:=gsub('.*- ', '', contrast)]
Tuk2 <- Tuk2[p.value<0.1, p_significant:=format(p.value, digits = 3)]

n <- Tuk2[!is.na(p_significant), .N]

Tuk2[p.value<0.001, stars:='***']
Tuk2[p.value<0.01 & p.value>0.001 , stars:='**']
Tuk2[p.value<0.05 & p.value>0.01 , stars:='*']
Tuk2[p.value>0.05 & p.value<0.1 , stars:='#']





z_scores <- table %>%
  group_by(subject) %>%
  mutate(z_score = scale(kgr))
table1 <- aggregate(z_scores$z_score, by=list(z_scores$type, z_scores$color), FUN=function(x) c(mean=mean(x))) # spectral density
colnames(table1) <- c('type',  'color', 'z_score')

table2 <- table1 %>%
  mutate(hyper = if_else(type %in% c("Fractal", "HoneyComb"), 'hyperbolic', 'nonhyperbolic'))%>% 
  mutate(set = if_else(type %in% c("Fractal", "kaleidoscope"), 'set1', 'set2'))
plot <- ggplot(table1, aes(x = type, y = z_score, group = color, color = color)) +
  geom_point(size = 3, position = position_dodge(width = 0.2)) +
  # geom_errorbar(aes(ymin = x[,"mean"] - x[,"std"], ymax = x[,"mean"] + x[,"std"]), 
  #               width = 0.2, position = position_dodge(width = 0.2)) +
  #geom_line(aes(group = color), position = position_dodge(width = 0.2)) +
  #scale_color_manual(breaks = c("compl_plus", "pictu_plus",  "sound_plus", "compl_minus",  "pictu_minus",  "sound_minus"),
  #                  values=c("darkred", "darkgreen", "darkblue", "red", "green", "blue")) +
  #scale_color_manual(breaks = c("non_reinforced", "reinforced"),
  #                   values=c("green", "red")) +
  
  # Facet the plot
  #facet_wrap(~set, ncol = 1) +
  

# Customize the appearance of the plot
theme_minimal() +
  labs(
    title = "GSR mean values - Baseline (-5 - 0)",
    x = "Type",
    y = "Mean",
    caption = ""
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    legend.position = "top"
  )

# Print the plot
print(plot)


#########################################
path<- "D:/hse/psychodelic_like_experience/data_processing/gsr_intervalrelated/"
read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}
df_kgr <-list.files(path,pattern = "*.csv", full.names = T) %>% 
  map_df(~read_plus(.))

replace_values <- function(x) {
  x <- ifelse(substr(x, 1, 1) == "1", "Fractal", 
              ifelse(substr(x, 1, 1) == "2", "kaleidoscope",
                     ifelse(substr(x, 1, 1) == "3", "CubesControl",
                            ifelse(substr(x, 1, 1) == "4", "HoneyComb", x))))
  return(x)
}

# Apply the function to the values
df_kgr$Condition <- replace_values(df_kgr$Label)
df_kgr <- df_kgr %>%
  mutate(color = case_when(
    Label == 101 ~ "Blue",
    Label == 102 ~ "Purple",
    Label == 103 ~ "Green",
    Label == 104 ~ "Red",
    Label == 105 ~ "Yellow",
    Label == 201 ~ "Blue",
    Label == 202 ~ "Purple",
    Label == 203 ~ "Green",
    Label == 204 ~ "Red",
    Label == 205 ~ "Yellow",
    Label == 301 ~ "BluePink",
    Label == 302 ~ "GreenPurple",
    Label == 303 ~ "RedBlue",
    Label == 304 ~ "OrangePink",
    Label == 305 ~ "GreenOrange",
    Label == 401 ~ "BluePink",
    Label == 402 ~ "GreenPurple",
    Label == 403 ~ "RedBlue",
    Label == 404 ~ "OrangePink",
    Label == 405 ~ "GreenOrange",
    TRUE ~ as.character(Label) # Handle other cases if needed
  ))

df_kgr <- df_kgr %>%
  mutate(subj = substr(filename, nchar(filename) - 7, nchar(filename) - 4))

colnames(df_kgr)
[1] "Label"                    "SCR_Peaks_N"              "SCR_Peaks_Amplitude_Mean" "EDA_Tonic_SD"            
[5] "EDA_Sympathetic"          "EDA_SympatheticN"         "EDA_Autocorrelation"      "filename"                
[9] "color"                    "Condition"                "subj"    
table <- aggregate(df_kgr$SCR_Peaks_Amplitude_Mean, by=list(df_kgr$Condition, df_kgr$color, df_kgr$subj), FUN=function(x) c(sum=sum(x))) # spectral density
colnames(table) <- c( 'type', 'color', 'subj', 'x')

table2 <- table %>%
  mutate(hyper = if_else(type %in% c("Fractal", "HoneyComb"), 'psechedelic_like', 'control'))%>% 
  mutate(set = if_else(type %in% c("Fractal", "kaleidoscope"), 'Mystic', 'Hyperbolic'))
table2$type <- factor(table2$type, levels = c("Fractal", "HoneyComb", "kaleidoscope", "CubesControl"))
#table2$color <- factor(table2$color, levels = c("Blue","BluePink","Purple","GreenPurple","Green","GreenOrange", "Red","RedBlue","Yellow","OrangePink"))
colnames(table3)
df_kgr <- df_kgr %>%
  mutate(hyper = if_else(Condition %in% c("Fractal", "HoneyComb"), 'psechedelic_like', 'control'))%>% 
  mutate(set = if_else(Condition %in% c("Fractal", "kaleidoscope"), 'Mystic', 'Hyperbolic'))
sets <- unique(df_kgr$set)


table4 <- aggregate(df_kgr$SCR_Peaks_N, by=list(df_kgr$Condition), FUN=function(x) c(sum=sum(x))) # spectral density
colnames(table) <- c( 'type',  'x')
table4
mystic = 77+78 #155
hyperb = 88+90 #178

binom.test(77, mystic, p = 0.5,
           alternative = c("two.sided", "less", "greater"),
           conf.level = 0.95)

bin = binom.test(90, hyperb, p = 0.5,
           alternative = c("two.sided", "less", "greater"),
           conf.level = 0.95)
bin<- data.table(bin,keep.rownames = TRUE)

table3_hyper <- filter(table3, set == 'Hyperbolic')
res.fried <- table3_hyper %>% friedman_test(x ~ hyper |color)
res.fried
pwc <- table3_hyper %>%
  wilcox_test(x ~ hyper, paired = TRUE, p.adjust.method = "bonferroni")
pwc
pwc <- table3_hyper %>%
  wilcox_test(x ~ color, paired = TRUE, p.adjust.method = "bonferroni")
pwc



table3_hyper <- filter(table3, set == 'Mystic')
res.fried <- table3_hyper %>% friedman_test(x ~ hyper |color)
res.fried
pwc <- table3_hyper %>%
  wilcox_test(x ~ hyper, paired = TRUE, p.adjust.method = "bonferroni")
pwc
pwc <- table3_hyper %>%
  wilcox_test(x ~ color, paired = TRUE, p.adjust.method = "bonferroni")
pwc

# Visualization: box plots with p-values
pwc <- pwc %>% #add_xy_position(x = "hyper")
ggboxplot(table3_hyper,aes( x = hyper , y = table3_hyper$x), add = "point") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried,  detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

table3 <- aggregate(df_kgr$SCR_Peaks_Amplitude_Mean, by=list(df_kgr$Condition, df_kgr$color), FUN=function(x) c(sum=sum(x), std=std.error(x))) # spectral density
colnames(table3) <- c( 'type', 'color',  'x')
table3 <- table3 %>%
  mutate(hyper = if_else(type %in% c("Fractal", "HoneyComb"), 'psechedelic_like', 'control'))%>% 
  mutate(set = if_else(type %in% c("Fractal", "kaleidoscope"), 'Mystic', 'Hyperbolic'))
#table2$type <- factor(table2$type, levels = c("Fractal", "HoneyComb", "kaleidoscope", "CubesControl"))




p_vals <- data.table()
#cols <- c("psd")
############### for green heads (main_effects) ##############
for (i in 1:length(sets)) {
  temp <-subset(table3, set == sets[i])
  

  table3_hyper <- filter(table2, set == 'Hyperbolic')
  
  p_vals <- data.table()
  #cols <- c("psd")
  ############### for green heads (main_effects) ##############
  for (i in 1:length(sets)) {
    temp <-subset(df_kgr, set == sets[i])
    m1 <- lmer(SCR_Peaks_N ~ hyper*color + (1|subj), data = temp)
    d<-romr.fnc(m1, temp, trim = 3) ##### remove outliers
    data<- d$data 
    m <- lmer(SCR_Peaks_N ~ hyper*color + (1|subj), data = data)
    an <- anova(m)
    an <- data.table(an,keep.rownames = TRUE)
    an_cols <- c('Sum Sq',  'Mean Sq', 'NumDF', 'DenDF', 'F value', 'Pr(>F)') 
    an <- an[, ..an_cols]
    #an$p_value <- format(an$`Pr(>F)`, digits = 3)
    #an$interval <- j
    #an$interval <- gsub('beta power','',an$interval)
    #an <- dcast(an,formula = interval~rn,value.var = 'Pr(>F)')
    an$set <- sets[i] 
    #an$sensor_name <- files[sensor==i]$Name
    p_vals <- rbind(p_vals,an)
    
  }  
  
  temp <-subset(table2, set == sets[2])
  m1 <- lmer(x ~ hyper*color + (1|subj), data = temp)
  d<-romr.fnc(m1, temp, trim = 3) ##### remove outliers
  data<- d$data 
  m <- lmer(x ~ hyper*color + (1|subj), data = data)
  emmeans(m, pairwise ~ color|hyper)
  

  Tuk1<- NULL
  Tuk1<-data.table(summary(emmeans(m, pairwise ~ color|hyper, adjust = 'Tukey'))$contrasts)
  Tuk1 <- Tuk1[, group1:=gsub(' -.*', '', contrast)][, group2:=gsub('.*- ', '', contrast)]
  Tuk1 <- Tuk1[p.value<0.1, p_significant:=format(p.value, digits = 3)]
  
  n <- Tuk1[!is.na(p_significant), .N]
  
  Tuk1[p.value<0.001, stars:='***']
  Tuk1[p.value<0.01 & p.value>0.001 , stars:='**']
  Tuk1[p.value<0.05 & p.value>0.01 , stars:='*']
  Tuk1[p.value>0.05 & p.value<0.1 , stars:='#']
  
  #if (n>1){
  #  Tuk <- Tuk[!is.na(p_significant), y.position := seq((thr1+0.01), (thr1+0.3), 0.29/(n-1))]
  #} else {
  #  Tuk <- Tuk[!is.na(p_significant), y.position := thr1+0.1]
  #}
  #y.position<-Tuk$y.position

  #Tuk$emmean<-y.position
  Tuk2<- NULL
  #thr1 <- max(data_test[, mean(data_test) + sterr(data_test), by=c('stimuli', 're')]$V1) 
  #thr1 <- thr1+0.02 #for RT
  
  #thr1_min <- min(means[!is.na(mean_beta), mean(mean_beta) - sterr(mean_beta), by=c('trial_type')]$V1) 
  
  Tuk2<-data.table(summary(emmeans(m, pairwise ~ hyper|color, adjust = 'Tukey',lmer.df = "satterthwaite", lower.tail = TRUE))$contrasts)
  Tuk2 <- Tuk2[, group1:=gsub(' -.*', '', contrast)][, group2:=gsub('.*- ', '', contrast)]
  Tuk2 <- Tuk2[p.value<0.1, p_significant:=format(p.value, digits = 3)]
  
  n <- Tuk2[!is.na(p_significant), .N]
  
  Tuk2[p.value<0.001, stars:='***']
  Tuk2[p.value<0.01 & p.value>0.001 , stars:='**']
  Tuk2[p.value<0.05 & p.value>0.01 , stars:='*']
  Tuk2[p.value>0.05 & p.value<0.1 , stars:='#']
  
  setwd('D:/hse/psychodelic_like_experience/data_processing/stats/')
  write.csv(p_vals, "ppg_models.csv")
  
    
  p_vals <- data.table()
  
  
p_vals <- data.table()
#cols <- c("psd")
############### for green heads (main_effects) ##############
for (i in 1:length(sets)) {
  temp <-subset(df_kgr, set == sets[i])
  m1 <- lmer(EDA_Peak_Amplitude ~ hyper*color + (1|subj), data = temp)
  d<-romr.fnc(m1, temp, trim = 3) ##### remove outliers
  data<- d$data 
  m <- lmer(EDA_Peak_Amplitude ~ hyper*color + (1|subj), data = data)
  an <- anova(m)
  an <- data.table(an,keep.rownames = TRUE)
  an_cols <- c('Sum Sq',  'Mean Sq', 'NumDF', 'DenDF', 'F value', 'Pr(>F)') 
  an <- an[, ..an_cols]
  #an$p_value <- format(an$`Pr(>F)`, digits = 3)
  #an$interval <- j
  #an$interval <- gsub('beta power','',an$interval)
  #an <- dcast(an,formula = interval~rn,value.var = 'Pr(>F)')
  an$set <- sets[i] 
  #an$sensor_name <- files[sensor==i]$Name
  p_vals <- rbind(p_vals,an)
  
}
#cols <- c("psd")
############### for green heads (main_effects) ##############
for (i in 1:length(sets)) {
  temp <-subset(df_kgr, set == sets[i])
  m1 <- lmer(EDA_Peak_Amplitude ~ hyper*color + (1|subj), data = temp)
  d<-romr.fnc(m1, temp, trim = 3) ##### remove outliers
  data<- d$data 
  m <- lmer(EDA_Peak_Amplitude ~ hyper*color + (1|subj), data = data)
  an <- anova(m)
  an <- data.table(an,keep.rownames = TRUE)
  an_cols <- c('Sum Sq',  'Mean Sq', 'NumDF', 'DenDF', 'F value', 'Pr(>F)') 
  an <- an[, ..an_cols]
  #an$p_value <- format(an$`Pr(>F)`, digits = 3)
  #an$interval <- j
  #an$interval <- gsub('beta power','',an$interval)
  #an <- dcast(an,formula = interval~rn,value.var = 'Pr(>F)')
  an$set <- sets[i] 
  #an$sensor_name <- files[sensor==i]$Name
  p_vals <- rbind(p_vals,an)
  
}
  
  
  
  
  
  
  
p_vals <- data.table()
#cols <- c("psd")
############### for green heads (main_effects) ##############
for (i in 1:length(sets)) {
  temp <-subset(table3, set == sets[i])
  m1 <- lmer(x[,"sum"] ~ hyper|color, data = temp)
  d<-romr.fnc(m1, temp, trim = 3) ##### remove outliers
  data<- d$data 
  m <- lmer(x[,"sum"] ~ hyper|color, data = data)
  an <- anova(m)
  an <- data.table(an,keep.rownames = TRUE)
  an_cols <- c('Sum Sq',  'Mean Sq', 'NumDF', 'DenDF', 'F value', 'Pr(>F)') 
  an <- an[, ..an_cols]
  #an$p_value <- format(an$`Pr(>F)`, digits = 3)
  #an$interval <- j
  #an$interval <- gsub('beta power','',an$interval)
  #an <- dcast(an,formula = interval~rn,value.var = 'Pr(>F)')
  an$set <- sets[i] 
  #an$sensor_name <- files[sensor==i]$Name
  p_vals <- rbind(p_vals,an)
  
}



Tuk1<- NULL
Tuk1<-data.table(summary(emmeans(m, pairwise ~ color|hyper, adjust = 'Tukey',lmer.df = "satterthwaite"))$contrasts)
Tuk1 <- Tuk1[, group1:=gsub(' -.*', '', contrast)][, group2:=gsub('.*- ', '', contrast)]
Tuk1 <- Tuk1[p.value<0.1, p_significant:=format(p.value, digits = 3)]

n <- Tuk1[!is.na(p_significant), .N]

Tuk1[p.value<0.001, stars:='***']
Tuk1[p.value<0.01 & p.value>0.001 , stars:='**']
Tuk1[p.value<0.05 & p.value>0.01 , stars:='*']
Tuk1[p.value>0.05 & p.value<0.1 , stars:='#']

#if (n>1){
#  Tuk <- Tuk[!is.na(p_significant), y.position := seq((thr1+0.01), (thr1+0.3), 0.29/(n-1))]
#} else {
#  Tuk <- Tuk[!is.na(p_significant), y.position := thr1+0.1]
#}
#y.position<-Tuk$y.position
temp <-subset(df_kgr, set == sets[1])
m1 <- lmer(EDA_SCR ~ hyper*color + (1|subj), data = temp)
d<-romr.fnc(m1, temp, trim = 3) ##### remove outliers
data<- d$data 
m <- lmer(EDA_SCR ~ hyper*color + (1|subj), data = data)

#Tuk$emmean<-y.position
Tuk2<- NULL
#thr1 <- max(data_test[, mean(data_test) + sterr(data_test), by=c('stimuli', 're')]$V1) 
#thr1 <- thr1+0.02 #for RT

#thr1_min <- min(means[!is.na(mean_beta), mean(mean_beta) - sterr(mean_beta), by=c('trial_type')]$V1) 

Tuk2<-data.table(summary(emmeans(m, pairwise ~ hyper|color, adjust = 'Tukey',lmer.df = "satterthwaite"))$contrasts)
Tuk2 <- Tuk2[, group1:=gsub(' -.*', '', contrast)][, group2:=gsub('.*- ', '', contrast)]
Tuk2 <- Tuk2[p.value<0.1, p_significant:=format(p.value, digits = 3)]

n <- Tuk2[!is.na(p_significant), .N]

Tuk2[p.value<0.001, stars:='***']
Tuk2[p.value<0.01 & p.value>0.001 , stars:='**']
Tuk2[p.value<0.05 & p.value>0.01 , stars:='*']
Tuk2[p.value>0.05 & p.value<0.1 , stars:='#']

setwd('D:/hse/psychodelic_like_experience/data_processing/stats/')
write.csv(p_vals, "ppg_models.csv")

colnames(df_kgr)
p_vals <- data.table()
#cols <- c("psd")
############### for green heads (main_effects) ##############
for (i in 1:length(sets)) {
  temp <-subset(df_kgr, set == sets[i])
  m1 <- lmer(EDA_Peak_Amplitude ~ hyper*color + (1|subj), data = temp)
  d<-romr.fnc(m1, temp, trim = 3) ##### remove outliers
  data<- d$data 
  m <- lmer(EDA_Peak_Amplitude ~ hyper*color + (1|subj), data = data)
  an <- anova(m)
  an <- data.table(an,keep.rownames = TRUE)
  an_cols <- c('Sum Sq',  'Mean Sq', 'NumDF', 'DenDF', 'F value', 'Pr(>F)') 
  an <- an[, ..an_cols]
  #an$p_value <- format(an$`Pr(>F)`, digits = 3)
  #an$interval <- j
  #an$interval <- gsub('beta power','',an$interval)
  #an <- dcast(an,formula = interval~rn,value.var = 'Pr(>F)')
  an$set <- sets[i] 
  #an$sensor_name <- files[sensor==i]$Name
  p_vals <- rbind(p_vals,an)
  
}


"SCR_Peaks_N"              "SCR_Peaks_Amplitude_Mean"
table3 <- aggregate(df_kgr$SCR_Peaks_N, by=list(df_kgr$Condition, df_kgr$color), FUN=function(x) c(mean=mean(x), std=std.error(x))) # spectral density
colnames(table3) <- c( 'type', 'color',  'x')
table3 <- table3 %>%
  mutate(hyper = if_else(type %in% c("Fractal", "HoneyComb"), 'psechedelic_like', 'control'))%>% 
  mutate(set = if_else(type %in% c("Fractal", "kaleidoscope"), 'Mystic', 'Hyperbolic'))



colnames(table3)
table3_hyper <- filter(table3, set == 'Mystic')
plot <- ggplot(table3_hyper, aes(x = color, y = x[,"mean"], group = hyper, color = color, fill = color)) +
  geom_col(position = position_dodge(width = 1), aes(x = color, alpha = hyper))+
  #geom_line(size = 2, aes(group = color), position = position_dodge(width = 0.5)) +
  #geom_point(size = 2.5, shape = 23, position = position_dodge(width = 1), aes(fill = hyper)) +
  
  scale_fill_manual(breaks = c("Blue","Purple","Green", "Red","Yellow"),
                     values=c("blue","purple","forestgreen","firebrick1","gold1")) +
  scale_color_manual(breaks = c("Blue","Purple","Green", "Red","Yellow"),
                     values=c("blue","purple","forestgreen","firebrick1","gold1")) +

  # Facet the plot
  #facet_wrap(~set, ncol = 2) +
  
  geom_errorbar(aes(ymin = x[,"mean"] - x[,"std"], ymax = x[,"mean"] + x[,"std"]), 
                width = 0.5, position = position_dodge(width = 1), size = 1, color='black') +
  
  
  # Customize the appearance of the plot
  theme_bw() +
  labs(
    title = "Number of SCR, Mystic",
    x = "Color",
    y = "Number",
    caption = "EDA, SE between trials"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 15, color = "black"),
    legend.position = "top",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 10),
    plot.caption = element_text(hjust = 0, size = 10)
  )

# Print the plot
print(plot)


table3_hyper <- filter(table3, set == 'Hyperbolic')
plot <- ggplot(table3_hyper, aes(x = color, y = x[,"mean"], group = hyper, color = color, fill = color)) +
  #geom_errorbar(aes(ymin = x[,"sum"] - x[,"std"], ymax = x[,"sum"] + x[,"std"]), 
  #             width = 0.5, position = position_dodge(width = 0.2), size = 2) +
  geom_col(position = position_dodge(width = 1), aes(x = color, alpha = hyper))+
  #geom_line(size = 2, aes(group = color), position = position_dodge(width = 0.5)) +
  #geom_point(size = 2.5, shape = 23, position = position_dodge(width = 1), aes(fill = hyper)) +
  scale_fill_manual(breaks = c("BluePink","GreenPurple","GreenOrange", "RedBlue","OrangePink"),
                    values=c("blue","purple","forestgreen","firebrick1","gold1")) +
  scale_color_manual(breaks = c("BluePink","GreenPurple","GreenOrange", "RedBlue","OrangePink"),
                     values=c("blue","purple","forestgreen","firebrick1","gold1")) +
  
  # Facet the plot
  #facet_wrap(~set, ncol = 2) +
  geom_errorbar(aes(ymin = x[,"mean"] - x[,"std"], ymax = x[,"mean"] + x[,"std"]), 
                width = 0.5, position = position_dodge(width = 1), size = 1, color='black') +
  
  
  
  # Customize the appearance of the plot
  theme_bw() +
  labs(
    title = "Number of SCR, Hyperbolic",
    x = "Color",
    y = "Number",
    caption = "EDA, SE between trials"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 15, color = "black"),
    legend.position = "top",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 10),
    plot.caption = element_text(hjust = 0, size = 10)
  )

# Print the plot
print(plot)








table3 <- aggregate(df_kgr$SCR_Peaks_Amplitude_Mean, by=list(df_kgr$Condition, df_kgr$color), FUN=function(x) c(mean=mean(x), std=std.error(x))) # spectral density
colnames(table3) <- c( 'type', 'color',  'x')
table3 <- table3 %>%
  mutate(hyper = if_else(type %in% c("Fractal", "HoneyComb"), 'psechedelic_like', 'control'))%>% 
  mutate(set = if_else(type %in% c("Fractal", "kaleidoscope"), 'Mystic', 'Hyperbolic'))
table2$type <- factor(table2$type, levels = c("Fractal", "HoneyComb", "kaleidoscope", "CubesControl"))



colnames(table3)
table3_hyper <- filter(table3, set == 'Mystic')
plot <- ggplot(table3_hyper, aes(x = color, y = x[,"mean"], group = hyper, color = color, fill = color)) +
  #geom_errorbar(aes(ymin = x[,"sum"] - x[,"std"], ymax = x[,"sum"] + x[,"std"]), 
  #             width = 0.5, position = position_dodge(width = 0.2), size = 2) +
  geom_col(position = position_dodge(width = 1), aes(x = color, alpha = hyper))+
  #geom_line(size = 2, aes(group = color), position = position_dodge(width = 0.5)) +
  #geom_point(size = 2.5, shape = 23, position = position_dodge(width = 1), aes(fill = hyper)) +
  
  scale_fill_manual(breaks = c("Blue","Purple","Green", "Red","Yellow"),
                    values=c("blue","purple","forestgreen","firebrick1","gold1")) +
  scale_color_manual(breaks = c("Blue","Purple","Green", "Red","Yellow"),
                     values=c("blue","purple","forestgreen","firebrick1","gold1")) +
  
  # Facet the plot
  #facet_wrap(~set, ncol = 2) +
  geom_errorbar(aes(ymin = x[,"mean"] - x[,"std"], ymax = x[,"mean"] + x[,"std"]), 
                width = 0.5, position = position_dodge(width = 1), size = 1, color='black') +
  
  
  
  # Customize the appearance of the plot
  theme_bw() +
  labs(
    title = "Mean Peak Amplitude, Mystic",
    x = "Color",
    y = "mS",
    caption = "EDA, SE between trials"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 15, color = "black"),
    legend.position = "top",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 10),
    plot.caption = element_text(hjust = 0, size = 10)
  )

# Print the plot
print(plot)


table3_hyper <- filter(table3, set == 'Hyperbolic')
plot <- ggplot(table3_hyper, aes(x = color, y = x[,"mean"], group = hyper, color = color, fill = color)) +
  #geom_errorbar(aes(ymin = x[,"sum"] - x[,"std"], ymax = x[,"sum"] + x[,"std"]), 
  #             width = 0.5, position = position_dodge(width = 0.2), size = 2) +
  geom_col(position = position_dodge(width = 1), aes(x = color, alpha = hyper))+
  #geom_line(size = 2, aes(group = color), position = position_dodge(width = 0.5)) +
  #geom_point(size = 2.5, shape = 23, position = position_dodge(width = 1), aes(fill = hyper)) +
  scale_fill_manual(breaks = c("BluePink","GreenPurple","GreenOrange", "RedBlue","OrangePink"),
                    values=c("blue","purple","forestgreen","firebrick1","gold1")) +
  scale_color_manual(breaks = c("BluePink","GreenPurple","GreenOrange", "RedBlue","OrangePink"),
                     values=c("blue","purple","forestgreen","firebrick1","gold1")) +
  
  # Facet the plot
  #facet_wrap(~set, ncol = 2) +
  geom_errorbar(aes(ymin = x[,"mean"] - x[,"std"], ymax = x[,"mean"] + x[,"std"]), 
                width = 0.5, position = position_dodge(width = 1), size = 1, color='black') +
  
  
  
  # Customize the appearance of the plot
  theme_bw() +
  labs(
    title = "Mean Peak Amplitude, Hyperbolic",
    x = "Color",
    y = "mS",
    caption = "EDA, SE between trials"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 15, color = "black"),
    legend.position = "top",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 10),
    plot.caption = element_text(hjust = 0, size = 10)
  )

# Print the plot
print(plot)


































data <- data.frame(Factor1, Factor2, Outcome)

# Проведение теста на равенство долей для каждой комбинации факторов
test_result <- prop.test(table(data$Outcome, data$Factor1, data$Factor2), by = c("Factor1", "Factor2"))
print(test_result)



################опроснички
read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm,
           number = row_number())
}
path<- "D:/hse/psychodelic_like_experience/data_processing/otvety/"

otvety <-list.files(path,pattern = "*.csv", full.names = T) %>% 
  map_df(~read_plus(.))
otvety <- otvety %>%
  mutate(subj = substr(filename, nchar(filename) - 7, nchar(filename) - 4))
colnames(otvety) <- c( "Subject",   "Condition", "Otvet",    "filename",  "number",   "subj")

otvety <- otvety[, c( "Subject",   "Condition", "Otvet",    "number")]



replacements <- list(
  "HoneyComb" = "HoneyComb/",
  "kaleidoscope" = "kaleidoscope/",
  "Fractal" = "Fractal/",
  "CubesControl" = "CubesControl/"
)
library(stringr)


for (pattern in names(replacements)) {
  replacement <- replacements[[pattern]]
  
  otvety$Condition <- gsub(pattern, replacement, otvety$Condition)
}
otvety <- separate(otvety, Condition, into = c("type", "color"), sep = "/")
write.csv(otvety, "D:/hse/psychodelic_like_experience/data_processing/otvety.csv")











