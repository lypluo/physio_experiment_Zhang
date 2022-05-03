#####################
#Aim: tidy the data and have the first visulation
#####################
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
#----------
#1.load the data
#----------
df<-read_xlsx("./data/Ori_data.xlsx",sheet = "re_tidy")
#set variable types:
df$Species<-factor(df$Species,levels = c("S","A","B","U"))
df$Add_sugar<-factor(df$Add_sugar,levels = c("Yes","No"))
df$Root_frac<-factor(df$Root_frac,levels = c("1/2",1))

df<-df %>%
  mutate(Treatment=paste0(Add_sugar,"Sugar-Root",Root_frac)) %>%
  mutate(Treatment=fct_relevel(Treatment,"NoSugar-Root1/2","NoSugar-Root1","YesSugar-Root1/2","YesSugar-Root1"))
#-----------
#(2)visulations
#----------
df %>%
  ggplot()+
  # geom_dotplot(aes(x=Treatment,y=delta_13C,fill=Species),
  #              binaxis='y', stackdir='center',dotsize = 1,binwidth = 1.5, method = "histodot")+
  geom_dotplot(aes(x=Treatment,y=delta_13C,fill=Species),
               binaxis='y', stackdir='center',dotsize = 1)+
  stat_summary(aes(x=Treatment,y=delta_13C),
               fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="red",width=0.1,size=1.2)+
  stat_summary(aes(x=Treatment,y=delta_13C),
               fun = mean,geom="point",color="red",size=3)
#------------
#(3)stats
#-------------