##########################################

rm(list =ls())

library(openxlsx)
library(ggplot2)
library(dplyr)
library(magrittr)
library(tidyr)


setwd("/Users/luchobarajas/Documents/Investigación/Artículos de Investigación/Paper 1_Exploring Delayed Access to Higher Education/")
options(digits = 1)

Ti = read.xlsx("Ti.xlsx")
Drop_out_level = read.xlsx("Drop Out Data.xlsx", sheet = 1)
Drop_out_sector = read.xlsx("Drop Out Data.xlsx", sheet = 2)
lag = read.xlsx("Drop Out Data.xlsx", sheet = 3)

# 1. Tiplot

options(scipen = 999)

Tiplot = ggplot(Ti) +
  aes(x = Cohort, y = Total.Students, fill = Cathegory) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15)
  )+ 
  scale_fill_manual(values = c("#EB6FB8","#01579b"))+ ylab("Total students")+
  geom_text(aes(x= "Cohort 2014", label="37.4 %", y= 150000), colour="white", angle=0, size = 8)+
  geom_text(aes(x= "Cohort 2015", label="38 %", y= 150000), colour="white", angle=0, size = 8)+
  geom_text(aes(x= "Cohort 2016", label="42.4 %", y= 150000), colour="white", angle=0, size = 8)+
  geom_text(aes(x= "Cohort 2017", label="38.7 %", y= 150000), colour="white", angle=0, size = 8)+
  geom_text(aes(x= "Cohort 2014", label="62.6 %", y= 390000), colour="white", angle=0, size = 8)+
  geom_text(aes(x= "Cohort 2015", label="62 %", y= 390000), colour="white", angle=0, size = 8)+
  geom_text(aes(x= "Cohort 2016", label="57.6 %", y= 390000), colour="white", angle=0, size = 8)+
  geom_text(aes(x= "Cohort 2017", label="61.3 %", y= 390000), colour="white", angle=0, size = 8)


ggsave("Tiplot.png", Tiplot, width = 12, height = 7) 

# 2. Drop Out - Level

Drop_out_level %<>% pivot_longer('2010':'2021', names_to = "Year")
Drop_out_level %<>% mutate(value = value * 100)
Drop_out_level %<>% filter(Year <= 2018)
Drop_out_level %<>% select('Drop out rate' = value, everything())
Drop_out_level %<>% filter(Level.of.Education != "Total")
Drop_out_level$`Drop out rate` %<>% round(1)

DOplot1 = ggplot(Drop_out_level, aes(x = Year, y = `Drop out rate`,
                                     group = Level.of.Education, color = Level.of.Education)) +
                   geom_line(linewidth=1) +
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15)) + 
  scale_color_manual(values = c("#01579B","#EB6FB8", "#00C9A2", "#7863B6")) + geom_text(label= Drop_out_level$`Drop out rate`,
                                                   vjust=-0.8, hjust= 0.2, colour = "black", size = 4)
                                                  

ggsave("Doplot.png",DOplot1, width = 12, height = 7) 

# 3. Drop Out - sector

Drop_out_sector %<>% pivot_longer('2010':'2021', names_to = "Year")
Drop_out_sector %<>% mutate(value = value * 100)
Drop_out_sector %<>% filter(Year >= 2015 & Year <= 2018)
Drop_out_sector %<>% select('Drop out rate' = value, everything())
Drop_out_sector$`Drop out rate` %<>% round(1)


DOplot2 = ggplot(Drop_out_sector, aes(x = Year, y = `Drop out rate`)) +
  geom_col(aes(group = Sector, fill = Sector), position = "dodge2") +
  facet_wrap(~Level) +
  scale_fill_manual(values = c("#01579b", "#EB6FB8")) +
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15)) 
 

ggsave("Doplot2.png",DOplot2, width = 12, height = 7) 

# 4. Delay magnitude

lag$Lag.Magnitude %<>% factor(levels=c("Total sample", "1 year", "2 years", "3 years", "4 years",         
                                           "more than 5 years"))

lag %<>% select('Delay magnitude' = Lag.Magnitude, everything())


Delayplot = ggplot(lag, aes(x = `Delay magnitude`, y = Effect, group = 1)) +
  geom_point(color = "#01579b")+ geom_line(color = "#01579b", linewidth=1)+
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15)) + 
  geom_text(label = lag$Effect, vjust=-0.8, hjust= 0.9, colour = "black", size = 4)

ggsave("Delayplot.png",Delayplot , width = 12, height = 7) 

