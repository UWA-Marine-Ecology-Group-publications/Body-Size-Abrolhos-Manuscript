library(ggplot2)
library(reshape2)
library(tidyverse)



AveLarge<-Large.complete.data %>% 
  group_by(Genus.Species, Status) %>% 
  summarise(average = mean(n),sd = sd(n), n=n())

data<-read.csv("Percentage of Sites.csv")
data2<-left_join(data,AveLarge)

data2$Genus.Species<-chartr(".", " ", data2$Genus.Species)# remove "." and replace with " " 
levels(data2$Status)<-c('Fished','Closure')


data2 <- data2 %>% mutate(se= sd/sqrt(n))
data2 <- data2 %>% mutate(errorbarpos= average + se)
data2 <- data2 %>% mutate(errorbarneg= average - se)

Large<-data2%>%
   filter(Percentage.of.Sites>=0.20)

xx = ggplot(Large, aes(x = Genus.Species, y = Status)) + 
  geom_point(aes(size = errorbarpos), alpha = 0.5, shape = 3)+
  geom_point(aes(size = average, fill = Status),alpha = 0.85, shape = 21) + 
  scale_fill_grey(start = 1, end = .75)+
    scale_size_continuous(limits = c(0.000001, 100), range = c(1,100), breaks = c(0,0.5,1,1.5)) +
  expand_limits(x = 76) +
  labs( x= "", y = "", size = "Abundance", fill = "Status")  + 
  theme(legend.key=element_blank(), 
        axis.text.x = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.y = element_text(colour = "black", face = "italic", size = 8), 
        legend.text = element_text(size = 10, colour ="black"), 
        legend.title = element_text(size = 12), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2), 
        legend.position = "right",
        panel.grid.major.y = element_line(colour = "lightgrey", linetype=3))+
   guides(fill = guide_legend(reverse = TRUE)) +
  #scale_fill_manual(values = colours, guide = FALSE) + 
  coord_flip() +
  scale_y_discrete(limits = rev(levels(Large$Status))) +
  ggtitle("Large Body Size")+
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(colour = FALSE)+
  scale_x_discrete(limits = rev)
  
xx



AveMed<-Med.complete.data %>% 
  group_by(Genus.Species, Status) %>% 
  summarise(average = mean(n),sd = sd(n), n=n())

data<-read.csv("Percentage of Sites.csv")
data3<-left_join(data,AveMed)

data3$Genus.Species<-chartr(".", " ", data3$Genus.Species)# remove "." and replace with " " 
levels(data3$Status)<-c('Fished','Closure')

data3 <- data3 %>% mutate(se= sd/sqrt(n))
data3 <- data3 %>% mutate(errorbarpos = average + se)
data3 <- data3 %>% mutate(errorbarneg= average - se)


Medium<-data3%>%
  filter(Percentage.of.Sites>=0.2)

xx = ggplot(Medium, aes(x = Genus.Species, y = Status)) + 
  geom_point(aes(size = errorbarpos), alpha = 0.5, shape = 3)+
  geom_point(aes(size = average, fill = Status),alpha = 0.85, shape = 21) + 
  scale_fill_grey(start = 1, end = .75)+
  scale_size_continuous(limits = c(0.000001, 100), range = c(1,100), breaks = c(0,0.5,1,1.5)) +
  expand_limits(x = 76) +
  labs( x= "", y = "", size = "Abundance", fill = "Status")  + 
  theme(legend.key=element_blank(), 
        axis.text.x = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.y = element_text(colour = "black", face = "italic", size = 8), 
        legend.text = element_text(size = 10, face ="bold", colour ="black"), 
        legend.title = element_text(size = 12, face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2), 
        legend.position = "right",
        panel.grid.major.y = element_line(colour = "lightgrey", linetype=3))+
  guides(fill = guide_legend(reverse = TRUE)) +
  #scale_fill_manual(values = colours, guide = FALSE) + 
  coord_flip() +
  scale_y_discrete(limits = rev(levels(Medium$Status))) +
  ggtitle("Medium Body Size")+
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(colour = FALSE)+
  scale_x_discrete(limits = rev)
xx



AveSmall<-Small.complete.data %>% 
  group_by(Genus.Species, Status) %>% 
  summarise(average = mean(n), sd = sd(n), n=n())

data<-read.csv("Percentage of Sites.csv")
data4<-left_join(data,AveSmall)
  
data4$Genus.Species<-chartr(".", " ", data4$Genus.Species)# remove "." and replace with " " 
levels(data4$Status)<-c('Fished','Closure')

data4 <- data4 %>% mutate(se= sd/sqrt(n))
data4 <- data4 %>% mutate(errorbarpos= average + se)





Small<-data4%>%
  filter(Percentage.of.Sites>=0.20)

xx = ggplot(Small, aes(x = Genus.Species, y = Status)) + 
  geom_point(aes(size = errorbarpos), alpha = 0.5, shape = 3)+
  geom_point(aes(size = average, fill = Status),alpha = 0.85, shape = 21) + 
  scale_fill_grey(start = 1, end = .75)+
  scale_size_continuous(limits = c(0.000001, 100), range = c(1,100), breaks = c(0,0.5,1,1.5)) +
  expand_limits(x = 76) +
  labs( x= "", y = "", size = "Abundance", fill = "Status")  + 
  theme(legend.key=element_blank(), 
        axis.text.x = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.y = element_text(colour = "black", face = "italic", size = 8), 
        legend.text = element_text(size = 10, face ="bold", colour ="black"), 
        legend.title = element_text(size = 12, face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2), 
        legend.position = "right",
        panel.grid.major.y = element_line(colour = "lightgrey", linetype=3))+
  guides(fill = guide_legend(reverse = TRUE)) +
  #scale_fill_manual(values = colours, guide = FALSE) + 
  coord_flip() +
  scale_y_discrete(limits = rev(levels(Small$Status))) +
  ggtitle("Small Body Size")+
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(colour = FALSE)+
  scale_x_discrete(limits = rev)
xx

