setwd("C:/Users/User/Documents/GitHub/Body-Size-Abrolhos-Manuscript/Results")

data<-read.csv("Body Size CAP scores.csv")


Small<-data%>%
filter(Body.Size.Class=="Small")%>%
 filter(Percentage.of.Sites>=0.20)%>%
 filter(CAP.Correlation<= -0.25 | CAP.Correlation>= 0.25)

Medium<-data%>%
  filter(Body.Size.Class=="Medium")%>%
  filter(Percentage.of.Sites>=0.20)%>%
  filter(CAP.Correlation<= -0.25 | CAP.Correlation>= 0.25)

Large<-data%>%
  filter(Body.Size.Class=="Large")%>%
  filter(Percentage.of.Sites>=0.20)%>%
  filter(CAP.Correlation<= -0.25 | CAP.Correlation>= 0.25)

require(dplyr)
require(ggplot2)


##########PLOTS###########

#As bars
Small %>%
  arrange(CAP.Correlation) %>%    #This sorts the dataframe but NOT the factor levels
  mutate(Species.Name=factor(Species.Name, levels=Species.Name)) %>%   # This trick updates the factor levels
  ggplot(aes(x=Species.Name, y=CAP.Correlation)) +
  geom_col(color = "black", fill = "grey", aes(Species.Name, CAP.Correlation)) +
  coord_flip() +
  xlab("Species") +
  ylab("Correlation") +
  theme(axis.text.y = element_text(face = "italic"))+
  scale_y_continuous(limits=c(-1,1))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line.x=element_line())+
     geom_hline(yintercept=0)+
    ggtitle("Small Body Size - 34 to 67 mm")
   
##geom_text(aes(label = paste(Species.Name), hjust = -0.1))# Add Labels

#As bars
Medium %>%
  arrange(CAP.Correlation) %>%    #This sorts the dataframe but NOT the factor levels
  mutate(Species.Name=factor(Species.Name, levels=Species.Name)) %>%   # This trick updates the factor levels
  ggplot(aes(x=Species.Name, y=CAP.Correlation)) +
  geom_col(color = "black", fill = "grey", aes(Species.Name, CAP.Correlation)) +
  coord_flip() +
  xlab("Species") +
  ylab("Correlation") +
  theme(axis.text.y = element_text(face = "italic"))+
  scale_y_continuous(limits=c(-1,1))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line.x=element_line())+
  geom_hline(yintercept=0)+
  ggtitle("Medium Body Size - 105 to 299 mm")
#geom_text(aes(label = paste(Species.Name),hjust = 1.0))# Add Labels

#As bars
Large %>%
  arrange(CAP.Correlation) %>%    #This sorts the dataframe but NOT the factor levels
  mutate(Species.Name=factor(Species.Name, levels=Species.Name)) %>%   # This trick updates the factor levels
  ggplot(aes(x=Species.Name, y=CAP.Correlation)) +
  geom_col(color = "black", fill = "grey", aes(Species.Name, CAP.Correlation)) +
  coord_flip() +
  xlab("Species") +
  ylab("Correlation") +
  theme(axis.text.y = element_text(face = "italic"))+
  scale_y_continuous(limits=c(-1,1))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line.x=element_line())+
  geom_hline(yintercept=0)+
  ggtitle("Large Body Size - 360 to 1000 mm")
  #geom_text(aes(label = paste(Species.Name),hjust = 1.0))# Add Labels







#Alternative plot style with lines
Small %>%
  arrange(CAP.Correlation) %>%    #This sorts the dataframe but NOT the factor levels
  mutate(Species.Name=factor(Species.Name, levels=Species.Name)) %>%   # This trick updates the factor levels
  ggplot(aes(x=Species.Name, y=CAP.Correlation)) +
  geom_segment(aes(xend=Species.Name, yend=0)) +
  coord_flip() +
  theme_bw() +
  xlab("Species") +
  ylab("CAP Score") +
  theme(axis.text.y = element_text(face = "italic"))+
  scale_y_continuous(limits=c(-1,1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
