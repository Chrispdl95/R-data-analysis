library(tidyverse)
library(ggplot2)
queen <- read.csv(file = 'queen.csv')
 
#1)
#A) bar plot num of tracks per album
barplot<-ggplot(data=queen, aes(x=album_name)) +
  geom_bar()+
  labs(title='Bar plot of tracks per album')+
  theme(axis.text.x = element_text(angle = 45,hjust=1))
barplot

#B)
histogram <- ggplot(data=queen, aes(x=tempo))+
  geom_histogram(bins=15,aes(fill=mode))+
  labs(title='Histogram of Tempo distribution')
histogram

#C)

boxplot <- ggplot(data=queen, aes(x=album_name, y= tempo))+
  geom_boxplot(fill='lightblue')+
  labs(title='Boxplot of Tempo per album')+
  theme(axis.text.x = element_text(angle = 45,hjust=1))
boxplot

#D)
scatter <- ggplot(data=queen, aes(x=energy, y=danceability))+
  geom_point(color="lightblue",aes(size=tempo)) + 
  facet_wrap(~ album_name)+
  labs(title='Scatterplots of danceability and energy per album')
  
scatter  

#E)
scatter2 <- ggplot(data=queen, aes(x=danceability, y=track_popularity))+
  geom_point(color="lightblue",aes(size=tempo)) + 
  labs(title='Scatterplot of danceability and track popularity')
scatter2

scatter3 <- ggplot(data=queen, aes(x=valence, y=energy))+ 
  geom_point(aes(color=track_popularity))+
  labs(title='Scatterplot of valence and energy')
  
scatter3

#2)

mac<- read.csv(file = 'mcdonalds.csv')
complete_mac <- mac[complete.cases(mac),]

final_mac <- complete_mac %>%
  transmute(country=name, price= local_price/dollar_ex,GDP_dollar= GDP_dollar)

scatter5 <- ggplot(data= final_mac, aes(x=GDP_dollar ,y=price))+
  geom_point()+
  labs(title='Scatterplot GDP - price of BigMac')
scatter5 
