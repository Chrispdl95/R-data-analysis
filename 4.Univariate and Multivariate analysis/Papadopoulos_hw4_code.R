
library(tidyverse)
path <- "C:/Users/Chrispdl/Desktop/edav/hw4/results.csv"
olympics <- read.csv(path)
#preview the data
olympics
summary(olympics)

#delete null values
olympics<- na.omit(olympics)
b<-table(olympics['Gender'])
b

#replace h in result column with :
olympics$Result <- str_replace(olympics$Result,'h',':')
olympics$Result <- str_replace(olympics$Result,'P','')
olympics
#delete rows with none in result column (but first check how many and if they are random)
x <- olympics%>%
  filter(Result == 'None')%>%
  group_by(Year)%>%
  summarize(count= n())

y<- olympics%>%
  group_by(Year)%>%
  summarize(count= n())



olympics
olympics <-olympics %>%
  filter(Result != 'None')
olympics

#check number of distinct events
num_events <- olympics%>%
  group_by(Event)

num_events

#check if ':' is present in olympics$Result
grepl(':',olympics$Result)

#separate the original data in 2 dfs 
#result in time
olym_1 <- olympics%>%
  filter(grepl(':',olympics$Result)| Event== '100M Men'| Event=='110M Hurdles Men' |Event=='200M Men'| Event== '400M Hurdles Men'|Event== '400M Hurdles Women'| Event=='400M Men' |Event=='4X100M Relay Men'| Event=='100M Hurdles Women' | Event=='100M Women'| Event=='200M Women' | Event=='400M Hurdles Women
'| Event== '400M Women'| Event=='4X100M Relay Women' )

#result not in time 
olym_2 <- olympics%>%
  filter(!(grepl(':',olympics$Result)|Event=='100M Men'| Event=='110M Hurdles Men' |Event=='200M Men'| Event== '400M Hurdles Men'|Event== '400M Hurdles Women'|Event=='400M Men' |Event=='4X100M Relay Men'| Event=='100M Hurdles Women' | Event=='100M Women'| Event=='200M Women' | Event=='400M Hurdles Women
'| Event== '400M Women'| Event=='4X100M Relay Women'))

#num of events in the original data 
events <- olympics%>%
  summarise(events= n_distinct(Event))
events
#events in the olym_1 df
unique(olym_1$Event)

#events in the olym_2 df
unique(olym_2$Event)



## Men and women medalists

plot1 = barplot(table(olympics$Gender), 
            main = "How many Olympic athletes are male and female?", 
            xlab = "Gender",
            ylab = "Number of athletes",
            ylim = c(0,2000),
            col = c("lightcoral", "cornflowerblue"))
plot1
text(y = table(olympics$Gender), 
     a,
     table(olympics$Gender), 
     cex=0.8, 
     pos = 3)
b<-table(olympics['Gender'])
b
olympics
c<-table(olympics['Year'])
c
fig_dat7<-olympics %>% filter(Medal=='G') %>%
  group_by(Year,Nationality) %>%
  summarise(Gold_Medal_count=n_distinct(Event))%>%
  top_n(1, Gold_Medal_count) %>% arrange(desc(Gold_Medal_count))

fig_dat7

fig_dat7 %>% ggplot(aes(x=factor(Year), y=Gold_Medal_count, fill=Nationality)) + 
  geom_bar(stat='identity',position =   position_dodge()) + 
  geom_text(aes(x=factor(Year), y=Gold_Medal_count/2,label=Nationality),size=2, position =position_dodge(width=1)) +
  geom_text(aes(x=factor(Year), y=Gold_Medal_count*1.05,label=Gold_Medal_count),size=2, position =position_dodge(width=1)) +
  coord_flip() +
  labs(x="Year",y="Gold Medals")


#Exploring the number of medals won by each country

winners <- olympics%>%
  group_by(Nationality,Medal, Year)%>%
  summarise(count= length(Medal))

#ordering countries by the total medal count and picking top 10
top_10 <- winners %>%
  group_by(Nationality)%>%
  summarize(Total_medals = sum(count))%>%
  arrange(desc(Total_medals))%>%
  slice(1:10)


#plot top_10
plot_3 <- top_10 %>%
  ggplot(aes(reorder(Nationality, -Total_medals),Total_medals))+
  geom_col()+
  labs(x= "Country code", y = "Total number of medals", title="Top 10 countries by the total number of medals") +
  theme_minimal()+
  scale_x_discrete(guide = guide_axis(angle = 90)) 
plot_3  


# Let's see what colors of the medals the team GREECE  scored over the years
Greece1 = dplyr::filter(olympics, Nationality == "GRE")%>%
  group_by(Medal)%>%
  summarize(Count= n())

plot_4 <-ggplot(Greece1, aes(x = Medal, y = Count, fill = Medal)) +
  geom_bar(stat="identity") + 
  theme_minimal() + 
  labs(x = " ", y = "Total number of medals", title="Number of medals of each color for Greece") +
  scale_fill_manual(values=c("saddlebrown", "gold", "gray62"))
plot_4

#Greece men vs women
Greece2 = dplyr::filter(olympics, Nationality == "GRE")%>%
  group_by(Gender)%>%
  summarize(Count=n())
plot_5 <-ggplot(Greece2, aes(x = Gender, y = Count, fill = Gender)) +
  geom_bar(stat="identity") + 
  theme_minimal() + 
  labs(x = " ", y = "Total number of medals", title="Number of medals of each color for Greece") +
  scale_fill_manual(values=c("lightblue", "pink"))
plot_5


#plot of 100M Golden Medalists Throught the years
men_100 <- olym_1 %>%
  filter(Medal == 'G', Event=='100M Men')
men_100$Result <- as.numeric(men_100$Result)


best_time <- min(men_100$Result)
plot_6 <- ggplot(men_100, aes(x=Year,y=as.numeric(Result), color=Nationality))+
  geom_point(size=4)+
  scale_y_continuous(breaks=seq(9,12,0.1)) +
  labs(x = "Year of Olympic Games", y = "Seconds") +
  ggtitle("ScatterPlot of 100M Men Golden Medalists Throughout The Years")
plot_6


  

# PARTICIPATION OF MALE AND FEMALE ATHLETES OVER TIME
count_sex <- olympics%>%
  group_by(Year,Gender)%>%
  summarize(Athletes= n_distinct(Name))
library("gganimate")

count_sex$Year <- as.integer(count_sex$Year)
# PLOT MALE AND FEMALE ATHLETES OVER TIME
plot8<-ggplot(count_sex, aes(x=Year, y=Athletes, group=Gender, color=Gender)) +
  geom_point(size=2) +
  geom_line()  +
  transition_time(Year)+
  scale_color_manual(values=c("deepskyblue4","red4")) +
  labs(x = "Year", y = "Athletes", 
       title="Male and Female athletes over time", 
       subtitle = "Olympic Games from 1896 to 2016")

plot8

#scatter plot for hammer throw throughout the years

hammer_men <- olym_2 %>%
  filter(Event=='Hammer Throw Men')%>%
  select(Gender,Year,Result)
max_ham_men<- max(hammer_men$Result)
max_ham_men
min_ham_men <- min(hammer_men$Result)
min_ham_men


hammer_women<-olym_2%>%
  filter(Event=='Hammer Throw Women',Gender=='W')%>%
  select(Gender,Year,Result)

max_ham_women<- max(hammer_women$Result)
max_ham_women
min_ham_women <- min(hammer_women$Result)
min_ham_women

hammer <- rbind(hammer_men,hammer_women)


#plot9
plot9 <- ggplot(hammer,aes(x=Year,y=as.numeric(Result),color=Gender))+
  geom_point()+
  scale_y_continuous(breaks=seq(35,100,5))+
  scale_colour_manual(values=c("blue", "red")) +
  ggtitle("ScatterPlot of Men/Women Hammer Throw Throughout The Years")+
  theme(legend.position = c(0.91, 0.08))+
  labs(x = "Year of Olympic Games", y = "Throw (in meters)") +
  geom_text(aes(label = ifelse((Result) %in% 
                                 c(max_ham_men, max_ham_women),
                               paste("Highsest: ", as.character(Result), "→", sep = " "),'')),
            hjust = 1.0, vjust = 0.5, col = "grey35") +
  geom_text(aes(label = ifelse((Result) %in% 
                                 c(min_ham_men,min_ham_women),
                               paste("←", as.character(Result), ":Lowest", sep = " "),'')),
            hjust = -0.12, vjust = 0.35, col = "grey35")

plot9


#Olympic records

records_1<- olym_1%>%
  group_by(Event)%>%
  summarize(min=min(Result),country=Nationality[which(Result==min(Result))])
records_1

olym_2$Result<-as.numeric(olym_2$Result)
records_2 <-olym_2%>%
  group_by(Event)%>%
  summarize(max=max(Result),country=Nationality[which(Result==max(Result))])
records_2

olympic_records<- rbind(records_1,records_2)%>%
  group_by(country)%>%
  summarize(count=n())
olympic_records

plot10 <- olympic_records%>%
  ggplot(aes(reorder(country,-count),count))+
  geom_col()+
  scale_y_continuous(breaks=seq(0,15,1))+
  labs(x= "Country code", y = "Total number of olympic records ", title="Countries which hold olympic record",subtitle='Best of the best')+
  scale_x_discrete(guide = guide_axis(angle = 90))
plot10

  
 


