install.packages("treemapify")
library(treemapify)
library(tidyverse)
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2021-11-30')
tuesdata$matches->matches
glimpse(matches)

matches%>%
  group_by(team1,team2)%>%
  count()%>%
  gather("team1","team2",1:2)%>%
  select(team2,n)%>%
  group_by(team2)%>%
  summarise(total=sum(n))%>%
  arrange(desc(total))->totalmatches

colnames(totalmatches)<-c("Country","Total")
colnames(winners)<-c("Country","Wins")
totalmatches%>%
  inner_join(winners)%>%
  rowwise()%>%
  mutate(Losses=(Total-Wins))->data

melt(data,measure.vars = c("Wins","Losses"),id.vars = "Country",value.name = "value")%>%
  arrange(Country)%>%
  group_by(Country)%>%
  mutate(Total = sum(value))->data

  
  data%>%
  arrange(desc(Total))%>%
  data.frame()->data
data%>%
  filter(Country!="Africa XI" & Country!="Asia XI" & Country!="ICC World XI")->data

data%>%data.frame()%>%
  filter(Country=="India"|
           Country=="Pakistan"|
           Country=="Sri Lanka"|
           Country=="Australia"|
           Country=="South Africa"|
           Country=="Zimbabwe"|
           Country=="New Zealand"|
           Country=="West Indies"|
           Country=="England"|
           Country=="Bangladesh")->datum

datum%>%
  mutate(Country=fct_relevel(Country,"India","Pakistan","Sri Lanka","Australia","South Africa","Zimbabwe","New Zealand","West Indies","England","Bangladesh"))->datum
  

datum%>%
  mutate(variable=recode(variable,"Wins"="Won","Losses"="Lost"))->datum

ggplot(datum, aes(area = Total, fill = value,label = paste(variable, value,sep="\n"))) +
  geom_treemap() +
  scale_fill_distiller(palette="RdBu",direction=1)+
   geom_treemap_text(place = "centre",
                    size = 12, grow = FALSE)+
  
  theme(legend.position = "none")+
  facet_wrap(~Country,ncol=2)+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.line=element_blank(),
        panel.background = element_rect(fill="black"),
        plot.background=element_rect(fill="black"),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5,1,0.5,1),"cm"),
        strip.background = element_rect(fill="gray10"),
        strip.text = element_text(colour="white",size=10,face="bold",margin = margin(t=5,b=5)),
        legend.position = "none",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_text(size=14,face="bold",color="white",margin=margin(b=15)),
        plot.subtitle = element_text(size=12, colour="white",margin=margin(b=21)),
        plot.caption = element_text(size=10, colour="white",hjust=0,margin=margin(t=15)))+  
        theme(panel.spacing = unit(1, "lines"))+
        labs(title="THE 10 COUNTRIES THAT PLAYED THE MOST ICC CRICKET WORLD CUP ODIs FROM 1996-2005",
       subtitle = "The blocks represent the number of matches the country won and the number of matches it lost",
       caption="Data from ESPN Cricinfo via Tidy Tuesday| Design and Analysis: @annapurani93")->plot

ggsave("cricket4.png",plot,width=10,height=10)   













