library(tidyverse)
library(lubridate)

r16 <- read_delim("~/AnacondaProjects/R/Projekte/RadWien/data/rad_vz_2016.csv", 
                    ";", escape_double = FALSE, col_types = cols(Datum = col_date(format = "%Y%m%d")), 
                    trim_ws = TRUE)
#plot -------------------------------------------

ggplot(r16,aes(x=Datum,y=Donaukanal))+
  geom_point()+
  geom_smooth()+
  theme_gray(base_size=14)+
  ggtitle("Radzählung Wien")  
  

# end_plot ---------------


r16_m <- r16 %>% 
  mutate (Monate=floor_date(Datum,unit='month')) %>% 
  select(-Datum) %>% 
  gather(mess,values,-Monate)

  ggplot(r16_m,aes(x=Monate,y=values))+
  geom_col()+
  facet_wrap(vars(mess))+
  theme_gray(base_size=8)+
  ggtitle("Radzählung Wien 2016")  

  
r16_d <- r16 %>% 
    gather(mess,values,-Datum)
  
  ggplot(r16_d,aes(x=Datum,y=values))+
    geom_point(aes(alpha=0.5),color='red')+
    geom_smooth()+
    facet_wrap(vars(mess))+
    theme_gray(base_size=10)+
    ggtitle("Radzählung Wien 2016") 
  
 summary(r16)
 
 r16_d<-r16_d%>%
   mutate(wt=as_factor(if_else((wday(Datum)%in% c(1,7)),'ft','at')))
 
 r16_d%>%
   group_by(mess,wt)%>%
   summarise(mw=mean(values))%>%
   ggplot(aes(mess,mw,fill=wt))+
   geom_bar(stat='identity',position="fill")+coord_flip()+
   labs(y="Mittelwertvergleich Fahrten pro Tag",x="Messpunkt",title="Vergleich Werktage und Wochendtage")+
   scale_fill_discrete(name='Wochentag',
                       labels=c('Arbeitstag',"Wochenende"))+
   theme(panel.grid.minor.x = element_line(colour="white",size=2.0))

         