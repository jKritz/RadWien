library(tidyverse)
library(lubridate)

r16 <- read_delim("~/AnacondaProjects/R/Projekte/RadWien/rad_vz_2016.csv", 
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
 
 head(r16)

# Vergleich Mittelwert Wochende mit Mittelwert Arbeitstag--------------------
 
r16_d %>%
  group_by(Datum) %>% 
  summarise(sum=sum(values)) %>% 
  ggplot(aes(Datum,sum))+ geom_point()  
  

we <- wday(r16$Datum)>=6
at <- wday(r16$Datum)<6

we_d <- r16_d$values[we]
at_d <- r16_d$values[at]

mean(r16_d$values[we])
mean(r16_d$values[at])

t.test(we_d,at_d)
