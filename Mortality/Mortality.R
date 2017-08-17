library(plyr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(ggthemes)

WorldMale<-read.csv("WM.csv", stringsAsFactors = F)
WorldFemale<-read.csv("WF.csv", stringsAsFactors = F)

#Rename all Year Columns
colnames(WorldFemale) <- gsub("^X","", colnames(WorldFemale))
colnames(WorldMale) <- gsub("^X", "", colnames(WorldMale))


#Filter Out Vietnam, Canada $ WOrld
Male_CVW<-WorldMale%>%
  select(-2016, -(Indicator.Name:Indicator.Code))%>%
  filter(Country.Code %in% c("VNM","WLD","CAN"))%>%
  gather(-Country.Name,-Country.Code, key="Year",value="Male.Mortality.Rate")
  
Female_CVW<-WorldFemale%>%
  select(-2016, -(Indicator.Name:Indicator.Code))%>%
  filter(Country.Code %in% c("VNM","WLD","CAN"))%>%
  gather(-Country.Name,-Country.Code, key="Year",value="Female.Mortality.Rate")  
  
cvw<-merge(Male_CVW,Female_CVW,by=c("Country.Name","Year","Country.Code"))

cvw$Year<-as.double(cvw$Year)
cvw<-cvw%>%
  mutate(gender_rate=Male.Mortality.Rate/Female.Mortality.Rate)

#Rough Comparison
cvw1<-cvw%>%
  filter(Year %in% 1960:2014)

ggplot(cvw1,aes(Year, gender_rate, color=Country.Name)) + 
  geom_point(na.rm=TRUE)+
  stat_smooth(method = "lm", size = 1.5, se=F) + 
  scale_x_continuous('Year',
                     breaks = seq(1960,2014,6), labels = seq(1960,2014,6)) +
  coord_flip()+
  ggtitle('Compare mortality rate male/female',
          subtitle = "Canada,Vietnam and World - World Bank")+
  theme_bw(base_size = 15, base_family="mono")+
  theme(panel.grid = element_line(colour = "grey75", size = .25))

# Let's animate that
library(gganimate)
library(showtext)
font.add("roboto", regular = "roboto-condensed.regular.ttf")
font.add("robotomono", regular = "RobotoMono-Regular.ttf")
windowsFonts(roboto=windowsFont("TT Roboto Condensed"))
windowsFonts(robotomono=windowsFont("TT Roboto Mono"))

p2 <- ggplot(cvw1,aes(Year, gender_rate)) + 
  geom_point(aes(color = factor(Country.Name)), alpha = 0.5)+
  geom_point(aes(frame = Year)) +
  scale_x_continuous('Year',
                     breaks = seq(1960,2014,6), labels = seq(1960,2014,6)) +
  coord_flip()+
  ggtitle('Mortality rate male/female',
          subtitle = "Canada,Vietnam and World - World Bank")+
  theme_bw(base_size = 15, base_family="mono")+
  theme(panel.grid = element_line(colour = "grey75", size = .25))
  
gganimate(p2,interval = .5, "World.gif")  

#CANADA vs JAPAN
can<-tbl_df(data.frame(read.table("CANmltper_1x1.txt",stringsAsFactors = F)))
names(can)<-c("Year","Age","mx","qx","ax","lx","dx","Lx","Tx","ex")
can<-can[2:10102,]
can$Year<-as.numeric(can$Year)
can$mx<-as.double(can$mx)

can_age_110 <- can %>% filter(Age == "110+")
can_age_110 <- adply(can_age_110, .margin = 1, function(x){
  x$Age <- str_replace(x$Age,"110\\+","110")
}, .parallel = F, .inform = T)
can_age_110$Age <- can_age_110$V1
can_age_110$V1 <- NULL
can <- bind_rows(can %>% filter(!Age == "110+"), can_age_110)

can$Age<-as.numeric(can$Age)

jap<-tbl_df(data.frame(read.table("mltper_1x1.txt", stringsAsFactors = F)))
names(jap)<-c("Year","Age","mx","qx","ax","lx","dx","Lx","Tx","ex")
jap<-jap[2:10102,]
jap$Year<-as.numeric(jap$Year)
jap$mx<-as.double(jap$mx)
jap<-jap[!is.na(jap$Age),]
jap$Age<-as.numeric(jap$Age)

# Let's animate the result
ca1 <- can %>% transmute(age = Age, can = mx, year = Year)
jp1 <- jap %>% transmute(age = Age, jpn = mx, year = Year)
df1 <- merge(jp1, ca1, by = c('age', 'year')) %>% mutate(ca_rate = can/jpn)

p3 <- ggplot(df1, aes(age, ca_rate)) + 
  geom_hline(yintercept = 1, color = 'red') +
  geom_line(aes(frame = year)) + 
  scale_y_continuous('mortality rate ratio',
                     breaks = seq(0,3,0.2), labels = seq(o,3,0.2), limits = c(0, 3)) +
  scale_x_continuous(breaks = seq(0,120,10), labels = seq(0,120,10)) +
  annotate('text',x=c(94, 1), y = c(1.3,2.3), 
           color = c('red','black'), hjust = 0, vjust = 1, size = 7,
           label = c('Japan','Canada'))+
  ggtitle('Males Mortality: ')+
  theme_bw(base_size = 15, base_family="mono")+
  theme(panel.grid = element_line(colour = "grey75", size = .25))
  
gganimate(p3, title_frame = TRUE, interval = .2, "Canada_plot.gif")
