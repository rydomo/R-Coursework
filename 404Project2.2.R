library(tidyverse)
library(lubridate)
library(ggspatial)
library(sf)
library(gridExtra)
library(patchwork)
library(maps)
library(plotly) 

OhioDF <- read_csv(file = "https://coronavirus.ohio.gov/static/dashboards/vaccine_data.csv")
#data wrangling end= date, county %5 %18 %all 

#find county population info and isolate it 
popnDF <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-39.csv")
#create DF with correctly labeled ohio counties and find 5+ pop numbers
OhioPopnDF <- popnDF %>% 
  filter(STNAME == "Ohio") %>% 
  filter(CTYNAME != "Ohio") %>% 
  filter(YEAR == 12)%>%
  select(CTYNAME, POPESTIMATE, UNDER5_TOT, AGE18PLUS_TOT) %>%
  mutate(str_remove_all(CTYNAME, " County"),
         fiveplus = POPESTIMATE-UNDER5_TOT)

#make sure the county columns have the same names
OhioPopnDF <- OhioPopnDF %>%
  rename("county"=`str_remove_all(CTYNAME, " County")`) %>%
  select(-"CTYNAME", -"UNDER5_TOT") 

#merge the case numbers and population together
MainDF <- merge(OhioDF, OhioPopnDF, by.x = "county", by.y="county")
MainDF <- MainDF%>%
  group_by(county)%>%
  arrange(date)%>%
  mutate(cumVaccinated = cumsum(vaccines_started))%>%
  select(-"vaccines_completed", -"vaccines_first_additional_dose")
# make population percentages  
MainDF <- MainDF%>%
  mutate(perc5 = (cumVaccinated/fiveplus)*100,
         perc18 = (cumVaccinated/AGE18PLUS_TOT)*100,
         percVax = (cumVaccinated/POPESTIMATE)*100)


# get correct Ohio map data and merge onto working dataset
counties <- map_data("county") %>%
  subset(region == 'ohio')%>%
  mutate(subregion= str_to_title(subregion))
mapDF <- merge(counties, MainDF, by.x = "subregion", by.y="county")

# Tab 1:  Line graph of responses over time

ggplot(data=MainDF) +
  geom_line(aes(x=date, y=perc5, group= county), color = "light blue")+
  geom_line(aes(x=date, y=perc5), data= filter(MainDF, county=="Butler"), color="dark blue") +
  scale_x_date()+
  ylim(0,100) +
  theme_minimal()

# Tab 2:  Bar Graph comparing counties (ordered from largest to smallest)
# need more data wrangling, narrow down to last day, filter(date = )
tab2DF <- MainDF %>%
  filter(date == max(date))
  
ggplot(tab2DF) +
  geom_bar(stat="identity", fill="light blue", width=0.5, aes(x=fct_reorder(county, perc5), y=perc5)) +
  geom_bar(data= filter(tab2DF, county=="Butler"), fill="dark blue", stat="identity", aes(x=county, y=perc5)) +
  theme(legend.position="none") +
  theme_minimal()+
  coord_flip()


# Tab 3:  Ohio Map with Counties
# make plotly with tooltip interactions
plot3<- mapDF %>% 
  ggplot() + #aes(text = country)
  geom_polygon(aes(x=long, y=lat, group=group, fill=perc5), color="white")  +
  scale_fill_gradient(low="light blue", high = "dark blue") +
  theme_void()
plot3
#ggplotly(plot3)

t3map<-  ggplot(mapDF) + 
  geom_polygon(aes(x=long, y=lat, group=group, fill=perc5, text = paste(subregion,  '<br>% population age 5+ vaccinated - started:', perc5)), color="white")  +
  scale_fill_gradient(low="light blue", high = "dark blue") +
  theme_void()
t3map
#ggplotly(t3map, tooltip= "text")

#static 3
ggplot(mapDF, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=perc5), color ="white")+
  scale_fill_gradient(low="light blue", high = "dark blue")+
  theme_void()

# aes(text = paste(subregion,  '<br>% population age 5+ vaccinated - started:', perc5)


# 4
pieDF1 <- MainDF %>%
  filter(date==as.Date("2021-12-7"), county == "Adams")

pieDF2 <- data.frame(
  group=c("Started Vaccination","Wholly Unvaccinated"),
  value=c(pieDF1$cumVaccinated[1], (pieDF1$POPESTIMATE[1]-pieDF1$cumVaccinated[1]))
)

pie <- ggplot(pieDF2, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  scale_fill_brewer(palette="Blues") +
  ggtitle("Adams County Vaccination as of 2021-12-07") +
  theme(legend.title = element_blank()) 
pie
# https://www.r-graph-gallery.com/piechart-ggplot2.html

