# project2.R
# Project 2
# Author: Rylee Domonkos
# Date: 12/10/21
# Version: 1?
# Purpose: To analyze Ohio COVID data with dynamic plots 


library(shiny)
library(tidyverse)
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

#merge so the map has all the needed data
mapDF <- merge(counties, MainDF, by.x = "subregion", by.y="county")

#just the last day (the solution to why my maps would never run)
mapDF <- mapDF %>%
  filter(date == max(date))

#just the last day
tab2DF <- MainDF %>%
  filter(date == max(date))

### Define UI for application

ui <-ui <- navbarPage("Domonkos Ohio Covid Exploration",

                      tabPanel("Tab 1", fluidPage(
                        sidebarLayout(
                          sidebarPanel(
                          # what county we looking at
                          selectInput("county1", 
                                      "Select a county",
                                     c("Adams", "Allen", "Ashland", "Ashtabula", "Athens", "Auglaize", "Belmont", "Brown", "Butler", "Carroll", 
                                       "Champaign", "Clark", "Clermont", "Clinton", "Columbiana", "Coshocton", "Crawford", "Cuyahoga", "Darke", 
                                       "Defiance", "Delaware", "Erie", "Fairfield", "Fayette", "Franklin", "Fulton", "Gallia", "Geauga", "Greene", "Guernsey",
                                       "Hamilton", "Hancock", "Hardin", "Harrison", "Henry", "Highland", "Hocking", "Holmes", "Huron", "Jackson", 
                                       "Jefferson", "Knox", "Lake", "Lawrence", "Licking", "Logan", "Lorain", "Lucas", "Madison", "Mahoning", "Marion", "Medina", "Meigs", "Mercer", 
                                       "Miami", "Monroe", "Montgomery", "Morgan", "Morrow", "Muskingum", "Noble", "Ottawa", "Paulding", "Perry", "Pickaway", "Pike", "Portage", "Preble",
                                       "Putnam", "Richland", "Ross", "Sandusky", "Scioto", "Seneca", "Shelby", "Stark", "Summit", "Trumbull", "Tuscarawas", "Union", "Van Wert", "Vinton", 
                                       "Warren", "Washington", "Wayne", "Williams", "Wood", "Wyandot")), # wouldn't have an initial selection if use the data
                          #what age range
                          selectInput("Age_Range1", # what its saved under
                                        "Age Range of Interest", # what appears
                                        c("age 5+","age 18+","All ages"))),
                      # Main panel typically used to display outputs
                        mainPanel(
                          plotlyOutput(outputId = "lineplot") 
                      )))),

                      tabPanel("Tab 2", fluidPage(
                        sidebarLayout(
                          # age range
                          selectInput("Age_Range2", # what its saved under
                                      "Age Range of Interest", # what appears
                                      c("age 5+","age 18+","All ages")),
                      # Main panel typically used to display outputs
                      mainPanel(
                        # without all the width/height fiddling this graph looks very smooshed
                        plotOutput(outputId = "bargraph", width = "120%", height = "120%")
                      )))),

                      tabPanel("Tab 3", fluidPage(
                        sidebarLayout(
                          #don't need sidebar for this one, could have used something other than fluidpage likely
                          sidebarPanel(width = 0),
                        # Main panel typically used to display outputs
                        mainPanel(
                          plotlyOutput(outputId = "map3") 
                        )
                      ))),
    
                      tabPanel("Tab 4", fluidPage(
                        sidebarLayout( # use county1 and the date to show how increasing
                          sidebarPanel(
                            #select date from the calendar icon, starting at the lowest value 
                            dateInput(inputId = "dateIn", "Select Date:",
                              min = min(MainDF$date), max = max(MainDF$date),  value=min(MainDF$date)
                            ),
                          ),
                      # Main panel typically used to display outputs
                      mainPanel(
                        plotOutput(outputId = "pie") 
                      )))),
                      # citations
                      tabPanel("Tab 5", fluidPage(
                        titlePanel("Rylee Domonkos, 12/10/21"),
                        sidebarLayout(
                          sidebarPanel(),
                          mainPanel(
                            p("Citations/ References:",
                              style = "font-family: 'times'; font-si16pt"),
                            
                            p("R Core Team (2021). R: A language and environment for statistical computing. 
                              R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.",
                              style = "font-family: 'times'; font-si16pt"),
                            
                            p("Winston Chang, Joe Cheng, JJ Allaire, Carson Sievert, Barret Schloerke, Yihui Xie, Jeff 
                              Allen, Jonathan McPherson, Alan Dipert and Barbara Borges (2021). shiny: Web Application 
                              Framework for R. R package version 1.7.1. https://CRAN.R-project.org/package=shiny", 
                              style = "font-family: 'times'; font-si16pt"),
                            
                            p("Wickham et al., (2019). Welcome to the tidyverse. Journal of Open Source Software, 4(43), 
                              1686, https://doi.org/10.21105/joss.01686", 
                              style = "font-family: 'times'; font-si16pt"),
                            
                            p("Original S code by Richard A. Becker, Allan R. Wilks. R version by Ray Brownrigg. 
                              Enhancements by Thomas P Minka and Alex Deckmyn. (2021). maps: Draw Geographical Maps. 
                              R package version 3.4.0. https://CRAN.R-project.org/package=maps", 
                              style = "font-family: 'times'; font-si16pt"),
                            
                            p("C. Sievert. Interactive Web-Based Data Visualization with R, plotly, and shiny. 
                              Chapman and Hall/CRC Florida, 2020.", style = "font-family: 'times'; font-si16pt"),
                            
                            p("HendyHendy8, and Ram NarasimhanRam Narasimhan 21.8k44 gold badges4747 silver badges5454 bronze badges.
                            “Scale and Size of Plot in Rstudio Shiny.” Stack Overflow, 1 Sept. 1961, 
                            https://stackoverflow.com/questions/17838709/scale-and-size-of-plot-in-rstudio-shiny.",
                              style = "font-family: 'times'; font-si16pt"),
                            
                            p("Creating Strings from Variables, http://www.cookbook-r.com/Strings/Creating_strings_from_variables/. ",
                              style = "font-family: 'times'; font-si16pt"),
                            
                            p("Rstudio. “Interactive Web Apps with Shiny Cheat Sheet UI - Rstudio.” 
                              Shiny Cheatsheet, Rstudio, https://shiny.rstudio.com/images/shiny-cheatsheet.pdf. ", 
                              style = "font-family: 'times'; font-si16pt"),
                            
                            p("“GGPLOT2 Line Plot : Quick Start Guide - R Software and Data Visualization.” STHDA, STHDA,
                              http://sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization. ", 
                              style = "font-family: 'times'; font-si16pt"),
                            
                            p("Holtz, Yan. “Ggplot2 Piechart.” – The R Graph Gallery, https://www.r-graph-gallery.com/piechart-ggplot2.html. ",
                              style = "font-family: 'times'; font-si16pt"),
                            
                            p("“Lesson 2 Build a User Interface.” Shiny, https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/. ",
                              style = "font-family: 'times'; font-si16pt"),
                            
                            p("Data Sources",
                              style = "font-family: 'times'; font-si16pt"),
                            
                            p("“Ohio Covid 19 Dashboards.” Overview, https://coronavirus.ohio.gov/wps/portal/gov/covid-19/dashboards. ",
                              style = "font-family: 'times'; font-si16pt"),
                            
                            p("“US Census Surveys From 2010 to 2019.” Index of /Programs-Surveys/Popest/Datasets/2010-2019/Counties/ASRH,
                              ASRH, https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/. ",
                              style = "font-family: 'times'; font-si16pt")
                          )
                        )
                      ))
)
  
### Define server behavior for application here
#  Expressions such as in renderPlot MUST be in {} 

server <- function(input, output) {
  
  output$lineplot <-
    renderPlotly({
      #base graph
      line <- ggplot(data=MainDF) +
        scale_x_date()+
        ylim(0,100) +
        theme_minimal()
      if(input$Age_Range1 == "age 5+"){ #if chose 5+
        line <- line + geom_line(aes(x=date, y=perc5, group= county, 
                                     text = paste(county,  '<br>% Population 5+ vaccinated - started:', round(perc5))), color = "light blue")+
          geom_line(aes(x=date, y=perc5), data= filter(MainDF, county==input$county1), color="dark blue")+
          ggtitle("The Percentage of Population 5+ Started Vaccination across Ohio Counties over Time") +
          ylab("Percentage of Population 5+ Started Vaccinated") +
          xlab("Time")
      }
      else if(input$Age_Range1 == "age 18+"){ #if chose 18+
        line <- line + geom_line(aes(x=date, y=perc18, group= county, 
                                     text = paste(county,  '<br>% Population 18+ vaccinated - started:', round(perc18))), color = "light blue")+
          geom_line(aes(x=date, y=perc18), data= filter(MainDF, county==input$county1), color="dark blue") +
          ggtitle("The Percentage of Population 18+ Started Vaccination across Ohio Counties over Time") +
          ylab("Percentage of Population 18+ Started Vaccinated") +
          xlab("Time")
      }
      else if(input$Age_Range1 == "All ages"){ #if chose all ages
        line <- line + geom_line(aes(x=date, y=percVax, group= county, 
                                     text = paste(county,  '<br>% Population vaccinated - started:', round(percVax))), color = "light blue")+
          geom_line(aes(x=date, y=percVax), data= filter(MainDF, county==input$county1), color="dark blue")+
          ggtitle("The Percentage of Population Started Vaccination across Ohio Counties over Time") +
          ylab("Percentage of Population Started Vaccinated") +
          xlab("Time")
      }
      #allows to see the text element that always gives warnings even though it is used
      ggplotly(line, tooltip= "text")
    })
  
  output$bargraph <-
    renderPlot({
      #base plot
      bar <- ggplot(tab2DF) +
        theme(legend.position="none") +
        theme_minimal()+
        coord_flip()
      if(input$Age_Range2 == "age 5+"){ #if chose 5+
        bar <- bar + geom_bar(stat="identity", fill="light blue", width=0.5, aes(x=fct_reorder(county, perc5), y=perc5)) +
          geom_bar(data= filter(tab2DF, county==input$county1), fill="dark blue", stat="identity", aes(x=county, y=perc5)) +
          ylab("Percentage of Population 5+ who have Started Vaccination") +
          xlab("County") +
          ggtitle("Comparison of the Percentages of People 5+ who have started vaccination across Ohio Counties")
        }
        else if(input$Age_Range2 == "age 18+"){ #if chose 18+
          bar <- bar + geom_bar(stat="identity", fill="light blue", width=0.5, aes(x=fct_reorder(county, perc18), y=perc18)) +
            geom_bar(data= filter(tab2DF, county==input$county1), fill="dark blue", stat="identity", aes(x=county, y=perc18)) +
            xlab("Percentage of Population 18+ who have Started Vaccination") +
            ylab("County") +
            ggtitle("Comparison of the Percentages of People 18+ who have started vaccination across Ohio Counties")
        }
        else if(input$Age_Range2 == "All ages"){
          bar <- bar + geom_bar(stat="identity", fill="light blue", width=0.5, aes(x=fct_reorder(county, percVax), y=percVax)) +
            geom_bar(data= filter(tab2DF, county==input$county1), fill="dark blue", stat="identity", aes(x=county, y=percVax)) +
            ylab("Percentage of Population who have Started Vaccination") +
            xlab("County") +
            ggtitle("Comparison of the Percentages of People who have started vaccination across Ohio Counties")
        }
      bar
    }, height = 1000) #still trying to stop that vertical compression
  
  output$map3 <-
    renderPlotly({
      #I had this so long ago, but the overabundance of data really hurt me
      t3map<-  ggplot(mapDF) + 
        geom_polygon(aes(x=long, y=lat, group=group, fill=perc5, 
                         text = paste(subregion,'<br>% Population - total:', POPESTIMATE,
                                      '<br>%Population - age 5+:', fiveplus,
                                      '<br>% Population - age 18+:', AGE18PLUS_TOT,
                                      '<br>% population age 5+ vaccinated - started:', round(perc5),
                                      '<br>% population age 18+ vaccinated - started:', round(perc18),
                                      '<br>% % population vaccinated - all ages - started', round(percVax))), color="white")  +
        scale_fill_gradient(low="light blue", high = "dark blue") +
        theme_void() +
        ggtitle("How Vaccinated are the Ohio Counties?")+
        coord_quickmap()+
        labs(perc5= "% 5+ Vaccinated-Started")
      ggplotly(t3map, tooltip="text")
    })
  
  output$pie <-
    renderPlot({
      #isolate the selected date and county
      pieDF1 <- MainDF %>%
        filter(date==as.Date(input$dateIn), county == input$county1)
      
      #make data frame of just vaccination numbers and labels 
      pieDF2 <- data.frame(
        group=c("Started Vaccination","Wholly Unvaccinated"),
        value=c(pieDF1$cumVaccinated[1], (pieDF1$POPESTIMATE[1]-pieDF1$cumVaccinated[1]))
      )
      #pie charts are just drunk bar graphs
      pie <- ggplot(pieDF2, aes(x="", y=value, fill=group)) +
        geom_bar(stat="identity", width=1, color="white") +
        coord_polar("y", start=0) +
        theme_void() +
        scale_fill_brewer(palette="Blues") +
        ggtitle(paste("Vaccinations Started in", input$county1, "as of Date Selected")) + # paste allows for the text to format correctly
        theme(legend.title = element_blank(), plot.title = element_text(size = 20), legend.text = element_text(size = 14)) 
      pie
    })
}

### specify the ui and server objects to be combined to make App
shinyApp(ui=ui, server=server)




