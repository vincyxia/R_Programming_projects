library(shiny)
library(datasets)
library(markdown)
library(tidyverse)
library(DT)
library(leaflet)
library(ggmap)
# Team member: Wanxin Xia, Lanxin Zhang, Tianqing Feng

# introdution: The original data is from Kaggle(https://www.kaggle.com/nsharan/h-1b-visa)
# This dataset contains five year's worth of H-1B petition data, with approximately 3 million records overall. 
# The columns in the dataset include case status, employer name, worksite coordinates, job title, prevailing wage, occupation code, and year filed.


# input data (choose "h1b_kaggle.csv")
data <- read.csv(file.choose(), header = T)
# Define UI
ui <- navbarPage("H1-B Visa Petitions Analysis",
                 # GIS Panel: drop down filter: YEAR
                 tabPanel("Certified Jobs on Map",
                          br(),
                          leafletOutput("map", height="800px"),
                          absolutePanel(top=20, left=70, selectInput("year", "YEAR", choices = c("2011","2012","2013","2014","2015","2016"),selected = "2016")),
                          br()
                 ),
                 
                 #Graph1 panel
                 tabPanel("Top H1B Applied Jobs",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("rownum", 
                                          "Show top n values:",
                                          min = 5, 
                                          max = 20,
                                          value = 5),
                              
                              selectInput(
                                "year", "YEAR", choices = c("2011","2012","2013","2014","2015","2016"),
                                selected = "2016"
                              )
                            ),
                            mainPanel(
                              plotOutput("plot1")
                            )
                          )
                          
                         ),
                 
                 
                 # Table panel
                 tabPanel("Top 10 Certified H1B Companies",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("salary", 
                                          "Choose min and max salary",
                                          min = 0, 
                                          max = 20000000,
                                          value = c(80000,500000)
                              ),
                              
                              selectInput(
                                "year", "YEAR", choices = c("2011","2012","2013","2014","2015","2016"),
                                selectize = FALSE
                              )
                            ),
                            
                            mainPanel(
                              # Output: table 
                              DT::dataTableOutput("table")                          
                            )
                          )
                 ),
                 #Graph2: panel
                 tabPanel("Average Prevailing Salary",
                          mainPanel(
                            plotOutput("plot2")
                          )
                        )
)



server <- function(input, output) 
{
  
  # table output(input year and salary)
  output$table <- DT::renderDataTable(DT::datatable({
    # certified data
    data_cer<-filter(data,CASE_STATUS=="CERTIFIED")
    # choose Year
    data_t1<- filter(data_cer,YEAR==input$year)
    # choose salary
    # salary<-c(80000.0,1200000)
    data_t1<-filter(data_t1,PREVAILING_WAGE>=input$salary[1] & PREVAILING_WAGE<=input$salary[2])
    # choose columns
    data_total <- data_t1[,which(colnames(data_t1) %in% c("EMPLOYER_NAME","JOB_TITLE"))]
    data_total_freq<- as.data.frame(table(data_total[,1]))
    
    # choose data analyst
    data_DA <- data_t1 %>% filter(str_detect(JOB_TITLE, "DATA ANAL"))
    data_DA<- data_DA[,which(colnames(data) %in% c("EMPLOYER_NAME","JOB_TITLE"))]
    data_DA_freq<- as.data.frame(table(data_DA[,1]))
    
    #combine
    data_tfin<-merge(data_total_freq,data_DA_freq,by="Var1")
    data_tfin<-filter(data_tfin,Freq.x!=0)
    
    # top 10 rate
    data_tfin_10<-top_n(data_tfin,10,Freq.y)
    colnames(data_tfin_10)<-c("EMPLOYER_NAME","All Certified Employee","Certified data analysts")
    data_tfin_10[order(data_tfin_10[,3],decreasing = T),]
  }))
  
  output$plot1 <- renderPlot({
    #clean the data in order to obtain top n HIB application of job_titles
    s1 <- data %>%
      filter(YEAR == input$year,
             !is.na(JOB_TITLE)) %>%
      group_by(JOB_TITLE) %>%
      summarise(count = n()) %>%
      top_n(input$rownum) 
    
    #obtain top n job_titles' names
    names <- s1$JOB_TITLE
    
    #obtain the whole data of the jobs, the jobs that are the top n job_titles selected above
    s2 <- data %>%
      filter(JOB_TITLE == names) %>%
      mutate(H1B_STATUS = ifelse(CASE_STATUS == "CERTIFIED","CERTIFIED","OTHERS")) %>%
      group_by(JOB_TITLE, H1B_STATUS) %>%
      summarise(n = n()) 
    
    #plot the top n job_titles, showing both certified and other status of HIB applications
    ggplot(s2, aes(x=reorder(JOB_TITLE,n),y= n,fill = H1B_STATUS))+
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        
        title = paste0("Approval condition of the top ", 
                       input$rownum, " job types "),
        x ="Job Types",
        y ="Count" ,
        fill = "H1B Status") +
      scale_fill_manual(values=c("#9ecae1", "#3182bd"))
  })
  
  
  # map of all certified jobs
  output$map <- renderLeaflet({
    # certified data
    data_map1<-filter(data,CASE_STATUS=="CERTIFIED")
    # choose Year
    data_map<- filter(data_map1,YEAR==input$year)
    # map 
    leaflet(data_map) %>% addTiles() %>% addMarkers(
      clusterOptions = markerClusterOptions()
    )
  })
  # purpose of this chart is to visualize changes in prevailing wages for data analyst and data scientist jobs.
  output$plot2 <- renderPlot({
    data_analyst <- data %>% filter(str_detect(JOB_TITLE, "DATA ANAL")) #filter for data analyst jobs by choosing rows that job tile includes "data anal", because in the raw data, there are different names for similar jobs.
    data_scien <- data %>% filter(str_detect(JOB_TITLE, "DATA SCIEN"))  #filter for data scientist jobs by choosing rows that job tile includes "data scien", because in the raw data, there are different names for similar jobs.
    
    nrow(data_scien) #validate job numbers 
    nrow(data_analyst) #validate job numbers
    
    certi_data_analyst <- filter(data_analyst, CASE_STATUS =="CERTIFIED")  #filter for only certified applications
    certi_data_scien <- filter(data_scien, CASE_STATUS =="CERTIFIED")      #filter for only certified applications
    
    certi_data_analyst$JOB_TITLE<- "Data Analyst"   #save the same name for similar jobs, such as "data analyst", "sr. data analyst", etc.
    certi_data_scien$JOB_TITLE <- "Data Scientist"  #save the same name for similar jobs, such as "data scientists", "sr. data scientists", etc.
    
    df <- rbind(certi_data_analyst,certi_data_scien) #put 2 data frames together
    df2 <- group_by(df,YEAR,JOB_TITLE)               # group combined data frame by year and job title.
    total <- summarise(df2, ave_wage = mean(PREVAILING_WAGE))
    
    ggplot(total,                                             #plot the chart.
           aes(x=YEAR, y=ave_wage/1000, col= JOB_TITLE)) +    # wage in thousand
      geom_line() + geom_point() + theme_bw()+ theme(legend.position="right")+ #add theme to the chart  
      labs(
        x="Year", y="Average salary (thousands USD)",         
        title="Average prevailing salaries (per year)",     # add title
        subtitle="Only CERTIFIED applications included"     # add subtile 
      )
  })
}
# Create app
shinyApp(ui = ui, server = server)


