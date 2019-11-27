#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#load packages
library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)

#load data

cash <- read_csv("merged_2.csv")

# Define UI (user interface) for application, which is the formating/way the interface will look to users
ui <- fluidPage(theme = shinytheme("flatly"),
                
                #used navbarpage to create a navbar shiny app set up, used tab panel to create the tabs within my shiny app, within the first tab map I called imageoutput referencing map, which I then specified in the server section below, inside the second tab, about I used h4 to set a header and p to specify a paragraph of text that I entered 
                navbarPage("Exploring Consumer Cash Usage Data",
                           tabPanel("Who Uses Cash?",
                                    tabsetPanel(
                                    tabPanel("Cash Usage by Education",
                                    sidebarPanel(
                                      selectInput("educ", "Select an education level:",
                                                  choices = c("Some High School, No Degree" = 1,
                                                              "High School Degree" = 2,
                                                              "Associate Degree" = 3,
                                                              "Bachelors Degree" = 4,
                                                              "Masters Degree" = 5,
                                                              "Professional Degree" = 6,
                                                              "Doctorate Degree" = 7)
                                      )),
                                    mainPanel(
                                      plotOutput("plot_1")
                                    )),
                                    tabPanel("Cash Usage by Income",
                                       sidebarPanel(
                                       selectInput("income", "Select a Household Income level:",
                                                    choices = c("Less than 10,000" = 1,
                                                                "10,000 - 19,999" = 2,
                                                                "20,000 - 29,999" = 3,
                                                                "30,000 - 39,999" = 4,
                                                                "40,000 - 49,999" = 5,
                                                                "50,000 - 59,999" = 6,
                                                                "60,000 - 74,999" = 7,
                                                                "75,000 - 99,999" = 8,
                                                                "100,000 - 124,999" = 9,
                                                                "125,000 - 199,999" = 10,
                                                                "200,000 - 499,999" = 11,
                                                                "500,000 Plus" = 12 )  
                                         )),                          
                                        mainPanel(
                                          plotOutput("plot_2")    
                                             )),
                                    tabPanel("Cash Usage by Age",
                                    sidebarPanel(
                                      sliderInput("age",
                                                  "Select an age:",
                                                  min = 18,
                                                  max = 90,
                                                  value = 18)
                                      ),
                                      mainPanel(
                                        plotOutput("plot_3")
                                      )))),
                           tabPanel("What Is Cash Used For?",
                                    sidebarPanel(
                                      checkboxGroupInput("year", 
                                                         h3("Select Year(s)"), 
                                                         choices = list("2016" = "2016", 
                                                                        "2017" = "2017"),
                                                         selected = c("2016", "2017"))),
                                    mainPanel(
                                      plotOutput("plot_4"))),
        
                tabPanel("When Is Cash Used?",
                         tabsetPanel(
                           tabPanel("Cash Usage by Payment Type",
                         sidebarPanel(
                           selectInput("payment", "Select a Payment Type",
                                       choices = c("Cash" = 1,
                                                   "Check" = 2,
                                                   "Credit Card" = 3,
                                                   "Debit Card" = 4,
                                                   "Prepaid/gift/EBT card" = 5,
                                                   "Bank Account" = 6,
                                                   "Online Banking" = 7)
                           )), 
                         mainPanel(
                           plotOutput("plot_5")
                         )),
                         tabPanel("Cash Usage by Transaction Type",
                                  sidebarPanel(
                                    selectInput("transaction", "Select a Transaction Type",
                                                choices = c("Convenience & Grocery" = 1,
                                                            "Gas stations" = 2,
                                                            "Sit-down Restaurants" = 3,
                                                            "Fast-food & Cafes" = 4,
                                                            "Shopping" = 5,
                                                            "General Services" = 6,
                                                            "Arts & Entertainment" = 7,
                                                            "Financial Payments" = 15,
                                                            "Donations" = 17,
                                                            "Medical expenses" = 18)
                                    )), 
                                  mainPanel(
                                    plotOutput("plot_6")
                                          
                         )))),
                tabPanel("What Factors Predict Cash Usage",
                         htmlOutput("regression")
                ),
                           tabPanel("About",
                                    h1("About Section"),
                                    h2("Background/Research Questions"),
                                    p("Technology has rapidly transformed almost every aspect of our lives - including the way we carry out payments. Within recent years, the devlopment of PayPal, Ripple, Venmo among other mobile money payment systesm have acclerated the presence and access of cashless technologies across societies. But, despite this growth in technology, cash continues to persist both for use in transactions and a store of value. In order to answer this question we must understand: Who is using cash? Who is holding cash? What types of transactions is it used for? And, how often is it used?"),
                                    h3("The Data"),
                                    p("To answer this question I used data from the Federal Reserve's Diary of Consumer Payment Choice (DCPC). The DCPC is a survey of consumer payment behavior run in conjunction with the University of Southern California’s Understanding America Study (UAS). Respondents were randomly assigned a three-day period and asked to track all of their payments using an online questionnaire. I plan to combine data from the 2017, 2016 and 2015 data sets as well as use both the individual level and transaction level data."),
                                    h4("About Me"),
                                    p("My name is Jessica, I'm a senior economics concentrator and I am writing my thesis on a similar topic analyzing the changes in cash demand across countries and across time. I'm really excited to be using data science to observe trends that I can potentially include in my larger inquiry into cash in  my thesis.")
                           )))


# Define server logic, which is what is required to create the data output (in this case showing our gif), made sure to set deletefile = FALSE so it does not delete our gif, output$ is set to map to corespond to map in the imageoutput specified in the UI above 
server <- function(input, output) {
  
  datareact1 <- reactive({
    cash %>%
      filter(education_level == input$educ) %>%
      filter(pi %in% c(1,2,3,4,5,6,7)) %>%
      group_by(pi) %>%
      count()
  })
  
  output$plot_1 <- renderPlot({
    
    datareact1() %>%
      ggplot(aes(x=factor(pi), y=n, fill=factor(ifelse(pi=="1","Highlighted","Normal")))) + geom_col(show.legend = FALSE) + scale_fill_manual(name = "pi", values = c("#18BC9C", "#2C3E50")) + labs(title = "Frequency of Payment Methods Used", x= "Payment Method", y = "Count") + scale_x_discrete(labels=c("1" = "Cash", "2" = "Check", "3" = "Credit Card", "4" = "Debit Card", "5" = "Prepaid/Gift", "6" = "Bank Account", "7" = "Online Payment")) + theme(axis.text.x=element_text(angle=45, hjust=1))
    
  }) 
  
  datareact2 <- reactive({
    cash %>%
      filter(income_level == input$income) %>%
      filter(pi %in% c(1,2,3,4,5,6,7)) %>%
      group_by(pi) %>%
      count()
  })
  
  output$plot_2 <- renderPlot({
    
    datareact2() %>%
      ggplot(aes(x=factor(pi), y=n, fill=factor(ifelse(pi=="1","Highlighted","Normal")))) + geom_col(show.legend = FALSE) + scale_fill_manual(name = "pi", values = c("#18BC9C", "#2C3E50")) + labs(title = "Frequency of Payment Methods Used", x= "Payment Method", y = "Count") + scale_x_discrete(labels=c("1" = "Cash", "2" = "Check", "3" = "Credit Card", "4" = "Debit Card", "5" = "Prepaid/Gift", "6" = "Bank Account", "7" = "Online Payment")) + theme(axis.text.x=element_text(angle=45, hjust=1))
      
  }) 
  
  datareact3 <- reactive({
    cash %>%
    filter(age == input$age) %>%
      filter(pi %in% c(1,2,3,4,5,6,7)) %>%
      group_by(pi) %>%
      count()
    
  }) 
  
  output$plot_3 <- renderPlot({
  
    datareact3() %>%
      ggplot(aes(x=factor(pi), y=n, fill=factor(ifelse(pi=="1","Highlighted","Normal")))) + geom_col(show.legend = FALSE) + scale_fill_manual(name = "pi", values = c("#18BC9C", "#2C3E50")) + labs(title = "Frequency of Payment Methods Used", x= "Payment Method", y = "Count") + scale_x_discrete(labels=c("1" = "Cash", "2" = "Check", "3" = "Credit Card", "4" = "Debit Card", "5" = "Prepaid/Gift", "6" = "Bank Account", "7" = "Online Payment")) + theme(axis.text.x=element_text(angle=45, hjust=1)) 
  }) 
  
  datareact4 <- reactive({
    cash %>%
      mutate(pi_2 = ifelse(pi == 1, 1,
                           ifelse(pi == 2, 2,
                                  ifelse(pi == 3, 3,
                                         ifelse(pi == 4, 4, 5))))) %>%
      filter(pi_2 %in% c(1,3,4)) %>%
      filter(merch %in% c(1,2,3,4,5,6,7)) %>%
      mutate(merch = fct_relevel(as.factor(merch), c("3", "5", "6", "2", "4", "1", "7"))) %>%
      drop_na(merch) %>%
      filter(year == input$year)
    
  })
  
  output$plot_4 <- renderPlot({
    
    datareact4() %>%
      ggplot() + 
      geom_bar(mapping = aes(x = as.factor(merch), fill = as.factor(pi_2))) +  scale_fill_manual(values = c("#18BC9C", "#2C3E50", "#3498DB"), name = "Payment Type", labels=c("Cash", "Debit Card", "Credit Card")) + scale_x_discrete(labels=c( "7" = "Arts & Entertainment", "1" = "Convenience & Grocery", "4" = "Fast-food & Cafes", "2" = "Gas stations", "6" = "General Services", "3" = "Sit-down Restaurants",  "5" = "Shopping")) + theme(axis.text.x=element_text(angle=45, hjust=1)) + coord_flip() + labs(title = "Frequency of Transaction Type by Payment", x = "Transaction Type", y = "Frequency", fill = "Payment Type")
    
  })
      
  datareact5 <- reactive({
    cash %>%
    mutate(time_rounded = 100*ceiling((time)/100)) %>%
      filter(!time_rounded %in% c("2450", NA, "0")) %>%
      mutate(payment = ifelse(pi == input$payment, 1, 0)) %>%
      filter(!is.na(payment)) %>% 
      group_by(time_rounded, payment) %>%
      count() %>% 
      ungroup() %>% 
      group_by(time_rounded) %>% 
      mutate(n = ifelse(payment == 0, sum(n), n))
  })
  
  output$plot_5 <- renderPlot({
    
    datareact5() %>%
      ggplot(aes(x=time_rounded, y=n, color = as.factor(payment))) + geom_point() + geom_line(type = "longdash") + labs(title = "Change in Payment Method Use Over the Course of a Day", col = "Key", x = "Time", y = "Frequency of Payment Type") + scale_color_manual(labels = c("All Payment Types","Selected Payment Type"), values = c("#2C3E50", "#18BC9C")) + scale_x_continuous(breaks = c(400, 800, 1200, 1600, 2000, 2400), labels=c("4am", "8am", "12pm", "4pm", "8pm", "12am"))
    
  })
  
  datareact6 <- reactive({
    cash %>%
      mutate(time_rounded = 100*ceiling((time)/100)) %>%
      filter(!time_rounded %in% c("2450", NA, "0")) %>%
      mutate(time_rounded = as.numeric(time_rounded)) %>%
      filter(!is.na(merch)) %>% 
      group_by(time_rounded) %>%
      summarise(avg = sum(merch == input$transaction)/n(), na.rm = TRUE) 

  })
  
  output$plot_6 <- renderPlot({
    
    datareact6() %>%
    ggplot(aes(x=time_rounded, y=avg)) + geom_point(color = "#18BC9C", size = 2) + geom_line(linetype = "longdash", color = "#18BC9C") + labs(title = "Change in Transaction Type Frequency Over the Course of a Day", x = "Time", y = "Percentage of Given Transaction Type") + scale_x_continuous(breaks = c(400, 800, 1200, 1600, 2000, 2400), labels=c("4am", "8am", "12pm", "4pm", "8pm", "12am"))
    
  })  
  
  getPage <- function() {
    return(includeHTML("regression.html"))
  }
  
  output$regression <- renderUI({
    
    getPage()})
  
}

# Run the application 
shinyApp(ui = ui, server = server)
