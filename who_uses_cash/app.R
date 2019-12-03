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
                                    
                        #used tabset panel to create a panel within my overall panel 
                                    tabsetPanel(
                                    tabPanel("Cash Usage by Education",
                                             
                                      #used sidebar panel to separate the sidebar from the main panel
                                      
                                      sidebarPanel(
                                        
                                      #used select input to create a drop down menu option of education levels
                                        
                                      selectInput("educ", "Select an education level:",
                                                  choices = c("Some High School, No Degree" = 1,
                                                              "High School Degree" = 2,
                                                              "Bachelors Degree" = 4,
                                                              "Masters Degree" = 5,
                                                              "Professional Degree" = 6,
                                                              "Doctorate Degree" = 7)
                                      )),
                                      
                                      #main panel plotoutput, the name "plot_1" corresponds to my function in the output section
                                      
                                    mainPanel(
                                      h2("Decreasing Cash Usage with Education"),
                                      plotOutput("plot_1")
                                    )),
                                    
                                    #new panel breaking down cash usage by income 
                                    
                                    tabPanel("Cash Usage by Income",
                                    
                                    #within sidebar table used select input to create a drop down of income level options
                                      
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
                                    
                                    #within main panel have plot output which corresponds with function in output section below
                                        mainPanel(
                                          h2("Decreasing Cash Usage with Income"),
                                          plotOutput("plot_2")    
                                             )),
                                    
                                    #new panel breaking down cash usage by age 
                                    
                                    tabPanel("Cash Usage by Age",
                                    
                                    #used slider iinput to create a slider of age values
                                             
                                      sidebarPanel(
                                      sliderInput("age",
                                                  "Select an age:",
                                                  min = 18,
                                                  max = 90,
                                                  value = 18)
                                      ),
                                    
                                    #within main panel have plot output which corresponds with function in output section below
                                      
                                    mainPanel(
                                        h2("Increasing Cash Usage with Age"),
                                        plotOutput("plot_3")
                                      )))),
                              
                        #Closed tabset panel and created a new tabpanel for what is cash used for information
                        
                           tabPanel("What Is Cash Used For?",
                                    
                            #within sidebar used checkbox group to make year selection options
                            
                                    sidebarPanel(
                                      checkboxGroupInput("year", 
                                                         h3("Select Year(s)"), 
                                                         choices = list("2016" = "2016", 
                                                                        "2017" = "2017"),
                                                         selected = c("2016", "2017"))),
                               
                            #within main panel have plot output which corresponds with function in output section below
                                
                             mainPanel(
                                      h2("Cash Usage Greatest for Food & Convenience"),
                                      plotOutput("plot_4"))),
                 
                         #new tabpanel representing when cash is used also used tabset here to create a panel within a panel    
                  
                tabPanel("When Is Cash Used?",
                         tabsetPanel(
                           tabPanel("Cash Usage by Payment Type",
                         
                          #used select input within sidebar panel to create payment type choices 
                                    
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
                          
                          #within main panel have plot output which corresponds with function in output section below
                          
                         mainPanel(
                           h2("Transaction Volume is Highest at 1pm"),
                           plotOutput("plot_5")
                         )),
                         
                         #new tabpanel within tabset representing cash usage by transaction type, used select input again just as in past examples
                         
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
                                  
                                  #within main panel have plot output which corresponds with function in output section below
                                  
                                  mainPanel(
                                    h2("Specific Trends in Cash Usage by Transaction Type"),
                                    plotOutput("plot_6")
                                          
                         )))),
                
                        #closed tabset and created a new tabpanel with my regression model - included multiple paragraphs before and after my html input function 
                
                         tabPanel("What Factors Predict Cash Usage",
                         h1("Regression Analysis"),
                         p("This model has the following specification: Y = a + Bx + controls"),
                         p("Where y is a binary variable for whether a transaction was carried out using cash, B is a vector of demographic characteristics of the person including age, income, education and race and controls is a vector of transaction level controls including the amount of payment, the year the payment was made and the transaction type."), 
                         p("Since the dependent variable of the regression is a binary variable, a logistic model was used. Within this regression specification, the coefficients are interpreted as a relative increase/decrease in the log of the probability of a cash transaction."),
                         htmlOutput("regression"),
                         p("Based on the findings of the model age, income and education level are strong predictors of cash usage. As we saw in the graphs, higher age is assocaited with higher probability of cash usage, while higher education and income level are associated with lower probabilities of cash usage."),
                         p("Additionally, the coefficients on the race dummy variables for Whites and Asians are significant and negative at the 10% level - meaning that white and Asian people have a lower probability of transacting with cash. While the transaction level variables were used as controls the coefficients on these variables are also very interesting. The coefficient on transaction is negative - suggesting that cash is more likely to be used on lower value transactions. The coefficient on services, fast food and restaurants are also signficant and postive - confirming trends shown in the transaction level graphs that cash is more likely to be used with these transaction types."),
                         p("Similarly, the coefficients on preferences for cash are very interesting. Preferences for cash are divided into dummy variables based on transaction value - where paypref_25to50, for example, represents preference for using cash for transactions between $25 and $50. Interestly, those who prefer cash for transactions lower than $50 are less likely to transact with cash, while those who prefer cash for transactions between $50 and $100 are more likely to transact with cash. This trend is especially interesting given the earlier finding that cash is more likely to be used for lower value transactions. It suggests that preference for higher values of cash is more indicative of an individual's overall affinity for cash use."),
                         p("Lastly, the dummy variable for year2017 is positive - suggesting that the probability of using cash is greater in 2017, though we cannot make any definitive conclusions because we have not controled for differences in the number of observations per year (the total number of transactions in 2017, however, is lower than the total number of transactions for both 2015 and 2016.")),
                
                #final tabpanel with the about section            
                
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

#data reactive code for section 1 connects filters for inputs that are selected in the sidebar tab   
  datareact1 <- reactive({
    cash %>%
      filter(education_level == input$educ) %>%
      filter(pi %in% c(1,2,3,4,5,6,7)) %>%
      group_by(pi) %>%
      count()
  })

#output code for section 1 that uses filtered data in the reactive function to create the given plot     
  
  output$plot_1 <- renderPlot({
    
    datareact1() %>%
      ggplot(aes(x=factor(pi), y=n, fill=factor(ifelse(pi=="1","Highlighted","Normal")))) + geom_col(show.legend = FALSE) + scale_fill_manual(name = "pi", values = c("#18BC9C", "#2C3E50")) + labs(title = "Frequency of Payment Methods Used", x= "Payment Method", y = "Count") + scale_x_discrete(labels=c("1" = "Cash", "2" = "Check", "3" = "Credit Card", "4" = "Debit Card", "5" = "Prepaid/Gift", "6" = "Bank Account", "7" = "Online Payment")) + theme(axis.text.x=element_text(angle=45, hjust=1))
    
  }) 

  #data reactive code for section 2 connects filters for inputs that are selected in the sidebar tab 

  datareact2 <- reactive({
    cash %>%
      filter(income_level == input$income) %>%
      filter(pi %in% c(1,2,3,4,5,6,7)) %>%
      group_by(pi) %>%
      count()
  })
  
  #output code for section 2 that uses filtered data in the reactive function to create the given plot 
  
  output$plot_2 <- renderPlot({
    
    datareact2() %>%
      ggplot(aes(x=factor(pi), y=n, fill=factor(ifelse(pi=="1","Highlighted","Normal")))) + geom_col(show.legend = FALSE) + scale_fill_manual(name = "pi", values = c("#18BC9C", "#2C3E50")) + labs(title = "Frequency of Payment Methods Used", x= "Payment Method", y = "Count") + scale_x_discrete(labels=c("1" = "Cash", "2" = "Check", "3" = "Credit Card", "4" = "Debit Card", "5" = "Prepaid/Gift", "6" = "Bank Account", "7" = "Online Payment")) + theme(axis.text.x=element_text(angle=45, hjust=1))
      
  }) 
  
  #data reactive code for section 3 connects filters for inputs that are selected in the sidebar tab 
  
  datareact3 <- reactive({
    cash %>%
    filter(age == input$age) %>%
      filter(pi %in% c(1,2,3,4,5,6,7)) %>%
      group_by(pi) %>%
      count()
    
  }) 

  #output code for section 3 that uses filtered data in the reactive function to create the given plot   
    
  output$plot_3 <- renderPlot({
  
    datareact3() %>%
      ggplot(aes(x=factor(pi), y=n, fill=factor(ifelse(pi=="1","Highlighted","Normal")))) + geom_col(show.legend = FALSE) + scale_fill_manual(name = "pi", values = c("#18BC9C", "#2C3E50")) + labs(title = "Frequency of Payment Methods Used", x= "Payment Method", y = "Count") + scale_x_discrete(labels=c("1" = "Cash", "2" = "Check", "3" = "Credit Card", "4" = "Debit Card", "5" = "Prepaid/Gift", "6" = "Bank Account", "7" = "Online Payment")) + theme(axis.text.x=element_text(angle=45, hjust=1)) 
  }) 
  
  
  #data reactive code for section 4 connects filters for inputs that are selected in the sidebar tab 
  
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
 
  #output code for section 4 that uses filtered data in the reactive function to create the given plot   
   
  output$plot_4 <- renderPlot({
    
    datareact4() %>%
      ggplot() + 
      geom_bar(mapping = aes(x = as.factor(merch), fill = as.factor(pi_2))) +  scale_fill_manual(values = c("#18BC9C", "#2C3E50", "#3498DB"), name = "Payment Type", labels=c("Cash", "Debit Card", "Credit Card")) + scale_x_discrete(labels=c( "7" = "Arts & Entertainment", "1" = "Convenience & Grocery", "4" = "Fast-food & Cafes", "2" = "Gas stations", "6" = "General Services", "3" = "Sit-down Restaurants",  "5" = "Shopping")) + theme(axis.text.x=element_text(angle=45, hjust=1)) + coord_flip() + labs(title = "Frequency of Transaction Type by Payment", x = "Transaction Type", y = "Frequency", fill = "Payment Type")
    
  })
  
  #data reactive code for section 5 connects filters for inputs that are selected in the sidebar tab 
      
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

  #output code for section 5 that uses filtered data in the reactive function to create the given plot   
  
  output$plot_5 <- renderPlot({
    
    datareact5() %>%
      ggplot(aes(x=time_rounded, y=n, color = as.factor(payment))) + geom_point() + geom_line(type = "longdash") + labs(title = "Change in Payment Method Use Over the Course of a Day", col = "Key", x = "Time", y = "Frequency of Payment Type") + scale_color_manual(labels = c("All Payment Types","Selected Payment Type"), values = c("#2C3E50", "#18BC9C")) + scale_x_continuous(breaks = c(400, 800, 1200, 1600, 2000, 2400), labels=c("4am", "8am", "12pm", "4pm", "8pm", "12am"))
    
  })
  
  #data reactive code for section 6 connects filters for inputs that are selected in the sidebar tab 
  
  datareact6 <- reactive({
    cash %>%
      mutate(time_rounded = 100*ceiling((time)/100)) %>%
      filter(!time_rounded %in% c("2450", NA, "0")) %>%
      mutate(time_rounded = as.numeric(time_rounded)) %>%
      filter(!is.na(merch)) %>% 
      group_by(time_rounded) %>%
      summarise(avg = sum(merch == input$transaction)/n(), na.rm = TRUE) 

  })

  #output code for section 6 that uses filtered data in the reactive function to create the given plot   
  
  output$plot_6 <- renderPlot({
    
    datareact6() %>%
    ggplot(aes(x=time_rounded, y=avg)) + geom_point(color = "#18BC9C", size = 2) + geom_line(linetype = "longdash", color = "#18BC9C") + labs(title = "Change in Transaction Type Frequency Over the Course of a Day", x = "Time", y = "Percentage of Given Transaction Type") + scale_x_continuous(breaks = c(400, 800, 1200, 1600, 2000, 2400), labels=c("4am", "8am", "12pm", "4pm", "8pm", "12am"))
    
  })  
  
#got this last code chunck online when I looked up how to load my regression html document to the shiny app, connects with my regression html input in above section
  
  getPage <- function() {
    return(includeHTML("regression.html"))
  }
  
  output$regression <- renderUI({
    
    getPage()})
  
}

# Run the application 
shinyApp(ui = ui, server = server)
