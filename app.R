#loading necessary packages
library("ggplot2")
library("tidyverse")
library("ggridges")
library("shiny")
library("shinydashboard")
library("gridExtra")

#Reading the dataset
Financial_dataset <- read.csv("C:\\Users\\$$shan$$\\OneDrive\\Desktop\\archive\\Fortune_1000.csv")
#printing first few rows
head(Financial_dataset)
#Changng the character datatype to numeric
Financial_dataset$Market.Cap <- as.numeric(Financial_dataset$Market.Cap)
#Deleting Null values from the dataset
Financial_dataset <- na.omit(Financial_dataset)

#Setting up the shiny environment
ui <- dashboardPage(
      dashboardHeader(title = "Financial trend for 1000 companies"),
      dashboardSidebar(
        sliderInput(inputId = "slider1",
                    label="Select Number of Top Companies",
                    min = 2,
                    max = 20,
                    value= 10
        ),
        # radioButtons(
        #   inputId = "Gender",
        #   label = "Select the gender",
        #   choices = c("Male"="M","Female"="F")
        
        # ),
        selectInput(
          inputId = "Sector",
          label = "Select the sector",
          choices = unique(Financial_dataset$sector)
        ),
        selectInput(
          inputId = "Yax",
          label = "Select Y axis for plot",
          choices = c("revenue"="revenue","profit"="profit","Market Capital"="Market.Cap")
        )
      ),
      dashboardBody(
        plotOutput("bar"),
        #plotOutput("Ridge")
        plotOutput("lines")
      )
       
)

#
server <- function(input,output){
  
  output$bar <- renderPlot({
    p1<- Financial_dataset %>% arrange(rank) %>% slice_head(n=input$slider1) %>% ggplot(mapping=aes(x=revenue,y=fct_reorder(company,revenue),fill=company))+geom_bar(stat="identity")+ theme_minimal()
    p2 <- Financial_dataset %>% filter(sector==input$Sector) %>% ggplot(mapping= aes(x = profit, y = ceo_woman)) + geom_density_ridges(fill = "#00AFBB", rel_min_height = 0.01, alpha=0.6)+ theme_minimal()
    grid.arrange(p1,p2, ncol=2)
  }
  )
  output$lines <- renderPlot({
    ggplot(Financial_dataset,mapping = aes(x=num..of.employees,y=Financial_dataset[[input$Yax]],color=sector)) +geom_point(size=2) +geom_smooth(method = "lm",formula = y ~ x)+ theme_minimal()
  })
  

}


shinyApp(ui,server)



#
# #revelant Question
# ##1. Map with intensity plot for different locations
# ##2. Bar graph for women/male CEO
# ##3. Maximum Rank Change compared to last time
# ##4. Profit distribution for Male/Female CEO
# ##5. Top 10 companies based on market cap
# ##6.
#
#
#
#
#
#
#

