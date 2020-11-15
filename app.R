## app.R ##

library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(plotly)
library(rsconnect)
library(maps)

data=read.csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vRX_JPMG9mYHhDPdWVt8NEpl8dFDYGpkmPmltysHbmPCkV-wTTT3-0BYcNG7PXuvJjApMUHmO2oWDWJ/pub?gid=738426506&single=true&output=csv')

attach(data) 


ui <- dashboardPage(
  
  # Application title
  dashboardHeader(title = "Bicycle Market"),
  # Settle of the sidebar
  dashboardSidebar(
    menuItem("Introduction",tabName = 'intro',icon=icon("globe")),
    menuItem("Profits per Category", tabName = "cat", icon = icon("dollar-sign")),
    menuItem("Discount on Categories", tabName = "discounts", icon = icon("percentage")),
    menuItem("Customers Analysis", tabName = "customer", icon = icon("venus-mars")),
    
    fluidRow(
      #filters by Product_Category
      selectInput(inputId = "CountryInput",
                  label = "Country",
                  choices = c("Australia", "Canada", "France","Germany", "United Kingdom", "United States"),  selected = "Australia",
                  multiple = TRUE),
      #filters by Product_Category 
      checkboxGroupInput(inputId = "CategoryInput",
                         label = "Product Category",
                         choices = c("Accessories","Bikes", "Clothing"), selected = c("Accessories","Bikes", "Clothing")),
      #filters by Sub_Category 
      checkboxGroupInput(inputId = "SubInput",
                         label = "Sub-category",
                         choices = c("Bike Racks", "Bike Stands","Bottles and Cages","Caps","Cleaners","Fenders","Gloves", "Helmets","Hydration Packs" , "Jerseys",
                                     "Mountain Bikes", "Road Bikes", "Shorts" ,"Socks", "Tires and Tubes", "Touring Bikes", "Vests"),
                         selected = c("Bike Racks", "Bike Stands","Bottles and Cages","Caps","Cleaners","Fenders","Gloves", "Helmets","Hydration Packs" , "Jerseys",
                                      "Mountain Bikes", "Road Bikes", "Shorts" ,"Socks", "Tires and Tubes", "Touring Bikes", "Vests")),
      #filters by Year
      checkboxGroupInput(inputId="YearInput",
                         label="Year",
                         choices= c("2011","2012","2013","2014","2015","2016"),
                         selected= c("2011","2012","2013","2014","2015","2016")))),
  
  #Settle of the dashboard body
  dashboardBody(
    #Title present on top of all pages
    h4('Welcome to our dashboard analysing the bicycle market in 6 countries'),
    #Defining the component of each items 
    tabItems(
      #Items included in the tab menu Item Map
      tabItem(tabName='intro',
              fluidRow(
                box(title = "Introduction", width = 12, status = "primary", background = "light-blue", 
                    "We chose to focus on the bike market. Let's imagine we are entrepreneurs and we want to create a bicycle brand, we need to know the market trends : overall information on the market in a first step, then we need to choose which product to focus on and a customer analysis to choose a target. 
                    We chose a dataset giving transactions in 6 countries : Australia, Canada, France, Germany, UK, US. For these transactions we have information about the product, the price and profit and some information about the buyer. This information has been recorded from 2011 to 2015 allowing us to analyse trends. 
                    We started from the observation that bikes are a trendy product, its use as travel mode is increasing, and it's also really used to practice sport.
                    We wanted to do a linear regression on Customer Gender and Customer Age but the p-value was >0.5, meaning it is not significant.")),
              fluidRow(
                box( title = "Use of bike as travel mode in Europe", "Data collected from ibike.org", status = "primary",
                     collapsible = TRUE, solidHeader=TRUE,
                     plotOutput("MapChart"),
                     width=6,
                     "Data collected from ibike.org"),
                box( title = "Bike Sales Y/Y", "Country filter applies here",status = "primary",
                     collapsible = TRUE, solidHeader=TRUE,
                     plotOutput("salesPlot"),
                     width=6)),
              fluidRow(
                box( width = 12, status = "primary", background = "purple", 
                     "The map shows the use of bicycles as travel mode in different european countries. We see that Scandinavian countries are leading-edge, but Germany, one of the countries we will focus on, is also quite advanced.  
                    The graph on the right hand-side show the evolution of the turnover on the bicycle market (including related products as accessories, clothing etc) for the 6 countries we focus from 2011 to 2015. It confirms that the market is growing for all of these countries. 
                    We'll now try to dig deeper into the categories and subcategories profitability to define what would be interesting to produce for our new brand (profitability per category and discount tables). In a second time we will perform a customer analysis to choose our target. " )),
              fluidRow(
                infoBox("GERGAUD", "THOMAS", icon = icon("thumbs-up", lib = "glyphicon"), color = "yellow", fill = TRUE),
                infoBox("ROBERT", "LEA", icon = icon("thumbs-up", lib = "glyphicon"), color = "maroon", fill = TRUE),
                infoBoxOutput("link")
              )),
      
      tabItem( tabName = 'cat',
               fluidRow( 
                 box( title = "Anglo Saxons countries generate more profits", "Country and Product filters apply here", status = "primary",
                      collapsible = TRUE, solidHeader=TRUE,
                      plotOutput("CountryChart"),
                      width = 6), 
                 box( title = "Profits from clothing are negligent compared to bikes", "Subcategory and Product filters apply here", status = "primary",
                      collapsible = TRUE, solidHeader=TRUE,
                      plotOutput("SubCatChart"), 
                      width = 6)),
               
               fluidRow(
                 box( title = "Accessories which represent low profit in this industry are sold in huge quantity" , "Subcategory and Product filters apply here", status = "primary",
                      collapsible = TRUE, solidHeader=TRUE,
                      plotOutput("SalesQuantChart"),
                      width = 6),
                 box( title = "In every country the more profits are made on Road Bikes" , "Country and Product filters apply here",  status = "primary",
                      collapsible = TRUE, solidHeader=TRUE,
                      plotOutput("SubCatR"),
                      width = 6))),
      
      tabItem( tabName = 'discounts',
               fluidRow(
                 box( title = "Accessories is the most discounted, whereas bikes is the most profitable", "Subcategory and Product filters apply here" , status = "primary",
                      collapsible = TRUE, solidHeader=TRUE,
                      plotOutput("Discounts"),
                      width = 6),
                 
                 box( title = "Canada and United Kingdom are offering low discounts in comparison with profits they generate" , "COuntry and Product filters apply here", status = "primary",
                      collapsible = TRUE, solidHeader=TRUE,
                      plotOutput("DiscountsR"),
                      width = 6))),
      
      tabItem(tabName= 'customer',
              fluidRow(
                box( title = "Proportion of males buying bikes increase Y/Y  " , "Year filter allies here", status = "primary",
                     collapsible = TRUE, solidHeader=TRUE,
                     plotOutput("GenderChart"),
                     width = 6),
                box( title = "Young Adult segment decreases to the benefit of the Adult one" , "Year filter allies here", status = "primary",
                     collapsible = TRUE, solidHeader=TRUE,
                     plotOutput("AgeChart"),
                     width = 6)))))
)

#Set-up of the server function which will implement our histogram and diagram on the UI
server <- function(input, output) {
  
  #INTRO tab
  
  #Creation of the map on the first sidebar item
  output$MapChart <- renderPlot({
    
    df <-data.frame(region=c("Netherlands", "Denmark", "Germany", "Switzerland", "Sweden", "Austria", "UK", "France", "Italy", "Spain", "Poland", "Norway", "Finland"),
                    value=c(0.30, 0.20, 0.12, 0.10, 0.10, 0.09, 0.08, 0.05, 0.05, 0.01, 0.01, 0.23, 0.22))
    
    WorldData <- map_data('world')
    
    Total <- WorldData[WorldData$region %in% df$region, ]
    Total$value <- df$value[match(Total$region, df$region)]
    
    ggplot(Total, aes(x=long, y=lat, group = group, fill = value)) + 
      geom_polygon(colour = "white") +
      scale_fill_continuous(low = "thistle2", 
                            high = "darkred", 
                            guide="colorbar") +
      theme_bw()  + 
      labs(fill = "% of trip by bike" , x="", y="") +
      scale_y_continuous(breaks=c()) + 
      scale_x_continuous(breaks=c()) + 
      theme(panel.border =  element_blank())
    
  })
  # Plot of revenue par year
  
  output$salesPlot <- renderPlot({
    rint <- data %>% filter(Country %in% input$CountryInput)
    
    filterint <- 
      group_by(rint, Year) %>%
      summarise(totRev=sum(Revenue)/1000000)
    
    plot(filterint$Year,
         filterint$totRev, 
         main = "Revenue (Million$) per Year of selling bike",
         ylab="Revenue (M$)",
         xlab="Year",
         type="l")
  })  
  #Creation of the infobox link of the dataset
  output$link <- renderInfoBox({
    infoBox(title = "Dataframe", "check out the dataframe (click here)", icon = shiny::icon("table"), color = "purple", href = "https://www.kaggle.com/liyingiris90/bike-sales", fill = TRUE )
  })
  
  #CATEGORY tab
  
  # create calculated columns using given values from the dataset 
  price=Unit_Price*Order_Quantity
  data$DicountGiven <- round((price-data$Revenue)/data$Revenue,2) 
  
  #Creation of the histogram Profits per category per Country
  output$CountryChart <- renderPlot({
    
    # Grouping data by Category and Country so it can be filtered
    rc <- group_by(data, Country, Product_Category) %>%
      summarise(TotalProfit = sum(Profit)/1000000)
    
    # allows the data displayed in the charts to react to the Countries and Product Category filters in the UI
    filtered <- 
      rc %>%
      filter(Country %in% input$CountryInput &
               Product_Category %in% input$CategoryInput)
    
    # Clustered Bar Chart showing total profits Per Product Category by Country 
    ggplot(filtered, aes(x=Country, y= TotalProfit, fill = Product_Category)) + 
      geom_bar(stat = "identity", position = "dodge", color = "black") + 
      ggtitle("Profits (MM$) per Category by Country") + 
      labs(y="Total Profits")
    #The same method is used for others graphic.
  })   
  
  #Creation of the histogram Profits per Sub-Category
  output$SubCatChart <- renderPlot({
    
    scc <- group_by(.data =  data, Sub_Category, Product_Category) %>%
      summarise(TotalProfit = sum(Profit)/1000000)
    
    sliced <- 
      scc %>%
      filter(  Product_Category %in% input$CategoryInput & 
                 Sub_Category %in% input$SubInput)
    
    ggplot(sliced, aes(x=Product_Category, y= TotalProfit, fill = Sub_Category, color = Country)) + 
      geom_bar(stat = "identity", position = "dodge", color = "black") + 
      ggtitle("Profits (MM$)per Sub-Category") + 
      labs(y="Total Profits")
  }) 
  
  #Creation of the histogram Quantity Sold per sub-category
  output$SalesQuantChart <- renderPlot({
    
    sqs <- group_by(data,Sub_Category, Product_Category) %>%
      summarise(QuantitySold = sum(Order_Quantity)/1000)
    
    filters <- 
      sqs %>%
      filter(
        Product_Category %in% input$CategoryInput& 
          Sub_Category %in% input$SubInput)
    
    ggplot(filters, aes(x=Product_Category, y= QuantitySold, fill = Sub_Category)) + 
      geom_bar(stat = "identity", position = "dodge", color = "black") + 
      ggtitle("Quantity Sold per Sub-Category") + 
      labs(y="Quantity Sold (thsd)")
  })
  
  #Creation of the histogram Discounts Given per Sub-Category
  output$Discounts <- renderPlot({
    
    dgsc <- group_by(data,Sub_Category, Product_Category) %>%
      summarise(TotalDiscounts = sum(DicountGiven))
    
    slices <- 
      dgsc %>%
      filter(
        Product_Category %in% input$CategoryInput& 
          Sub_Category %in% input$SubInput)
    
    ggplot(slices, aes(x=Product_Category, y= TotalDiscounts, fill = Sub_Category)) + 
      geom_bar(stat = "identity", position = "dodge", color = "black") + 
      ggtitle("Discounts Given per Sub-Category") + 
      labs(y="Total Discounts")
  })
  
  #DISCOUNT tab
  
  #Creation of the histogram Discounts given by region and by Sub-Category
  output$DiscountsR <- renderPlot({
    
    dgsc <- group_by(data, Sub_Category, Country) %>%
      summarise(TotalDiscounts = sum(DicountGiven))
    
    slices <- 
      dgsc %>%
      filter(Country %in% input$CountryInput &
               Sub_Category %in% input$SubInput)
    
    ggplot(slices, aes(x=Country, y= TotalDiscounts, fill = Sub_Category)) + 
      geom_bar(stat = "identity", position = "dodge", color = "black") + 
      ggtitle("Discounts by region per Sub-Category") + 
      labs(y="Total Discounts")
  })
  
  #Creation of the histogram Profits per category by country
  output$SubCatR <- renderPlot({
    
    dgsc <- group_by(data, Sub_Category, Country) %>%
      summarise(TotalProfit = sum(Profit)/1000000)
    
    slices <- 
      dgsc %>%
      filter(Country %in% input$CountryInput &
               Sub_Category %in% c("Mountain Bikes", "Road Bikes", "Touring Bikes", "Fasteners"))
    
    ggplot(slices, aes(x=Sub_Category, y= TotalProfit, fill = Country)) + 
      geom_bar(stat = "identity", position = "dodge", color = "black") + 
      ggtitle("Profit (MM$)per type of bikes per country") + 
      labs(y="Total Profit")
  })
  
  #CUSTOMER ANALYSIS tab
  
  #Creation of the diagram Quantity sold by Gender
  output$GenderChart <- renderPlot({
    
    rca <- group_by(data, Year, Customer_Gender) %>%
      summarise(TotalSalesBike=sum(Order_Quantity)/1000)
    
    filtereda <- 
      rca %>%
      filter(Year %in% input$YearInput)
    
    ggplot(filtereda, aes(x="", y= TotalSalesBike, fill = Customer_Gender)) + 
      geom_bar(width=3, stat = "identity") + 
      ggtitle("Quantity Sold (thsd) by Gender") +
      coord_polar("y",start=0)
    
  }) 
  
  #Creation of the diagram quantity Sold by Age_Category
  output$AgeChart <- renderPlot({
    
    rco <- group_by(data, Year, Age_Group) %>%
      summarise(TotalSalesBike=sum(Order_Quantity)/1000)
    
    filteredo <- 
      rco %>%
      filter(Year %in% input$YearInput)
    
    ggplot(filteredo, aes(x="", y= TotalSalesBike, fill = Age_Group)) + 
      geom_bar(width=3, stat = "identity") + 
      ggtitle("Quantity (thsd) Sold by Age Category") +
      coord_polar("y",start=0)+
      scale_fill_brewer(palette="Dark2")
  })
  
}

shinyApp(ui = ui, server = server)

