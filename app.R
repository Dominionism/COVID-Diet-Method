library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(DT)

diet <- read_delim("data/Fat_Supply_Quantity_Data.csv")

filterSet <- diet %>%
  select(Country, `Animal Products`, `Animal fats`, Eggs, `Fish, Seafood`, Meat, `Vegetal Products`, Vegetables, `Cereals - Excluding Beer`, Oilcrops, Deaths, Recovered, `Unit (all except Population)`) %>%
  filter(!is.na(c("Country, `Animal Products`, `Animal fats`, Eggs, `Fish, Seafood`, Meat, `Vegetal Products`, Vegetables, `Cereals - Excluding Beer`, Oilcrops, Deaths, Recovered, `Unit (all except Population)`")))

ui <- fluidPage(
  titlePanel("Diet Practices for COVID-19 & Effectiveness"),
  tabsetPanel(
    
    # Displays the home page.
    tabPanel("Home Page",
             setBackgroundImage(src = "snow.jpeg"),
             h3("COVID-19 Country Diet Data Set"),
             p("For this project, the goal is to provide awareness for the public regarding the different diet practices, and how it can potentially mitigate 
               the effects of the COVID-19 pandemic for the people. To be more specific, the primary focus is to provide the relevant information through different 
               visual representations about the impact of", em("Non-Vegetarian"), "and", em("Vegetarian"), "diets, and especially which option of the 
               the two is more effective. The data set used for this project comes from", strong(em("Johns Hopkins Center for Systems Science and Engineering CSSE website.")), 
               "Finally, it is also noteworthy that the project will be working with a data set that contains", strong(nrow(diet)), "rows of data values."),
             img (src = "finalBanner.jpeg", height = 900, width = 1400),
             h3("Data Render Table"),
             dataTableOutput("sample")
    ),
    
    # Displays the meat chart.
    tabPanel("Meat Diet & Recovery",
             sidebarPanel(
               checkboxGroupInput("selection",
                                  "Choose Countries:",
                                  choices = unique(filterSet$Country),
                                  selected = "Afghanistan")
             ),
             mainPanel(
               h3("Consumption Percentage Visual Chart"),
               plotOutput("chart"),
               h3("Instructions & Basic Steps"),
               p("The bar chart automatically selects", strong("Afghanistan"), "as its default value. To include more countries, you must use the checkbox to the left of the screen. 
                 The checkbox contains all of the countries that exist within the data set. The objective of this chart is to display the different percentages of meat consumption per country and recovery rate. 
                 To start the comparison process, select the countries of interest and specifically look for a pattern that concludes a", strong("correlational relationship"), "between the percentage of meat consumption per country and their recovery rate."),
               h3("Avoiding Confusion"),
               p("In order to interpret the bar chart with proper comprehension, you must", strong("monitor"), "the meat consumption percentage to the right of the chart. This is crucial because the table only show the different selected", strong("countries"), "and 
                 their", strong("recovery"), "rate percentages. Therefore, by observing the meat consumption percentages of each country, which is color-coded, and comparing them to their corresponding recovery rate, only then can you potentially see a pattern."),
               h3("Constraints & Additional Notes"),
               p("For this chart panel, the maximum amount of countries selected contains no restrictions. Therefore, it is", strong(em("highly suggested")), "for the user to keep their selections within the range of", strong("4-6"), "countries. This is extremely 
                 important to note given that excessive amounts of selections can cause", strong("overlap"), "between the different country names on the x-axis."),
               h3("Meat Quality & Types"),
               p(
                 img(src = "meats.jpeg", height = 600, width = 900)
               ),
               p(strong(em("This image depicts many types of meat that a typical consumer may find at their local grocery store. Because varied meat compositions are preferred for certain cuisines, 
                 understanding and awareness of the range of meat products is critical. The grade and kind of meat vary across different sorts of cuisines, and culture can have a serious influence."))),
               p(
                 img(src = "beef.webp", height = 600, width = 900)
               ),
               p(strong(em("The image above displays the different parts of a cattle numbered in an orderly format. The numebers on the image communicate the part of the cattle's body in which the meat portion will 
                           be extracted from and what type of cuisines can be made with that particular meat component."))),
               h3("Meat Consumption & Health"),
               p(
                 img(src = "healthfacts.webp", height = 900, width = 900)
               ),
               h3("Seafood Quality & Types Visual Representation"),
               p(
                 img(src = "fish.jpeg", height = 600, width = 900)
               ),
               p(strong(em("Following the different meat qualities, another subcategory is seafood. Similar to its more general category, knowing the different types of seafood products (excluding plant-based contents) 
                           is extremely important because varying dishes require different qualities of seafood products. Once again, culture plays a major role in influencing the different types of seafood products that go into the cultural dishes."))),

               h3("Findings"),
               p("The", strong("purpose"), "of this section is to communicate that there is", strong(em("no correlation")), "between the amount of meat consumption and recovery rate post-COVID. According to extended research conducted on this topic, meat consumption 
                 'did not influence the length of COVID symptoms.' In this case, rather than having a correlational relationship, the pattern is more", strong("random"), "compared to what is anticipated. To solidify this argument, despite the different countries the user 
                 selects from the country list, there will be random patterns with no particular indication that meat consumption influences the recovery rate of an individual post-COVID contraction."),
               h3("Data Set Source"),
               p(
                 a("Website: COVID-19 Healthy Diet Set", href = "https://www.kaggle.com/datasets/mariaren/covid19-healthy-diet-dataset")
               ),
               h3("Supporting Resources"),
               p(
                 a("Website: Do Different Dietary Measures Affect post-COVID Contraction Recovery", href = "https://www.gavi.org/vaccineswork/does-plant-based-diet-really-help-beat-covid-19")
               ),
               p(
                 a("Website: Can Different Dietary Measures Prevent COVID?", href = "https://www.health.harvard.edu/staying-healthy/harvard-study-healthy-diet-associated-with-lower-covid-19-risk-and-severity")
               )
             ),
    ),
    
    # Displays the vegetarian chart.
    tabPanel("Vegetarian Diet & Recovery",
             sidebarPanel(
               checkboxGroupInput("response",
                                  "Choose Countries:",
                                  choices = unique(filterSet$Country),
                                  selected = "Afghanistan")
             ),
             mainPanel(
               h3("Consumption Percentage Visual Chart"),
               plotOutput("chartTwo"),
               h3("Instructions & Basic Steps"),
               p("The bar chart automatically selects", strong("Afghanistan"), "as its default value. To include more countries, you must use the checkbox to the left of the screen. 
                 The checkbox contains all of the countries that exist within the data set. The objective of this chart is to display the different percentages of meat consumption per country and recovery rate. 
                 To start the comparison process, select the countries of interest and specifically look for a pattern that concludes a", strong("correlational relationship"), "between the percentage of meat consumption per country and their recovery rate."),
               h3("Avoiding Confusion"),
               p("In order to interpret the bar chart with proper comprehension, you must", strong("monitor"), "the meat consumption percentage to the right of the chart. This is crucial because the table only show the different selected", strong("countries"), "and 
                 their", strong("recovery"), "rate percentages. Therefore, by observing the meat consumption percentages of each country, which is color-coded, and comparing them to their corresponding recovery rate, only then can you potentially see a pattern."),
               h3("Constraints & Additional Notes"),
               p("For this chart panel, the maximum amount of countries selected contains no restrictions. Therefore, it is", strong(em("highly suggested")), "for the user to keep their selections within the range of", strong("4-6"), "countries. This is extremely 
                 important to note given that excessive amounts of selections can cause", strong("overlap"), "between the different country names on the x-axis."),
               h3("Vegetable Product Quality & Types"),
               p(
                 img(src = "vegetables.webp", height = 600, width = 900)
               ),
               p(strong(em("This image depicts a range of vegetables that a typical consumer may purchase at their local grocery store. Recognizing the vegetable product spectrum is critical since different vegetable compositions are preferred 
                           for different cuisines. The inclusion of various vegetable products in a dish may be influenced by cultural factors."))),
               p(
                 img(src = "vitamins.png", height = 600, width = 900)
               ),
               p(strong(em("The image above displays the different types of vegetables that are rich in 'Vitamin K.' Vitmain K is important because the human body requires vitamin K for post-synthesis modification 
                           of certain proteins that are required for blood coagulation or for controlling binding of calcium in bones and other tissues."))),
               h3("Vegetal Product Consumption & Health"),
               p(
                 img(src = "vegan.jpeg", height = 800, width = 900)
               ),
               p(strong(em("Outside of the scope of COVID-19, following a vegetarian or vegan diet has a mixture of benefits and drawbacks. As the graphic above shows, there is a negative result for every beneficial outcome of eating vegetables.
                 Similar to the consumption of meat products, there are also caveats to adopting a vegetarian or vegan diet. "))),
               p(
                 img(src = "strictvegan.webp", height = 800, width = 900)
               ),
               h3("Findings"),
               p("The", strong("purpose"), "of this section is to underline that there is", strong(em("no correlation")), "between the amount of vegetable product consumed and the rate of recovery following COVID. Vegetable consumption has no influence on the duration of 
               COVID symptoms, according to considerable research on the issue.The pattern in this circumstance is more", strong("random"), "than predicted rather than having a correlational pattern.To back up this claim, regardless of which countries 
               the user chooses from the list, there will be random patterns with no indication that a vegetable diet improves a person's recovery rate after COVID contraction."),
               h3("Data Set Source"),
               p(
                 a("Website: COVID-19 Healthy Diet Set", href = "https://www.kaggle.com/datasets/mariaren/covid19-healthy-diet-dataset")
               ),
               h3("Supporting Resources"),
               p(
                 a("Website: Do Different Dietary Measures Affect post-COVID Contraction Recovery", href = "https://www.gavi.org/vaccineswork/does-plant-based-diet-really-help-beat-covid-19")
               ),
               p(
                 a("Website: Can Different Dietary Measures Prevent COVID?", href = "https://www.health.harvard.edu/staying-healthy/harvard-study-healthy-diet-associated-with-lower-covid-19-risk-and-severity")
               )
             )
      
    ),
    
    # Displays the table of values corresponding to the data set.
    tabPanel("Table Display",
             sidebarPanel(
               h4("This chart helps the viewer better understand the dataset because it provides raw data and calculated data. 
                      Now, the viewer can compare the different variables used in the plot and bar chart using the data in the table."),
               p("Select multiple countries to observe the data."), 
               selectizeInput("Country", "Select Countries:", 
                              choices = unique(diet$Country), multiple = TRUE, select = "Afghanistan")
             ),
             
             mainPanel(
               h4("This table displays the percent of meat and vegetarian eaters, percent of confirmed and recovered COVID cases, and the total population for the selected country (not as a percentage)."),
               tableOutput("table"),
               textOutput("summary"),
               h4("This table displays the numbers of meat and vegetarian eaters for each country along with the number of people
                           people who have had and recovered from COVID-19 in that country."),
               DT::dataTableOutput("tabledata"),
               h5("You are currently viewing", textOutput("num_obs"), 
                  "observations.")
             )
             
    ),
    
    # Displays the plot page.
    tabPanel("Plot Display",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("n","Number of Countries:",
                             min = 0,
                             max = 170,
                             value = 10),
                 radioButtons("color1", label = h3("Pick your color scheme for plot 1:"),
                              choices= list("Red" = "red", "Orange" = "orange")),
                 sliderInput("m","Number of Countries:",
                             min = 0,
                             max = 170,
                             value = 10),
                 radioButtons("color2", label = h3("Pick your color scheme for plot 2:"),
                              choices= list("Skyblue" = "skyblue", "Yellow" = "yellow")),
                 h3("First Plot"),
                 p("The first graph displays the relationship between vegetable consumption and recovery rate. 
                   Each dot represents a country and the size of each dot protrays the degree of recovery."),
                 h3("Second Plot"),
                 p("The second graph displays the relationship between meat consumption and recovery rate. 
                   Each dot represents a country and the size of each dot protrays the degree of recovery."),
               ),
               mainPanel(
                 plotOutput("plot1"),
                 textOutput("plotSummary"),
                 plotOutput("plot2"),
               )
             )
    ),
    
    # Displays the conclusion page 
    tabPanel("Conclusion", 
             h3("Takeaway/Implication"),
             p("From this project so far, we have seen no correlational insight that we can tell from the charts. We believe that this is because of the data being too general to properly lead to a concrete correlation in the graphs we’ve created. 
               Due to this, we’ve learned that we must look for datasets that are more specific in order to identify a proper correlation between recovery rates and diets/food groups."),
             p(""),
             img(src = "takeaway.jpg", height = 600, height = 900),
             h3("Data Quality"),
             p("When we first read our data set, we figured that the data was specific and in detail and was used by many other sources, therefore 
                      we determined it was a medium-high quality data set. However, we recognize that the data has the potential of harming certain population groups
                        specifcally ones that don't have as much accurate data collected about them. The data presentation, such as in the plot and barchart
                        can favor certain diets versus others because of the majority of meat eaters versus vegetarians in the world; it might show that eating 
                        meat is better when in reality there is just more data about the consumption of meat. This is why we included the table, so that the 
                        viewer can view the raw and calculated data and make inferences on their own as well, based on their selection of countries."),
             h3("Limitations"),
             p("While helping the people worldwide to get rid of the anxiety and worry of how to deal with COVID, we compared it to ourselves first. 
                        To be responsible to anyone who may check this web, we would like to point out some aspects that restrict the perfection of this dataset."),
             p("We would like to note all the errors as follow:"),
             tags$ul(tags$li(strong("Comprehensiveness error:"), "There are still a few columns of data missing, it influences the user to compare the 
                                      data on every aspect they care about.")),
             tags$ul(tags$li(strong("Coverage error:"), "Even though the dataset has included most of the country throughout the world, to be a 
                                      comprehensive dataset, we still need the information from every country. This dataset is not only for people to 
                                      compare each country but also learn from each other. Any missing data can be an important solution.")),
             tags$ul(tags$li(strong("Correlation error:"), "So far, we can't tell any correlation from the data we select.")),
             h3("Future Ideas"),
             p("We created this web with the aim to find out the correlation between different countries' daily food intake and their COVID situation. 
                      This dataset is built in 2021, which means it contains the average data, for all the countries included, in the first two COVID years. 
                      It gives us a clear picture of how eating habits correlate with the virus with similar symptoms. This dataset can be a good source for 
                      future use or analysis if a similar pandemic happened. Even though food intake is not the only variable for deciding the recovery rate 
                      from sickness, it is an important portion of everyone’s everyday lives, when we try to find a way to make us feel better, we can know 
                      where to find the data, which country’s eating habit we can choose to follow, and how much we can take. In the future, we could use 
                       a more reliable data set that has other factors that influence covid as well.")
             
    )
  )
)


server <- function(input, output) {
  output$sample <- renderDataTable({
    filterSet %>% 
      sample_n(12)
  })
  
  outputSet <- reactive({
    filterSet %>% 
      filter(Country %in% input$selection) %>% 
      ggplot(aes(Country, Recovered, fill = factor(Meat))) +
      geom_col(pos = "dodge") +
      labs(x = "Countries",
           y = "Recovery Percentage",
           fill = "Meat Consumption")
      
  })
  
  output$chart <- renderPlot({
    outputSet()
  })
  
  otherSet <- reactive({
    filterSet %>% 
      filter(Country %in% input$response) %>% 
      ggplot(aes(Country, Recovered, fill = factor(`Vegetal Products`))) +
      geom_col(pos = "dodge")+
      labs(x = "Countries",
           y = "Recovery Percentage",
           fill = "Vegetal Product Consumption")
    
  })
  
  output$chartTwo <- renderPlot({
    otherSet()
  })
  
  #Create data table for respective variables with list 
  output$tabledata <- DT::renderDataTable({
    veggie_data() %>%
      arrange(desc(totalVeg)) %>%
      arrange(desc(totalMeat)) %>%
      arrange(desc(totalRecovered)) %>%
      arrange(desc(totalConfirmed)) %>%
      datatable(rownames = FALSE, options = list(lengthMenu = c(5, 10, 15), pageLength = 10))
  })
  #Use filter data in the table
  output$table <- renderTable({
    filteredData()
    
  })
  
  #Create data table with respective variables based on new calculated columns 
  veggie_data <- reactive({
    filteredData() %>% 
      mutate(totalVeg = round(`Vegetal Products` * Population / 100, 0)) %>% 
      mutate(totalMeat = round(Meat * Population / 100, 0)) %>% 
      mutate(totalRecovered = round(Recovered * Population / 100, 0)) %>% 
      mutate(totalConfirmed = round(Confirmed * Population / 100, 0)) %>% 
      group_by(Country) %>% 
      summarise(totalVeg = sum(totalVeg), totalMeat = sum(totalMeat),totalRecovered = sum(totalRecovered), totalConfirmed = sum(totalConfirmed))
  })
  #Filter and select appropriate variables from original data set
  filteredData <- reactive({
    diet %>%
      filter(Country %in% input$Country) %>%
      select(Country, `Vegetal Products`, Meat, Recovered, Confirmed, Population)
  })
  
  #Make vegetable product consumption plot
  output$plot1 <- renderPlot({
    diet %>% 
      filter(!is.na(`Vegetal Products`)) %>% 
      sample_n(input$n) %>% 
      ggplot(aes(`Vegetal Products`, Recovered, size = Recovered, color = Recovered)) + 
      geom_point()+
      labs(title = "COVID Recovered Cases vs. Vegetables Consumption")+
      scale_color_gradient(low = "pink", high = input$color1)
    
  })
  
  #Make meat product consumption plot 
  output$plot2 <- renderPlot({
    diet %>% 
      filter(!is.na(Meat)) %>% 
      sample_n(input$m) %>% 
      ggplot(aes(Meat, Recovered, size = Recovered, color = Recovered)) + 
      geom_point()+
      labs(title = "COVID Recovered Cases vs. Meat Consumption")+
      scale_color_gradient(low = "green", high = input$color2)
    
  })
  
  #Paste number of values selected
  output$plotSummary <- renderText({
    paste("You selected", input$n, "values.")
    
  })
}

shinyApp(ui = ui, server = server)
