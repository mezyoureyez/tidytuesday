
#Load packages and data
library(tidyverse)

#Load the data
diwali <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-14/diwali_sales_data.csv')

#Group by user because there are multiple rows per user. Based on results from regression analysis, only include Gender and Product_Category as predictors.
diwali_shiny <- na.omit(diwali) %>%
  group_by(User_ID, Gender, Product_Category) %>%
  summarise(total_amount = sum(Amount)) %>%
  ungroup() 

#Ensure categorical variables are treated as factors
diwali_shiny$Gender = as.factor(diwali_shiny$Gender)
diwali_shiny$Product_Category = as.factor(diwali_shiny$Product_Category)

library(shiny)
library(shinythemes)
library(RColorBrewer)

# UI for the app
ui <- fluidPage(theme = shinytheme("paper"),
  
  # Application title
  titlePanel("Diwali Sales Analysis"),
  
  # Sidebar with a dropdown to select product category
  sidebarLayout(
    sidebarPanel(
      selectInput("Product_Category", "Product Category", choices = sort(unique(diwali_shiny$Product_Category)))
    ),
    
    # Main panel with a bar graph
    mainPanel(
      plotOutput("total_amount_bar_graph")
    )
  )
)

# Server logic required to generate the bar graph
server <- function(input, output) {
  
  output$total_amount_bar_graph <- renderPlot({
    
    # Filter data based on selected product category
    filtered_data <- diwali_shiny %>% filter(Product_Category == input$Product_Category)
    
    # Calculate average amount spent on each product category by gender
    average_amount_by_gender <- filtered_data %>%
      group_by(Gender, Product_Category) %>%
      summarise(average_amount = mean(total_amount), .groups = "keep") %>%
      ungroup()
    
    # Extract colors from the RColorBrewer package
    colors <- brewer.pal(6, "Paired")
    
    # Create a bar graph with different colors for male and female
    ggplot(average_amount_by_gender, aes(x = Gender, y = average_amount)) +
      geom_bar(aes(fill = Gender), stat = "identity") +
      scale_fill_manual(values = colors) +
      labs(title = "Average Amount Spent by Gender", x = "Gender", y = "Average Amount Spent") +
      theme_bw()
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)

