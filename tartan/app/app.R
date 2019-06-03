#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)

dta_Mx <- readRDS("Mx_data.rds")

distinct_codes <- unique(dta_Mx$code)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Trend correlation plots "),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        sliderInput("age", label = "Select age range", 
                    min = 0, max = 110, 
                    value = c(0, 95)
        ),
        sliderInput("year", label = "Select year range",
                    min = 1859, max = 2017, sep = "",
                    value = c(1950, 2016),
                    animate = animationOptions(interval = 1000)
                    
                    ),
        selectInput("country", label = "Select country", 
                    choices = distinct_codes, 
                    multiple = TRUE, selected = "USA"),
        
        selectInput("sex", label = "Select gender",
                    choices = c("total", "male", "female"),
                    selected = "total"
          
        ),
        selectInput("pal_option", label = "Select palette option", 
                    choices = c("magma", "inferno", "plasma", "viridis", "cividis",
                                "RdBu", "Paired"),
                    selected = "viridis"
        ),
        sliderInput("scale_range", label = "Select scale range",
                    min = -1, max = 1, step = 0.01,  value = c(-1, 1)),
        checkboxInput("scale_symmetric", "Make scale symmetric?",
                      value = FALSE),
        checkboxInput("scale_invert", "Invert scale?", 
                      value = FALSE)
      ),        
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("tartanplot")
      )
   )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$tartanplot <- renderPlot({
     
    if (input$scale_symmetric){
      tmp <- max(abs(input$scale_range))
      scale_limits <- c(-tmp, tmp)
      rm(tmp)
    } else {
      scale_limits <- input$scale_range
    }
     
     dta_trnd <- dta_Mx %>% 
       filter(sex == input$sex) %>% 
       filter(code %in% input$country) %>% 
       filter(between(year, input$year[1], input$year[2]))  %>% 
       filter(between(age, input$age[1], input$age[2])) %>% 
       group_by(year, age) %>% 
       summarise(mean_Mx = mean(Mx, na.rm = T)) %>% 
       ungroup() %>% 
       mutate(log_mean_Mx = log(mean_Mx, 10)) 
     
     tmp <- dta_trnd %>% 
       select(-mean_Mx) %>% 
       spread(age, log_mean_Mx) %>%
       select(-year) %>% 
       cor() 
     
     cor_df <- tmp %>% 
       as_tibble() %>% 
       mutate(from_age = rownames(tmp)) %>% 
       gather(key="to_age", value = "value", -from_age) %>% 
       mutate(from_age = as.numeric(from_age), to_age = as.numeric(to_age))
     

     p <- cor_df %>% 
       filter(from_age <= 100, to_age <= 100) %>% 
       ggplot(aes(x = from_age, y = to_age, fill = value)) + 
       geom_tile() +
       scale_x_continuous(breaks = seq(0, 100, by = 10)) +
       scale_y_continuous(breaks = seq(0, 100, by = 10)) +
       coord_equal() +
       labs(
         x = "Age", y = "Age"
       )
     
     p + scale_fill_viridis_c(
       limits = scale_limits,
       option = input$pal_option,
       direction = ifelse(input$scale_invert, -1, 1)
     ) 
       
     })
}

# Run the application 
shinyApp(ui = ui, server = server)

