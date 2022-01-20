library(shiny)
library(tidyverse)
library(bslib)

gpa = read_csv("data/uiuc-gpa-dataset.csv")

gpa = gpa %>% 
    filter(Term %in% c("Fall", "Spring"))
gpa = gpa %>% 
    mutate(Avg_GPA = apply(gpa[8:20], MARGIN = 1, FUN = calc_avg_gpa))

ui = navbarPage(
    theme = bs_theme(
        bootswatch = "yeti"
    ), 
    title = "Historical GPAs",
    tabPanel("App", 
        titlePanel("Average Course GPAs Over Time"),
        
        sidebarLayout(
            sidebarPanel(
                selectInput(inputId = "subject", 
                            label = "Course Subject:", 
                            choices = sort(unique(gpa$Subject)), 
                            selected = "STAT"), 
                selectInput(inputId = "number", 
                            label = "Course Number:", 
                            choices = sort(unique(gpa$Number))), 
                numericInput(inputId = "sections", 
                             label = "Minimum Sections Taught:", 
                             value = 2, 
                             min = 1, 
                             step = 1)
            ),
         
            mainPanel(
                plotOutput(outputId = "plot", 
                           height = "500px")
            )
        )
    ),
    tabPanel("Table", dataTableOutput(outputId = "table")),
    tabPanel("About", includeMarkdown("about.Rmd"))
)

server = function(input, output) {

    subject_data = reactive(
        gpa %>% 
            filter(Subject == input$subject)
    )
    
    observeEvent(input$subject, {
        updateSelectInput(inputId = "number", 
                          choices = sort(unique(subject_data()$Number)))
    })
    
    course_data = reactive(
        subject_data() %>% 
            filter(Number == input$number) %>% 
            group_by(`Primary Instructor`) %>% 
            filter(n() >= input$sections)
    )
    
    output$plot = renderPlot({
        ggplot(data = course_data(), aes(x = fct_rev(fct_inorder(YearTerm)), 
                                         y = Avg_GPA, 
                                         group = `Primary Instructor`, 
                                         color=`Primary Instructor`)) + 
            scale_y_continuous(breaks = seq(0, 4, by = 0.1)) + 
            guides(colour = guide_legend(ncol = 4, title.position = "top")) + 
            labs(title = paste("Course Title:", course_data()$`Course Title`[1]), x = "Term", y = "Average GPA") + 
            theme(plot.title = element_text(face = "bold"), 
                  axis.text.x = element_text(angle = 45, hjust = 1), 
                  legend.position = "bottom") + 
            geom_point() + 
            geom_line()
    })
    
    gpa_table = reactive({
        gpa %>% 
            filter(Subject == input$subject) %>% 
            filter(Number == input$number) %>% 
            group_by(`Primary Instructor`) %>% 
            filter(n() >= input$sections) %>% 
            select(Year, Term, Subject, Number, `Course Title`, `Primary Instructor`, Avg_GPA)
            # select(-YearTerm, -`Sched Type`, -W)
    })
    
    output$table = renderDataTable(gpa_table())
}

# Run the application 
shinyApp(ui = ui, server = server)
