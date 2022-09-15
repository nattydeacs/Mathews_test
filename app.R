## Only run examples in interactive R sessions
library(ggplot2)    
library(shinyWidgets)
library(shiny)
ui <- fluidPage(
    setBackgroundColor(
        color = c("white", "#D6D2C4"),
        gradient = "linear",
        direction = "bottom"
    ),
    titlePanel(em("Mathews Test")),
    sidebarLayout(  
        sidebarPanel( style = "max-height: 100%;",
         sliderInput("pi", "Private interest (0 = lowest, 100 = highest)",
                     min = 0, max = 100, value = 50, ticks = FALSE
            ),
         sliderInput("re", "Risk of error given procedures",
                    min = 0, max = 100, value = 50, ticks = FALSE
        ),
        sliderInput("mit", "Mitigation of risk of error from addtl. procedures",
                    min = 0, max = 100, value = 50,   ticks = FALSE
        ),
        
        sliderInput("gi", "Government interest",
                    min = 0, max = 100, value = 50, ticks = FALSE
        ),
        sliderInput("cp", "Cost of addtl. procedures to government",
                    min = 0, max = 100, value = 50, ticks = FALSE
        ),
        fluid = FALSE,
        width = 3
        
    ),
    
    mainPanel(
    plotOutput("distPlot"),
    p("The", span(em("Mathews")),"balancing test is used to determine the level of process due as required by the Consitution. The court balances three broad factors:"),
    p("        1. The private interest at stake from government action (positively correlated with more procedure due)"),
    p("        2. The risk of an unwarrented deprivation given the current procedures (i.e. type 1 error), and the ability of additional procedures to mitigate this risk (positively correlated with more procedure due)"),
    p("        3.The interest of the government in the current procedures, and the costs of providing additional procedures (negatively correlated with more procedure due)"),
    p ("Strictly speaking, '2' and '3' each contain two factors, which is why the slider to the left has five options."),
    fluid = FALSE
    )
    )

)
    # Server logic
    server <- function(input, output) {
        output$distPlot <- renderPlot({
            ggplot(data.frame(y= input$re+input$pi-input$gi-input$cp+input$mit, x = input$re+input$pi-input$gi-input$cp+input$mit), aes(x, y)) +
                geom_point(size = 10, color = '#3E8ACC', alpha =.9) +
                coord_cartesian(xlim = c(-200,300), ylim = c(-210,300)) +
                xlab("Mathews Balance (Private interest + risk of error + mitigation of error w/ addtl. procedures - interest of govt. - cost of addtl. procedures) ") +
                ylab("Due process required") +
                theme_light() +
                theme(axis.ticks.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.y =element_blank(),
                      axis.text.y=element_blank(),
                      axis.title = element_text(size =12, face = "bold"),
                      ) +
                annotate(geom="text", x=-190, y=-225, label="Least Process") +
                annotate(geom="text", x=-190, y=315, label="Most Process")+
                geom_abline(slope = 1, yintercept = 150, linetype="dashed")
            
                
        })
    }
    
    # Complete app with UI and server components
    shinyApp(ui, server)
   