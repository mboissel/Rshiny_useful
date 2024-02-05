options(stringsAsFactors = FALSE)
options(encoding = "UTF-8")
library(shiny)
library(shinydashboard)
library(tidyverse) ## useful toolbox to have ggplot, %>%, ... 
main_path <- "/disks/PROJECT/SB_mboissel/ShinyApp"
setwd(main_path)


####################################################################################################
#### UI : Interface ####
####

ui <- dashboardPage(

	dashboardHeader(title = HTML("DashBoard App example")),
  
	dashboardSidebar(
	
		sidebarUserPanel(
      name = a(tags$i(style = "color:#1995dc", icon("envelope", lib = "glyphicon")), 
               "Mathilde Boissel", href = 'mailto:boissel.mathilde@gmail.com'),
      subtitle = tags$span(style = "color:#1995dc", "(Biostatistician)")
    ),
		hr(),
		
		sidebarMenu(
			menuItem(text = "Data & Methodo blabla", tabName = "INTRO",
        selected = TRUE, icon = icon("dashboard")
      ),
		  hr(),
      menuItem(text = "Results", 
        menuSubItem(text = "Tables etc", tabName = "TABLES"),
        menuSubItem(text = "Graphs etc", tabName = "GRAPHS"),
        icon = tags$i(style = "color:#1995dc", icon("blackboard", lib = "glyphicon"))
      )
		)
	),
	
	dashboardBody(
		tabItems(
			tabItem(
			  tabName = "INTRO",
			  fluidRow(
					box(
						includeMarkdown("02-DATA.md"),
						width = 4, collapsible = FALSE, title = "Data", solidHeader = TRUE, status = "info"
					),
					box(
						includeMarkdown("02-METHODO.md"),
						width = 8, collapsible = FALSE, title = "Methodology", solidHeader = TRUE, status = "info"
					)
				),
				fluidRow(
					box(
						selectInput(
						  inputId = "Variable_of_interst", label = "Focus on a phenotype",
							choices = names(iris)[1:4], selected = names(iris)[4]
						),
						br(),
						selectInput(
						  inputId = "Plot_type", label = "Choose the type of plot",
							choices = c("histogram", "boxplot"), selected = "boxplot"
						),
						width = 12,
						collapsible = FALSE,
						title = "Select your analysis",
						solidHeader = TRUE,
						status = "info"
          )
        )
			),
			tabItem(
			  tabName = "TABLES",
				fluidRow(
					box(
					  dataTableOutput('table1_head'),
						width = 12, collapsible = FALSE, title = "head of data", solidHeader = TRUE, status = "info"
          )
        ),
				fluidRow(
					box(
					  dataTableOutput('table2_summary'),
						width = 12, collapsible = FALSE, title = "summary of data", solidHeader = TRUE, status = "info"
          )
        )
			),
			tabItem(
			  tabName = "GRAPHS",
        fluidRow(
					box(
					  plotOutput("plot_to_render"), 
            width = 12,
						collapsible = FALSE,
						title = "Graphics",
						solidHeader = TRUE,
						status = "info"
					)
				)
			)
		)
	)
)


####################################################################################################
#### SERVER : get input, make object, return output ####
####

server <- function(input, output, session) {

  ## REACTIVE functions
  Select_data <- reactive({
    ## read some xslx files here, or exec mongo cmd to get back data... whatever...
    iris %>% 
      select(input$Variable_of_interst, Species)
  })

  Do_plot <- reactive({
    mydta <- Select_data()
    switch(
      EXPR = input$Plot_type,
      "histogram" = {
        my_p <- ggplot(data = mydta, mapping = aes(x = get(input$Variable_of_interst), color = Species)) +
          geom_histogram() +
          facet_grid(rows = vars(Species)) + 
          labs(x = input$Variable_of_interst)
      },
      "boxplot" = {
        my_p <- ggplot(data = mydta, mapping = aes(y = get(input$Variable_of_interst), color = Species)) +
          geom_boxplot() +
          facet_grid(cols = vars(Species)) + 
          labs(x = input$Variable_of_interst)
      }
    )
    return(my_p)
  })

  ## RENDER functions
  output$table1_head <- renderDataTable({
    return(head(Select_data()))
  })
  
  output$table2_summary <- renderDataTable({
    table2 <- Select_data() %>% 
      group_by(Species) %>% 
      summarise(n = n())
    return(table2)
  })
  
  output$plot_to_render <- renderPlot({
    Do_plot()
  })

}

####################################################################################################
#### RUN #### 
####
shinyApp(ui, server)


#### Autres structures ####
## si Ui et Server sont dans des scripts séparés: 
## commande runApp(“~/mondossier/mon_appli”)
## qqe exemples : http://www.dacta.fr/blog/r-shiny.html
