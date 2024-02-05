options(stringsAsFactors = FALSE)
options(encoding = "UTF-8")
library(shiny)
library(shinydashboard)
library(kableExtra) ## make beautiful tables
library(tidyverse) ## useful toolbox to have ggplot, %>%, ... 
library(mongolite) ## to use MongoDB

####################################################################################################
#### General params ####
####
main_path <- "/disks/PROJECT/SB_mboissel/ShinyApp"
setwd(main_path)

## read local data 
Phenotypes <- readxl::read_excel(
  path = paste0(main_path, "/www/PhenotypesTVcohort.xlsx"),
  col_names = TRUE,
  na = "NA",
  guess_max = 9365
) %>%
  as.data.frame() 
rownames(Phenotypes) <- paste(Phenotypes$RUN, Phenotypes$ID, sep = "_")

ListeGenesTVcohort <- readxl::read_excel(
  path = paste0(main_path, "/www/ListeGenesTVcohort.xlsx"),
  col_names = TRUE
)
list_genes <- head(unique(ListeGenesTVcohort$gene_name), 10)
# list_transcrits <- ListeGenesTVcohort %>% 
#   filter(gene_name  %in% list_genes) %>% 
#   select(Transcript)

## MongoDB setting 
url <- scan(file = paste0(main_path, "/www/settings_mongo.txt"), what = "character")

## connexion to MongoDB
# Log_gene <- mongo(
#   collection = "Log_gene",
#   db = "TVcohort",
#   url = url,
#   verbose = FALSE,
#   options = ssl_options()
# )
# QC_db <- mongo(
#   collection = "QC_db",
#   db = "TVcohort",
#   url = url,
#   verbose = FALSE,
#   options = ssl_options()
# )
RES_RARE <- mongo(
  collection = "RES_RARE",
  db = "TVcohort",
  url = url,
  verbose = TRUE,
  options = ssl_options()
)
RES_FREQ <- mongo(
  collection = "RES_FREQ",
  db = "TVcohort",
  url = url,
  verbose = FALSE,
  options = ssl_options()
)


####################################################################################################
#### UI : Interface ####
####

ui <- dashboardPage(

	# Application title
	dashboardHeader(title = HTML("MongoTVcohort App example")),
  
	dashboardSidebar(
	
		sidebarUserPanel(
      name = a(tags$i(style = "color:#1995dc", icon("envelope", lib = "glyphicon")), 
               "Mathilde Boissel", href = 'mailto:mathilde.boissel@cnrs.fr'),
      subtitle = tags$span(style = "color:#1995dc", "(Biostatistician)")
    ),
		hr(),
		
		sidebarMenu(
			menuItem(text = "Data & Methodo blabla", tabName = "INTRO",
        selected = TRUE, icon = icon("book")
      ),
		  hr(),
			menuItem(text = "Set params to run", tabName = "PARAMS",
        icon = icon("dashboard")
      ),
		  hr(),
      menuItem(text = "Results", 
        menuSubItem(text = "Tables about rare variants", tabName = "TABLES_rare"),
        menuSubItem(text = "Tables about frenquent variants", tabName = "TABLES_freq"),
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
						includeMarkdown("03-DATA.md"),
						width = 8, collapsible = FALSE, title = "Data", solidHeader = TRUE, status = "info"
					),
					box(
						includeMarkdown("03-METHODO.md"),
						width = 4, collapsible = FALSE, title = "Methodology", solidHeader = TRUE, status = "info"
					)
				), 
				fluidRow(
					box(
					  plotOutput("plot_ethny"), 
            width = 6,
						collapsible = FALSE,
						title = "Graphics about ethnicity",
						solidHeader = TRUE,
						status = "info"
					), 
					box(
					  tableOutput('table0_summary'), 
            width = 6,
						collapsible = FALSE,
						title = "Table Count",
						solidHeader = TRUE,
						status = "info"
					)
				)
			),
			tabItem(
			  tabName = "PARAMS",
				fluidRow(
					box(
						selectInput(
						  inputId = "CC_of_interst", label = "Focus on a phenotype",
							choices = c("CC_T2D61", "CC_OBESITE"), selected = "CC_T2D61"
						),
						br(),
						selectInput(
						  inputId = "The_gene_of_interst", label = "Select a gene of interest",
							choices = list_genes , selected = list_genes[1]
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
			  tabName = "TABLES_rare",
				fluidRow(
					box(
					  dataTableOutput('table1_rare'),
						width = 12, collapsible = FALSE, title = "Rare analysis", solidHeader = TRUE, status = "info"
          )
        )
			),
			tabItem(
			  tabName = "TABLES_freq",
        fluidRow(
					box(
					  dataTableOutput('table2_freq'),
						width = 12, collapsible = FALSE, title = "Freq analysis", solidHeader = TRUE, status = "info"
          )
        )
			)
		)
	)
)


####################################################################################################
#### SERVER : get input, make object, return output ####
####

server <- function(input, output) {

  ## REACTIVE functions
  
  gene_tr <- shiny::reactive({
    ListeGenesTVcohort %>% 
      filter(gene_name %in% input$The_gene_of_interst)
  })
  
  res_rare <- shiny::reactive({
    df_to_target <- gene_tr()
    shiny::withProgress(
      message = "Building rare variants results ...", 
      expr = {
        get_res_rare <- lapply(X = 1:nrow(df_to_target), function(x) {
          RES_RARE$find(
            query = paste0(
              '{',
                '"Gene_Symbol": "', df_to_target[["gene_name"]][x], '", ',
                '"ENST": "', df_to_target[["Transcript"]][x], '"',
              '}'
            ),
            fields = paste(
              '{', 
                '"Gene_Symbol": true,', 
                '"ENST": true,', 
                '"analyse_LARGE_STRICT": true,', 
                '"y_name": true,', 
                '"res_rare": true,', 
                '"qc11_done": true,', 
                '"_id": 0',
              '}'
            )
          )
        })
        
        dplyr::bind_rows(
          lapply(seq_along(get_res_rare), function(x) {
            out <- get_res_rare[[x]]
            if (nrow(out) > 0) {
              out %>%
                dplyr::as_tibble() %>%
                cbind(., dplyr::bind_rows(.$res_rare)) %>%
                dplyr::select(-res_rare)
            }
          })
        ) %>%
          dplyr::mutate(SubClusters1=NULL) %>% 
          dplyr::select(-error, error) %>% 
          dplyr::filter(y_name %in% input$CC_of_interst)
      }
    )
  }) 

  res_freq <- shiny::reactive({
    df_to_target <- gene_tr()
    shiny::withProgress(
      message = "Building frequent variants results ...",
      expr = { 
        get_res_freq <- lapply(1:nrow(df_to_target), function(x) {
          RES_FREQ$find(
            query = paste0(
              '{',
                '"Gene_Symbol": "', df_to_target[["gene_name"]][x], '", ',
                '"ENST": "', df_to_target[["Transcript"]][x], '"',
              '}'
            ),
            fields = paste(
              '{',
                '"threshold": false,', 
                '"ExludeSynonymous": false,', 
                '"cluster": false,', 
                '"y_name": false,', 
                '"binary": false,',
                '"_id":0',
              '}'
            )
          )
        })
        
        dplyr::bind_rows(
          lapply(seq_along(get_res_freq), function(x) {
            out <- get_res_freq[[x]]
            if (nrow(out) > 0) { out }
          })
        ) %>%
          dplyr::select(Gene_Symbol, ENST, analyse_LARGE_STRICT, dplyr::everything()) %>%
          dplyr::filter(Y_name %in% input$CC_of_interst)
      }
    )
  })
 
  # dt_res_rare <- shiny::reactive({shiny::withProgress(message = "Building datatable ...", expr = {
  #   my_res_rare <- res_rare()
  #   shiny::req(nrow(my_res_rare)!=0)
  #   DT::datatable(
  #     data = my_res_rare,
  #     rownames = FALSE,
  #     extensions = 'Buttons',
  #     options = list(
  #       dom = 'Bfrtip',
  #       buttons = c('csv', 'excel'),
  #       scrollX = TRUE
  #     )
  #   )
  # })})
  
  ## RENDER functions
  output$plot_ethny <- renderPlot({
    ggplot(
      data = dplyr::filter(Phenotypes, !is.na(Pop_pred)), 
      mapping = aes(x = PC1, y = PC2, colour = Pop_pred, shape = Pop_pred)
    ) +
      geom_point() +
      stat_ellipse(size = 1.5, colour = "white") +
      stat_ellipse(size = 1) +
      scale_colour_viridis_d() +
      scale_shape_discrete(solid = FALSE) +
      labs(
        title = "Predicted ethnicity of the TVcohort population",
        subtitle = paste0(
          "(Ethnicity was not computed for ", 
          scales::comma(nrow(dplyr::filter(Phenotypes, is.na(Pop_pred)))), 
          " samples)"), 
        colour = "Predicted\nPopulation",
        shape = "Predicted\nPopulation"
      ) 
  })
  
  # output$table0_summary <- renderTable({
  #   table0 <- with(Phenotypes, table(CC_T2D61, CC_OBESITE, useNA = "always")) %>% 
  #     broom::tidy() %>% 
  #     mutate(CC_T2D61 = paste0("CC_T2D61: ",CC_T2D61)) %>% 
  #     mutate(CC_OBESITE = paste0("CC_OBESITE: ",CC_OBESITE)) %>% 
  #     spread(key = CC_OBESITE, value = n) %>% 
  #     rename(` \ ` = "CC_T2D61")
  #   return(table0)
  # })
  
  output$table0_summary <- function(){
    tab0 <- with(Phenotypes, table(CC_T2D61, CC_OBESITE, useNA = "always")) %>% 
      broom::tidy() %>% 
      spread(key = CC_OBESITE, value = n) %>% 
      mutate(` ` = "CC_T2D61") %>% 
      select(` `, everything()) %>% 
      rename(`Modalities` = "CC_T2D61") %>% 
      rename(`NA` = "<NA>") 
    tab0 %>% 
      knitr::kable("html") %>%
      kable_styling("bordered", full_width = FALSE) %>%
      # add_header_above(c(" " = 2, "CC_OBESITE" = 3)) %>% 
      add_header_above(c(rep(" ", 2), "CC_OBESITE" = 3)) %>% 
      collapse_rows(columns = 1, valign = "middle")
   }
  
  output$table1_rare <- renderDataTable({
    # return(DT::datatable(res_rare(), options = list(scrollX = TRUE)))
    # return(dt_res_rare())
    return(res_rare())
  })
  
  output$table2_freq <- renderDataTable({
    return(res_freq())
  })

}

####################################################################################################
#### RUN #### 
####
shinyApp(ui, server)



