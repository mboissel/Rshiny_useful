library(shiny)

project <- "/root/PROJECT/Whatever"
exp_dir <- "data/raw_docs"

ui <- fluidPage(
  titlePanel("Upload Whatever target files"),
  sidebarLayout(
    sidebarPanel(
      shiny::fileInput("xlsx_files", "Choose One or Several Excel Files",
        multiple = TRUE, width = "90%",
        accept = c(".xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
      )
    ),
    mainPanel(
      tableOutput("contents")
    )
  )
)

server <- function(input, output) {

  output$contents <- renderTable({
    mapply(
      FUN = function(x, y) {
        file.copy(from = x, to = file.path(project, exp_dir, y), overwrite = TRUE)
        gert::git_add(
          files = file.path(exp_dir, y),
          repo = project
        )
        gert::git_commit(
          message = "chore: new Whatever files",
          author = gert::git_signature(
            name = "[BOT] Shiny",
            email = "username@youradresse.fr"
          ),
          repo = project
        )
      },
      x = input[["xlsx_files"]][["datapath"]],
      y = input[["xlsx_files"]][["name"]]
    )

    data.frame(Uploaded.Files = list.files(file.path(project, exp_dir)))
  })
}

shinyApp(ui, server)
