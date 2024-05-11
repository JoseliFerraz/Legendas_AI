# ------------------------------------------- # 
# Desafio Alura Google Gimini AI
# Joseli
# Data 11-05-2024
# ------------------------------------------- # 


 # library
library(shiny)
library(httr)
library(jsonlite)
library(base64enc)

# Function
gemini_vision <- function(prompt, 
                          image,
                          temperature=1.0,
                          max_output_tokens=100,
                          api_key=Sys.getenv("GEMINI_API_KEY"),
                          model = "gemini-pro-vision") {
  
  if(nchar(api_key)<1) {
    api_key <- readline("KEY XXXX")
    Sys.setenv(GEMINI_API_KEY = api_key)
  }
  
  model_query <- paste0(model, ":generateContent")
  
  response <- POST(
    url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query),
    query = list(key = api_key),
    content_type_json(),
    encode = "json",
    body = list(
      contents = list(
        parts = list(
          list(
            text = prompt
          ),
          list(
            inlineData = list(
              mimeType = "image/png",
              data = base64encode(image)
            )
          )
        )
      ),
      generationConfig = list(
        temperature = temperature,
        maxOutputTokens = max_output_tokens
      )
    )
  )
  
  if(response$status_code>200) {
    stop(paste("Error - ", content(response)$error$message))
  }
  
  candidates <- content(response)$candidates
  outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))
  
  return(outputs)
  
}

# KEY
Sys.setenv(GEMINI_API_KEY = "KEY XXXX")

ui <- fluidPage(
  titlePanel("Crie sua postagem"),
  mainPanel(
    fluidRow(
      fileInput(
        inputId = "imgFile",
        label = "Selecione sua imagem",
      ),
      textInput(
        inputId = "prompt",
        label = "Digite sua pesquisa",
        placeholder = "Crie uma postagem para a imagem"
      ),
      actionButton("submit", "Talk to Gemini"),
      textOutput("response")
    ),
    imageOutput(outputId = "myimage")
  )
)

server <- function(input, output) {

  observeEvent(input$imgFile, {
    path <- input$imgFile$datapath
    output$myimage <- renderImage({
      list(
        src = path
      )
    }, deleteFile = FALSE)
  })

  observeEvent(input$submit, {
    output$response <- renderText({
      gemini_vision(input$prompt, input$imgFile$datapath)
    })
  })
}

shinyApp(ui = ui, server = server)