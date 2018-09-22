library(shiny)
library(tidyverse)
library(plotly)
library(readxl)

#Exporta a base
est_popai <- read_excel("C:/Users/Arthur Vaz/Downloads/estudo_concorrencia.xlsx")
#Transforma tudo em letra maiscula
est_popai  <-  data.frame(lapply(est_popai, function(v) {
                            if (is.character(v)) return(toupper(v))
                            else return(v)
                          }))
#criar concatenata
est_popai$concat <- paste(est_popai$Marca,est_popai$Produto,est_popai$Sabor, sep = "_")
#Selecionar as colunas de valores numericos
t <- sapply(est_popai, class,simplify = FALSE,USE.NAMES = TRUE)


ui <- fluidPage(
  headerPanel(
    h1("Combinacao de atributos")
  ),
  sidebarPanel(
    selectInput("xcol"," Eixo X", names(t[t=='numeric']),
                selected = 'Kcal'),
    selectInput("ycol"," Eixo y",names(t[t=='numeric']),
                selected = 'Preco'),
    selectInput("zcol"," Tamanho dos pontos",names(t[t=='numeric']),
                selected = 'Gramatura'),
    selectInput("tcol"," Cor dos pontos", c('Marca','Produto'),
                selected = 'Marca'),
    #FILTRO NO DASHBOARD
    selectInput("control"," Filtro", names(t[t=='numeric']),
                selected = 'Gramatura'),
    sliderInput("receive", "Receiver:", min=0, max=20, value=c(5,10),
                step=1),
    checkboxGroupInput('tipo1','Tipo de snack', c('Vegano','Vegetariano'),
                       selected = c('Vegano','Vegetariano')),
    checkboxGroupInput('tipo2','Tipo de açucar', c('Natural da Fruta','Adoçante','Açucar','Artificial'),
                       selected =c('Natural da Fruta','Adoçante','Açucar','Artificial')),
    selectInput("ncol","Marca para tabela nutricional", c('Marca','Produto'),
                selected =c('Marca','Produto')),
    selectInput("mcol","Ingrediente diferente", c('Marca','Produto'),
                selected = 'Marca'),
    selectInput("pcol","Escolha do Word Cloud", c('Atributos','Atributos_destaque'),
                selected = 'Atributos')
  ),
  mainPanel(
    plotlyOutput("plot1"),
    tableOutput("table")
  )
)


server <- function(input, output, session) {

  observe({
    filtro_mx <- max(est_popai[est_popai$variable == input$control,'value'])
    filtro_mn<- min(est_popai[est_popai$variable == input$control,'value'])
    # Control the value, min, max, and step.
    # Step size is 2 when input value is even; 1 when value is odd.
    updateSliderInput(session, "receive",
                      min = filtro_mn, max = filtro_mx, value =c(filtro_mn,filtro_mx))
  })
  
  
    #--------------------OUTPUT
    dados <- reactive ({
      est_popai[,c(input$xcol, input$ycol, input$zcol,input$tcol,input$control)]
    })
    
    #GRAFICO BOLHA DO DASHBOARD  
    output$plot1 <- renderPlotly({
      v <-  dados()[dados()[input$control] < input$receive[2],] %>%
              ggplot(aes_string(x = input$xcol, y = input$ycol, fill = input$tcol)) +
              geom_point(aes_string(size = input$zcol))
      ggplotly(v)
    })
  
  output$SliderWidget <- renderUI({
    sliderInput("ucol","",min = filtro_mn, max = filtro_mx, value =c (filtro_mn,filtro_mx))
  })
  output$table <- renderTable(dados(),options = list(lengthMenu = c(5, 30, 50), pageLength = 5))

}

shinyApp(ui = ui, server = server)



#ADICIONAR NO SHINY VERSAO PLOTLY
plot_ly(selectData, x = selectData$Proteina, y = selectData$Preco, color = selectData$Produto, size = selectData$Gramatura)
