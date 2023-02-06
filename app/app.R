# Load packages ----------------------------------------------------------------
#install.packages("shinyjs")

library(shiny)
library(ggplot2)
library(shinyjs)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h2("Options"),
      
      selectInput(
        "shape",
        "Shape",
        choices = list(
          "Triangle" = 1,
          "Square" = 2,
          "Pentagon" = 3
        ),
        selected = 1
      ),
      
      numericInput(
        "number_of_points",
        "Number of points",
        value = 1000
      ),
      
      sliderInput(
        "point_size",
        "Point Size",
        min = 1,
        max = 3,
        value = 1.5,
        step = 0.1
      ),
      
      sliderInput(
        "fraction",
        "Fraction of distance",
        min = 0,
        max = 1,
        value = 0.5,
        step = 0.01
      ),
      
      selectInput(
        "restrictions",
        "Restrictions",
        choices = list(
          "None" = 1,
          "Choose any vertex but not the previous" = 2,
          "Choose any vertex but not the previous vertex's clockwise neighbour" = 3,
          "Choose any vertex but not the previous vertex's counterclockwise neighbour" = 4,
          "Choose any vertex that cannot be 2 places away from the previously chosen vertex clockwise" = 5,
          "Choose any vertex that cannot neighbor the previously chosen vertice if the last 2 vertices are the same" = 6
        ),
        selected = 1
      ),
      
      checkboxGroupInput(
        "freedoms",
        "Freedoms",
        choices = list(
          "Center of the shape" = 1,
          "midpoints of the sides" = 2
        )
      )
    ),
    mainPanel(
      h1("Chaos Game"),
      plotOutput("fractal")
    )
  )
)

server <- function(input, output) {
  dat <- reactive({
    numberPoints <- input$number_of_points
    xPos <- 123
    yPos <- 47
    xList <- c(xPos)
    yList <- c(yPos)
    vertexesDirection <- c("Towards left")
    directions <- c()
    colors <- c()
    r <- input$fraction
    xShape <- c()
    yShape <- c()
    nrVertexes <- 0
    currentVertex <- -1
    previousVertex <- -1
    previous2Vertex <- -1
    titlePlot <- "Fractal"
    
    if(input$shape == 1) {
      xShape <- c(0, 200, 400)
      yShape <- c(0, 400, 0)
      colors <- c("red", "blue", "green")
      directions <- c("Towards left", "Towards up", "Towards right")
      nrVertexes <- 3
    } else if(input$shape == 2) {
      vertexesDirection <- c("Towards bottom-left")
      if(length(input$freedoms) == 1) {
        if(input$freedoms[1] == 1) {
          xShape <- c(0, 0,   400, 400, 200)
          yShape <- c(0, 400, 400, 0,   200)
          colors <- c("red", "blue", "green", "turquoise3", "purple")
          directions <- c("Towards bottom-left", "Towards top-left", "Towards top-right", "Towards bottom-right", "Towards center")
          nrVertexes <- 5
        } else {
          xShape <- c(0, 0,   0,   200, 400, 400, 400, 200)
          yShape <- c(0, 200, 400, 400, 400, 200, 0,   0)
          colors <- c("red", "orange","blue", "pink", "green", "brown", "turquoise3", "khaki")
          directions <- c("Towards bottom-left", "Towards middle-left", "Towards top-left", "Towards top-middle", "Towards top-right", "Towards middle-right", "Towards bottom-right", "Towards bottom-middle")
          nrVertexes <- 8
        }
      } else if(length(input$freedoms) == 2){
        xShape <- c(0, 0,   0,   200, 400, 400, 400, 200, 200)
        yShape <- c(0, 400, 400, 400, 400, 200, 0,   200, 0)
        colors <- c("red", "orange","blue", "pink", "green", "brown", "turquoise3", "purple", "khaki")
        directions <- c("Towards bottom-left", "Towards middle-left", "Towards top-left", "Towards top-middle", "Towards top-right", "Towards middle-right", "Towards bottom-right", "Towards center", "Towards bottom-middle")
        nrVertexes <- 9
      } else {
        xShape <- c(0, 0,   400, 400)
        yShape <- c(0, 400, 400, 0)
        colors <- c("red", "blue", "green", "turquoise3")
        directions <- c("Towards bottom-left", "Towards top-left", "Towards top-right", "Towards bottom-right")
        nrVertexes <- 4
      }
    } else if(input$shape == 3) {
      if(length(input$freedoms) == 1) {
        if(input$freedoms[1] == 1) {
          xShape <- c(100, 0, 200, 400, 300, 200)
          yShape <- c(0, 300, 400, 300, 0, 200)
          colors <- c("red", "blue", "green", "turquoise3", "brown", "purple")
          vertexesDirection <- c("Towards bottom-left")
          directions <- c("Towards bottom-left", "Towards left", "Towards top", "Towards right", "Towards bottom-right", "Towards center")
          nrVertexes <- 6
        } else {
          xShape <- c(100, 50, 0, 100, 200, 300, 400, 350, 300, 200)
          yShape <- c(0, 150, 300, 350, 400, 350, 300, 150, 0, 0)
          colors <- c("red", "orange","blue", "pink", "green", "brown", "turquoise3", "khaki", "aquamarine2", "cadetblue1")
          vertexesDirection <- c("Towards bottom-left")
          directions <- c("Towards bottom-left", "Towards bottom-middle-left", "Towards left", "Towards top-middle-left", "Towards top", "Towards top-middle-right","Towards right", "Towards bottom-middle-right", "Towards bottom-right", "Towards bottom-middle")
          nrVertexes <- 10
        }
      } else if(length(input$freedoms) == 2) {
        xShape <- c(100, 50, 0, 100, 200, 300, 400, 350, 300, 200, 200)
        yShape <- c(0, 150, 300, 350, 400, 350, 300, 150, 0, 0, 200)
        colors <- c("red", "orange","blue", "pink", "green", "brown", "turquoise3", "khaki", "aquamarine2", "cadetblue1", "purple")
        vertexesDirection <- c("Towards bottom-left")
        directions <- c("Towards bottom-left", "Towards bottom-middle-left", "Towards left", "Towards top-middle-left", "Towards top", "Towards top-middle-right","Towards right", "Towards bottom-middle-right", "Towards bottom-right", "Towards bottom-middle", "Towards center")
        nrVertexes <- 11
      } else {
        xShape <- c(100, 0, 200, 400, 300)
        yShape <- c(0, 300, 400, 300, 0)
        colors <- c("red", "blue", "green", "turquoise3", "purple")
        vertexesDirection <- c("Towards bottom-left")
        directions <- c("Towards bottom-left", "Towards left", "Towards top", "Towards right", "Towards bottom-right")
        nrVertexes <- 5
      }
    }
    
    for(i in 1:numberPoints) {
      currentVertex <- sample(1:nrVertexes, 1)
      
      if(input$restrictions[1] == 2) {
        while(currentVertex == previousVertex) {
          currentVertex <- sample(1:nrVertexes, 1)
        }
      } else if(input$restrictions[1] == 3) {
        notCurrentVertex <- previousVertex %% nrVertexes + 1
        
        while(currentVertex == notCurrentVertex) {
          currentVertex <- sample(1:nrVertexes, 1)
        }
      } else if(input$restrictions[1] == 4) {
        notCurrentVertex <- previousVertex - 1
        
        if(notCurrentVertex == 0) {
          notCurrentVertex = nrVertexes
        }
        
        while(currentVertex == notCurrentVertex) {
          currentVertex <- sample(1:nrVertexes, 1)
        }
      } else if(input$restrictions[1] == 5) {
        notCurrentVertex <- (previousVertex %% nrVertexes + 1) %% nrVertexes + 1
        #notCurrentVertex2 <- previousVertex - 2
        
        # if(notCurrentVertex2 == -1) {
        #   notCurrentVertex2 = nrVertexes - 1
        # } else {
        #   notCurrentVertex2 = nrVertexes
        # }
        
        while(currentVertex == notCurrentVertex) {
          currentVertex <- sample(1:nrVertexes, 1)
        }
      } else if(input$restrictions[1] == 6 && nrVertexes >= 4 && previousVertex != -1 && previous2Vertex == previousVertex) {
        notCurrentVertexClockwise <- previousVertex %% nrVertexes + 1
        notCurrentVertexCounterclockwise <- previousVertex - 1
        
        if(notCurrentVertexCounterclockwise == 0)
          notCurrentVertexCounterclockwise <- nrVertexes
        
        while(currentVertex == notCurrentVertexClockwise || currentVertex == notCurrentVertexCounterclockwise) {
          currentVertex <- sample(1:nrVertexes, 1)
        }
      }
      
      xPos <- xPos + (xShape[currentVertex] - xPos) * r
      yPos <- yPos + (yShape[currentVertex] - yPos) * r
      vertexesDirection <- c(vertexesDirection, directions[currentVertex])
      xList <- c(xList, xPos)
      yList <- c(yList, yPos)
      previous2Vertex <- previousVertex
      previousVertex <- currentVertex
    }
    
    fractalPlot <- data.frame(x = xList, y = yList, direction = vertexesDirection)
    list(fractalPlot = fractalPlot, directions = directions, colors = colors, titlePlot = titlePlot)
  })
  
  output$fractal <- renderPlot({
    idk = dat()
    ggplot(idk$fractalPlot, aes(x = x, y = y, color=direction)) +
      geom_point(size=input$point_size) +
      scale_color_manual(breaks = idk$directions,
                         values = idk$colors) +
      theme(panel.background = 
              element_rect(fill = "white", colour="black", size = 1, linetype = "solid"), 
            panel.grid = element_line(color = "grey", size = 0.1, linetype = "solid"),
            plot.title = element_text(size=20),
            plot.subtitle = element_text(size=16),
            legend.title = element_text(size=14),
            legend.text = element_text(size=12)) +
      labs(title=idk$titlePlot, subtitle = "Sierpinski Triangle")
  })
}

shinyApp(ui, server)
