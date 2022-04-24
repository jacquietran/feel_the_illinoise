library(shiny)
library(dplyr)
library(ggplot2)
source(here::here("illinoise.R"))

noise_types <- c("perlin", "simplex", "cubic", "value")
line_colour <- "#161C32"
bg_colour <- "#FFFFFF"

ui <- fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "darkly"),
  
  titlePanel("Feel The Illinoise!"),
  sidebarLayout(
    sidebarPanel(
      HTML(r"(
           <p>An app by <strong><a href="https://art.jacquietran.com" target="_blank">Jacquie Tran</a></strong>.</p>
           <hr>)"),
      p("Modify the values below to generate your own noise art."),
      numericInput(
        "seed", "Seed value", value = 1000),
      sliderInput(
        "grid_length", "Grid length", value = 100, min = 10, max = 130,
        step = 5),
      sliderInput(
        "warp", "Warp factor", value = 100, min = 10, max = 150,
        step = 10),
      selectInput("noise", "Noise type", noise_types),
      HTML(r"(
           <hr>
           <small><strong>Footnote:</strong> The name of this app is in reference to the album <strong><a href="https://music.sufjan.com/album/illinois" target="_blank">"Illinois" by Sufjan Stevens</a></strong>.</small>)")
      
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    
    # Generate data
    grid <- illinoise(
      seed = input$seed, grid_length = input$grid_length,
      warp_factor = input$warp, geom_size_min = 0.1, geom_size_max = 1,
      noise_type = input$noise)
    
    ggplot() +
      geom_curve(
        data = grid,
        aes(x = x, y = y, xend = x_warped, yend = y_warped, size = size),
        colour = line_colour, curvature = -0.2) +
      geom_point(
        data = grid %>% filter(subset > 18),
        aes(x = x_warped, y = y_warped, size = size / 3),
        colour = bg_colour, shape = 18) +
      #geom_point(
      #  data = grid %>% filter(subset < 10 & subset >= 3),
      #  aes(x = x_warped, y = y_warped, size = size),
      #  colour = line_colour, shape = 0) +
      geom_point(
        data = grid %>% filter(subset < 3),
        aes(x = x_warped, y = y_warped, size = size / 2),
        colour = line_colour, shape = 15) +
      scale_size_identity() +
      coord_equal(expand = FALSE) +
      theme_void() +
      theme(
        legend.position = "none",
        plot.background = element_rect(fill = bg_colour, colour = bg_colour),
        plot.margin     = margin(20,20,20,20, unit = "pt"))
    
  }, width = 800, height = 800, res = 300)
  
}

shinyApp(ui, server)