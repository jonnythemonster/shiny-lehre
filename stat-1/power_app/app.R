library(shiny)
library(ggplot2)

# UI Definition
ui <- fluidPage(
  titlePanel("Interaktive Teststärke-Analyse"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("alpha",
                  "Signifikanzniveau α:",
                  min = 0.01,
                  max = 0.20,
                  value = 0.05,
                  step = 0.01),
      
      sliderInput("n",
                  "Stichprobengröße n:",
                  min = 10,
                  max = 200,
                  value = 30,
                  step = 5),
      
      sliderInput("effect_size",
                  "Effektgröße d (Cohen's d):",
                  min = 0.1,
                  max = 1.5,
                  value = 0.5,
                  step = 0.1),
      
      hr(),
      
      h4("Berechnete Werte:"),
      textOutput("lambda_text"),
      textOutput("tcrit_text"),
      hr(),
      textOutput("alpha_text"),
      textOutput("beta_text"),
      textOutput("power_text"),
      
      hr(),
      p("Kleine Effektgröße: d = 0.2"),
      p("Mittlere Effektgröße: d = 0.5"),
      p("Große Effektgröße: d = 0.8")
    ),
    
    mainPanel(
      plotOutput("distribution_plot", height = "500px"),
      
      br(),
      
      wellPanel(
        h4("Interpretation:"),
        tags$ul(
          tags$li(strong("Blaue Kurve (H₀):"), " Verteilung wenn kein Effekt existiert (μ = 0)"),
          tags$li(strong("Violette Kurve (H₁):"), " Verteilung wenn Effekt existiert (μ ≠ 0)"),
          tags$li(strong("Roter Bereich (α):"), " Wahrscheinlichkeit H₀ fälschlicherweise abzulehnen"),
          tags$li(strong("Oranger Bereich (β):"), " Wahrscheinlichkeit H₁ zu übersehen (Fehler 2. Art)"),
          tags$li(strong("Grüner Bereich (1-β):"), " Wahrscheinlichkeit H₁ korrekt zu erkennen (Teststärke)")
        )
      ),
      
      wellPanel(
        h4("Zusammenhänge:"),
        tags$ul(
          tags$li("Größeres n → höhere Teststärke"),
          tags$li("Größere Effektgröße d → höhere Teststärke"),
          tags$li("Größeres α → höhere Teststärke (aber mehr Fehler 1. Art!)"),
          tags$li("Bei konstantem n: Trade-off zwischen α und β")
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output) {
  
  # Reaktive Berechnungen
  calculations <- reactive({
    alpha <- input$alpha
    n <- input$n
    d <- input$effect_size
    
    # Freiheitsgrade
    df <- n - 1
    
    # Kritischer Wert (einseitiger Test)
    t_crit <- qt(1 - alpha, df)
    
    # Nichtzentralitätsparameter
    lambda <- d * sqrt(n)
    
    # Teststärke berechnen
    power <- 1 - pt(t_crit, df, ncp = lambda)
    beta <- 1 - power
    
    list(
      alpha = alpha,
      n = n,
      d = d,
      df = df,
      t_crit = t_crit,
      lambda = lambda,
      power = power,
      beta = beta
    )
  })
  
  # Textausgaben
  output$lambda_text <- renderText({
    calc <- calculations()
    paste0("λ (Nichtzentralität) = ", round(calc$lambda, 2))
  })
  
  output$tcrit_text <- renderText({
    calc <- calculations()
    paste0("t_kritisch = ", round(calc$t_crit, 3), " (df = ", calc$df, ")")
  })
  
  output$alpha_text <- renderText({
    calc <- calculations()
    paste0("α = ", round(calc$alpha * 100, 1), "% (Fehler 1. Art)")
  })
  
  output$beta_text <- renderText({
    calc <- calculations()
    paste0("β = ", round(calc$beta * 100, 1), "% (Fehler 2. Art)")
  })
  
  output$power_text <- renderText({
    calc <- calculations()
    paste0("1 - β = ", round(calc$power * 100, 1), "% (Teststärke)")
  })
  
  # Hauptplot
  output$distribution_plot <- renderPlot({
    calc <- calculations()
    
    # Erstelle Datenbereich
    x_range <- seq(-4, calc$lambda + 4, length.out = 500)
    
    # Berechne Dichten
    df <- calc$df
    h0_density <- dt(x_range, df)
    h1_density <- dt(x_range, df, ncp = calc$lambda)
    
    # Erstelle Dataframe
    plot_data <- data.frame(
      x = x_range,
      h0 = h0_density,
      h1 = h1_density
    )
    
    # Bereiche für Flächen
    alpha_region <- plot_data[plot_data$x >= calc$t_crit, ]
    beta_region <- plot_data[plot_data$x < calc$t_crit, ]
    power_region <- plot_data[plot_data$x >= calc$t_crit, ]
    
    # Erstelle Plot
    p <- ggplot() +
      # Alpha-Bereich (unter H0)
      geom_area(data = alpha_region, 
                aes(x = x, y = h0), 
                fill = "#ef4444", alpha = 0.3) +
      
      # Beta-Bereich (unter H1)
      geom_area(data = beta_region, 
                aes(x = x, y = h1), 
                fill = "#f97316", alpha = 0.3) +
      
      # Power-Bereich (unter H1)
      geom_area(data = power_region, 
                aes(x = x, y = h1), 
                fill = "#22c55e", alpha = 0.3) +
      
      # H0-Verteilung
      geom_line(data = plot_data, 
                aes(x = x, y = h0, color = "H₀: μ = 0"), 
                size = 1.5) +
      
      # H1-Verteilung
      geom_line(data = plot_data, 
                aes(x = x, y = h1, color = "H₁: μ ≠ 0"), 
                size = 1.5) +
      
      # Kritischer Wert
      geom_vline(xintercept = calc$t_crit, 
                 linetype = "dashed", 
                 color = "#ef4444", 
                 size = 1) +
      
      annotate("text", 
               x = calc$t_crit, 
               y = max(h0_density) * 0.9,
               label = paste0("t_krit = ", round(calc$t_crit, 2)),
               color = "#ef4444",
               fontface = "bold",
               size = 5,
               hjust = -0.1) +
      
      # Beschriftungen für Bereiche
      annotate("text", 
               x = calc$t_crit + 0.5, 
               y = max(h0_density) * 0.5,
               label = paste0("α = ", round(calc$alpha * 100, 1), "%"),
               color = "#ef4444",
               fontface = "bold",
               size = 4) +
      
      annotate("text", 
               x = calc$t_crit - 0.5, 
               y = max(h1_density) * 0.5,
               label = paste0("β = ", round(calc$beta * 100, 1), "%"),
               color = "#f97316",
               fontface = "bold",
               size = 4) +
      
      annotate("text", 
               x = calc$lambda + 1, 
               y = max(h1_density) * 0.7,
               label = paste0("Power = ", round(calc$power * 100, 1), "%"),
               color = "#22c55e",
               fontface = "bold",
               size = 4) +
      
      # Styling
      scale_color_manual(
        name = "Verteilungen",
        values = c("H₀: μ = 0" = "#3b82f6", 
                   "H₁: μ ≠ 0" = "#8b5cf6")
      ) +
      
      labs(
        title = "Verteilungen unter H₀ und H₁",
        subtitle = paste0("λ = d × √n = ", round(calc$d, 1), " × √", calc$n, " = ", round(calc$lambda, 2)),
        x = "Teststatistik t",
        y = "Dichte"
      ) +
      
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 12),
        legend.position = "top",
        legend.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
    
    print(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)