library(shiny)
library(ggplot2)
library(gridExtra)

ui <- fluidPage(
  
  titlePanel("Simulation Stichprobenkennwerteverteilung"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Populationsparameter"),
      
      selectInput("dist_type", "Populationsverteilung:",
                  choices = c("Normal" = "norm",
                              "Uniform" = "unif",
                              "Rechtsschief" = "exp")),
      
      numericInput("pop_mean", "Populationsmittelwert:", value = 100, step = 1),
      numericInput("pop_sd", "Populationsstandardabweichung:", value = 15, min = 1, step = 1),
      
      hr(),
      
      h4("Stichprobenparameter"),
      
      sliderInput("sample_size", "Stichprobengröße (n):",
                  min = 5, max = 1000, value = 30, step = 5),
      
      sliderInput("num_samples", "Anzahl der zu ziehenden Stichproben:",
                  min = 1, max = 500, value = 1, step = 1),
      
      hr(),
      
      actionButton("draw_samples", "Stichprobe(n) ziehen", class = "btn-primary"),
      actionButton("reset", "Zurücksetzen", class = "btn-secondary"),
      
      hr(),
      
      textOutput("sample_info"),
      textOutput("theory_info")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Visualisierung",
                 plotOutput("main_plot", height = "700px")
        ),
        
        tabPanel("Hilfe",
                 br(),
                 h4("Wie man diese App verwendet:"),
                 p("1. Wählen Sie den Verteilungstyp und die Parameter der Population"),
                 p("2. Wählen Sie die Stichprobengröße und wie viele Stichproben gezogen werden sollen"),
                 p("3. Klicken Sie auf 'Stichproben ziehen', um die Mittelwerte zu erfassen"),
                 p("4. Beobachten Sie, wie sich die Stichprobenverteilung entfaltet"),
                 br(),
                 h4("Was Sie sehen:"),
                 p("• Oben links: Verteilung der Population"),
                 p("• Oben rechts: Verteilung der aktuellen Stichprobe"),
                 p("• Unten links: Stichprobenverteilung der Mittelwerte (wächst mit jeder Ziehung)"),
                 p("• Unten rechts: Zusammenfassung der Statistiken"),
                 br(),
                 h4("Linientypen in den Histogrammen:"),
                 p("• Rote durchgezogene Linie: Mittelwert der angezeigten Daten"),
                 p("• Blaue gepunktete Linie (Sampling Distribution): Theoretischer Populationsmittelwert (μ)")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive value to store all sample means
  sample_means <- reactiveVal(numeric(0))
  current_sample <- reactiveVal(numeric(0))
  population <- reactiveVal(NULL)
  
  # Generate population data only once, store it
  generate_and_store_population <- function() {
    n <- 100000
    
    if (input$dist_type == "norm") {
      rnorm(n, mean = input$pop_mean, sd = input$pop_sd)
    } else if (input$dist_type == "unif") {
      runif(n, min = input$pop_mean - 2*input$pop_sd, 
            max = input$pop_mean + 2*input$pop_sd)
    } else if (input$dist_type == "exp") {
      rexp(n, rate = 1/input$pop_sd) + input$pop_mean - input$pop_sd
    }
  }
  
  # Get or create population
  get_population <- function() {
    if (is.null(population())) {
      population(generate_and_store_population())
    }
    population()
  }
  
  # Draw new samples and add to sampling distribution
  observeEvent(input$draw_samples, {
    pop <- get_population()
    new_means <- numeric(input$num_samples)
    
    for (i in 1:input$num_samples) {
      samp <- sample(pop, size = input$sample_size, replace = FALSE)
      new_means[i] <- mean(samp)
      current_sample(samp)
    }
    
    # Append new means to existing ones
    sample_means(c(sample_means(), new_means))
  })
  
  # Reset all data
  observeEvent(input$reset, {
    sample_means(numeric(0))
    current_sample(numeric(0))
    population(NULL)
  })
  
  # Create statistics table as a grob (graphical object)
  create_stats_grob <- function() {
    population_data <- get_population()
    
    if (length(sample_means()) > 0) {
      se_observed <- sd(sample_means())
      
      stats_df <- data.frame(
        Statistik = c("Populationsmittelwert (μ)", 
                      "Mittelwert der Stichprobenmittelwerte",
                      "Populationsstandardabweichung (σ)",
                      "Standardabweichung der Stichprobenmittelwerte"),
        Wert = c(round(mean(population_data), 2), 
                 round(mean(sample_means()), 2),
                 round(sd(population_data), 2),
                 round(se_observed, 2))
      )
    } else {
      stats_df <- data.frame(
        Statistik = "Noch keine Daten",
        Wert = "Ziehen Sie Stichproben"
      )
    }
    
    # Create table as grob
    tableGrob(stats_df, rows = NULL, theme = ttheme_minimal(
      base_size = 12,
      padding = unit(c(4, 4), "mm")
    ))
  }
  
  # Main visualization
  output$main_plot <- renderPlot({
    population_data <- get_population()
    
    # Plot 1: Population Distribution
    pop_plot <- ggplot(data.frame(x = population_data), aes(x = x)) +
      geom_histogram(bins = 40, fill = "skyblue1", color = "black", alpha = 0.7) +
      geom_vline(aes(xintercept = mean(x), color = "Populationsmittelwert"), linewidth = 1.5, linetype = "dashed") +
      labs(title = "Populationsverteilung",
           subtitle = paste("μ =", round(mean(population_data), 2)),
           x = "Wert", y = "Häufigkeit") +
      scale_color_manual(values = c("Populationsmittelwert" = "steelblue4")) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold"),
            legend.position = "bottom",
            legend.title = element_blank())
    
    # Plot 2: Current Sample Distribution
    if (length(current_sample()) > 0) {
      sample_plot <- ggplot(data.frame(x = current_sample()), aes(x = x)) +
        geom_histogram(bins = 20, fill = "palevioletred1", color = "black", alpha = 0.7) +
        geom_vline(aes(xintercept = mean(x), color = "Stichprobenmittelwert"), linewidth = 1.5, linetype = "dashed") +
        labs(title = "Aktuelle Stichprobenverteilung",
             subtitle = paste("M =", round(mean(current_sample()), 2)),
             x = "Wert", y = "Häufigkeit") +
        scale_color_manual(values = c("Stichprobenmittelwert" = "orchid4")) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold"),
              legend.position = "bottom",
              legend.title = element_blank())
    } else {
      sample_plot <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Ziehen Sie eine Stichprobe, um die Stichprobenverteilung zu sehen",
                 size = 5, color = "gray50") +
        theme_void()
    }
    
    # Plot 3: Sampling Distribution
    if (length(sample_means()) > 0) {
      sampling_plot <- ggplot(data.frame(x = sample_means()), aes(x = x)) +
        geom_histogram(bins = 40, fill = "coral", color = "black", alpha = 0.7) +
        geom_vline(aes(xintercept = mean(x), color = "Mittelwert der Stichprobenmittelwerte"), 
                   linewidth = 1.5, linetype = "dashed") +
        geom_vline(aes(xintercept = input$pop_mean, color = "Populationsmittelwert"), 
                   linewidth = 1.5, linetype = "dashed") +
        labs(title = paste("Stichprobenkennwerteverteilung (n =", length(sample_means()), "Stichproben)"),
             subtitle = paste("M =", round(mean(sample_means()), 2), "| Standardabweichung =", round(sd(sample_means()), 2)),
             x = "Stichprobenmittelwert", y = "Häufigkeit") +
        scale_color_manual(values = c("Mittelwert der Stichprobenmittelwerte" = "coral4",
                                      "Populationsmittelwert" = "steelblue4")) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold"),
              legend.position = "bottom",
              legend.title = element_blank())
    } else {
      sampling_plot <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Ziehen Sie eine Stichprobe, um die Stichprobenkennwerteverteilung zu sehen",
                 size = 5, color = "gray50") +
        theme_void()
    }
    
    # Plot 4: Statistics Table
    stats_grob <- create_stats_grob()
    stats_table_plot <- ggplot() +
      annotation_custom(stats_grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
      theme_void()
    
    grid.arrange(pop_plot, sample_plot, sampling_plot, stats_table_plot, nrow = 2)
  })
  
  # Sample information
  output$sample_info <- renderText({
    if (length(sample_means()) > 0) {
      paste("Stichproben gezogen:", length(sample_means()))
    } else {
      "Noch keine Stichproben gezogen"
    }
  })
  
}

shinyApp(ui, server)