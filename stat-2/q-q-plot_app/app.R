# ============================================================
# Q-Q-Plot Explorer & Simulation
# ------------------------------------------------------------
# Tab 1 (Explorer): klickbare Punkte mit Erklärung
# Tab 2 (Simulation): schrittweiser Aufbau des Q-Q-Plots
#
# Ausführen mit:  shiny::runApp("app.R")
# ============================================================

library(shiny)
library(ggplot2)

# ---- Daten generieren --------------------------------------------------
generate_data <- function(type, n, seed) {
  set.seed(seed)
  x <- switch(type,
    "normal"     = rnorm(n),
    "right_skew" = rlnorm(n, meanlog = 0, sdlog = 0.7),
    "left_skew"  = -rlnorm(n, meanlog = 0, sdlog = 0.7),
    "heavy"      = rt(n, df = 3),
    "light"      = runif(n, -2, 2),
    "bimodal"    = c(rnorm(round(n/2), -2.5, 0.7),
                     rnorm(n - round(n/2),  2.5, 0.7)),
    "outliers"   = c(rnorm(n - 5), c(6, 6.5, 7, -6, -6.5))
  )
  as.numeric(scale(x))
}

dist_choices <- c(
  "Normalverteilung (Referenz)"      = "normal",
  "Rechtsschief (positive Schiefe)"  = "right_skew",
  "Linksschief (negative Schiefe)"   = "left_skew",
  "Heavy Tails (leptokurtisch)"      = "heavy",
  "Light Tails (platykurtisch)"      = "light",
  "Bimodal (zweigipflig)"            = "bimodal",
  "Mit Ausreißern"                   = "outliers"
)

compute_qq_df <- function(x) {
  n <- length(x)
  probs <- (seq_len(n) - 0.5) / n
  data.frame(
    rank        = seq_len(n),
    empirical   = sort(x),
    probability = probs,
    theoretical = qnorm(probs),
    percentile  = probs * 100
  )
}

# ---- Erklärungen pro Verteilung ---------------------------------------
explanations <- list(
  normal     = HTML("<b>Normalverteilung (Referenz)</b><br>Punkte liegen auf der Linie."),
  right_skew = HTML("<b>Rechtsschief</b> — konvexer Bogen oberhalb der Linie."),
  left_skew  = HTML("<b>Linksschief</b> — konkaver Bogen unterhalb der Linie."),
  heavy      = HTML("<b>Heavy Tails</b> — umgekehrtes S."),
  light      = HTML("<b>Light Tails</b> — normales S."),
  bimodal    = HTML("<b>Bimodal</b> — Plateau / Knick in der Mitte."),
  outliers   = HTML("<b>Ausreißer</b> — Mittelteil ok, Enden verschoben.")
)

# ---- Punkt-spezifische Erklärung --------------------------------------
explain_point <- function(percentile, empirical, theoretical) {
  diff <- empirical - theoretical
  if (abs(diff) < 0.1) {
    return(HTML(sprintf(
      "<div style='padding:10px;background:#E3FCEF;border-left:4px solid #00875A;'>
       <b>Perzentil %.1f%%</b> — Empirisch: %.2f, Theoretisch: %.2f → liegt auf der Linie ✓
       </div>", percentile, empirical, theoretical)))
  }
  flanke   <- if (percentile < 50) "linke (untere)" else "rechte (obere)"
  above    <- diff > 0
  bedeutung <- if (above && percentile > 50) {
    "Die <b>rechte Flanke</b> reicht WEITER hinaus → länger / dicker."
  } else if (above && percentile <= 50) {
    "Die <b>linke Flanke</b> reicht WENIGER weit ins Negative → kürzer."
  } else if (!above && percentile > 50) {
    "Die <b>rechte Flanke</b> ist <b>kürzer</b> → weniger extreme hohe Werte."
  } else {
    "Die <b>linke Flanke</b> reicht WEITER ins Negative → länger / dicker."
  }
  farbe <- if (above) "#FFEBE6" else "#DEEBFF"
  rand  <- if (above) "#DE350B" else "#0052CC"
  label <- if (above) "OBERHALB" else "UNTERHALB"
  HTML(sprintf(
    "<div style='padding:10px;background:%s;border-left:4px solid %s;'>
     <b>Perzentil %.1f%%</b> (%s Flanke)<br>
     Empirisch: <b>%.2f</b> | Theoretisch: <b>%.2f</b> | Abweichung: <b>%+.2f</b><br>
     Punkt liegt <b>%s</b> der Linie.<br><br>%s</div>",
    farbe, rand, percentile, flanke, empirical, theoretical, diff, label, bedeutung))
}

# ---- UI ----------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Q-Q-Plot Explorer & Simulation"),

  tabsetPanel(
    # -------------------- TAB 1: Explorer --------------------
    tabPanel("Explorer",
      sidebarLayout(
        sidebarPanel(
          selectInput("dist", "Verteilungstyp:", choices = dist_choices),
          sliderInput("n", "Stichprobengröße:",
                      min = 50, max = 2000, value = 500, step = 50),
          actionButton("resample", "Neue Stichprobe", class = "btn-primary", width = "100%"),
          hr(),
          htmlOutput("explanation")
        ),
        mainPanel(
          fluidRow(
            column(6, plotOutput("histPlot", height = "320px")),
            column(6, plotOutput("qqPlot",   height = "320px", click = "qq_click"))
          ),
          hr(),
          h4("📍 Punkt-Erklärung"),
          htmlOutput("click_info")
        )
      )
    ),

    # -------------------- TAB 2: Simulation --------------------
    tabPanel("Simulation: Wie wird der Q-Q-Plot aufgebaut?",
      sidebarLayout(
        sidebarPanel(
          selectInput("sim_dist", "Verteilungstyp:", choices = dist_choices,
                      selected = "right_skew"),
          sliderInput("sim_n", "Anzahl Datenpunkte:",
                      min = 10, max = 50, value = 20, step = 1),
          hr(),
          h4("Steuerung"),
          fluidRow(
            column(6, actionButton("sim_reset", "↺ Reset", width = "100%")),
            column(6, actionButton("sim_next",  "Schritt →", width = "100%",
                                   class = "btn-primary"))
          ),
          br(),
          actionButton("sim_play", "▶ Auto-Play / ⏸ Pause", width = "100%"),
          br(), br(),
          sliderInput("sim_speed", "Sekunden pro Schritt:",
                      min = 0.2, max = 2.0, value = 0.7, step = 0.1),
          hr(),
          uiOutput("sim_progress"),
          hr(),
          HTML("<small><b>Idee:</b> Jeder sortierte Datenpunkt ist
                automatisch ein Quantil. Bei n = 20 hast du also 20 Quantile
                — eines pro Rang. Diese werden hier nacheinander
                gegen ihre theoretischen Gegenstücke abgetragen.</small>")
        ),
        mainPanel(
          fluidRow(
            column(6, plotOutput("sim_data_plot", height = "260px")),
            column(6, plotOutput("sim_qq_plot",   height = "260px"))
          ),
          hr(),
          h4("Berechnung für aktuellen Schritt"),
          htmlOutput("sim_calculation")
        )
      )
    )
  )
)

# ---- Server ------------------------------------------------------------
server <- function(input, output, session) {

  # ============== EXPLORER TAB ==============
  current_seed <- reactiveVal(42)
  observeEvent(input$resample, { current_seed(sample.int(1e6, 1)) })

  data_r  <- reactive({ generate_data(input$dist, input$n, current_seed()) })
  qq_df_r <- reactive({ compute_qq_df(data_r()) })

  clicked <- reactiveVal(NULL)
  observeEvent(list(input$dist, input$n, current_seed()), { clicked(NULL) })
  observeEvent(input$qq_click, {
    pt <- nearPoints(qq_df_r(), input$qq_click,
                     xvar = "theoretical", yvar = "empirical",
                     threshold = 15, maxpoints = 1)
    if (nrow(pt) > 0) clicked(pt)
  })

  output$histPlot <- renderPlot({
    df <- data.frame(x = data_r())
    ggplot(df, aes(x = x)) +
      geom_histogram(aes(y = after_stat(density)),
                     bins = 40, fill = "#4C9AFF", color = "white") +
      geom_density(color = "#0747A6", linewidth = 1) +
      stat_function(fun = dnorm, color = "red", linetype = "dashed", linewidth = 1) +
      labs(title = "Histogramm + Dichte", x = "Wert (standardisiert)", y = "Dichte") +
      theme_minimal(base_size = 13)
  })

  output$qqPlot <- renderPlot({
    qq_df <- qq_df_r()
    p <- ggplot(qq_df, aes(x = theoretical, y = empirical)) +
      geom_abline(slope = 1, intercept = 0, color = "red",
                  linetype = "dashed", linewidth = 1) +
      geom_point(color = "#0747A6", alpha = 0.6, size = 1.8) +
      labs(title = "Q-Q-Plot (klick auf einen Punkt!)",
           x = "Theoretische Quantile", y = "Empirische Quantile") +
      theme_minimal(base_size = 13)
    pt <- clicked()
    if (!is.null(pt) && nrow(pt) > 0) {
      p <- p +
        geom_segment(data = pt, aes(x = theoretical, xend = theoretical,
                                    y = theoretical, yend = empirical),
                     color = "#FF8B00", linewidth = 1) +
        geom_point(data = pt, color = "#FF8B00", size = 5) +
        geom_point(data = pt, color = "white", size = 2)
    }
    p
  })

  output$explanation <- renderUI({ explanations[[input$dist]] })
  output$click_info <- renderUI({
    pt <- clicked()
    if (is.null(pt) || nrow(pt) == 0) {
      return(HTML("<div style='padding:10px;background:#F4F5F7;border-left:4px solid #6B778C;'>
                   <i>Klicke auf einen Punkt im Q-Q-Plot.</i></div>"))
    }
    explain_point(pt$percentile, pt$empirical, pt$theoretical)
  })

  # ============== SIMULATION TAB ==============
  sim_data <- reactive({
    x <- generate_data(input$sim_dist, input$sim_n, 123)
    compute_qq_df(x)
  })

  sim_step       <- reactiveVal(0)
  auto_play_active <- reactiveVal(FALSE)

  # Reset bei Inputänderung
  observeEvent(list(input$sim_dist, input$sim_n), {
    sim_step(0); auto_play_active(FALSE)
  })
  observeEvent(input$sim_reset, {
    sim_step(0); auto_play_active(FALSE)
  })
  observeEvent(input$sim_next, {
    if (sim_step() < nrow(sim_data())) sim_step(sim_step() + 1)
  })
  observeEvent(input$sim_play, {
    if (sim_step() >= nrow(sim_data())) sim_step(0)
    auto_play_active(!auto_play_active())
  })

  # Auto-Play Loop
  observe({
    if (!auto_play_active()) return()
    invalidateLater(isolate(input$sim_speed) * 1000)
    isolate({
      n <- nrow(sim_data())
      if (sim_step() < n) {
        sim_step(sim_step() + 1)
      } else {
        auto_play_active(FALSE)
      }
    })
  })

  # --- Datenplot (sortierte Werte mit aktuellem Rang) ---
  output$sim_data_plot <- renderPlot({
    df <- sim_data()
    k  <- sim_step()
    df$status <- "noch nicht"
    if (k > 0) df$status[1:k] <- "fertig"
    if (k > 0) df$status[k]   <- "aktuell"
    df$status <- factor(df$status, levels = c("noch nicht", "fertig", "aktuell"))

    p <- ggplot(df, aes(x = empirical, y = 0)) +
      geom_point(aes(color = status, size = status), alpha = 0.85) +
      scale_color_manual(values = c("noch nicht" = "#C1C7D0",
                                    "fertig" = "#4C9AFF",
                                    "aktuell" = "#FF8B00"),
                         drop = FALSE) +
      scale_size_manual(values = c("noch nicht" = 3, "fertig" = 3, "aktuell" = 6),
                        drop = FALSE, guide = "none") +
      coord_cartesian(ylim = c(-1, 1.2)) +
      labs(title = "Sortierte Datenpunkte",
           subtitle = "Orange: aktueller Rang | Blau: schon abgetragen | Grau: kommt noch",
           x = "Wert (standardisiert)", y = "", color = "") +
      theme_minimal(base_size = 12) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            legend.position = "bottom")

    if (k > 0) {
      cur <- df[k, ]
      p <- p + annotate("text", x = cur$empirical, y = 0.6,
                        label = sprintf("Rang %d", k),
                        color = "#FF8B00", fontface = "bold", size = 4.2)
    }
    p
  })

  # --- QQ-Plot wird Punkt für Punkt aufgebaut ---
  output$sim_qq_plot <- renderPlot({
    df <- sim_data()
    k  <- sim_step()
    xr <- range(df$theoretical) + c(-0.3, 0.3)
    yr <- range(df$empirical)   + c(-0.3, 0.3)

    p <- ggplot() +
      geom_abline(slope = 1, intercept = 0, color = "red",
                  linetype = "dashed", linewidth = 1) +
      coord_cartesian(xlim = xr, ylim = yr) +
      labs(title = "Q-Q-Plot (wird aufgebaut)",
           x = "Theoretisches Quantil (Normal)",
           y = "Empirisches Quantil (Daten)") +
      theme_minimal(base_size = 12)

    if (k >= 1) {
      done <- df[seq_len(k), ]
      p <- p + geom_point(data = done, aes(x = theoretical, y = empirical),
                          color = "#0747A6", size = 2.2, alpha = 0.7)
      cur <- df[k, ]
      p <- p +
        geom_segment(aes(x = cur$theoretical, xend = cur$theoretical,
                         y = yr[1],           yend = cur$empirical),
                     linetype = "dotted", color = "#FF8B00") +
        geom_segment(aes(x = xr[1],           xend = cur$theoretical,
                         y = cur$empirical,   yend = cur$empirical),
                     linetype = "dotted", color = "#FF8B00") +
        geom_point(aes(x = cur$theoretical, y = cur$empirical),
                   color = "#FF8B00", size = 5) +
        geom_point(aes(x = cur$theoretical, y = cur$empirical),
                   color = "white", size = 2)
    }
    p
  })

  output$sim_progress <- renderUI({
    HTML(sprintf("<b>Fortschritt:</b> %d / %d", sim_step(), input$sim_n))
  })

  output$sim_calculation <- renderUI({
    df <- sim_data(); k <- sim_step(); n <- nrow(df)
    if (k == 0) {
      return(HTML("<div style='padding:15px;background:#F4F5F7;border-left:4px solid #6B778C;'>
        Klicke auf <b>'Schritt →'</b> oder <b>'Auto-Play'</b>, um die Simulation zu starten.<br><br>
        <b>Was passiert in jedem Schritt?</b><br>
        1. Datenpunkt mit Rang k auswählen → empirischer Wert x<sub>(k)</sub><br>
        2. Empirisches Perzentil berechnen: <code>p = (k − 0.5) / n</code><br>
        3. Theoretisches Quantil ermitteln: <code>qnorm(p)</code><br>
        4. Punkt (theoretisch, empirisch) im Q-Q-Plot abtragen.</div>"))
    }
    row <- df[k, ]
    HTML(sprintf("
      <div style='padding:15px;background:#FFFAE6;border-left:4px solid #FF8B00;
                  font-family:monospace;line-height:1.7;'>
        <span style='font-family:sans-serif;'><b>Schritt %d von %d</b></span><br><br>
        <b>1.</b> Sortierter Datenwert (Rang %d): &nbsp;
        x<sub>(%d)</sub> = <b>%.3f</b><br>
        <b>2.</b> Empirisches Perzentil: &nbsp;
        p = (%d − 0.5) / %d = <b>%.4f</b> &nbsp;(= %.2f%%)<br>
        <b>3.</b> Theoretisches Quantil: &nbsp;
        qnorm(%.4f) = <b>%.3f</b><br>
        <b>4.</b> → Plotte Punkt: &nbsp;
        (<b>%.3f</b>, <b>%.3f</b>)
      </div>",
      k, n, k, k, row$empirical, k, n, row$probability, row$percentile,
      row$probability, row$theoretical, row$theoretical, row$empirical))
  })
}

shinyApp(ui, server)
