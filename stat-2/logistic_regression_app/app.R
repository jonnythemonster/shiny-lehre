# install.packages(c("shiny", "plotly", "bslib", "bsicons"))

library(shiny)
library(plotly)
library(bslib)
library(bsicons)

# ── Farben ────────────────────────────────────────────────────────────────────
COL_KURVE    <- "#22D3EE"   # teal
COL_LM       <- "#FB923C"   # orange
COL_WP       <- "#F87171"   # red
COL_PASS     <- "#4ADE80"   # green
COL_FAIL     <- "#F87171"   # red
COL_SELECTED <- "#FCD34D"   # amber
COL_TRUE     <- "#C084FC"   # purple

# ── Szenarien-Presets ─────────────────────────────────────────────────────────
PRESETS <- list(
  "Schwacher Effekt (b1=0.5)"  = list(dgp_b0 = -3,   dgp_b1 = 0.5,  n = 200,  seed = 7),
  "Mittlerer Effekt (b1=1.0)"  = list(dgp_b0 = -5,   dgp_b1 = 1.0,  n = 200,  seed = 7),
  "Starker Effekt (b1=1.5)"    = list(dgp_b0 = -7.5, dgp_b1 = 1.5,  n = 200,  seed = 7),
  "Sehr steile Kurve (b1=2.0)" = list(dgp_b0 = -10,  dgp_b1 = 2.0,  n = 200,  seed = 7),
  "Grosses n (n=1000)"         = list(dgp_b0 = -3,   dgp_b1 = 0.8,  n = 1000, seed = 42)
)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- page_sidebar(
  title = "Logistische Regression - Interaktive Simulation",
  theme = bs_theme(
    bootswatch   = "darkly",
    base_font    = font_google("DM Sans"),
    heading_font = font_google("Syne"),
    code_font    = font_google("JetBrains Mono"),
    bg           = "#0F172A",
    fg           = "#E2E8F0",
    primary      = "#22D3EE",
    secondary    = "#334155",
    success      = "#4ADE80",
    danger       = "#F87171",
    "border-radius" = "10px",
    "card-border-color" = "#1E293B"
  ),

  # ── Custom CSS ──────────────────────────────────────────────────────────────
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$style(HTML("
      /* ── Global ── */
      body, .bslib-page-sidebar { background: #0F172A !important; }

      /* ── Navbar / Title ── */
      .navbar, .bslib-sidebar-layout > .bslib-main > .bslib-navs-card-tab > .card-header {
        background: #0F172A !important;
      }
      .navbar-brand {
        font-family: 'Syne', sans-serif !important;
        font-weight: 800 !important;
        font-size: 1.15rem !important;
        letter-spacing: -0.02em;
        color: #E2E8F0 !important;
      }
      .navbar-brand::before {
        content: '◈ ';
        color: #22D3EE;
      }

      /* ── Sidebar ── */
      .bslib-sidebar-layout > .sidebar {
        background: #111827 !important;
        border-right: 1px solid #1E293B !important;
      }
      .sidebar .sidebar-content { padding: 16px 14px; }

      /* Section labels */
      .sidebar-section-label {
        font-family: 'JetBrains Mono', monospace;
        font-size: 10px;
        font-weight: 600;
        letter-spacing: 0.12em;
        text-transform: uppercase;
        color: #67E8F9;
        margin: 14px 0 6px 0;
        padding-bottom: 4px;
        border-bottom: 1px solid #1E293B;
      }

      /* Inputs */
      .form-control, .form-select {
        background: #1E293B !important;
        border: 1px solid #334155 !important;
        color: #E2E8F0 !important;
        border-radius: 6px !important;
        font-size: 13px !important;
      }
      .form-control:focus, .form-select:focus {
        border-color: #22D3EE !important;
        box-shadow: 0 0 0 2px rgba(34,211,238,0.15) !important;
      }
      label { color: #CBD5E1 !important; font-size: 12px !important; }

      /* Sliders */
      .irs--shiny .irs-bar { background: #22D3EE !important; border-top-color: #22D3EE !important; border-bottom-color: #22D3EE !important; }
      .irs--shiny .irs-handle { background: #22D3EE !important; border-color: #0F172A !important; box-shadow: 0 0 0 3px rgba(34,211,238,0.3) !important; }
      .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
        background: #22D3EE !important; color: #0F172A !important;
        font-family: 'JetBrains Mono', monospace !important;
        font-size: 11px !important; font-weight: 700 !important;
      }
      .irs--shiny .irs-line { background: #334155 !important; border-color: #334155 !important; }
      .irs--shiny .irs-min, .irs--shiny .irs-max { color: #475569 !important; font-size: 10px !important; }

      /* Buttons */
      .btn-primary {
        background: linear-gradient(135deg, #0891B2, #22D3EE) !important;
        border: none !important;
        color: #0F172A !important;
        font-weight: 700 !important;
        font-size: 12px !important;
        letter-spacing: 0.03em;
        border-radius: 6px !important;
        transition: all 0.2s ease !important;
      }
      .btn-primary:hover {
        transform: translateY(-1px);
        box-shadow: 0 4px 12px rgba(34,211,238,0.3) !important;
      }
      .btn-outline-primary {
        border: 1px solid #22D3EE !important;
        color: #22D3EE !important;
        background: transparent !important;
        font-size: 12px !important;
        font-weight: 600 !important;
        border-radius: 6px !important;
        transition: all 0.2s ease !important;
      }
      .btn-outline-primary:hover {
        background: rgba(34,211,238,0.1) !important;
        transform: translateY(-1px);
      }
      .btn-outline-secondary {
        border: 1px solid #334155 !important;
        color: #94A3B8 !important;
        background: transparent !important;
        font-size: 12px !important;
        border-radius: 6px !important;
        transition: all 0.2s ease !important;
      }
      .btn-outline-secondary:hover {
        background: rgba(148,163,184,0.08) !important;
        border-color: #94A3B8 !important;
        color: #E2E8F0 !important;
      }

      /* Checkboxes */
      .form-check-input { background-color: #1E293B !important; border-color: #334155 !important; }
      .form-check-input:checked { background-color: #22D3EE !important; border-color: #22D3EE !important; }
      .form-check-label { color: #E2E8F0 !important; font-size: 13px !important; }

      /* HR divider */
      hr { border-color: #1E293B !important; margin: 10px 0 !important; }

      /* ── Cards ── */
      .card {
        background: #111827 !important;
        border: 1px solid #1E293B !important;
        border-radius: 10px !important;
        box-shadow: 0 4px 24px rgba(0,0,0,0.4) !important;
      }
      .card-header {
        background: #0F172A !important;
        border-bottom: 1px solid #1E293B !important;
        font-family: 'Syne', sans-serif !important;
        font-size: 12px !important;
        font-weight: 700 !important;
        letter-spacing: 0.08em !important;
        text-transform: uppercase !important;
        color: #94A3B8 !important;
        padding: 10px 16px !important;
      }

      /* ── Nav tabs ── */
      .nav-tabs { border-bottom: 1px solid #1E293B !important; }
      .nav-tabs .nav-link {
        color: #94A3B8 !important;
        font-size: 13px !important;
        font-weight: 600 !important;
        border: none !important;
        padding: 10px 18px !important;
        transition: color 0.2s;
      }
      .nav-tabs .nav-link:hover { color: #CBD5E1 !important; background: transparent !important; }
      .nav-tabs .nav-link.active {
        color: #22D3EE !important;
        background: transparent !important;
        border-bottom: 2px solid #22D3EE !important;
      }

      /* ── Table ── */
      table { font-family: 'JetBrains Mono', monospace !important; font-size: 12px !important; }
      thead tr th { color: #94A3B8 !important; font-size: 11px !important; letter-spacing: 0.06em; text-transform: uppercase; border-bottom: 1px solid #1E293B !important; }
      tbody tr { border-color: #1E293B !important; }
      tbody tr:hover { background: rgba(34,211,238,0.05) !important; }
      .table-striped > tbody > tr:nth-of-type(odd) > * { background-color: rgba(255,255,255,0.02) !important; }

      /* ── Kennzahlen-Werte ── */
      .kz-value { font-family: 'JetBrains Mono', monospace !important; }
    "))
  ),

  sidebar = sidebar(
    width = 300,

    tags$div("DATENGENERIERUNG", class = "sidebar-section-label"),

    selectInput("preset", "Szenario",
                choices = names(PRESETS), selected = names(PRESETS)[2]),

    layout_columns(
      col_widths = c(6, 6),
      numericInput("dgp_b0", HTML("b0 (wahr)"), value = -5,  step = 0.5),
      numericInput("dgp_b1", HTML("b1 (wahr)"), value =  1,  step = 0.1)
    ),
    layout_columns(
      col_widths = c(6, 6),
      numericInput("dgp_n",    "n",    value = 200, min = 50, max = 2000, step = 50),
      numericInput("dgp_seed", "Seed", value = 7,   min = 1,  max = 999,  step = 1)
    ),
    actionButton("new_data", "Neue Daten generieren",
                 icon  = icon("shuffle"),
                 class = "btn-outline-primary btn-sm w-100 mt-1 mb-3"),

    hr(),

    tags$div("MODELLPARAMETER", class = "sidebar-section-label"),

    sliderInput("b0", label = HTML("b<sub>0</sub> (Intercept)"),
                min = -12, max = 4, value = -5, step = 0.1),
    sliderInput("b1", label = HTML("b<sub>1</sub> (Steigung)"),
                min = 0.05, max = 4, value = 1.0, step = 0.05),

    layout_columns(
      col_widths = c(6, 6),
      actionButton("fit_mle",  "MLE",
                   icon  = icon("wand-magic-sparkles"),
                   class = "btn-primary btn-sm w-100"),
      actionButton("fit_true", "Wahrer DGP",
                   icon  = icon("bullseye"),
                   class = "btn-outline-secondary btn-sm w-100")
    ),

    hr(),

    tags$div("ANZEIGE", class = "sidebar-section-label"),
    checkboxInput("show_data",       "Datenpunkte anzeigen",       value = TRUE),
    checkboxInput("show_true",       "Wahren DGP einblenden",      value = TRUE),
    checkboxInput("show_lm",         "Lineare Regression zeigen",  value = FALSE),
    checkboxInput("show_wendepunkt", "Wendepunkt markieren",        value = TRUE),
    checkboxInput("show_ci",         "95%-Konfidenzband (MLE)",     value = FALSE),

    hr(),

    tags$div("KENNZAHLEN", class = "sidebar-section-label"),
    uiOutput("kennzahlen_box")
  ),

  navset_card_tab(

    nav_panel(
      "Wahrscheinlichkeitskurve",
      layout_columns(
        col_widths = c(8, 4),
        card(
          card_header("Klick auf einen Datenpunkt fur Details"),
          plotlyOutput("kurve_plot", height = "420px")
        ),
        tagList(
          card(
            card_header("Ausgewahlter Punkt"),
            uiOutput("punkt_info"),
            min_height = "170px"
          ),
          card(
            card_header("Vorhergesagte Wahrscheinlichkeiten"),
            tableOutput("prob_tabelle")
          )
        )
      )
    ),

    nav_panel(
      "Residuenplot",
      card(
        card_header("Pearson-Residuen vs. angepasste Werte"),
        plotlyOutput("residuen_plot", height = "420px")
      )
    ),

    nav_panel(
      "Log-Likelihood",
      card(
        card_header(
          "Log-Likelihood-Flache - Contour uber b0 / b1",
          tooltip(bs_icon("info-circle"),
                  "Hellere Farbe = hohere LL. Kreuz = aktuelles Modell, Stern = MLE, Raute = wahrer DGP.")
        ),
        plotlyOutput("loglik_plot", height = "460px")
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  selected_id <- reactiveVal(NULL)

  # ── Reaktive Daten ────────────────────────────────────────────────────────
  klausur_df <- reactive({
    input$new_data
    set.seed(isolate(input$dgp_seed))
    n   <- isolate(input$dgp_n)
    b0t <- isolate(input$dgp_b0)
    b1t <- isolate(input$dgp_b1)
    x   <- runif(n, 0, 12)
    y   <- rbinom(n, 1, plogis(b0t + b1t * x))
    data.frame(id = seq_len(n), lernstunden = round(x, 2), bestanden = y)
  })

  # ── Preset -> DGP-Felder aktualisieren ───────────────────────────────────
  observeEvent(input$preset, {
    p <- PRESETS[[input$preset]]
    updateNumericInput(session, "dgp_b0",   value = p$dgp_b0)
    updateNumericInput(session, "dgp_b1",   value = p$dgp_b1)
    updateNumericInput(session, "dgp_n",    value = p$n)
    updateNumericInput(session, "dgp_seed", value = p$seed)
    updateSliderInput(session,  "b0",       value = p$dgp_b0)
    updateSliderInput(session,  "b1",       value = p$dgp_b1)
    selected_id(NULL)
  })

  # ── MLE reaktiv ───────────────────────────────────────────────────────────
  mle_fit <- reactive({
    df <- klausur_df()
    tryCatch(
      glm(bestanden ~ lernstunden, data = df, family = binomial(link = "logit")),
      error = function(e) NULL
    )
  })







  mle_coefs <- reactive({
    fit <- mle_fit()
    if (is.null(fit)) return(c(NA, NA))
    unname(round(coef(fit), 3))  # unname() verhindert JSON-Fehler in Shiny
  })

  # ── Buttons ───────────────────────────────────────────────────────────────
  observeEvent(input$fit_mle, {
    coefs <- mle_coefs()
    if (!anyNA(coefs)) {
      updateSliderInput(session, "b0", value = round(coefs[[1]], 1))
      updateSliderInput(session, "b1", value = round(coefs[[2]], 2))
    }
  })

  observeEvent(input$fit_true, {
    updateSliderInput(session, "b0", value = input$dgp_b0)
    updateSliderInput(session, "b1", value = input$dgp_b1)
  })

  observeEvent(input$new_data, { selected_id(NULL) })

  # ── Klick-Event ───────────────────────────────────────────────────────────
  observeEvent(event_data("plotly_click", source = "main"), {
    d <- event_data("plotly_click", source = "main")
    if (!is.null(d) && !is.null(d$customdata)) selected_id(d$customdata)
  })

  # ── Hilfswerte ────────────────────────────────────────────────────────────
  wendepunkt <- reactive({ round(-input$b0 / input$b1, 2) })
  or_val     <- reactive({ round(exp(input$b1), 3) })

  loglik_val <- reactive({
    df  <- klausur_df()
    eta <- input$b0 + input$b1 * df$lernstunden
    round(sum(dbinom(df$bestanden, 1, plogis(eta), log = TRUE)), 2)
  })

  # ── Kennzahlen-Box ────────────────────────────────────────────────────────
  output$kennzahlen_box <- renderUI({
    coefs <- mle_coefs()
    row <- function(label, value, col = "#94A3B8") {
      tags$div(
        style = "display:flex; justify-content:space-between; align-items:center; padding:6px 0; border-bottom:1px solid #1E293B;",
        tags$span(label, style = "color:#94A3B8; font-size:11px; letter-spacing:0.04em; text-transform:uppercase;"),
        tags$span(value, style = paste0("font-family:'JetBrains Mono',monospace; font-weight:700; font-size:13px; color:", col, ";"), class="kz-value")
      )
    }
    mle_str <- if (anyNA(coefs)) "-" else paste0(coefs[1], " / ", coefs[2])
    tags$div(
      style = "background:#0F172A; border-radius:8px; padding:4px 10px; border:1px solid #1E293B;",
      row("OR (e^b₁):",    or_val()),
      row("Wendepunkt X:",      wendepunkt()),
      row("P(best.|X=8):",      round(plogis(input$b0 + input$b1 * 8), 3)),
      row("Log-Likelihood:",    loglik_val()),
      row("MLE b₀/b₁:", mle_str, "#22D3EE"),
      tags$div(
        style = "display:flex; justify-content:space-between; align-items:center; padding:6px 0;",
        tags$span("DGP b₀/b₁:", style="color:#94A3B8; font-size:11px; letter-spacing:0.04em; text-transform:uppercase;"),
        tags$span(paste0(input$dgp_b0, " / ", input$dgp_b1),
                  style="font-family:'JetBrains Mono',monospace; font-weight:700; font-size:13px; color:#C084FC;")
      )
    )
  })

  # ── Punkt-Info-Box ────────────────────────────────────────────────────────
  output$punkt_info <- renderUI({
    sid <- selected_id()
    if (is.null(sid)) {
      return(tags$p(HTML("\u2197 Klicke auf einen Datenpunkt im Plot."),
                    style = "color:#475569; font-size:12px; padding:12px; font-style:italic;"))
    }
    df  <- klausur_df()
    row <- df[df$id == sid, ]
    if (nrow(row) == 0) return(NULL)
    eta <- input$b0 + input$b1 * row$lernstunden
    p   <- round(plogis(eta), 3)
    res <- round(row$bestanden - p, 3)
    col <- if (row$bestanden == 1) COL_PASS else COL_FAIL
    txt <- if (row$bestanden == 1) "✓ Bestanden" else "✗ Nicht bestanden"
    bg  <- if (row$bestanden == 1) "rgba(74,222,128,0.08)" else "rgba(248,113,113,0.08)"

    pi_row <- function(label, value) {
      tags$div(
        style = "display:flex; justify-content:space-between; padding:5px 0; border-bottom:1px solid #1E293B;",
        tags$span(label, style = "color:#94A3B8; font-size:11px; text-transform:uppercase; letter-spacing:0.04em;"),
        tags$span(value, style = "font-family:'JetBrains Mono',monospace; font-weight:700; font-size:12px; color:#F1F5F9;")
      )
    }

    tags$div(
      style = "padding:10px;",
      tags$div(
        style = paste0("background:", bg, "; border-left:3px solid ", col,
                       "; border-radius:6px; padding:8px 12px; margin-bottom:10px;"),
        tags$span(txt, style = paste0("color:", col, "; font-weight:800; font-size:14px; font-family:'Syne',sans-serif;"))
      ),
      pi_row("Studierenden-ID", sid),
      pi_row("Lernstunden",     paste0(row$lernstunden, " h")),
      pi_row("P̂ (aktuell)", p),
      pi_row("Log-Odds η",  round(eta, 3)),
      pi_row("Residuum",        res),

      # ── LL-Beitrag ──────────────────────────────────────────────────────
      tags$div(style="margin-top:10px;"),
      {
        ll_beitrag <- if (row$bestanden == 1) round(log(p), 3) else round(log(1 - p), 3)
        ll_formel  <- if (row$bestanden == 1)
          paste0("log(P̂) = log(", p, ")")
        else
          paste0("log(1−P̂) = log(", round(1-p, 3), ")")
        ll_col <- if (ll_beitrag > -0.5) "#4ADE80"
                  else if (ll_beitrag > -1.5) "#FCD34D"
                  else "#F87171"
        ll_bar_pct <- round(100 * min(1, max(0, 1 + ll_beitrag / 5)), 1)

        tags$div(
          style = paste0("background:#0F172A; border:1px solid #1E293B;
            border-left:3px solid ", ll_col, ";
            border-radius:6px; padding:10px 12px; margin-top:2px;"),
          tags$div(
            style = "display:flex; justify-content:space-between; margin-bottom:6px;",
            tags$span("LL-BEITRAG",
              style = "color:#94A3B8; font-size:10px; letter-spacing:0.1em; text-transform:uppercase;"),
            tags$span(ll_beitrag,
              style = paste0("font-family:'JetBrains Mono',monospace; font-weight:800;
                font-size:16px; color:", ll_col, ";"))
          ),
          tags$div(
            style = "font-family:'JetBrains Mono',monospace; font-size:11px;
              color:#64748B; margin-bottom:8px;",
            ll_formel
          ),
          # Balken: Qualität der Vorhersage
          tags$div(
            style = "background:#1E293B; border-radius:4px; height:6px; overflow:hidden;",
            tags$div(style = paste0("background:", ll_col, "; width:", ll_bar_pct,
              "%; height:100%; border-radius:4px;
              transition:width 0.4s ease;"))
          ),
          tags$div(
            style = "display:flex; justify-content:space-between; margin-top:3px;",
            tags$span("schlechte Vorhersage",
              style="font-size:9px; color:#475569;"),
            tags$span("gute Vorhersage",
              style="font-size:9px; color:#475569;")
          )
        )
      }
    )
  })

  # ── Hauptplot ─────────────────────────────────────────────────────────────
  output$kurve_plot <- renderPlotly({
    df    <- klausur_df()
    sid   <- selected_id()
    x_seq <- seq(0, 12, length.out = 400)
    df_k  <- data.frame(x = x_seq,
                        p = plogis(input$b0 + input$b1 * x_seq))

    df$farbe <- ifelse(df$bestanden == 1, COL_PASS, COL_FAIL)
    if (is.null(sid)) {
      df$rahmen <- "white"; df$groesse <- 6
    } else {
      df$rahmen  <- ifelse(df$id == sid, COL_SELECTED, "white")
      df$groesse <- ifelse(df$id == sid, 11, 6)
    }
    df$p_hat <- round(plogis(input$b0 + input$b1 * df$lernstunden), 3)

    plt <- plot_ly(source = "main")

    # 95%-KI
    if (input$show_ci) {
      fit <- mle_fit()
      if (!is.null(fit)) {
        pred <- predict(fit, newdata = data.frame(lernstunden = x_seq),
                        se.fit = TRUE, type = "link")
        plt <- plt %>% add_ribbons(
          x = x_seq,
          ymin = plogis(pred$fit - 1.96 * pred$se.fit),
          ymax = plogis(pred$fit + 1.96 * pred$se.fit),
          fillcolor = paste0(COL_KURVE, "25"),
          line = list(color = "transparent"),
          name = "95%-KI", hoverinfo = "skip"
        )
      }
    }

    # Wahrer DGP
    if (input$show_true) {
      plt <- plt %>% add_lines(
        x = x_seq,
        y = plogis(input$dgp_b0 + input$dgp_b1 * x_seq),
        line = list(color = COL_TRUE, width = 1.8, dash = "dot"),
        name = paste0("Wahrer DGP (b0=", input$dgp_b0, ", b1=", input$dgp_b1, ")"),
        hovertemplate = "X=%{x:.2f}  P(wahr)=%{y:.3f}<extra></extra>"
      )
    }

    # Lineare Regression
    if (input$show_lm) {
      lm_c <- coef(lm(bestanden ~ lernstunden, data = df))
      plt <- plt %>% add_lines(
        x = x_seq, y = lm_c[1] + lm_c[2] * x_seq,
        line = list(color = COL_LM, width = 1.8, dash = "dash"),
        name = "Lineares Modell"
      )
    }

    # Datenpunkte
    if (input$show_data) {
      set.seed(99)
      jy <- df$bestanden + runif(nrow(df), -0.018, 0.018)
      plt <- plt %>% add_markers(
        data = df, x = ~lernstunden, y = ~jy, customdata = ~id,
        marker = list(color = ~farbe,
                      line  = list(color = ~rahmen, width = 2),
                      size  = ~groesse, opacity = 0.72),
        text = ~paste0("<b>ID ", id, "</b><br>",
                       "Stunden: ", lernstunden, " h<br>",
                       ifelse(bestanden == 1, "Bestanden", "Nicht bestanden"),
                       "<br>P = ", p_hat),
        hoverinfo = "text", name = "Beobachtungen"
      )
    }

    # Logistische Kurve
    plt <- plt %>%
      add_lines(data = df_k, x = ~x, y = ~p,
                line = list(color = COL_KURVE, width = 2.6),
                name = "Aktuelles Modell",
                hovertemplate = "X=%{x:.2f}  P=%{y:.3f}<extra></extra>") %>%
      add_lines(x = c(0,12), y = c(.5,.5),
                line = list(color="grey70", width=1, dash="dash"),
                showlegend = FALSE, hoverinfo = "skip")

    # Wendepunkt
    wp <- wendepunkt()
    if (input$show_wendepunkt && !is.nan(wp) && wp >= 0 && wp <= 12) {
      plt <- plt %>%
        add_segments(x=wp, xend=wp, y=-0.05, yend=0.5,
                     line=list(color=COL_WP, width=1.2, dash="dot"),
                     showlegend=FALSE, hoverinfo="skip") %>%
        add_markers(x=wp, y=0.5,
                    marker=list(color=COL_WP, size=10),
                    name=paste0("Wendepunkt X=", wp),
                    hovertemplate=paste0("Wendepunkt X=",wp,"  P=0.50<extra></extra>"))
    }

    # ── Likelihood-Beitrag-Visualisierung ──────────────────────────────────
    if (!is.null(sid)) {
      sel       <- df[df$id == sid, ]
      x_sel     <- sel$lernstunden
      y_obs     <- sel$bestanden          # 0 oder 1
      p_sel     <- plogis(input$b0 + input$b1 * x_sel)

      ll_beitrag <- if (y_obs == 1) round(log(p_sel), 3) else round(log(1 - p_sel), 3)
      ll_col     <- if (ll_beitrag > -0.5) COL_PASS
                    else if (ll_beitrag > -1.5) COL_SELECTED
                    else COL_FAIL

      # Schattiertes Band zwischen Datenpunkt und Kurve
      plt <- plt %>%
        add_ribbons(
          x    = c(x_sel - 0.08, x_sel + 0.08),
          ymin = c(min(y_obs, p_sel), min(y_obs, p_sel)),
          ymax = c(max(y_obs, p_sel), max(y_obs, p_sel)),
          fillcolor = paste0(ll_col, "30"),
          line      = list(color = "transparent"),
          showlegend = FALSE, hoverinfo = "skip"
        ) %>%

        # Vertikale "Brücke" vom Beobachtungspunkt zur Kurve
        add_segments(
          x = x_sel, xend = x_sel,
          y = y_obs,  yend = p_sel,
          line = list(color = ll_col, width = 2.5, dash = "solid"),
          showlegend = FALSE,
          hovertemplate = paste0(
            "<b>Likelihood-Brücke</b><br>",
            "y = ", y_obs, " → P̂ = ", round(p_sel, 3), "<br>",
            "LL-Beitrag: ", ll_beitrag,
            "<extra></extra>"
          )
        ) %>%

        # Horizontale Referenzlinie: x-Achse bis Kurvenpunkt
        add_segments(
          x = 0, xend = x_sel,
          y = p_sel, yend = p_sel,
          line = list(color = ll_col, width = 1, dash = "dot"),
          showlegend = FALSE, hoverinfo = "skip"
        ) %>%

        # Leuchtender Punkt auf der Kurve
        add_markers(
          x = x_sel, y = p_sel,
          marker = list(
            color  = ll_col,
            size   = 14,
            symbol = "circle",
            line   = list(color = "white", width = 2)
          ),
          name = paste0("P̂ = ", round(p_sel, 3)),
          hovertemplate = paste0(
            "Kurvenpunkt<br>X = ", x_sel,
            "<br>P̂ = ", round(p_sel, 3),
            "<extra></extra>"
          )
        ) %>%

        # Annotation: LL-Formel und Wert
        add_annotations(
          x    = x_sel,
          y    = (y_obs + p_sel) / 2,
          text = paste0(
            "<b>LL = ", ll_beitrag, "</b><br>",
            if (y_obs == 1)
              paste0("log(<b>", round(p_sel,3), "</b>)")
            else
              paste0("log(1−<b>", round(p_sel,3), "</b>)")
          ),
          showarrow  = TRUE,
          arrowhead  = 2,
          arrowsize  = 0.8,
          arrowcolor = ll_col,
          ax         = 50,
          ay         = 0,
          font       = list(color = ll_col, size = 12,
                            family = "JetBrains Mono"),
          bgcolor    = "#0F172A",
          bordercolor = ll_col,
          borderwidth = 1,
          borderpad   = 5,
          opacity     = 0.92
        )
    }

    plt %>% layout(
      xaxis = list(
        title     = "Lernstunden X",
        range     = c(0, 12),
        tickvals  = seq(0, 12, 2),
        gridcolor = "#1E293B",
        zerolinecolor = "#1E293B",
        linecolor     = "#334155",
        tickfont  = list(family = "JetBrains Mono", size = 11, color = "#94A3B8")
      ),
      yaxis = list(
        title     = "P(Bestanden)",
        range     = c(-0.05, 1.05),
        tickvals  = c(0, .25, .5, .75, 1),
        gridcolor = "#1E293B",
        zerolinecolor = "#1E293B",
        linecolor     = "#334155",
        tickfont  = list(family = "JetBrains Mono", size = 11, color = "#94A3B8")
      ),
      title = list(
        text = paste0("Logit(P) = ", input$b0, " + ", input$b1,
                      " · X  |  OR = ", or_val(), "  |  LL = ", loglik_val()),
        font = list(size = 12, family = "JetBrains Mono", color = "#94A3B8")
      ),
      legend = list(orientation = "h", x = 0, y = -0.22,
                    font = list(size = 11, color = "#CBD5E1"),
                    bgcolor = "rgba(0,0,0,0)"),
      hovermode     = "closest",
      plot_bgcolor  = "#111827",
      paper_bgcolor = "#111827",
      font          = list(color = "#CBD5E1", family = "DM Sans")
    ) %>% config(
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c("lasso2d","select2d"),
      toImageButtonOptions = list(format="png", filename="logit_kurve",
                                  width=900, height=520)
    )
  })

  # ── Residuenplot ──────────────────────────────────────────────────────────
  output$residuen_plot <- renderPlotly({
    df    <- klausur_df()
    p_hat <- plogis(input$b0 + input$b1 * df$lernstunden)
    resid <- (df$bestanden - p_hat) / sqrt(p_hat * (1 - p_hat))
    dr    <- data.frame(id=df$id, lernstunden=df$lernstunden,
                        bestanden=df$bestanden,
                        p_hat=round(p_hat,3), resid=round(resid,3))
    dr$farbe  <- ifelse(dr$bestanden==1, COL_PASS, COL_FAIL)
    dr$ausrei <- abs(dr$resid) > 2

    plot_ly(dr) %>%
      add_markers(
        x=~p_hat, y=~resid, customdata=~id,
        marker=list(color=~farbe,
                    size=~ifelse(ausrei,10,7), opacity=0.7,
                    line=list(color=~ifelse(ausrei,COL_WP,"white"),
                              width=~ifelse(ausrei,2,1))),
        text=~paste0("<b>ID ",id,"</b><br>",
                     "Lernstunden: ",lernstunden," h<br>",
                     "P = ",p_hat,"<br>",
                     "Pearson-Residuum: ",resid,
                     ifelse(ausrei,"<br><b>Moglicher Ausreisser</b>","")),
        hoverinfo="text", name="Beobachtungen"
      ) %>%
      add_lines(x=c(0,1),y=c(0,0),
                line=list(color="grey60",width=1.2,dash="dash"),
                showlegend=FALSE,hoverinfo="skip") %>%
      add_lines(x=c(0,1),y=c(2,2),
                line=list(color=COL_WP,width=1,dash="dot"),
                name="+-2 Grenze",hoverinfo="skip") %>%
      add_lines(x=c(0,1),y=c(-2,-2),
                line=list(color=COL_WP,width=1,dash="dot"),
                showlegend=FALSE,hoverinfo="skip") %>%
      layout(
        xaxis=list(title="Angepasste Wahrscheinlichkeit",
                   range=c(0,1), gridcolor="#1E293B", zerolinecolor="#1E293B",
                   linecolor="#334155", tickfont=list(family="JetBrains Mono",size=11, color="#94A3B8")),
        yaxis=list(title="Pearson-Residuum", gridcolor="#1E293B",
                   zerolinecolor="#334155", linecolor="#334155",
                   tickfont=list(family="JetBrains Mono",size=11, color="#94A3B8")),
        title=list(text=paste0("Pearson-Residuen  |  ",
                               sum(abs(resid)>2)," mogliche Ausreisser (|r|>2)"),
                   font=list(size=12, family="JetBrains Mono", color="#64748B")),
        hovermode="closest",
        plot_bgcolor="#111827", paper_bgcolor="#111827",
        font = list(color="#94A3B8")
      ) %>%
      config(displayModeBar=TRUE,
             modeBarButtonsToRemove=c("lasso2d","select2d"))
  })

  # ── Log-Likelihood-Konturplot ─────────────────────────────────────────────
  output$loglik_plot <- renderPlotly({
    df     <- klausur_df()
    b0_seq <- seq(-12, 4,   length.out = 80)
    b1_seq <- seq(0.05, 4,  length.out = 80)

    ll_mat <- outer(b0_seq, b1_seq, FUN = function(b0v, b1v) {
      mapply(function(a, b) {
        sum(dbinom(df$bestanden, 1, plogis(a + b*df$lernstunden), log=TRUE))
      }, a=b0v, b=b1v)
    })

    coefs <- mle_coefs()

    plt <- plot_ly() %>%
      add_contour(
        x=b1_seq, y=b0_seq, z=ll_mat,
        colorscale=list(c(0,"#0D1B2A"),c(0.3,"#1565C0"),
                        c(0.6,"#42A5F5"),c(0.85,"#FDD835"),c(1,"#FFEE58")),
        contours=list(showlabels=TRUE, labelfont=list(size=10,color="white")),
        colorbar=list(title="Log-Lik."),
        hovertemplate="b1=%{x:.2f}<br>b0=%{y:.2f}<br>LL=%{z:.1f}<extra></extra>"
      ) %>%
      add_markers(  # Wahrer DGP
        x=input$dgp_b1, y=input$dgp_b0,
        marker=list(symbol="diamond",size=14,color=COL_TRUE,
                    line=list(color="white",width=2)),
        name=paste0("Wahrer DGP (b0=",input$dgp_b0,", b1=",input$dgp_b1,")")
      )

    if (!anyNA(coefs)) {
      ll_mle <- round(sum(dbinom(df$bestanden,1,
                                 plogis(coefs[1]+coefs[2]*df$lernstunden),
                                 log=TRUE)),2)
      plt <- plt %>% add_markers(
        x=coefs[2], y=coefs[1],
        marker=list(symbol="star",size=16,color=COL_SELECTED,
                    line=list(color="white",width=1.5)),
        name=paste0("MLE (b0=",coefs[1],", b1=",coefs[2],")"),
        hovertemplate=paste0("MLE b0=",coefs[1],", b1=",coefs[2],
                             "<br>LL=",ll_mle,"<extra></extra>")
      )
    }

    plt %>%
      add_markers(  # Aktuelles Modell
        x=input$b1, y=input$b0,
        marker=list(symbol="cross",size=14,color=COL_WP,
                    line=list(color="white",width=2)),
        name="Aktuelles Modell",
        hovertemplate=paste0("Aktuell b0=",input$b0,", b1=",input$b1,
                             "<br>LL=",loglik_val(),"<extra></extra>")
      ) %>%
      layout(
        xaxis=list(title="b₁ (Steigung)", gridcolor="rgba(255,255,255,0.06)",
                   linecolor="#334155", tickfont=list(family="JetBrains Mono",size=11, color="#94A3B8")),
        yaxis=list(title="b₀ (Intercept)", gridcolor="rgba(255,255,255,0.06)",
                   linecolor="#334155", tickfont=list(family="JetBrains Mono",size=11, color="#94A3B8")),
        title=list(text="Log-Likelihood  |  ★ MLE  ◆ Wahrer DGP  ✖ Aktuell",
                   font=list(size=12, family="JetBrains Mono", color="#64748B")),
        legend=list(orientation="h",x=0,y=-0.15,
                    font=list(color="#CBD5E1",size=11),
                    bgcolor="rgba(0,0,0,0)"),
        paper_bgcolor="#111827", plot_bgcolor="#111827",
        font=list(color="#94A3B8", family="DM Sans")
      ) %>%
      config(displayModeBar=TRUE,
             modeBarButtonsToRemove=c("lasso2d","select2d"))
  })

  # ── Wahrscheinlichkeitstabelle ────────────────────────────────────────────
  output$prob_tabelle <- renderTable({
    x_vals <- c(2,4,6,8,10,12)
    eta    <- input$b0 + input$b1 * x_vals
    p_vals <- plogis(eta)
    data.frame(
      "X (h)"        = x_vals,
      "eta"          = round(eta, 2),
      "Odds"         = round(p_vals/(1-p_vals), 2),
      "P(Bestanden)" = round(p_vals, 3),
      check.names    = FALSE
    )
  }, striped=TRUE, hover=TRUE, bordered=FALSE, align="r")

}

shinyApp(ui, server)
