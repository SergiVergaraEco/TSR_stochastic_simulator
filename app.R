library(shiny)
options(scipen = 999)

source("R/funciones_modelo.R")
source("R/simular_individuo.R")
source("R/presets.R")
source("R/plots.R")

D <- ficha_default()

num <- function(id, lab, val, step = 0.001, ...) numericInput(id, lab, val, step = step, ...)
eur <- function(id, lab, val, step = 1000) numericInput(id, lab, val, min = 0, step = step)

card <- function(titulo, valor, sub = NULL, color = "#1C7293") {
  div(style = paste0("background:#fff;border:1px solid #e4e8ee;border-left:5px solid ",
                     color, ";border-radius:8px;padding:12px 14px;margin-bottom:10px;"),
      div(style = "font-size:12px;color:#6B7280;text-transform:uppercase;letter-spacing:.04em;", titulo),
      div(style = paste0("font-size:26px;font-weight:700;color:", color, ";line-height:1.1;margin-top:2px;"), valor),
      if (!is.null(sub)) div(style = "font-size:11.5px;color:#8090ac;margin-top:1px;", sub))
}

fmt_eur <- function(x) paste0(format(round(x), big.mark = ".", decimal.mark = ",") , " €")

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body{background:#f4f6f9;}
    .well{background:#fff;border:1px solid #e4e8ee;}
    h4.sec{margin-top:14px;margin-bottom:6px;font-weight:700;color:#21295C;
           border-bottom:2px solid #eef1f5;padding-bottom:3px;font-size:14px;}
    .titulo{font-weight:800;color:#21295C;font-size:22px;}
    .subt{color:#6B7280;font-size:13px;margin-bottom:6px;}
  "))),
  div(style = "padding:14px 8px 4px;",
      div(class = "titulo", "Simulador de desacumulación en la jubilación"),
      div(class = "subt", "Renta vitalicia + hipoteca inversa + colchón de ahorro · simulación estocástica para un individuo · TFM")),

  sidebarLayout(
    sidebarPanel(width = 4,
      helpText("Las variables se introducen en su unidad natural (fracciones, no porcentajes): ",
               tags$b("0.0096"), " = 0,96 %. Consulta la pestaña «Ejemplo de parámetros» como guía."),

      h4(class = "sec", "Individuo"),
      eur("Wf", "Patrimonio financiero (€)", D$Wf),
      eur("H0", "Valor neto de la vivienda (€)", D$H0),
      eur("S",  "Pensión / salario base (€/año)", D$S),

      h4(class = "sec", "Tipo de interés (Vasicek)"),
      fluidRow(column(4, num("theta_r", "θ media", D$theta_r, step = 0.001)),
               column(4, num("kappa_r", "κ revers.", D$kappa_r, step = 0.01)),
               column(4, num("sigma_r", "σ vol.", D$sigma_r, step = 0.001))),
      h4(class = "sec", "Inflación (Vasicek)"),
      fluidRow(column(4, num("theta_pi", "θ media", D$theta_pi, step = 0.001)),
               column(4, num("kappa_pi", "κ revers.", D$kappa_pi, step = 0.01)),
               column(4, num("sigma_pi", "σ vol.", D$sigma_pi, step = 0.001))),
      h4(class = "sec", "Revalorización inmobiliaria (Vasicek)"),
      fluidRow(column(4, num("theta_H", "θ media", D$theta_H, step = 0.001)),
               column(4, num("kappa_H", "κ revers.", D$kappa_H, step = 0.01)),
               column(4, num("sigma_H", "σ vol.", D$sigma_H, step = 0.001))),
      h4(class = "sec", "Correlaciones"),
      fluidRow(column(4, num("rho_r_pi", "ρ(r,π)", D$rho_r_pi, step = 0.01)),
               column(4, num("rho_r_H",  "ρ(r,H)", D$rho_r_H, step = 0.01)),
               column(4, num("rho_pi_H", "ρ(π,H)", D$rho_pi_H, step = 0.01))),
      h4(class = "sec", "Condición inicial"),
      fluidRow(column(6, num("r0",  "r₀",  D$r0,  step = 0.001)),
               column(6, num("pi0", "π₀", D$pi0, step = 0.001))),

      h4(class = "sec", "Biométrico"),
      num("factor_mort", "Factor de mortalidad (×)", D$factor_mort, step = 0.05),
      helpText("1 = tabla PER2020. >1 = más mortalidad (menos longevidad)."),

      h4(class = "sec", "Producto y reforma"),
      fluidRow(column(6, num("alpha_garantia", "Garantía RV α", D$alpha_garantia, step = 0.05)),
               column(6, num("Delta_TS", "Δ_TS objetivo", D$Delta_TS, step = 0.01))),
      fluidRow(column(6, num("reforma_pts", "Reforma (caída)", D$reforma_pts, step = 0.01)),
               column(6, num("reforma_plazo", "Plazo (años)", D$reforma_plazo, step = 1))),
      num("var_HI", "VaR calibración HI", D$var_HI, step = 0.01),
      helpText("Reforma 0.20 = −20 pp de tasa de sustitución. VaR 0.95 = 95 %."),

      h4(class = "sec", "Simulación"),
      fluidRow(column(4, num("N_sim", "N escen.", D$N_sim, step = 500)),
               column(4, num("N_sim_traj", "N tray.", D$N_sim_traj, step = 500)),
               column(4, num("semilla", "Semilla", D$semilla, step = 1))),

      br(),
      actionButton("simular", "▶  Simular", class = "btn-primary btn-block",
                   style = "font-weight:700;font-size:16px;padding:10px;")
    ),

    mainPanel(width = 8,
      uiOutput("cards"),
      tabsetPanel(
        tabPanel("Cobertura (TSR) en el tiempo", plotOutput("p_tsr_fan", height = 430)),
        tabPanel("Patrimonio neto",              plotOutput("p_patrimonio", height = 430)),
        tabPanel("Distribución TSR vitalicia",   plotOutput("p_tsr_dist", height = 430)),
        tabPanel("Herencia",                     plotOutput("p_herencia", height = 430)),
        tabPanel("Ficha técnica",                tableOutput("ficha")),
        tabPanel("Ejemplo de parámetros",
                 br(),
                 helpText("Guía de referencia. La columna ", tags$b("Central"),
                          " es la calibración base del TFM (la que carga la app). ",
                          tags$b("Ej. favorable"), " y ", tags$b("Ej. adverso"),
                          " son ejemplos de calibración alternativos — no presets: cópialos a mano si los quieres usar."),
                 tableOutput("ejemplos"))
      ),
      div(style = "color:#8090ac;font-size:11px;margin-top:8px;",
          "Modelo: Vasicek correlacionado (r, π, H) + tablas PER2020 + calibración de la HI por VaR. ",
          "TSR = consumo sostenido / objetivo de consumo (1 = cobertura plena).")
    )
  )
)

server <- function(input, output, session) {

  leer_ficha <- function() {
    list(
      Wf = input$Wf, H0 = input$H0, S = input$S,
      theta_r = input$theta_r,  kappa_r = input$kappa_r,  sigma_r = input$sigma_r,
      theta_pi= input$theta_pi, kappa_pi= input$kappa_pi, sigma_pi= input$sigma_pi,
      theta_H = input$theta_H,  kappa_H = input$kappa_H,  sigma_H = input$sigma_H,
      rho_r_pi= input$rho_r_pi, rho_r_H = input$rho_r_H, rho_pi_H = input$rho_pi_H,
      r0 = input$r0, pi0 = input$pi0,
      factor_mort = input$factor_mort,
      alpha_garantia = input$alpha_garantia,
      Delta_TS = input$Delta_TS,
      reforma_pts = input$reforma_pts, reforma_plazo = input$reforma_plazo,
      var_HI = input$var_HI,
      N_sim = input$N_sim, N_sim_traj = input$N_sim_traj, semilla = input$semilla
    )
  }

  resultado <- eventReactive(input$simular, {
    ft <- leer_ficha()
    withProgress(message = "Simulando…", value = 0, {
      simular_individuo(ft, "data/qx_mensual_unisex.rds",
                        progress = function(f, m) setProgress(value = f, detail = m))
    })
  }, ignoreNULL = FALSE)

  output$cards <- renderUI({
    res <- resultado(); m <- res$metricas
    fluidRow(
      column(3, card("TSR vitalicia (mediana)", sprintf("%.2f", m$tsr_vida_med),
                     sprintf("P10 = %.2f", m$tsr_vida_p10),
                     color = if (m$tsr_vida_med >= 0.75) "#2C5F2D" else "#C8102E")),
      column(3, card("P(shortfall)", sprintf("%.1f %%", m$p_shortfall*100),
                     "algún mes con déficit", color = "#C8102E")),
      column(3, card("Ahorro residual (med.)", fmt_eur(m$ahorro_final_med),
                     "al fallecer", color = "#1C7293")),
      column(3, card("Herencia potencial (med.)", fmt_eur(m$herencia_total_med),
                     sprintf("E[vida] %.1f años", m$edad_media), color = "#21295C")),
      column(3, card("Renta vitalicia Rf", paste0(fmt_eur(res$Rf), "/mes"),
                     "de tu patrimonio financiero", color = "#1C7293")),
      column(3, card("Renta hipoteca inversa", paste0(fmt_eur(res$R_HI), "/mes"),
                     sprintf("ρ_HI = %.5f", res$rho_HI), color = "#1C7293")),
      column(3, card("Gap de la reforma", paste0(fmt_eur(res$R_gap_final), "/mes"),
                     "a cubrir a plena reforma", color = "#6B7280")),
      column(3, card("Esperanza de vida", sprintf("%.1f años", res$e65/12 + 65),
                     "desde los 65", color = "#6B7280"))
    )
  })

  output$p_tsr_fan    <- renderPlot(plot_tsr_fan(resultado()))
  output$p_patrimonio <- renderPlot(plot_patrimonio_fan(resultado()))
  output$p_tsr_dist   <- renderPlot(plot_tsr_dist(resultado()))
  output$p_herencia   <- renderPlot(plot_herencia(resultado()))

  output$ficha <- renderTable({
    res <- resultado(); ft <- res$ft
    data.frame(
      Parámetro = c("Patrimonio financiero (Wf)", "Vivienda neta (H0)", "Pensión base (S)",
                    "θ r / κ r / σ r", "θ π / κ π / σ π", "θ H / κ H / σ H",
                    "ρ(r,π) / ρ(r,H) / ρ(π,H)", "r₀ / π₀",
                    "Factor mortalidad", "Garantía RV (α)", "Δ_TS objetivo",
                    "Reforma", "VaR calibración HI",
                    "N escenarios / N trayectorias / semilla"),
      Valor = c(
        fmt_eur(ft$Wf), fmt_eur(ft$H0), fmt_eur(ft$S),
        sprintf("%.4f / %.3f / %.4f", ft$theta_r, ft$kappa_r, ft$sigma_r),
        sprintf("%.4f / %.3f / %.4f", ft$theta_pi, ft$kappa_pi, ft$sigma_pi),
        sprintf("%.4f / %.3f / %.4f", ft$theta_H, ft$kappa_H, ft$sigma_H),
        sprintf("%.3f / %.3f / %.3f", ft$rho_r_pi, ft$rho_r_H, ft$rho_pi_H),
        sprintf("%.4f / %.4f", ft$r0, ft$pi0),
        sprintf("× %.2f", ft$factor_mort),
        sprintf("%.2f", ft$alpha_garantia),
        sprintf("%.2f", ft$Delta_TS),
        sprintf("%.2f (caída) en %.0f años", ft$reforma_pts, ft$reforma_plazo),
        sprintf("%.2f", ft$var_HI),
        sprintf("%s / %s / %s", format(ft$N_sim, big.mark=".", decimal.mark=","),
                format(ft$N_sim_traj, big.mark=".", decimal.mark=","), ft$semilla))
    )
  }, striped = TRUE, width = "100%")

  output$ejemplos <- renderTable({
    tabla_ejemplo_parametros()
  }, striped = TRUE, width = "100%", digits = 4, na = "")
}

shinyApp(ui, server)
