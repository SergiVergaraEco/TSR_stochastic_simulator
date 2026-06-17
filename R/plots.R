library(ggplot2)
library(scales)

ACENTO  <- "#1C7293"
ACENTO2 <- "#21295C"
VERDE   <- "#4DAA57"
ROJO    <- "#C8102E"

tema_app <- theme_minimal(base_size = 13) + theme(
  plot.title       = element_text(face = "bold", size = 14, hjust = 0),
  plot.subtitle    = element_text(size = 10.5, color = "grey40", hjust = 0),
  plot.caption     = element_text(size = 8, color = "grey55", hjust = 0),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color = "grey92", linewidth = 0.35),
  legend.position  = "bottom",
  plot.background  = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA)
)

extraer_fan <- function(traj, metrica, ancla0 = NULL) {
  arr <- traj[[metrica]]
  df  <- data.frame(
    t_anios = traj$t_anios,
    p10 = arr[, 1, 1], p25 = arr[, 1, 2], med = arr[, 1, 3],
    p75 = arr[, 1, 4], p90 = arr[, 1, 5]
  )
  df <- df[stats::complete.cases(df), ]
  if (!is.null(ancla0))
    df <- rbind(data.frame(t_anios = 0, p10 = ancla0, p25 = ancla0,
                           med = ancla0, p75 = ancla0, p90 = ancla0), df)
  df
}

plot_tsr_fan <- function(res) {
  df <- extraer_fan(res$traj, "tsr_acum", ancla0 = 1)
  ggplot(df, aes(t_anios)) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.75, ymax = 1.02,
             fill = "#f0f7ee", alpha = 0.6) +
    geom_hline(yintercept = 1,    linetype = "dashed", color = "grey50", linewidth = 0.4) +
    geom_hline(yintercept = 0.75, linetype = "dotted", color = "grey50", linewidth = 0.4) +
    geom_ribbon(aes(ymin = p10, ymax = p90), fill = ACENTO, alpha = 0.13) +
    geom_ribbon(aes(ymin = p25, ymax = p75), fill = ACENTO, alpha = 0.28) +
    geom_line(aes(y = med), color = ACENTO2, linewidth = 1.1) +
    scale_x_continuous(breaks = seq(0, 30, 5), labels = paste0(seq(0, 30, 5), " a")) +
    scale_y_continuous(labels = label_percent(1), limits = c(0, 1.02)) +
    labs(title    = "Cobertura del objetivo de consumo a lo largo de la jubilación",
         subtitle = "TSR acumulada hasta el año t. Banda: p10–p90; línea: mediana. Zona verde: TSR ≥ 75 %.",
         x = "Años desde la jubilación", y = "TSR acumulada (consumo / objetivo)") +
    tema_app
}

plot_patrimonio_fan <- function(res) {
  df <- extraer_fan(res$traj, "patrimonio")
  ggplot(df, aes(t_anios)) +
    geom_ribbon(aes(ymin = p10, ymax = p90), fill = VERDE, alpha = 0.13) +
    geom_ribbon(aes(ymin = p25, ymax = p75), fill = VERDE, alpha = 0.28) +
    geom_line(aes(y = med), color = "#2C5F2D", linewidth = 1.1) +
    scale_x_continuous(breaks = seq(0, 30, 5), labels = paste0(seq(0, 30, 5), " a")) +
    scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",",
                                             suffix = " €"), limits = c(0, NA)) +
    labs(title    = "Patrimonio neto a lo largo de la jubilación",
         subtitle = "Ahorro financiero + excedente inmobiliario. Banda: p10–p90; línea: mediana.",
         x = "Años desde la jubilación", y = "Patrimonio neto") +
    tema_app
}

plot_tsr_dist <- function(res) {
  v   <- pmin(res$sim$tsr_vida, 1.02)
  med <- median(res$sim$tsr_vida)
  df  <- data.frame(tsr = v)
  ggplot(df, aes(tsr)) +
    annotate("rect", xmin = 0.75, xmax = 1.02, ymin = -Inf, ymax = Inf,
             fill = "#f0f7ee", alpha = 0.6) +
    geom_histogram(aes(y = after_stat(density)), bins = 40,
                   fill = ACENTO, color = "white", alpha = 0.85, linewidth = 0.2) +
    geom_vline(xintercept = med, color = ROJO, linewidth = 0.9, linetype = "dashed") +
    annotate("text", x = med, y = Inf, vjust = 1.6, hjust = -0.05,
             label = sprintf("mediana %.2f", med), color = ROJO, size = 3.6) +
    scale_x_continuous(labels = label_percent(1)) +
    coord_cartesian(xlim = c(0, 1.03)) +
    labs(title    = "Distribución de la TSR vitalicia",
         subtitle = sprintf("%s escenarios Monte Carlo. Zona verde: TSR ≥ 75 %%.",
                            format(length(res$sim$tsr_vida), big.mark = ".", decimal.mark = ",")),
         x = "TSR vitalicia (consumo / objetivo, acumulado de por vida)",
         y = "Densidad") +
    tema_app
}

plot_herencia <- function(res) {
  comp <- c(
    "Ahorro residual"        = median(res$sim$ahorro_final),
    "Garantía RV"            = median(res$sim$cap_garantia),
    "Excedente inmobiliario" = median(res$sim$excedente_inmob)
  )
  df <- data.frame(componente = factor(names(comp), levels = names(comp)),
                   valor = as.numeric(comp))
  df$etiqueta <- format(round(df$valor), big.mark = ".", decimal.mark = ",")
  ggplot(df, aes(componente, valor, fill = componente)) +
    geom_col(width = 0.62, alpha = 0.9) +
    geom_text(aes(label = paste0(etiqueta, " €")), vjust = -0.5, size = 3.8) +
    scale_fill_manual(values = c(VERDE, ACENTO, ACENTO2), guide = "none") +
    scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",",
                                             suffix = " €"),
                       expand = expansion(mult = c(0, 0.15))) +
    labs(title    = "Herencia potencial: composición (mediana)",
         subtitle = "Lo que queda al fallecer, por origen.",
         x = NULL, y = "Importe mediano") +
    tema_app
}
