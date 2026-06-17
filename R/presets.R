.comunes <- list(
  kappa_r = 0.276,  kappa_pi = 5.002, kappa_H = 1.708,
  sigma_r = 0.0108, sigma_pi = 0.0939, sigma_H = 0.1349,
  rho_r_pi = 0.310, rho_r_H = 0.152, rho_pi_H = 0.251,
  alpha_garantia = 1.00,
  Delta_TS       = 0.00,
  reforma_pts    = 0.20,
  reforma_plazo  = 25,
  var_HI         = 0.95,
  N_sim      = 3000,
  N_sim_traj = 1000,
  semilla    = 2024
)

.escenarios <- list(
  Mejor   = list(theta_r = 0.0350, theta_pi = 0.0000, theta_H = 0.0500, factor_mort = 1.30),
  Central = list(theta_r = 0.0096, theta_pi = 0.0214, theta_H = 0.0101, factor_mort = 1.00),
  Peor    = list(theta_r = 0.0000, theta_pi = 0.0500, theta_H = 0.0000, factor_mort = 0.70)
)

.individuo_default <- list(
  Wf = 15000,
  H0 = 120000,
  S  = 22000
)

ficha_preset <- function(escenario = "Central", individuo = .individuo_default) {
  e  <- .escenarios[[escenario]]
  ft <- c(individuo, .comunes, e)
  ft$r0  <- e$theta_r
  ft$pi0 <- e$theta_pi
  ft$escenario <- escenario
  ft
}

ficha_default <- function() ficha_preset("Central")

tabla_ejemplo_parametros <- function() {
  fmt_plain <- function(v) vapply(v, function(x)
    format(x, scientific = FALSE, trim = TRUE), character(1))

  cen <- c(.individuo_default$Wf, .individuo_default$H0, .individuo_default$S,
           .escenarios$Central$theta_r, .comunes$kappa_r, .comunes$sigma_r,
           .escenarios$Central$theta_pi, .comunes$kappa_pi, .comunes$sigma_pi,
           .escenarios$Central$theta_H, .comunes$kappa_H, .comunes$sigma_H,
           .comunes$rho_r_pi, .comunes$rho_r_H, .comunes$rho_pi_H,
           .escenarios$Central$theta_r, .escenarios$Central$theta_pi,
           .escenarios$Central$factor_mort, .comunes$alpha_garantia, .comunes$Delta_TS,
           .comunes$reforma_pts, .comunes$reforma_plazo, .comunes$var_HI,
           .comunes$N_sim, .comunes$N_sim_traj, .comunes$semilla)
  fav <- cen; adv <- cen
  fav[c(4,7,10,16,17,18)] <- c(.escenarios$Mejor$theta_r, .escenarios$Mejor$theta_pi,
           .escenarios$Mejor$theta_H, .escenarios$Mejor$theta_r,
           .escenarios$Mejor$theta_pi, .escenarios$Mejor$factor_mort)
  adv[c(4,7,10,16,17,18)] <- c(.escenarios$Peor$theta_r, .escenarios$Peor$theta_pi,
           .escenarios$Peor$theta_H, .escenarios$Peor$theta_r,
           .escenarios$Peor$theta_pi, .escenarios$Peor$factor_mort)

  data.frame(
    Parámetro   = c("Patrimonio financiero (Wf)", "Valor neto de la vivienda (H0)",
                    "Pensión / salario base (S)",
                    "θ_r — tipo de interés (media LP)", "κ_r — reversión", "σ_r — volatilidad",
                    "θ_π — inflación (media LP)", "κ_π — reversión", "σ_π — volatilidad",
                    "θ_H — revaloriz. inmob. (media LP)", "κ_H — reversión", "σ_H — volatilidad",
                    "ρ(r,π) — correlación", "ρ(r,H) — correlación", "ρ(π,H) — correlación",
                    "r₀ — tipo inicial", "π₀ — inflación inicial",
                    "Factor de mortalidad", "Garantía RV (α)", "Δ_TS — objetivo adicional",
                    "Reforma (caída tasa sustitución)", "Plazo de la reforma",
                    "VaR calibración HI", "N escenarios", "N trayectorias", "Semilla"),
    Unidad      = c("€", "€", "€/año",
                    "fracción (0,0096 = 0,96 %)", "—", "fracción",
                    "fracción", "—", "fracción",
                    "fracción", "—", "fracción",
                    "−1 a 1", "−1 a 1", "−1 a 1",
                    "fracción", "fracción",
                    "× (1 = PER2020)", "fracción (1 = 100 %)", "fracción",
                    "fracción (0,20 = −20 pp)", "años",
                    "fracción (0,95 = 95 %)", "entero", "entero", "entero"),
    Central         = fmt_plain(cen),
    "Ej. favorable" = fmt_plain(fav),
    "Ej. adverso"   = fmt_plain(adv),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}
