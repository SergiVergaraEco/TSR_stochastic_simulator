construir_params <- function(ft) {
  list(
    r0       = ft$r0,        pi0      = ft$pi0,
    kappa_r  = ft$kappa_r,   theta_r  = ft$theta_r,   sigma_r  = ft$sigma_r,
    kappa_pi = ft$kappa_pi,  theta_pi = ft$theta_pi,  sigma_pi = ft$sigma_pi,
    kappa_H  = ft$kappa_H,   theta_H  = ft$theta_H,   sigma_H  = ft$sigma_H,
    rho_r_pi = ft$rho_r_pi,
    rho_r_g  = ft$rho_r_H,
    rho_pi_g = ft$rho_pi_H,
    alpha    = ft$alpha_garantia,
    delta_t  = 1/12,
    Delta_TS = ft$Delta_TS,
    reforma_pts   = ft$reforma_pts,
    reforma_plazo = ft$reforma_plazo
  )
}

simular_individuo <- function(ft, ruta_qx = "data/qx_mensual_unisex.rds",
                              progress = NULL) {
  tic <- function(f, m) if (!is.null(progress)) progress(f, m)

  tic(0.05, "Tablas biométricas…")
  qx_mensual <- readRDS(ruta_qx)
  bio        <- construir_tpx(qx_mensual, ft$factor_mort)
  tpx        <- bio$tpx
  e65        <- bio$e65
  tpx_e65    <- bio$tpx_e65

  params <- construir_params(ft)

  poblacion <- data.frame(
    Wf          = ft$Wf,
    H0          = ft$H0,
    S           = ft$S,
    facine3     = 1,
    R_target_0  = ft$S * params$Delta_TS / 12
  )

  set.seed(ft$semilla)
  tic(0.15, sprintf("Generando %d escenarios macro…", ft$N_sim))
  macro <- simular_macro(length(tpx), ft$N_sim, params)
  taus  <- simular_muertes(ft$N_sim, tpx)

  tic(0.45, "Calibrando hipoteca inversa (VaR)…")
  resultado_HI   <- calibrar_HI(macro, taus, params, tpx, alpha = ft$var_HI)
  rho_HI         <- resultado_HI$rho_HI
  poblacion$R_HI <- rho_HI * poblacion$H0

  tic(0.65, "Simulación principal…")
  sim_base <- simular_jubilados(poblacion, ft$N_sim, macro, params, tpx, tpx_e65)
  metricas <- calcular_metricas(poblacion, sim_base)

  tic(0.85, sprintf("Trayectorias (%d esc.)…", ft$N_sim_traj))
  set.seed(ft$semilla + 1)
  macro_traj <- simular_macro(length(tpx), ft$N_sim_traj, params)
  traj <- simular_trayectorias(poblacion, factor("Q1", levels = c("Q1","Q2","Q3","Q4")),
                               ft$N_sim_traj, macro_traj, params, tpx, tpx_e65)

  tic(1.0, "Listo")
  list(
    metricas   = metricas,
    sim        = sim_base[[1]],
    traj       = traj,
    params     = params,
    tpx        = tpx,
    e65        = e65,
    tpx_e65    = tpx_e65,
    rho_HI     = rho_HI,
    R_HI       = poblacion$R_HI,
    Rf         = metricas$Rf,
    R_target_0 = poblacion$R_target_0,
    R_gap_final= ft$S * ft$reforma_pts / 12,
    individuo  = poblacion,
    ft         = ft
  )
}
