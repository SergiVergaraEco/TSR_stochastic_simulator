fmt <- function(x) format(round(x), big.mark = ".", decimal.mark = ",")

wq <- function(x, w, p) {
  ord   <- order(x)
  x_o   <- x[ord]; w_o <- w[ord] / sum(w)
  w_cum <- cumsum(w_o) - w_o / 2
  sapply(p, function(pi) {
    if (pi <= w_cum[1])               return(x_o[1])
    if (pi >= w_cum[length(w_cum)])   return(x_o[length(x_o)])
    lo <- max(which(w_cum <= pi)); hi <- min(which(w_cum >= pi))
    if (lo == hi) return(x_o[lo])
    t <- (pi - w_cum[lo]) / (w_cum[hi] - w_cum[lo])
    x_o[lo] * (1 - t) + x_o[hi] * t
  })
}

acum_trimestral <- function(ret_m, n_q) {
  sapply(1:n_q, function(q) {
    mes <- ((q-1)*3 + 1) : (q*3)
    prod(1 + ret_m[mes]) - 1
  })
}

fit_vasicek <- function(x, dt = 0.25) {
  reg   <- lm(x[-1] ~ x[-length(x)])
  b     <- unname(coef(reg)[2])
  a     <- unname(coef(reg)[1])
  list(kappa = (1 - b) / dt,
       theta = a / (1 - b),
       sigma = sd(residuals(reg)) / sqrt(dt),
       resid = residuals(reg))
}

annuity_factor <- function(theta, k, tpx, delta_t = 1/12) {
  s <- seq_len(length(tpx) - k)
  if (length(s) == 0) return(0)
  sum((tpx[k + s] / tpx[k]) * (1 / (1 + theta * delta_t))^s)
}

A_garantia <- function(theta, k, tpx, tpx_e65, alpha = 1, delta_t = 1/12) {
  s <- seq_len(length(tpx) - k - 1)
  if (length(s) == 0) return(0)
  p_surv <- tpx[k + s] / tpx[k]
  p_die  <- 1 - tpx[k + s + 1] / tpx[k + s]
  G      <- alpha * pmax((tpx[k + s] - tpx_e65) / (1 - tpx_e65), 0)
  disc   <- (1 / (1 + theta * delta_t))^s
  sum(p_surv * p_die * G * disc)
}

construir_tpx <- function(qx_mensual, factor_mort = 1) {
  qx  <- pmin(qx_mensual * factor_mort, 1)
  tpx <- cumprod(c(1, 1 - qx))
  e65 <- min(round(sum(tpx[-1])), length(tpx))
  list(tpx = tpx, tpx_e65 = tpx[e65], e65 = e65)
}

simular_muertes <- function(N_sim, tpx) {
  pmin(findInterval(runif(N_sim), 1 - tpx) + 1L, length(tpx))
}

simular_macro <- function(T_max, N_sim, params) {
  dt  <- params$delta_t
  sdt <- sqrt(dt)

  L <- t(chol(matrix(c(
    1,               params$rho_r_pi, params$rho_r_g,
    params$rho_r_pi, 1,               params$rho_pi_g,
    params$rho_r_g,  params$rho_pi_g, 1
  ), 3, 3)))

  eps_r  <- matrix(0, T_max, N_sim)
  eps_pi <- matrix(0, T_max, N_sim)
  eps_H  <- matrix(0, T_max, N_sim)
  for (ii in seq_len(T_max)) {
    zz           <- L %*% matrix(rnorm(3L * N_sim), 3L, N_sim)
    eps_r[ii, ]  <- zz[1L, ]
    eps_pi[ii, ] <- zz[2L, ]
    eps_H[ii, ]  <- zz[3L, ]
  }

  r_m      <- matrix(0, T_max, N_sim)
  r_m[1, ] <- params$r0
  for (ii in 2:T_max) {
    r_m[ii, ] <- pmax(
      r_m[ii-1, ] + params$kappa_r * (params$theta_r - r_m[ii-1, ]) * dt
                  + params$sigma_r * sdt * eps_r[ii, ],
      -0.01)
  }

  pi_m      <- matrix(0, T_max, N_sim)
  pi_m[1, ] <- params$pi0
  for (ii in 2:T_max) {
    pi_m[ii, ] <- pmax(
      pi_m[ii-1, ] + params$kappa_pi * (params$theta_pi - pi_m[ii-1, ]) * dt
                   + params$sigma_pi * sdt * eps_pi[ii, ],
      -0.005)
  }

  b_H       <- 1 - params$kappa_H * dt
  a_H       <- params$kappa_H * params$theta_H * dt
  lH_m      <- matrix(0, T_max, N_sim)
  lH_m[1, ] <- params$theta_H
  for (ii in 2:T_max) {
    lH_m[ii, ] <- pmax(
      b_H * lH_m[ii-1, ] + a_H + params$sigma_H * sdt * eps_H[ii, ],
      -0.99)
  }

  list(r     = (1 + r_m)^(1/12)  - 1,
       pi    = (1 + pi_m)^(1/12) - 1,
       H_ret = (1 + lH_m)^(1/12) - 1)
}

simular_jubilados <- function(poblacion, N_sim, macro, params, tpx, tpx_e65) {
  N_pop  <- nrow(poblacion)
  T_max  <- length(tpx)
  a_0    <- annuity_factor(params$theta_r, 1, tpx)
  A_0    <- A_garantia(params$theta_r, 1, tpx, tpx_e65, params$alpha)
  Rf_vec <- poblacion$Wf * max(1 - params$alpha * A_0, 0.01) / a_0

  tau_muerte <- matrix(0L, N_pop, N_sim)
  for (i in 1:N_pop) tau_muerte[i, ] <- simular_muertes(N_sim, tpx)

  ingreso_m       <- matrix(Rf_vec + poblacion$R_HI, N_pop, N_sim)
  R_tgt           <- matrix(poblacion$R_target_0,    N_pop, N_sim)
  ahorro          <- matrix(0,    N_pop, N_sim)
  alive           <- matrix(TRUE, N_pop, N_sim)
  meses_shortfall <- matrix(0L,   N_pop, N_sim)
  shortfall_acum  <- matrix(0,    N_pop, N_sim)
  consumo_acum    <- matrix(0,    N_pop, N_sim)
  target_acum     <- matrix(0,    N_pop, N_sim)
  R_HI_m          <- matrix(poblacion$R_HI, N_pop, N_sim)
  D               <- matrix(0,    N_pop, N_sim)
  D_at_death      <- matrix(0,    N_pop, N_sim)
  P_t             <- rep(1, N_sim)
  P_at_death      <- matrix(1,    N_pop, N_sim)
  escen_col       <- col(ahorro)
  ind_fila        <- row(ahorro)

  reforma_activa <- isTRUE(params$reforma_pts > 0) && isTRUE(params$reforma_plazo > 0)
  if (reforma_activa) {
    T_reforma_m <- round(params$reforma_plazo * 12)
    corte_base  <- poblacion$S * params$reforma_pts / 12
    CPI_vec_r   <- rep(1, N_sim)
  }

  for (mes in 1:T_max) {
    if (!any(alive)) break
    r_t  <- macro$r[mes, ]
    pi_t <- macro$pi[mes, ]
    P_t  <- P_t * (1 + macro$H_ret[mes, ])
    R_tgt <- sweep(R_tgt, 2, 1 + pi_t, "*")
    if (reforma_activa) CPI_vec_r <- CPI_vec_r * (1 + pi_t)

    vivos <- which(alive)
    if (length(vivos) > 0) {
      r_v <- r_t[escen_col[vivos]]
      if (reforma_activa) {
        factor_r    <- min(mes / T_reforma_m, 1)
        corte_v     <- corte_base[ind_fila[vivos]] * factor_r * CPI_vec_r[escen_col[vivos]]
        R_tgt_eff_v <- R_tgt[vivos] + corte_v
      } else {
        R_tgt_eff_v <- R_tgt[vivos]
      }
      ahorro_nuevo <- ahorro[vivos] * (1 + r_v) + ingreso_m[vivos] - R_tgt_eff_v
      deficit      <- pmax(-ahorro_nuevo, 0)
      ahorro[vivos]           <- pmax(ahorro_nuevo, 0)
      meses_shortfall[vivos]  <- meses_shortfall[vivos] + (deficit > 0)
      shortfall_acum[vivos]   <- shortfall_acum[vivos]  + deficit
      consumo_acum[vivos]     <- consumo_acum[vivos]    + R_tgt_eff_v - deficit
      target_acum[vivos]      <- target_acum[vivos]     + R_tgt_eff_v
      D[vivos] <- (D[vivos] + R_HI_m[vivos]) * (1 + r_v)
    }
    mueren <- alive & (tau_muerte == mes)
    if (any(mueren)) {
      idx <- which(mueren)
      D_at_death[idx] <- D[idx]
      P_at_death[idx] <- P_t[escen_col[idx]]
    }
    alive[mueren] <- FALSE
  }

  G_final         <- params$alpha * pmax((tpx[tau_muerte] - tpx_e65) / (1 - tpx_e65), 0)
  cap_garantia    <- G_final * matrix(poblacion$Wf, N_pop, N_sim)
  excedente_inmob <- pmax(matrix(poblacion$H0, N_pop, N_sim) * P_at_death - D_at_death, 0)
  herencia_total  <- ahorro + cap_garantia + excedente_inmob

  lapply(1:N_pop, function(i) list(
    tau             = tau_muerte[i, ],
    Rf              = Rf_vec[i],
    ahorro_final    = ahorro[i, ],
    meses_shortfall = meses_shortfall[i, ],
    shortfall_acum  = shortfall_acum[i, ],
    tsr_vida        = consumo_acum[i, ] / pmax(target_acum[i, ], 1),
    cap_garantia    = cap_garantia[i, ],
    excedente_inmob = excedente_inmob[i, ],
    herencia_total  = herencia_total[i, ]
  ))
}

calibrar_HI <- function(macro, taus, params, tpx, alpha = 0.95) {
  N <- length(taus)
  perdidas_rho <- function(rho) {
    sapply(1:N, function(i) {
      r_v   <- macro$r[1:taus[i], i]
      H_tau <- prod(1 + macro$H_ret[1:taus[i], i])
      D     <- 0
      for (s in seq_along(r_v)) D <- (D + rho) * (1 + r_v[s])
      max(D - H_tau, 0) * prod(1 / (1 + r_v))
    })
  }
  f_obj <- function(rho) mean(perdidas_rho(rho) > 0) - (1 - alpha)
  lo <- 1e-5; hi <- 0.001
  while (f_obj(hi) < 0 && hi < 0.10) hi <- hi * 2
  cat(sprintf("  f(%.5f)=%.4f  f(%.5f)=%.4f\n", lo, f_obj(lo), hi, f_obj(hi)))
  tol <- 1e-6
  while ((hi - lo) > tol) {
    mid <- (lo + hi) / 2
    if (f_obj(mid) < 0) lo <- mid else hi <- mid
  }
  rho_HI <- (lo + hi) / 2
  perd   <- perdidas_rho(rho_HI)
  tvar   <- if (any(perd > 0)) mean(perd[perd > 0]) else 0
  cat(sprintf("  rho_HI=%.6f  P(perd)=%.1f%%  TVaR=%.4f\n",
              rho_HI, mean(perd > 0) * 100, tvar))
  cat(sprintf("  Renta para vivienda de 250.000 EUR: %.0f EUR/mes\n", rho_HI * 250000))
  list(rho_HI = rho_HI, perdidas = perd,
       var    = as.numeric(quantile(perd, alpha)), tvar = tvar)
}

calcular_metricas <- function(poblacion, resultados) {
  N        <- nrow(poblacion)
  umbral85 <- (85 - 65) * 12
  cada <- function(fn) sapply(resultados, fn)
  Rf_vec <- cada(function(r) r$Rf)
  Wr_col <- if ("Wr" %in% names(poblacion)) poblacion$Wr else rep(0, N)
  data.frame(
    Wf              = poblacion$Wf,
    H0              = poblacion$H0,
    Wr              = Wr_col,
    S               = poblacion$S,
    R_target_0      = poblacion$R_target_0,
    R_HI            = poblacion$R_HI,
    Rf              = Rf_vec,
    facine3         = if ("facine3" %in% names(poblacion)) poblacion$facine3 else rep(1, N),
    patrimonio      = (poblacion$Wf - Wr_col) + poblacion$H0,
    tsr_0           = ifelse(poblacion$R_target_0 > 0, (Rf_vec + poblacion$R_HI) / poblacion$R_target_0, NA_real_),
    tsr_vida_med    = cada(function(r) median(r$tsr_vida)),
    tsr_vida_p10    = cada(function(r) quantile(r$tsr_vida, 0.10)),
    tsr_vida_p25    = cada(function(r) quantile(r$tsr_vida, 0.25)),
    p_shortfall     = cada(function(r) mean(r$meses_shortfall > 0)),
    p_shortfall_85  = cada(function(r) mean(r$meses_shortfall > 0 & r$tau <= umbral85)),
    meses_short_med = cada(function(r) median(r$meses_shortfall)),
    fracc_short_med = cada(function(r) median(r$meses_shortfall / pmax(r$tau, 1))),
    shortfall_med   = cada(function(r) median(r$shortfall_acum)),
    ahorro_final_med    = cada(function(r) median(r$ahorro_final)),
    ahorro_final_p90    = cada(function(r) quantile(r$ahorro_final, 0.90)),
    cap_garantia_med    = cada(function(r) median(r$cap_garantia)),
    excedente_inmob_med = cada(function(r) median(r$excedente_inmob)),
    herencia_total_med  = cada(function(r) median(r$herencia_total)),
    herencia_total_p25  = cada(function(r) quantile(r$herencia_total, 0.25)),
    herencia_total_p75  = cada(function(r) quantile(r$herencia_total, 0.75)),
    edad_media      = cada(function(r) mean(r$tau) / 12 + 65),
    edad_p25        = cada(function(r) quantile(r$tau, 0.25) / 12 + 65),
    edad_p75        = cada(function(r) quantile(r$tau, 0.75) / 12 + 65),
    p_vive90        = cada(function(r) mean(r$tau > (90 - 65) * 12))
  )
}

asignar_cuartil <- function(met) {
  q <- wq(met$patrimonio, met$facine3, c(0.25, 0.50, 0.75))
  met$cuartil <- cut(met$patrimonio,
                     breaks = c(-Inf, q, Inf),
                     labels = c("Q1","Q2","Q3","Q4"))
  met
}

imprimir_resultados <- function(met, etiqueta = "BASE") {
  met <- asignar_cuartil(met)
  Q   <- c("Q1","Q2","Q3","Q4")
  wm <- function(x, s = rep(TRUE, nrow(met))) weighted.mean(x[s], met$facine3[s])
  cat(sprintf("\n====================================\n  RESULTADOS - %s\n====================================\n", etiqueta))
  cat(sprintf("  Individuos:                  %d  (repr. %.0f hogares)\n",
              nrow(met), sum(met$facine3)))
  cat(sprintf("  TSR vitalicia mediana pond.: %.3f\n",      wm(met$tsr_vida_med)))
  cat(sprintf("  TSR vitalicia P10 pond.:     %.3f\n",      wm(met$tsr_vida_p10)))
  cat(sprintf("  P(algun mes con shortfall):  %.1f%%\n",    wm(met$p_shortfall)*100))
  cat(sprintf("  %% vida con shortfall:       %.1f%%\n",    wm(met$fracc_short_med)*100))
  cat(sprintf("  Shortfall acum. mediano:     %s EUR\n",    fmt(wm(met$shortfall_med))))
  cat(sprintf("  Ahorro residual mediano:     %s EUR\n",    fmt(wm(met$ahorro_final_med))))
  cat(sprintf("  Edad media al fallecer:      %.1f anos\n", wm(met$edad_media)))
  cat("====================================\n")
  cat(sprintf("  %-4s  %9s  %10s  %10s  %13s\n",
              "Q","TSR.vida","P(short)","Meses.sh","Short.acum"))
  for (q in Q) {
    s <- met$cuartil == q
    cat(sprintf("  %-4s  %8.3f  %9.1f%%  %9.0f  %13s\n",
                q, wm(met$tsr_vida_med, s),
                wm(met$p_shortfall, s)*100, wm(met$meses_short_med, s),
                fmt(wm(met$shortfall_med, s))))
  }
  invisible(met)
}

simular_trayectorias <- function(poblacion, cuartil_vec, N_sim_traj,
                                  macro_traj, params, tpx, tpx_e65,
                                  t_check = sort(unique(c(1:12, seq(6, min(300, length(tpx)), by = 6))))) {
  N_pop   <- nrow(poblacion)
  T_max   <- length(tpx)
  t_check <- t_check[t_check <= T_max]
  n_t     <- length(t_check)

  Rf_vec  <- poblacion$Wf * max(1 - params$alpha *
               A_garantia(params$theta_r, 1, tpx, tpx_e65, params$alpha), 0.01) /
             annuity_factor(params$theta_r, 1, tpx)

  ahorro       <- matrix(0,    N_pop, N_sim_traj)
  consumo_acum <- matrix(0,    N_pop, N_sim_traj)
  target_acum  <- matrix(0,    N_pop, N_sim_traj)
  D_m          <- matrix(0,    N_pop, N_sim_traj)
  R_tgt        <- matrix(poblacion$R_target_0,    N_pop, N_sim_traj)
  ingreso_m    <- matrix(Rf_vec + poblacion$R_HI, N_pop, N_sim_traj)
  R_HI_m       <- matrix(poblacion$R_HI,          N_pop, N_sim_traj)
  alive        <- matrix(TRUE, N_pop, N_sim_traj)
  P_t          <- rep(1, N_sim_traj)

  tau_muerte <- matrix(0L, N_pop, N_sim_traj)
  for (i in 1:N_pop) tau_muerte[i, ] <- simular_muertes(N_sim_traj, tpx)

  escen_col <- col(ahorro)
  ind_fila  <- row(ahorro)
  probs     <- c(.10, .25, .50, .75, .90)

  reforma_activa <- isTRUE(params$reforma_pts > 0) && isTRUE(params$reforma_plazo > 0)
  if (reforma_activa) {
    T_reforma_m <- round(params$reforma_plazo * 12)
    corte_base  <- poblacion$S * params$reforma_pts / 12
    CPI_vec_r   <- rep(1, N_sim_traj)
  }

  out_ahorro     <- array(NA_real_, c(n_t, N_pop, 5))
  out_herencia   <- array(NA_real_, c(n_t, N_pop, 5))
  out_tsr        <- array(NA_real_, c(n_t, N_pop, 5))
  out_equity     <- array(NA_real_, c(n_t, N_pop, 5))
  out_patrimonio <- array(NA_real_, c(n_t, N_pop, 5))

  for (mes in 1:T_max) {
    r_t <- macro_traj$r[mes, ]
    P_t <- P_t * (1 + macro_traj$H_ret[mes, ])
    R_tgt <- sweep(R_tgt, 2, 1 + macro_traj$pi[mes, ], "*")
    if (reforma_activa) CPI_vec_r <- CPI_vec_r * (1 + macro_traj$pi[mes, ])

    vivos <- which(alive)
    if (length(vivos) > 0) {
      r_v <- r_t[escen_col[vivos]]
      if (reforma_activa) {
        factor_r    <- min(mes / T_reforma_m, 1)
        corte_v     <- corte_base[ind_fila[vivos]] * factor_r * CPI_vec_r[escen_col[vivos]]
        R_tgt_eff_v <- R_tgt[vivos] + corte_v
      } else {
        R_tgt_eff_v <- R_tgt[vivos]
      }
      ahorro_nuevo <- ahorro[vivos] * (1 + r_v) + ingreso_m[vivos] - R_tgt_eff_v
      deficit      <- pmax(-ahorro_nuevo, 0)
      ahorro[vivos]       <- pmax(ahorro_nuevo, 0)
      consumo_acum[vivos] <- consumo_acum[vivos] + R_tgt_eff_v - deficit
      target_acum[vivos]  <- target_acum[vivos]  + R_tgt_eff_v
      D_m[vivos] <- (D_m[vivos] + R_HI_m[vivos]) * (1 + r_v)
    }
    alive[alive & tau_muerte == mes] <- FALSE

    if (mes %in% t_check) {
      t_idx <- which(t_check == mes)
      G_t   <- params$alpha * max((tpx[mes] - tpx_e65) / (1 - tpx_e65), 0)
      for (i in 1:N_pop) {
        scen <- tau_muerte[i, ] >= mes
        if (sum(scen) < 5) next
        ah_i       <- ahorro[i, scen]
        equity_i   <- pmax(poblacion$H0[i] * P_t[scen] - D_m[i, scen], 0)
        herencia_i <- ah_i + G_t * poblacion$Wf[i] + equity_i
        tsr_i      <- pmin(ifelse(target_acum[i, scen] > 0, consumo_acum[i, scen] / target_acum[i, scen], 1), 1)
        out_ahorro[t_idx, i, ]     <- quantile(ah_i,            probs)
        out_herencia[t_idx, i, ]   <- quantile(herencia_i,      probs)
        out_tsr[t_idx, i, ]        <- quantile(tsr_i,           probs)
        out_equity[t_idx, i, ]     <- quantile(equity_i,        probs)
        out_patrimonio[t_idx, i, ] <- quantile(ah_i + equity_i, probs)
      }
    }
  }
  list(t_anios    = t_check / 12, t_check = t_check, cuartil = cuartil_vec,
       ahorro     = out_ahorro,   herencia = out_herencia, tsr_acum = out_tsr,
       equity     = out_equity,   patrimonio = out_patrimonio)
}

agregar_trayectoria <- function(traj, metrica) {
  mat  <- traj[[metrica]]
  Q    <- c("Q1","Q2","Q3","Q4")
  n_t  <- dim(mat)[1]
  rows <- lapply(Q, function(q) {
    ind <- which(traj$cuartil == q)
    lapply(seq_len(n_t), function(ti) {
      qs <- apply(mat[ti, ind, , drop = FALSE], 3, mean, na.rm = TRUE)
      data.frame(cuartil = q, t_anios = traj$t_anios[ti],
                 p10 = qs[1], p25 = qs[2], med = qs[3], p75 = qs[4], p90 = qs[5])
    })
  })
  df <- do.call(rbind, do.call(c, rows))
  df$cuartil <- factor(df$cuartil, levels = Q)
  df
}
