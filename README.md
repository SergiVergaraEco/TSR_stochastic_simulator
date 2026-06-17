# Simulador estocástico de desacumulación en la jubilación

Aplicación **Shiny** que ejecuta un modelo de *«Heterogeneidad patrimonial y
suficiencia del consumo ante la reducción de la tasa de sustitución de las
pensiones en España»* para **un individuo** cuyos datos se introducen manualmente.

La estrategia combina tres palancas: **renta vitalicia** (del patrimonio
financiero), **hipoteca inversa** (de la vivienda) y un **colchón de ahorro**.
La app simula miles de trayectorias Monte Carlo (Vasicek correlacionado para
tipo de interés, inflación y revalorización inmobiliaria + mortalidad PER2020) y
mide la **TSR vitalicia** = consumo sostenido / objetivo de consumo.

**Toda la ficha técnica de la simulación es editable**. Todas las variables se introducen en su
**unidad natural** (fracciones, no porcentajes): un tipo de interés del 0,96 % se
escribe `0.0096`. La pestaña **«Ejemplo de parámetros»** documenta cada variable
con un valor de referencia.

## Cómo ejecutarla

Necesitas **R (≥ 4.2)** y los paquetes `shiny`, `ggplot2` y `scales`:

```r
install.packages(c("shiny", "ggplot2", "scales"))
```

### Directamente desde GitHub

```r
shiny::runGitHub("SergiVergaraEco/TSR_stochastic_simulator")
```

## Qué muestra

- **Indicadores**: TSR vitalicia mediana y P10, P(shortfall), ahorro residual,
  herencia potencial, renta vitalicia `Rf`, renta de la hipoteca inversa `R_HI`,
  gap de la reforma y esperanza de vida.
- **Cobertura (TSR) en el tiempo** — abanico p10–p90 de la TSR acumulada.
- **Patrimonio neto** — abanico del ahorro financiero + excedente inmobiliario.
- **Distribución de la TSR vitalicia** — histograma sobre las N simulaciones.
- **Herencia** — descomposición: ahorro residual + garantía RV + excedente inmobiliario.
- **Ficha técnica** — resumen de todos los parámetros usados en la simulación.
- **Ejemplo de parámetros** — guía de referencia con la unidad y un valor de
  ejemplo de cada variable.

## Estructura

```
app.R                    Main: UI + server (hace source() de los módulos)
R/funciones_modelo.R     Núcleo del modelo (idéntico al del TFM)
R/simular_individuo.R    Orquesta el pipeline para una poblacion de 1 fila
R/presets.R              Ficha técnica por defecto + tabla de ejemplo de parámetros
R/plots.R                Gráficos para un individuo
data/qx_mensual_unisex.rds   Tabla de mortalidad mensual unisex (PER2020)
```

## Notas del modelo

- La vivienda `H0` se interpreta como **valor neto** (equity), coherente con el TFM.
- La renta de la hipoteca inversa se calibra **endógenamente** por VaR en cada
  escenario macro (a más volatilidad/peor entorno, renta más conservadora).
- `Δ_TS` es el objetivo adicional de consumo; la **reforma** (−pp en N años)
  define el gap que la estrategia debe cubrir.
- Reproducible vía semilla.

## Licencia

Distribuido bajo licencia **MIT** (ver [`LICENSE`](LICENSE)). Si reutilizas el
código, cita el Trabajo Fin de Máster del que procede.

---
*Elaborado a partir del código de simulación del TFM. Máster en Ciencias
Actuariales y Financieras, Universitat de Barcelona.*
