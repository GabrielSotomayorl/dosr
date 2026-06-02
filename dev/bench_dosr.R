# =============================================================================
# dev/bench_dosr.R — Benchmark portable dosr
# =============================================================================
# CÓMO USAR:
#   En desarrollo (M1):  devtools::load_all("."); source("dev/bench_dosr.R")
#   En otra máquina:     instalar dosr; source("bench_dosr.R")
#                        (copiar solo este archivo a la máquina destino)
#
# RESULTADO: CSV en dev/bench_out/bench_<maquina_id>.csv listo para comparar.
#
# EDITAR las 3 líneas de abajo antes de correr en cada máquina:
maquina_id <- "m1_dev"       # p.ej. "win_i7_2010_oficina"
cpu_modelo <- "Apple M1"     # modelo del procesador
ram_gb     <- 16             # RAM física en GB
# =============================================================================

if (!requireNamespace("bench",    quietly = TRUE)) stop("instalar: install.packages('bench')")
if (!requireNamespace("srvyr",    quietly = TRUE)) stop("instalar: install.packages('srvyr')")
if (!requireNamespace("dosr",     quietly = TRUE)) stop("instalar: remotes::install_github('GabrielSotomayorl/dosr')")
library(srvyr)

# Cargar dosr: funciona tanto con devtools::load_all() previo como con library()
if (!isNamespaceLoaded("dosr")) library(dosr)

cat("\n=== BENCHMARK dosr ===\n")
cat("Maquina:", maquina_id, "|", cpu_modelo, "|", ram_gb, "GB RAM\n")
cat("OS:", Sys.info()[["sysname"]], Sys.info()[["release"]], "\n")
cat("R:", paste(R.version$major, R.version$minor, sep = "."), "\n")
cat("Cores detectados:", parallel::detectCores(), "| Fork disponible:", future::supportsMulticore(), "\n\n")

# -----------------------------------------------------------------------------
# Construir serie bianual 2009-2024 (~8 rondas) simulada por duplicación.
# El costo de cómputo por ronda es realista; los p-valores saldrán degenerados
# (datos idénticos), lo que es irrelevante para medir tiempo.
# -----------------------------------------------------------------------------
anios   <- c(2009, 2011, 2013, 2015, 2017, 2019, 2022, 2024)
bases   <- list(casen_2022, casen_2024)
designs <- lapply(seq_along(anios), function(i)
  as_survey_design(bases[[(i - 1) %% 2 + 1]],
                   ids = varunit, strata = varstrat, weights = expr, nest = TRUE))
sufijos <- as.character(anios)

d1  <- designs[[1]]   # diseño único (escenarios de un solo año)
d_8 <- designs[[8]]   # último año de la serie
tmp <- tempdir()

cat("Diseños listos. Iniciando benchmark (3 iteraciones por escenario)...\n")
cat("Esto puede tardar varios minutos.\n\n")

res <- bench::mark(
  iterations = 3, check = FALSE, filter_gc = FALSE,

  # ----- Serie de 8 años (caso más común en el Observatorio Social) ----------
  serie8_1des_sec  = obs_media(designs, sufijos, "ytotcorh", des = "region",
                               parallel = FALSE, save_xlsx = FALSE, verbose = FALSE),

  serie8_1des_par  = obs_media(designs, sufijos, "ytotcorh", des = "region",
                               parallel = TRUE,  save_xlsx = FALSE, verbose = FALSE),

  serie8_sig_xlsx  = obs_media(designs, sufijos, "ytotcorh", des = "region",
                               sig = TRUE, parallel = TRUE, dir = tmp, verbose = FALSE),

  # ----- Un solo año con 7 combos de desagregación (multi_des) ---------------
  anio1_3des_multi = obs_media(d1, "2022", "ytotcorh",
                               des = c("region", "sexo", "area"),
                               multi_des = TRUE, save_xlsx = FALSE, verbose = FALSE),

  # ----- multi_bin: 8 indicadores × 16 regiones (objetivo de PERF-3) ---------
  multibin_8x_reg  = multi_bin(d_8, paste0("r8", letters[1:8]),
                               des = "region", dir = tmp, verbose = FALSE)
)

# -----------------------------------------------------------------------------
# Tabla de resultados
# -----------------------------------------------------------------------------
tabla <- data.frame(
  maquina_id = maquina_id,
  cpu        = cpu_modelo,
  ram_gb     = ram_gb,
  os         = paste(Sys.info()[["sysname"]], Sys.info()[["release"]]),
  arch       = Sys.info()[["machine"]],
  r_version  = paste(R.version$major, R.version$minor, sep = "."),
  cores      = parallel::detectCores(),
  fork       = future::supportsMulticore(),
  dosr_ver   = as.character(utils::packageVersion("dosr")),
  escenario  = as.character(res$expression),
  mediana_s  = round(as.numeric(res$median), 2),
  min_s      = round(as.numeric(res$min), 2),
  max_s      = round(vapply(res$time, function(t) as.numeric(max(t)), numeric(1)), 2),
  mem_mb     = round(as.numeric(res$mem_alloc) / 1e6, 1)
)

cat("\n=== RESULTADOS ===\n")
print(tabla[, c("escenario", "mediana_s", "min_s", "max_s", "mem_mb")],
      row.names = FALSE)

cat("\nSpeedup serie8 paralelo vs secuencial:",
    round(tabla$mediana_s[tabla$escenario == "serie8_1des_sec"] /
          tabla$mediana_s[tabla$escenario == "serie8_1des_par"], 2), "x\n")

dir.create("dev/bench_out", showWarnings = FALSE, recursive = TRUE)
out_path <- file.path("dev/bench_out", paste0("bench_", maquina_id, ".csv"))
write.csv(tabla, out_path, row.names = FALSE)
cat("\nGuardado:", out_path, "\n")

# -----------------------------------------------------------------------------
# PROFILING (ejecutar manualmente si se quiere ver el flamegraph)
# Descomentar las líneas deseadas:
# -----------------------------------------------------------------------------
# profvis::profvis(
#   obs_media(designs, sufijos, "ytotcorh", des = "region",
#             sig = TRUE, parallel = FALSE, dir = tempdir(), verbose = FALSE)
# )
# profvis::profvis(
#   obs_media(d1, "2022", "ytotcorh", des = c("region","sexo","area"),
#             save_xlsx = FALSE, verbose = FALSE)
# )
