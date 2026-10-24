# =============================================================================
# 03_indicadores.R — indicadores por período y datos para la página.
#
# Se corre desde la RAÍZ del proyecto bcall_nominate_chile/, después de
# 02_bcall.R:
#   Rscript post_bcall_periodos/R/03_indicadores.R
#
# QUÉ CALCULA
#   - Familias políticas a partir del partido vigente al inicio del período.
#     PDG e independientes van aparte, sin familia.
#   - Por período y bloque B-Call: mediana de d2, desviación estándar de d1
#     dentro del bloque (ancho del bloque) y tasa de voto ganador.
#     Voto ganador = el diputado votó con la opción (a favor o en contra) que
#     reunió más votos entre los emitidos en esa votación. No usa el resultado
#     oficial, que depende del quórum.
#   - Votaciones disputadas, aquellas en que la opción perdedora reúne al
#     menos 25% de los votos emitidos (abstenciones incluidas en el total).
#     Unánimes, sin votos en contra ni abstenciones, o sin votos a favor.
#   - Persistencia. Para quien estuvo en dos períodos consecutivos, percentil
#     de d1 y de d2 dentro de cada período. Se usan percentiles porque cada
#     período se estima por separado y las escalas no son idénticas.
#
# ESCRIBE
#   post_bcall_periodos/datos/datos.js       objeto BCALL que lee index.html
#   post_bcall_periodos/datos/resumen_periodos.csv
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
})

DIR_IN   <- "outputs/data/post_bcall"
DIR_REPO <- "post_bcall_periodos/datos"

est <- read_csv(file.path(DIR_REPO, "bcall_periodos.csv"), show_col_types = FALSE,
                col_types = cols(id_parlamentario = "c"))
votos <- read_rds(file.path(DIR_IN, "votos_periodo.rds"))
vot   <- read_rds(file.path(DIR_IN, "votaciones_periodo.rds"))
corr  <- read_rds(file.path(DIR_IN, "resumen_corridas.rds"))

GOBIERNO <- tibble(
  periodo = c("2002-2006", "2006-2010", "2010-2014", "2014-2018",
              "2018-2022", "2022-2026", "2026-2030"),
  presidente = c("Lagos", "Bachelet", "Piñera", "Bachelet", "Piñera", "Boric", "Kast"),
  bloque_gob = c("left", "left", "right", "left", "right", "left", "right")
)

DERECHA   <- c("UDI", "RN", "PREP", "EVOP", "PNL", "PSC", "PCC", "AMPL")
CENTRO    <- c("DC", "PRI", "DEM", "AMA")
IZQUIERDA <- c("PS", "PPD", "PC", "PR", "FA", "RD", "PCS", "COMUNES", "FRVS",
               "IC", "IGUAL", "LIBERAL", "PL", "PEV", "PH", "PRO", "PAH")

est <- est %>%
  mutate(familia = case_when(partido %in% IZQUIERDA ~ "izq",
                             partido %in% CENTRO    ~ "cen",
                             partido %in% DERECHA   ~ "der",
                             partido == "PDG"       ~ "pdg",
                             partido == "IND"       ~ "ind",
                             TRUE                   ~ "otro"),
         nombre = sub("^(.*), (.*)$", "\\2 \\1", nombre_completo))
stopifnot(!any(est$familia == "otro"))


# ---------------------------------------------------------------------------
# PASO 1 — votaciones disputadas y voto ganador
# ---------------------------------------------------------------------------
emitidos <- votos %>% filter(!is.na(voto_num))

por_votacion <- emitidos %>%
  group_by(periodo, id_votacion) %>%
  summarise(si = sum(voto_num == 1), no = sum(voto_num == -1), ab = sum(voto_num == 0),
            .groups = "drop") %>%
  mutate(lado = if_else(si >= no, 1, -1),
         perdedora = pmin(si, no) / (si + no + ab),
         disputada = perdedora >= 0.25,
         unanime = (no == 0 & ab == 0) | (si == 0 & ab == 0))

gana <- emitidos %>%
  inner_join(por_votacion %>% select(id_votacion, lado), by = "id_votacion") %>%
  group_by(periodo, id_parlamentario) %>%
  summarise(voto_ganador = mean(voto_num == lado), .groups = "drop")

est <- est %>% left_join(gana, by = c("periodo", "id_parlamentario"))


# ---------------------------------------------------------------------------
# PASO 2 — resumen por período
# ---------------------------------------------------------------------------
por_bloque <- est %>%
  group_by(periodo, bloque) %>%
  summarise(n = n(), d2_med = median(d2), d1_sd = sd(d1), d1_med = median(d1),
            gana = mean(voto_ganador), .groups = "drop") %>%
  pivot_wider(names_from = bloque, values_from = c(n, d2_med, d1_sd, d1_med, gana))

resumen <- corr %>%
  left_join(GOBIERNO, by = "periodo") %>%
  left_join(por_bloque, by = "periodo") %>%
  left_join(por_votacion %>% group_by(periodo) %>%
              summarise(disputadas = mean(disputada), unanimes = mean(unanime)),
            by = "periodo") %>%
  left_join(vot %>% group_by(periodo) %>% summarise(desde = min(fecha), hasta = max(fecha)),
            by = "periodo")

cat("== resumen ==\n")
print(as.data.frame(resumen), digits = 3)

# el bloque de gobierno, ¿tiene menor d2 y gana más?
chk <- resumen %>%
  mutate(d2_gob = if_else(bloque_gob == "left", d2_med_left, d2_med_right),
         d2_opo = if_else(bloque_gob == "left", d2_med_right, d2_med_left),
         gana_gob = if_else(bloque_gob == "left", gana_left, gana_right),
         gana_opo = if_else(bloque_gob == "left", gana_right, gana_left),
         n_gob = if_else(bloque_gob == "left", n_left, n_right),
         n_opo = if_else(bloque_gob == "left", n_right, n_left))
cat("\nperíodos con d2 de gobierno < oposición:", sum(chk$d2_gob < chk$d2_opo), "de", nrow(chk), "\n")
cat("períodos con gobierno ganando más:", sum(chk$gana_gob > chk$gana_opo), "de", nrow(chk), "\n")
cat("períodos con bloque de gobierno más grande:", sum(chk$n_gob > chk$n_opo), "de", nrow(chk), "\n")


# ---------------------------------------------------------------------------
# PASO 3 — persistencia entre períodos consecutivos
# ---------------------------------------------------------------------------
orden <- GOBIERNO$periodo
est <- est %>%
  group_by(periodo) %>%
  mutate(d1_pct = percent_rank(d1), d2_pct = percent_rank(d2)) %>%
  ungroup()

pares <- est %>%
  mutate(siguiente = orden[match(periodo, orden) + 1]) %>%
  select(id_parlamentario, nombre, periodo, siguiente, d1_pct, d2_pct) %>%
  inner_join(est %>% select(id_parlamentario, periodo, d1_pct_sig = d1_pct, d2_pct_sig = d2_pct),
             by = c("id_parlamentario", "siguiente" = "periodo"))

r_todos <- with(pares, c(d1 = cor(d1_pct, d1_pct_sig), d2 = cor(d2_pct, d2_pct_sig)))
sin26 <- pares %>% filter(siguiente != "2026-2030")
r_sin26 <- with(sin26, c(d1 = cor(d1_pct, d1_pct_sig), d2 = cor(d2_pct, d2_pct_sig)))
cat("\npares consecutivos:", nrow(pares), "| sin 2026-2030:", nrow(sin26), "\n")
cat("r todos   d1", round(r_todos["d1"], 3), " d2", round(r_todos["d2"], 3), "\n")
cat("r sin 26  d1", round(r_sin26["d1"], 3), " d2", round(r_sin26["d2"], 3), "\n")
cuartil <- sin26 %>% filter(d2_pct >= 0.75)
cat("del cuartil superior de d2, siguen en él:", sum(cuartil$d2_pct_sig >= 0.75), "de", nrow(cuartil), "\n")

por_par <- pares %>%
  group_by(periodo, siguiente) %>%
  summarise(n = n(), r_d1 = cor(d1_pct, d1_pct_sig), r_d2 = cor(d2_pct, d2_pct_sig), .groups = "drop")
print(as.data.frame(por_par), digits = 2)

# quienes repiten en el cuartil superior de d2 en tres o más períodos
repiten <- est %>%
  group_by(id_parlamentario, nombre) %>%
  summarise(periodos = n(), en_cuartil = sum(d2_pct >= 0.75), .groups = "drop") %>%
  filter(periodos >= 3) %>%
  arrange(desc(en_cuartil / periodos), desc(periodos))
cat("\ncon 3+ períodos:", nrow(repiten), "| siempre en el cuartil superior de d2:",
    sum(repiten$en_cuartil == repiten$periodos), "\n")
print(as.data.frame(head(repiten, 12)))


# ---------------------------------------------------------------------------
# PASO 4 — exportar
# ---------------------------------------------------------------------------
r3 <- function(x) round(x, 3)

datos <- list(
  generado = format(Sys.Date()),
  ultima_votacion = format(max(vot$fecha)),
  totales = list(votaciones = sum(resumen$votaciones),
                 usadas = sum(resumen$votaciones_usadas),
                 estimaciones = nrow(est),
                 personas = n_distinct(est$id_parlamentario)),
  persistencia = list(n = nrow(pares), n_sin26 = nrow(sin26),
                      r_d1 = r3(r_todos[["d1"]]), r_d2 = r3(r_todos[["d2"]]),
                      r_d1_sin26 = r3(r_sin26[["d1"]]), r_d2_sin26 = r3(r_sin26[["d2"]]),
                      cuartil_n = nrow(cuartil), cuartil_sigue = sum(cuartil$d2_pct_sig >= 0.75)),
  resumen = resumen %>%
    transmute(p = periodo, presidente, bloque_gob, votaciones, usadas = votaciones_usadas,
              diputados = diputados_estimados, pivot,
              desde = format(desde), hasta = format(hasta),
              n_left, n_right,
              d2_left = r3(d2_med_left), d2_right = r3(d2_med_right),
              sd_left = r3(d1_sd_left), sd_right = r3(d1_sd_right),
              gana_left = r3(gana_left), gana_right = r3(gana_right),
              disputadas = r3(disputadas), unanimes = r3(unanimes)),
  est = est %>%
    transmute(p = periodo, n = nombre, pa = partido, f = familia, b = bloque,
              d1 = r3(d1), d2 = r3(d2), part = r3(participacion), gana = r3(voto_ganador)),
  pares = pares %>%
    transmute(n = nombre, p = periodo, s = siguiente,
              d1a = r3(d1_pct), d1b = r3(d1_pct_sig), d2a = r3(d2_pct), d2b = r3(d2_pct_sig))
)

writeLines(c("// Generado por R/03_indicadores.R. No editar a mano.",
             paste0("window.BCALL = ", toJSON(datos, auto_unbox = TRUE, dataframe = "rows", na = "null"), ";")),
           file.path(DIR_REPO, "datos.js"), useBytes = FALSE)
write_csv(resumen, file.path(DIR_REPO, "resumen_periodos.csv"))
cat("\nescrito:", file.path(DIR_REPO, "datos.js"),
    round(file.size(file.path(DIR_REPO, "datos.js")) / 1024), "KB\n")
