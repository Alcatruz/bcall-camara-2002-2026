# =============================================================================
# 05_centro.R — ¿se sostienen los hallazgos sobre d2 si se compara a
# diputados a igual distancia del centro?
#
# Se corre desde la RAÍZ del proyecto bcall_nominate_chile/, después de
# 02_bcall.R, 03_indicadores.R y 04_seguridad.R:
#   Rscript post_bcall_periodos/R/05_centro.R
#
# POR QUÉ
#   B-Call estandariza cada votación, le da signo según el bloque y calcula d2
#   como la desviación estándar de esos valores por diputado. Con dos bloques,
#   un diputado de centro que vota siempre según su posición igual vota unas
#   veces con cada bloque, y su d2 sube por construcción. La simulación está
#   en Alcatruz/bcall-wnominate-camara (R/02_simulacion.R).
#
# DECISIONES
#   1. Centro de la sala = punto medio entre las medianas de d1 de los dos
#      bloques. Distancia al centro = |d1 - centro|.
#   2. Dentro de cada período, los diputados se agrupan en quintiles de
#      distancia al centro (los dos bloques juntos). No se ajusta d2 con una
#      función de d1, porque los bloques están separados en d1 y esa función
#      absorbería la misma diferencia entre bloques que se quiere medir.
#   3. Gobierno. En cada quintil, mediana de d2 de la oposición menos la del
#      gobierno, si cada bloque tiene al menos 3 diputados en el quintil.
#      Se promedia entre quintiles con el número de diputados como peso.
#   4. Persistencia y los que siempre están arriba. Percentil de d2 dentro
#      del quintil de distancia del período ("d2 relativo").
#   5. Seguridad. Se reestima B-Call por subconjunto igual que en
#      04_seguridad.R y se compara por quintiles de la distancia al centro
#      de la estimación completa.
#
# ESCRIBE
#   post_bcall_periodos/datos/d2_relativo.csv     diputado x período
#   post_bcall_periodos/datos/centro_periodos.csv una fila por período
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(bcall)
})

DIR_IN   <- "outputs/data/post_bcall"
DIR_REPO <- "post_bcall_periodos/datos"

GOBIERNO <- tibble(
  periodo = c("2002-2006", "2006-2010", "2010-2014", "2014-2018",
              "2018-2022", "2022-2026", "2026-2030"),
  presidente = c("Lagos", "Bachelet", "Piñera", "Bachelet", "Piñera", "Boric", "Kast"),
  bloque_gob = c("left", "left", "right", "left", "right", "left", "right")
)
orden <- GOBIERNO$periodo

est <- read_csv(file.path(DIR_REPO, "bcall_periodos.csv"), show_col_types = FALSE,
                col_types = cols(id_parlamentario = "c")) %>%
  mutate(nombre = sub("^(.*), (.*)$", "\\2 \\1", nombre_completo)) %>%
  left_join(GOBIERNO, by = "periodo")

# distancia al centro y quintil, dentro de cada período
con_distancia <- function(d) {
  centro <- (median(d$d1[d$bloque == "left"]) + median(d$d1[d$bloque == "right"])) / 2
  d %>% mutate(dist = abs(d1 - centro), q_dist = ntile(dist, 5))
}

# brecha oposición menos gobierno en d2, dentro de quintiles de distancia
brecha_por_quintil <- function(d, var = "d2") {
  q <- d %>%
    mutate(lado = if_else(bloque == bloque_gob, "gob", "opo")) %>%
    group_by(q_dist) %>%
    summarise(n_gob = sum(lado == "gob"), n_opo = sum(lado == "opo"),
              dif = median(.data[[var]][lado == "opo"]) - median(.data[[var]][lado == "gob"]), .groups = "drop") %>%
    filter(n_gob >= 3, n_opo >= 3)
  tibble(brecha_q = weighted.mean(q$dif, q$n_gob + q$n_opo), quintiles = nrow(q),
         quintiles_gob_menor = sum(q$dif > 0))
}

est <- est %>% group_by(periodo) %>% group_modify(~ con_distancia(.x)) %>% ungroup() %>%
  group_by(periodo) %>% mutate(d1_pct = percent_rank(d1), d2_pct = percent_rank(d2)) %>% ungroup() %>%
  group_by(periodo, q_dist) %>% mutate(d2_rel_pct = percent_rank(d2)) %>% ungroup()


# ---------------------------------------------------------------------------
# PASO 1 — cuánto se relaciona d2 con la distancia al centro
# ---------------------------------------------------------------------------
cat("== Spearman entre d2 y distancia al centro, y mediana de d2 por quintil (1 = centro) ==\n")
print.data.frame(est %>% group_by(periodo) %>%
                   summarise(rho = round(cor(d2, dist, method = "spearman"), 2),
                             q1 = round(median(d2[q_dist == 1]), 2), q3 = round(median(d2[q_dist == 3]), 2),
                             q5 = round(median(d2[q_dist == 5]), 2), .groups = "drop"), row.names = FALSE)
cat("\nquintil 1 (más central): cuántos de cada bloque\n")
print.data.frame(est %>% filter(q_dist == 1) %>% mutate(lado = if_else(bloque == bloque_gob, "gob", "opo")) %>%
                   count(periodo, lado) %>% pivot_wider(names_from = lado, values_from = n), row.names = FALSE)


# ---------------------------------------------------------------------------
# PASO 2 — gobierno y oposición
# ---------------------------------------------------------------------------
gob <- est %>%
  group_by(periodo, presidente) %>%
  group_modify(~ bind_cols(
    tibble(brecha = median(.x$d2[.x$bloque != .x$bloque_gob]) - median(.x$d2[.x$bloque == .x$bloque_gob])),
    brecha_por_quintil(.x))) %>%
  ungroup()
cat("\n== gobierno: oposición menos gobierno, en bruto y dentro de quintiles de distancia ==\n")
print.data.frame(gob %>% mutate(across(where(is.double), ~ round(.x, 3))), row.names = FALSE)
cat("períodos con gobierno más parejo, en bruto:", sum(gob$brecha > 0), "| por quintiles:", sum(gob$brecha_q > 0), "de", nrow(gob), "\n")


# ---------------------------------------------------------------------------
# PASO 3 — persistencia y los que siempre están arriba
# ---------------------------------------------------------------------------
pares <- est %>%
  mutate(siguiente = orden[match(periodo, orden) + 1]) %>%
  select(id_parlamentario, nombre, periodo, siguiente, d2_pct, d2_rel_pct) %>%
  inner_join(est %>% select(id_parlamentario, periodo, d2_pct_sig = d2_pct, d2_rel_pct_sig = d2_rel_pct),
             by = c("id_parlamentario", "siguiente" = "periodo")) %>%
  filter(siguiente != "2026-2030")
cat("\n== persistencia (", nrow(pares), "pares ) ==\n")
cat("r d2", round(cor(pares$d2_pct, pares$d2_pct_sig), 3), "| r d2 relativo", round(cor(pares$d2_rel_pct, pares$d2_rel_pct_sig), 3), "\n")
cq  <- pares %>% filter(d2_pct >= 0.75)
cqr <- pares %>% filter(d2_rel_pct >= 0.75)
cat("cuartil superior que sigue, d2:", sum(cq$d2_pct_sig >= 0.75), "de", nrow(cq),
    "| d2 relativo:", sum(cqr$d2_rel_pct_sig >= 0.75), "de", nrow(cqr), "\n")

repiten <- est %>%
  group_by(id_parlamentario, nombre) %>%
  summarise(periodos = n(), en_cuartil = sum(d2_pct >= 0.75), en_cuartil_rel = sum(d2_rel_pct >= 0.75),
            q_dist_medio = mean(q_dist), .groups = "drop") %>%
  filter(periodos >= 3)
cat("con 3+ períodos:", nrow(repiten), "| siempre arriba, d2:", sum(repiten$en_cuartil == repiten$periodos),
    "| d2 relativo:", sum(repiten$en_cuartil_rel == repiten$periodos), "\n")
print.data.frame(repiten %>% filter(en_cuartil == periodos | en_cuartil_rel == periodos) %>%
                   mutate(q_dist_medio = round(q_dist_medio, 1)) %>% arrange(desc(en_cuartil_rel)), row.names = FALSE)
seis <- repiten$id_parlamentario[repiten$en_cuartil == repiten$periodos | repiten$en_cuartil_rel == repiten$periodos]
print.data.frame(est %>% filter(id_parlamentario %in% seis) %>%
                   transmute(nombre, periodo, partido, q_dist, d2_pct = round(d2_pct, 2), d2_rel_pct = round(d2_rel_pct, 2)) %>%
                   arrange(nombre, periodo), row.names = FALSE)


# ---------------------------------------------------------------------------
# PASO 4 — seguridad, por quintiles de distancia
# ---------------------------------------------------------------------------
votos <- read_rds(file.path(DIR_IN, "votos_periodo.rds"))
meta  <- read_rds(file.path(DIR_IN, "meta_periodo.rds"))
vseg  <- read_rds(file.path(DIR_IN, "votaciones_seguridad.rds"))
PIVOTS <- c("Longueira Montes, Pablo", "Chahuán Chahuán, Francisco",
            "Kast Rist, José Antonio", "Alessandri Vergara, Jorge")
compl <- est %>% select(periodo, id_parlamentario, d1_compl = d1, bloque, bloque_gob, q_dist)

estimar <- function(per, ids) {
  rc <- votos %>% filter(periodo == per, id_votacion %in% ids) %>%
    select(id_parlamentario, id_votacion, voto_num) %>%
    pivot_wider(names_from = id_votacion, values_from = voto_num) %>%
    column_to_rownames("id_parlamentario") %>% as.data.frame()
  rc <- rc[rowSums(!is.na(rc)) > 0, , drop = FALSE]
  m <- meta %>% filter(periodo == per)
  piv <- m$id_parlamentario[match(PIVOTS, m$nombre_completo)]
  piv <- piv[!is.na(piv) & piv %in% rownames(rc)][1]
  r <- bcall_auto(rollcall = rc, pivot = piv, threshold = 0.1, verbose = FALSE)$results
  r %>% rownames_to_column("id_parlamentario") %>% select(id_parlamentario, d1, d2) %>%
    mutate(periodo = per) %>%
    inner_join(compl, by = c("periodo", "id_parlamentario"))
}

seg <- map_dfr(orden, function(p) {
  s <- vseg %>% filter(periodo == p)
  map_dfr(c(TRUE, FALSE), function(es) {
    r <- estimar(p, s$id_votacion[s$seguridad == es])
    bind_cols(tibble(periodo = p, materia = if (es) "seguridad" else "resto",
                     brecha = median(r$d2[r$bloque != r$bloque_gob]) - median(r$d2[r$bloque == r$bloque_gob])),
              brecha_por_quintil(r))
  })
}) %>% left_join(GOBIERNO %>% select(periodo, presidente), by = "periodo")
cat("\n== seguridad y resto: oposición menos gobierno (positivo = gobierno más parejo) ==\n")
print.data.frame(seg %>% select(periodo, presidente, materia, brecha, brecha_q, quintiles, quintiles_gob_menor) %>%
                   mutate(across(where(is.double), ~ round(.x, 3))), row.names = FALSE)


# ---------------------------------------------------------------------------
# PASO 5 — exportar
# ---------------------------------------------------------------------------
write_csv(est %>% select(periodo, id_parlamentario, nombre_completo, bloque, d1, d2, dist, q_dist, d2_pct, d2_rel_pct),
          file.path(DIR_REPO, "d2_relativo.csv"))
write_csv(gob %>% left_join(seg %>% select(periodo, materia, brecha, brecha_q) %>%
                              pivot_wider(names_from = materia, values_from = c(brecha, brecha_q), names_prefix = ""),
                            by = "periodo"),
          file.path(DIR_REPO, "centro_periodos.csv"))
cat("\nlisto\n")
