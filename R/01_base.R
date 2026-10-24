# =============================================================================
# 01_base.R — base de votos nominales por período legislativo, Cámara de
# Diputados, marzo de 2002 a septiembre de 2026.
#
# Se corre desde la RAÍZ del proyecto bcall_nominate_chile/:
#   Rscript post_bcall_periodos/R/01_base.R
#
# FUENTES
#   data/votaciones.rds, data/votos_nominales.rds   opendata.camara.cl, hasta
#                                                   el 10 de junio de 2026
#   outputs/data/balance_2026/crudo/camara_*_2026_*  misma API, bajado en
#                                                   octubre de 2026 para el
#                                                   balance; cubre del 15 de
#                                                   junio al 9 de septiembre
#   data/diputados.rds                               nombre y sexo
#   data/militancias_x_periodo.rds                   última militancia que la
#                                                   Cámara registra en el período
#
# FILTROS (todos explícitos acá)
#   1. Solo votaciones de proyectos de ley con número de boletín (convención
#      del proyecto). Quedan fuera resoluciones, acuerdos y votaciones
#      reglamentarias.
#   2. Los períodos se definen por FECHA (11 de marzo a 10 de marzo, cuatro
#      años), no por periodo_id, que en opendata está corrido.
#   3. Codificación B-Call. A favor 1, En contra -1, Abstención 0. Dispensado,
#      Pareo y ausencia quedan NA.
#   4. Si un diputado aparece dos veces en la misma votación se conserva una
#      fila, priorizando la que trae voto emitido.
#
# ESCRIBE
#   outputs/data/post_bcall/votos_periodo.rds   una fila por voto
#   outputs/data/post_bcall/meta_periodo.rds    una fila por diputado x período
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

if (!file.exists("data/votaciones.rds")) stop("Correr desde la raíz de bcall_nominate_chile/.")
DIR_OUT   <- "outputs/data/post_bcall"
DIR_CRUDO <- "outputs/data/balance_2026/crudo"
dir.create(DIR_OUT, recursive = TRUE, showWarnings = FALSE)

PERIODOS <- tibble(
  periodo = c("2002-2006", "2006-2010", "2010-2014", "2014-2018",
              "2018-2022", "2022-2026", "2026-2030"),
  inicio  = as.Date(paste0(seq(2002, 2026, 4), "-03-11")),
  fin     = as.Date(paste0(seq(2006, 2030, 4), "-03-10"))
)


# ---------------------------------------------------------------------------
# PASO 1 — votaciones
# ---------------------------------------------------------------------------
vot <- bind_rows(read_rds("data/votaciones.rds"),
                 read_rds(file.path(DIR_CRUDO, "camara_votaciones_2026_nuevas.rds"))) %>%
  distinct(id_votacion, .keep_all = TRUE)
cat("votaciones totales:", nrow(vot), "| rango", format(range(vot$fecha, na.rm = TRUE)), "\n")

vot_pl <- vot %>%
  filter(tipo == "Proyecto de Ley", !is.na(numero_boletin), !is.na(fecha))
cat("de proyectos de ley con boletín:", nrow(vot_pl), "\n")

# período por fecha
vot_pl <- vot_pl %>%
  mutate(periodo = PERIODOS$periodo[findInterval(as.numeric(fecha), as.numeric(PERIODOS$inicio))]) %>%
  filter(!is.na(periodo)) %>%
  select(id_votacion, fecha, periodo, numero_boletin, quorum, total_si, total_no, total_abstencion)

print(count(vot_pl, periodo, name = "votaciones") %>% as.data.frame(), row.names = FALSE)


# ---------------------------------------------------------------------------
# PASO 2 — votos nominales
# ---------------------------------------------------------------------------
vn <- bind_rows(
  read_rds("data/votos_nominales.rds") %>% select(id_votacion, id_parlamentario, voto),
  read_rds(file.path(DIR_CRUDO, "camara_votos_2026_nuevos.rds")) %>% select(id_votacion, id_parlamentario, voto)
)
cat("\nvotos nominales leídos:", nrow(vn), "\n")
print(table(vn$voto, useNA = "ifany"))

votos <- vn %>%
  filter(!is.na(id_parlamentario)) %>%
  inner_join(vot_pl %>% select(id_votacion, periodo), by = "id_votacion") %>%
  mutate(id_parlamentario = as.character(id_parlamentario),
         voto_num = case_when(
           voto == "A favor"                       ~  1,
           voto == "En contra"                     ~ -1,
           voto %in% c("Abstencion", "Abstención") ~  0,
           TRUE                                    ~ NA_real_)) %>%
  arrange(id_votacion, id_parlamentario, is.na(voto_num))

n_antes <- nrow(votos)
votos <- votos %>% distinct(id_votacion, id_parlamentario, .keep_all = TRUE)
cat("filas duplicadas diputado x votación descartadas:", n_antes - nrow(votos), "\n")

# votaciones de proyecto de ley sin ningún voto nominal (no entran a la matriz)
sin_votos <- setdiff(vot_pl$id_votacion, votos$id_votacion)
cat("votaciones con boletín sin votos nominales:", length(sin_votos), "\n")


# ---------------------------------------------------------------------------
# PASO 3 — metadatos por diputado x período
# ---------------------------------------------------------------------------
# Partido. militancias_x_periodo guarda la última militancia que la Cámara
# registra para el diputado dentro del período (por ejemplo, Kast figura como
# independiente en 2014-2018 porque la Cámara lo registra así desde julio de
# 2017). Si no está ahí se usa partido_id de diputados.rds.
dip <- read_rds("data/diputados.rds") %>%
  mutate(id_parlamentario = as.character(id_parlamentario)) %>%
  inner_join(PERIODOS, by = c("periodo_inicio" = "inicio")) %>%
  select(id_parlamentario, periodo, nombre_completo, sexo, partido_dip = partido_id) %>%
  distinct(id_parlamentario, periodo, .keep_all = TRUE)

mil <- read_rds("data/militancias_x_periodo.rds") %>%
  mutate(id_parlamentario = as.character(id_parlamentario)) %>%
  inner_join(PERIODOS, by = c("periodo_inicio" = "inicio")) %>%
  select(id_parlamentario, periodo, partido_mil = partido_id) %>%
  distinct(id_parlamentario, periodo, .keep_all = TRUE)

presentes <- votos %>% distinct(id_parlamentario, periodo)
meta <- presentes %>%
  left_join(dip, by = c("id_parlamentario", "periodo")) %>%
  left_join(mil, by = c("id_parlamentario", "periodo")) %>%
  mutate(partido = coalesce(partido_mil, partido_dip))

# quien vota en un período pero no figura en el catálogo de ese período
falta <- meta %>% filter(is.na(nombre_completo))
if (nrow(falta)) {
  otro <- read_rds("data/diputados.rds") %>%
    mutate(id_parlamentario = as.character(id_parlamentario)) %>%
    arrange(desc(periodo_inicio)) %>%
    distinct(id_parlamentario, .keep_all = TRUE) %>%
    select(id_parlamentario, nombre_otro = nombre_completo, sexo_otro = sexo)
  meta <- meta %>% left_join(otro, by = "id_parlamentario") %>%
    mutate(nombre_completo = coalesce(nombre_completo, nombre_otro),
           sexo = coalesce(sexo, sexo_otro)) %>%
    select(-nombre_otro, -sexo_otro)
}
cat("\ndiputados x período:", nrow(meta),
    "| sin nombre:", sum(is.na(meta$nombre_completo)),
    "| sin partido:", sum(is.na(meta$partido)),
    "| partido distinto entre fuentes:", sum(meta$partido_mil != meta$partido_dip, na.rm = TRUE), "\n")

write_rds(votos %>% select(periodo, id_votacion, id_parlamentario, voto_num),
          file.path(DIR_OUT, "votos_periodo.rds"))
write_rds(vot_pl, file.path(DIR_OUT, "votaciones_periodo.rds"))
write_rds(meta %>% select(periodo, id_parlamentario, nombre_completo, sexo, partido),
          file.path(DIR_OUT, "meta_periodo.rds"))
cat("escrito en", DIR_OUT, "\n")
