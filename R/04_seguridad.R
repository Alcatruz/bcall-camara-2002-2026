# =============================================================================
# 04_seguridad.R — votaciones de seguridad contra el resto, B-Call por
# separado en cada período.
#
# Se corre desde la RAÍZ del proyecto bcall_nominate_chile/, después de
# 02_bcall.R:
#   Rscript post_bcall_periodos/R/04_seguridad.R
#
# CLASIFICACIÓN
#   Una votación es de seguridad si el título de su proyecto calza con el
#   diccionario de abajo (delitos y penas, policías, Ministerio Público,
#   cárceles, drogas, armas, terrorismo, orden público) y no con las
#   exclusiones, o si su boletín tiene sufijo 25 (comisión de Seguridad
#   Ciudadana). Quedan fuera los tratados internacionales, los juzgados de
#   policía local y los proyectos de derechos humanos y de aborto, que tocan
#   el Código Penal sin ser agenda de seguridad.
#   Los títulos vienen de la cosecha del observatorio (tramitacion.senado.cl).
#
# VALIDACIÓN, SIN CONTAMINAR
#   Referencia, los descriptores temáticos de la BCN que trae cada ficha del
#   Senado, clasificados con un diccionario propio. Cubren bien hasta 2018 y
#   casi nada después, por eso no sirven para clasificar pero sí para validar.
#   Los boletines votados con descriptores se partieron al azar en dos
#   mitades (semilla 20261024). El diccionario de títulos se armó mirando solo
#   la mitad de desarrollo, se congeló, y la mitad de prueba se mide una vez
#   acá. La referencia es otra codificación automática, no una codificación
#   humana, así que lo que se mide es acuerdo, no exactitud.
#
# ESTIMACIÓN
#   bcall_auto() por período sobre las votaciones de seguridad y, por
#   separado, sobre el resto, con el mismo pivot que 02_bcall.R. Para no
#   depender de cómo etiqueta los bloques cada corrida, los diputados se
#   agrupan por el bloque de la estimación completa del período
#   (datos/bcall_periodos.csv). El signo de d1 de cada subconjunto se revisa
#   contra el d1 completo.
#
# ESCRIBE
#   post_bcall_periodos/datos/seguridad_periodos.csv
#   post_bcall_periodos/datos/seguridad.js
#   outputs/data/post_bcall/votaciones_seguridad.rds
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(bcall)
  library(jsonlite)
})

DIR_IN    <- "outputs/data/post_bcall"
DIR_REPO  <- "post_bcall_periodos/datos"
PROYECTOS <- "observatorio/parquet_respaldo_20260913/proyecto.parquet"
MATERIAS  <- "outputs/data/materias_boletines.csv"
FICHAS    <- "C:/Users/dalca/observatorio_datos/crudo_archivos/wspublico"


# ---------------------------------------------------------------------------
# PASO 0 — descriptores de la BCN desde las fichas del Senado (una vez)
# ---------------------------------------------------------------------------
if (!file.exists(MATERIAS)) {
  arch <- list.files(FICHAS, pattern = "\\.xml$", full.names = TRUE)
  mat <- map_dfr(arch, function(f) {
    s <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = " ")
    bloque <- str_match(s, "<materias>(.*?)</materias>")[, 2]
    if (is.na(bloque)) return(NULL)
    d <- unique(str_trim(str_match_all(bloque, "<DESCRIPCION>(.*?)</DESCRIPCION>")[[1]][, 2]))
    d <- d[d != ""]
    if (!length(d)) return(NULL)
    tibble(boletin = str_extract(basename(f), "^[0-9]+-[0-9]+"), materia = d)
  })
  write_csv(mat, MATERIAS)
}
mat <- read_csv(MATERIAS, show_col_types = FALSE)


# ---------------------------------------------------------------------------
# PASO 1 — diccionarios (congelados el 2026-10-24)
# ---------------------------------------------------------------------------
norm <- function(x) x %>% str_to_lower() %>% stringi::stri_trans_general("Latin-ASCII") %>% str_squish()

RX_TIT <- paste0(
  "codigo penal|procesal penal|procedimiento penal|responsabilidad penal|\\bpenal(es)?\\b|\\bpenas?\\b|",
  "delit|delincuen|carabiner|policia de investigaciones|\\bpolicias?\\b|gendarmer|ministerio publico|",
  "fiscal nacional|defensoria penal|\\barmas?\\b|municion|explosiv|terroris|droga|estupefac|narco|",
  "\\brobos?\\b|hurto|homicid|femicid|secuestro|abigeato|agravante|prision|carcel|penitenciari|",
  "control (preventivo )?de identidad|seguridad (ciudadana|publica)|orden publico|crimen organizado|",
  "criminal|lavado de activos|flagran|usurpaci|receptaci|sicari|incivil|",
  "libertad (condicional|provisional|vigilada)|reincidencia")
EXCL <- paste0(
  "seguridad social|fiscalia nacional economica|policia local|lesa humanidad|genocidio|tortura|",
  "prision politica|detenidos desaparecidos|derechos humanos|11 de septiembre|aborto|",
  "minas u otros artefactos|",
  "^(proyecto de acuerdo que )?aprueba (el |la |los |las )?(convenio|convencion|tratado|acuerdo|protocolo|enmienda|estatuto)|",
  "^acuerdo entre")

RX_DESC <- paste0(
  "C[OÓ]DIGO PENAL|PROCESAL PENAL|PROCEDIMIENTO PENAL|RESPONSABILIDAD PENAL|DERECHO PENAL|\\bPENAS\\b|",
  "AUMENTO DE PENAS|DELIT|CARABINER|POLIC[IÍ]A DE INVESTIGACIONES|GENDARMER|MINISTERIO P[UÚ]BLICO|",
  "DEFENSOR[IÍ]A PENAL|\\bARMAS\\b|CONTROL DE ARMA|TERROR|DROGA|ESTUPEFAC|NARCO|\\bROBO|HURTO|HOMICID|",
  "SECUESTRO|ABIGEATO|AGRAVANTE|PRISI[OÓ]N PREVENTIVA|C[AÁ]RCEL|CONTROL DE IDENTIDAD|",
  "SEGURIDAD CIUDADANA|SEGURIDAD P[UÚ]BLICA|FUERZAS DE ORDEN|LAVADO DE DINERO|CRIMEN ORGANIZADO|CRIMINAL")

es_seguridad <- function(titulo, boletin) {
  t <- norm(titulo)
  (coalesce(str_detect(t, RX_TIT), FALSE) & !coalesce(str_detect(t, EXCL), FALSE)) |
    str_detect(boletin, "-25$")
}


# ---------------------------------------------------------------------------
# PASO 2 — votaciones con título
# ---------------------------------------------------------------------------
proy <- read_parquet(PROYECTOS) %>% select(boletin, titulo)
vot <- read_rds(file.path(DIR_IN, "votaciones_periodo.rds")) %>%
  mutate(boletin = str_extract(numero_boletin, "^[0-9]+-[0-9]+"))
bv <- vot %>% distinct(boletin, periodo) %>% left_join(proy, by = "boletin")
cat("votaciones:", nrow(vot), "| con título:",
    round(100 * mean(vot$boletin %in% proy$boletin[!is.na(proy$titulo)]), 1), "%\n")


# ---------------------------------------------------------------------------
# PASO 3 — validación en la muestra de prueba (una sola medición)
# ---------------------------------------------------------------------------
ref <- mat %>% group_by(boletin) %>% summarise(ref = any(str_detect(materia, RX_DESC)), .groups = "drop")
d <- bv %>% distinct(boletin, titulo) %>% inner_join(ref, by = "boletin") %>% filter(!is.na(titulo)) %>%
  mutate(suf25 = str_detect(boletin, "-25$"), ref = ref | suf25)
set.seed(20261024)
d <- d %>% mutate(muestra = if_else(runif(n()) < 0.5, "desarrollo", "prueba"),
                  tit = es_seguridad(titulo, boletin))
prueba <- d %>% filter(muestra == "prueba")
tp <- sum(prueba$tit & prueba$ref); fp <- sum(prueba$tit & !prueba$ref); fn <- sum(!prueba$tit & prueba$ref)
po <- mean(prueba$tit == prueba$ref)
pe <- mean(prueba$tit) * mean(prueba$ref) + mean(!prueba$tit) * mean(!prueba$ref)
valid <- tibble(boletines = nrow(prueba), ref_seguridad = sum(prueba$ref), titulo_seguridad = sum(prueba$tit),
                acuerdo = po, kappa = (po - pe) / (1 - pe),
                recupera = tp / (tp + fn), coincide = tp / (tp + fp))
cat("\n== validación, muestra de prueba ==\n"); print(as.data.frame(valid), digits = 3)


# ---------------------------------------------------------------------------
# PASO 4 — clasificar todas las votaciones
# ---------------------------------------------------------------------------
clasif <- bv %>% distinct(boletin, titulo) %>%
  mutate(seguridad = es_seguridad(coalesce(titulo, ""), boletin))
vot <- vot %>% left_join(clasif %>% select(boletin, seguridad), by = "boletin") %>%
  mutate(seguridad = coalesce(seguridad, FALSE))
write_rds(vot %>% select(periodo, id_votacion, boletin, seguridad), file.path(DIR_IN, "votaciones_seguridad.rds"))
cat("\nvotaciones de seguridad:", sum(vot$seguridad), "(", round(100 * mean(vot$seguridad), 1), "%) en",
    n_distinct(vot$boletin[vot$seguridad]), "boletines\n")


# ---------------------------------------------------------------------------
# PASO 5 — B-Call por subconjunto
# ---------------------------------------------------------------------------
votos <- read_rds(file.path(DIR_IN, "votos_periodo.rds"))
meta  <- read_rds(file.path(DIR_IN, "meta_periodo.rds"))
compl <- read_csv(file.path(DIR_REPO, "bcall_periodos.csv"), show_col_types = FALSE,
                  col_types = cols(id_parlamentario = "c")) %>%
  select(periodo, id_parlamentario, d1_compl = d1, bloque_compl = bloque)
PIVOTS <- c("Longueira Montes, Pablo", "Chahuán Chahuán, Francisco",
            "Kast Rist, José Antonio", "Alessandri Vergara, Jorge")

disputa <- votos %>% filter(!is.na(voto_num)) %>% group_by(id_votacion) %>%
  summarise(si = sum(voto_num == 1), no = sum(voto_num == -1), ab = sum(voto_num == 0), .groups = "drop") %>%
  mutate(disputada = pmin(si, no) / (si + no + ab) >= 0.25)
vot <- vot %>% left_join(disputa %>% select(id_votacion, disputada), by = "id_votacion")

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
  r <- r %>% rownames_to_column("id_parlamentario") %>% select(id_parlamentario, d1, d2) %>%
    mutate(periodo = per) %>%
    inner_join(compl, by = c("periodo", "id_parlamentario"))
  # orientación: d1 del subconjunto debe ir en el mismo sentido que el completo
  if (cor(r$d1, r$d1_compl) < 0) r$d1 <- -r$d1
  r
}

res <- map_dfr(unique(vot$periodo), function(p) {
  s <- vot %>% filter(periodo == p)
  map_dfr(c(TRUE, FALSE), function(seg) {
    ids <- s$id_votacion[s$seguridad == seg]
    r <- estimar(p, ids)
    tibble(periodo = p, materia = if (seg) "seguridad" else "resto",
           votaciones = length(ids),
           disputadas = mean(s$disputada[s$seguridad == seg], na.rm = TRUE),
           r_d1 = cor(r$d1, r$d1_compl),
           d2_left = median(r$d2[r$bloque_compl == "left"]),
           d2_right = median(r$d2[r$bloque_compl == "right"]),
           diputados = nrow(r))
  })
})

gob <- tibble(periodo = c("2002-2006", "2006-2010", "2010-2014", "2014-2018", "2018-2022", "2022-2026", "2026-2030"),
              presidente = c("Lagos", "Bachelet", "Piñera", "Bachelet", "Piñera", "Boric", "Kast"),
              bloque_gob = c("left", "left", "right", "left", "right", "left", "right"))
res <- res %>% left_join(gob, by = "periodo") %>%
  mutate(brecha_der = d2_left - d2_right,   # positivo, la derecha vota más parejo
         gob_mas_parejo = if_else(bloque_gob == "left", d2_left < d2_right, d2_right < d2_left))
cat("\n"); print(as.data.frame(res %>% mutate(across(where(is.numeric), ~ round(.x, 3)))))

r3 <- function(x) round(x, 3)
write_csv(res, file.path(DIR_REPO, "seguridad_periodos.csv"))
seg_js <- list(
  validacion = valid %>% mutate(across(where(is.double), r3)),
  total = list(votaciones = sum(vot$seguridad), pct = r3(mean(vot$seguridad)),
               boletines = n_distinct(vot$boletin[vot$seguridad])),
  periodos = res %>% mutate(across(where(is.double), r3))
)
writeLines(c("// Generado por R/04_seguridad.R. No editar a mano.",
             paste0("window.BCALL_SEG = ", toJSON(seg_js, auto_unbox = TRUE, dataframe = "rows"), ";")),
           file.path(DIR_REPO, "seguridad.js"), useBytes = TRUE)
cat("\nescrito:", file.path(DIR_REPO, "seguridad.js"), "\n")
