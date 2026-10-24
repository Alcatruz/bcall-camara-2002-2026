# =============================================================================
# 02_bcall.R — B-Call por período legislativo, una corrida por período.
#
# Se corre desde la RAÍZ del proyecto bcall_nominate_chile/, después de
# 01_base.R:
#   Rscript post_bcall_periodos/R/02_bcall.R
#
# DECISIONES
#   - Una matriz por período, filas = id_parlamentario (nunca el nombre, que
#     se repite entre personas), columnas = votaciones.
#   - bcall_auto() con distancia Manhattan y threshold = 0.1, los valores por
#     defecto del paquete. Quien vota en 10% o menos de las votaciones del
#     período no recibe estimación (reemplazos breves, licencias largas).
#   - El paquete descarta por su cuenta las votaciones sin varianza entre los
#     diputados que quedan (unánimes). Se reporta cuántas usa.
#   - Pivot para orientar d1 (derecha positiva). Primero el primero de la
#     lista que esté en la matriz; si ninguno, el primer UDI o RN del período.
#   - Chequeo de polaridad. La media de d1 de UDI y RN debe quedar sobre la de
#     PS, PPD y PC. Si no, el script se detiene.
#   - Cada período se estima por separado. Las escalas de d1 y d2 están en
#     unidades estandarizadas dentro de cada período, así que entre períodos
#     se comparan formas y distancias relativas, no niveles absolutos.
#
# ESCRIBE
#   post_bcall_periodos/datos/bcall_periodos.csv   diputado x período
#   outputs/data/post_bcall/resumen_corridas.rds   una fila por período
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(bcall)
})

DIR_IN   <- "outputs/data/post_bcall"
DIR_REPO <- "post_bcall_periodos/datos"
dir.create(DIR_REPO, recursive = TRUE, showWarnings = FALSE)

votos <- read_rds(file.path(DIR_IN, "votos_periodo.rds"))
meta  <- read_rds(file.path(DIR_IN, "meta_periodo.rds"))

PIVOTS <- c("Longueira Montes, Pablo", "Chahuán Chahuán, Francisco",
            "Kast Rist, José Antonio", "Alessandri Vergara, Jorge")


correr <- function(per) {
  v <- votos %>% filter(periodo == per)
  rc <- v %>%
    select(id_parlamentario, id_votacion, voto_num) %>%
    pivot_wider(names_from = id_votacion, values_from = voto_num, names_prefix = "v_") %>%
    column_to_rownames("id_parlamentario") %>%
    as.data.frame()
  rc <- rc[rowSums(!is.na(rc)) > 0, colSums(!is.na(rc)) > 0, drop = FALSE]

  m <- meta %>% filter(periodo == per)
  cand <- m$id_parlamentario[match(PIVOTS, m$nombre_completo)]
  cand <- cand[!is.na(cand) & cand %in% rownames(rc)]
  if (!length(cand)) cand <- m$id_parlamentario[m$partido %in% c("UDI", "RN") & m$id_parlamentario %in% rownames(rc)]
  pivot <- cand[1]

  res <- bcall_auto(rollcall = rc, pivot = pivot, threshold = 0.1, verbose = FALSE)

  # votaciones con varianza entre quienes pasan el umbral (las que usa bcall)
  part <- rowMeans(!is.na(rc))
  X <- as.matrix(rc[part > 0.1, , drop = FALSE])
  sdv <- apply(X, 2, sd, na.rm = TRUE)

  est <- res$results %>%
    as.data.frame() %>%
    rownames_to_column("id_parlamentario") %>%
    transmute(periodo = per, id_parlamentario, d1, d2, bloque = auto_cluster) %>%
    mutate(n_votos = rowSums(!is.na(rc))[id_parlamentario],
           participacion = part[id_parlamentario])

  list(est = est,
       resumen = tibble(periodo = per,
                        votaciones = ncol(rc),
                        votaciones_usadas = sum(sdv > 0, na.rm = TRUE),
                        diputados_con_voto = nrow(rc),
                        diputados_estimados = nrow(est),
                        pivot = m$nombre_completo[match(pivot, m$id_parlamentario)],
                        primera = min(v$id_votacion), ultima = max(v$id_votacion)))
}

periodos <- sort(unique(votos$periodo))
out <- map(periodos, function(p) { cat("corriendo", p, "\n"); correr(p) })

est <- bind_rows(map(out, "est")) %>%
  left_join(meta, by = c("periodo", "id_parlamentario"))
resumen <- bind_rows(map(out, "resumen")) %>% select(-primera, -ultima)

# polaridad
pol <- est %>%
  mutate(lado = case_when(partido %in% c("UDI", "RN") ~ "der",
                          partido %in% c("PS", "PPD", "PC") ~ "izq")) %>%
  filter(!is.na(lado)) %>%
  group_by(periodo, lado) %>% summarise(d1 = mean(d1), .groups = "drop") %>%
  pivot_wider(names_from = lado, values_from = d1)
print(as.data.frame(pol))
stopifnot(all(pol$der > pol$izq))

cat("\n")
print(as.data.frame(resumen), row.names = FALSE)

est <- est %>%
  select(periodo, id_parlamentario, nombre_completo, sexo, partido,
         d1, d2, bloque, n_votos, participacion) %>%
  arrange(periodo, d1)

write_csv(est, file.path(DIR_REPO, "bcall_periodos.csv"))
write_rds(resumen, file.path(DIR_IN, "resumen_corridas.rds"))
cat("\nescrito:", file.path(DIR_REPO, "bcall_periodos.csv"), nrow(est), "filas\n")
