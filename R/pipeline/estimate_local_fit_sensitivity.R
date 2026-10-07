# -*- coding: UTF-8 -*-
# Stage 03 implementation: local FIT sensitivity ------------------------------
#
# Population: participants with reported family history (highrisk == 1), for
# whom FIT and colonoscopy were both offered. Unit of analysis: participant.
# Random component: one of the two available FIT measurements is selected with
# the configured seed; participants with one observed FIT retain that result.
# Outputs: Wilson sensitivity and specificity estimates plus audit counts.

# Sensibilidad del FIT usando una sola medicion por individuo.
# Si ambos tests estan disponibles, se elige uno con probabilidad 0,5.
# Si solo uno esta disponible, se usa ese resultado. Si ninguno esta
# disponible, el individuo no aporta al denominador de sensibilidad.

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
})

project_directory <- Sys.getenv(
  "PRENEC_PROJECT_ROOT",
  unset = normalizePath(getwd(), winslash = "/", mustWork = TRUE)
)

semilla_por_defecto <- Sys.getenv("PRENEC_SEED", unset = "20260923")
SEMILLA_SENSIBILIDAD <- suppressWarnings(as.integer(
  Sys.getenv("PRENEC_SEED_SENS", unset = semilla_por_defecto)
))
if (is.na(SEMILLA_SENSIBILIDAD)) {
  stop("PRENEC_SEED_SENS debe ser un numero entero.")
}

RUTA_BASE_SENSIBILIDAD <- Sys.getenv(
  "PRENEC_BASE_SENS",
  unset = file.path(project_directory, "data", "private", "Base_prenec.xlsx")
)
if (!file.exists(RUTA_BASE_SENSIBILIDAD)) {
  stop("No existe la base para sensibilidad: ", RUTA_BASE_SENSIBILIDAD)
}

BaseSens <- read_excel(RUTA_BASE_SENSIBILIDAD)

columnas_requeridas <- c(
  "Codigo", "highrisk", "Tipo_Histologico_1", "Tamano_mm", "adenoma_size",
  "Valor_TSDO_1_1", "Valor_TSDO_2_1"
)
faltantes <- setdiff(columnas_requeridas, names(BaseSens))
if (length(faltantes) > 0L) {
  stop("Faltan columnas en la base de sensibilidad: ", paste(faltantes, collapse = ", "))
}

# La cohorte de sensibilidad del proyecto corresponde a highrisk == 1.
BaseSens <- BaseSens %>% filter(highrisk == 1)

if (anyNA(BaseSens$Codigo) || anyDuplicated(BaseSens$Codigo) > 0L) {
  stop("Codigo debe ser unico y no faltante en la cohorte de sensibilidad.")
}

condiciones_adenoma <- c(
  "Adenoma tubular con atipia de bajo grado",
  "Adenoma tubular con atipia de alto grado",
  "Adenoma tubulo-velloso con atipia de bajo grado",
  "Adenoma tubulo-velloso con atipia de alto grado",
  "Adenoma velloso con atipia de alto grado",
  "Adenocarcinoma intramucoso"
)

normalizar_tamano <- function(x) {
  x_ascii <- iconv(tolower(trimws(as.character(x))), from = "", to = "ASCII//TRANSLIT")
  case_when(
    x_ascii == "pequeno" ~ "small",
    x_ascii == "mediano" ~ "medium",
    x_ascii == "grande" ~ "large",
    x_ascii %in% c("small", "medium", "large") ~ x_ascii,
    TRUE ~ NA_character_
  )
}

BaseSens <- BaseSens %>%
  mutate(
    Tamano_mm = suppressWarnings(as.numeric(Tamano_mm)),
    adenomas = as.integer(Tipo_Histologico_1 %in% condiciones_adenoma),
    adenoma_risk = case_when(
      Tipo_Histologico_1 == "Adenoma tubular con atipia de bajo grado" & Tamano_mm >= 10 ~ 1,
      Tipo_Histologico_1 %in% condiciones_adenoma[-1] ~ 1,
      Tipo_Histologico_1 == "Adenoma tubular con atipia de bajo grado" & Tamano_mm < 10 ~ 0,
      TRUE ~ NA_real_
    ),
    cancer = ifelse(
      Tipo_Histologico_1 %in% c(
        "Adenocarcinoma bien diferenciado",
        "Adenocarcinoma moderadamente diferenciado",
        "Adenocarcinoma pobremente diferenciado"
      ),
      1,
      NA_real_
    ),
    adenoma_size = normalizar_tamano(adenoma_size),
    FIT_1 = case_when(
      is.na(Valor_TSDO_1_1) ~ NA_character_,
      Valor_TSDO_1_1 >= 100 ~ "Positivo",
      TRUE ~ "Negativo"
    ),
    FIT_2 = case_when(
      is.na(Valor_TSDO_2_1) ~ NA_character_,
      Valor_TSDO_2_1 >= 100 ~ "Positivo",
      TRUE ~ "Negativo"
    )
  )

# Una sola moneda por persona. El orden por Codigo hace que el resultado no
# dependa del orden de las filas mientras la cohorte sea la misma.
orden_codigo <- order(as.character(BaseSens$Codigo), method = "radix")
RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
set.seed(SEMILLA_SENSIBILIDAD)
u_sensibilidad <- numeric(nrow(BaseSens))
u_sensibilidad[orden_codigo] <- runif(nrow(BaseSens))

BaseSens <- BaseSens %>%
  mutate(
    ambas_pruebas_observadas = !is.na(FIT_1) & !is.na(FIT_2),
    test_sorteado = case_when(
      ambas_pruebas_observadas & u_sensibilidad < 0.5 ~ 1L,
      ambas_pruebas_observadas ~ 2L,
      !is.na(FIT_1) ~ 1L,
      !is.na(FIT_2) ~ 2L,
      TRUE ~ NA_integer_
    ),
    FIT_sorteado = case_when(
      test_sorteado == 1L ~ FIT_1,
      test_sorteado == 2L ~ FIT_2,
      TRUE ~ NA_character_
    )
  )

if (any(BaseSens$ambas_pruebas_observadas & is.na(BaseSens$test_sorteado))) {
  stop("Fallo de integridad: existen personas con ambos tests sin asignacion.")
}
if (any(is.na(BaseSens$FIT_1) & is.na(BaseSens$FIT_2) & !is.na(BaseSens$FIT_sorteado))) {
  stop("Fallo de integridad: se asigno FIT a una persona sin tests observados.")
}

wilson <- function(x, n, conf_level = 0.95) {
  if (length(x) != 1L || length(n) != 1L || is.na(x) || is.na(n) || n <= 0 || x < 0 || x > n) {
    return(c(estimacion = NA_real_, inferior = NA_real_, superior = NA_real_))
  }
  z <- qnorm(1 - (1 - conf_level) / 2)
  p <- x / n
  denominador <- 1 + z^2 / n
  centro <- (p + z^2 / (2 * n)) / denominador
  semiancho <- z * sqrt((p * (1 - p) / n) + (z^2 / (4 * n^2))) / denominador
  c(
    estimacion = p,
    inferior = max(0, centro - semiancho),
    superior = min(1, centro + semiancho)
  )
}

# Estado de referencia por lesion: 1 = lesion presente, 0 = lesion ausente.
# La especificidad se calcula uno-contra-el-resto: una lesion diferente cuenta
# como ausencia de la lesion objetivo. Si hay un adenoma pero falta su tamano o
# clasificacion de riesgo, el estado queda como NA y se excluye solo del endpoint
# que no puede clasificarse. Esto evita convertir informacion desconocida en 0.
estados_lesion <- list(
  adenoma_total = as.integer(BaseSens$adenomas == 1),
  small = case_when(
    BaseSens$adenomas == 1 & BaseSens$adenoma_size == "small" ~ 1L,
    BaseSens$adenomas == 0 ~ 0L,
    BaseSens$adenomas == 1 & !is.na(BaseSens$adenoma_size) ~ 0L,
    TRUE ~ NA_integer_
  ),
  medium = case_when(
    BaseSens$adenomas == 1 & BaseSens$adenoma_size == "medium" ~ 1L,
    BaseSens$adenomas == 0 ~ 0L,
    BaseSens$adenomas == 1 & !is.na(BaseSens$adenoma_size) ~ 0L,
    TRUE ~ NA_integer_
  ),
  small_medium = case_when(
    BaseSens$adenomas == 1 & BaseSens$adenoma_size %in% c("small", "medium") ~ 1L,
    BaseSens$adenomas == 0 ~ 0L,
    BaseSens$adenomas == 1 & !is.na(BaseSens$adenoma_size) ~ 0L,
    TRUE ~ NA_integer_
  ),
  large = case_when(
    BaseSens$adenomas == 1 & BaseSens$adenoma_size == "large" ~ 1L,
    BaseSens$adenomas == 0 ~ 0L,
    BaseSens$adenomas == 1 & !is.na(BaseSens$adenoma_size) ~ 0L,
    TRUE ~ NA_integer_
  ),
  low_risk = case_when(
    BaseSens$adenomas == 1 & BaseSens$adenoma_risk == 0 ~ 1L,
    BaseSens$adenomas == 0 ~ 0L,
    BaseSens$adenomas == 1 & !is.na(BaseSens$adenoma_risk) ~ 0L,
    TRUE ~ NA_integer_
  ),
  high_risk = case_when(
    BaseSens$adenomas == 1 & BaseSens$adenoma_risk == 1 ~ 1L,
    BaseSens$adenomas == 0 ~ 0L,
    BaseSens$adenomas == 1 & !is.na(BaseSens$adenoma_risk) ~ 0L,
    TRUE ~ NA_integer_
  ),
  cancer = as.integer(!is.na(BaseSens$cancer) & BaseSens$cancer == 1)
)

# Se mantiene este objeto por compatibilidad con el resto del script.
casos <- lapply(estados_lesion, function(estado) estado == 1L)

etiquetas <- c(
  adenoma_total = "Any adenoma",
  small = "Small adenomas",
  medium = "Medium adenomas",
  small_medium = "Small and Medium ad.",
  large = "Large adenomas",
  low_risk = "Low risk adenomas",
  high_risk = "High risk adenomas",
  cancer = "Cancer"
)

calcular_sensibilidad <- function(clave, indicador_caso, fit = BaseSens$FIT_sorteado) {
  indicador_caso[is.na(indicador_caso)] <- FALSE
  incluidos <- indicador_caso & !is.na(fit)
  n <- sum(incluidos)
  x <- sum(incluidos & fit == "Positivo")
  intervalo <- wilson(x, n)

  tibble(
    clave = clave,
    Category = unname(etiquetas[[clave]]),
    Positives = x,
    N = n,
    Missing_selected_test = sum(indicador_caso & is.na(fit)),
    Mean = unname(intervalo[["estimacion"]]),
    Lower_CI = unname(intervalo[["inferior"]]),
    Upper_CI = unname(intervalo[["superior"]])
  )
}

detalle_sensibilidad <- bind_rows(
  Map(calcular_sensibilidad, names(casos), casos)
)

if (any(detalle_sensibilidad$Positives < 0) ||
    any(detalle_sensibilidad$Positives > detalle_sensibilidad$N) ||
    any(detalle_sensibilidad$Lower_CI > detalle_sensibilidad$Mean) ||
    any(detalle_sensibilidad$Upper_CI < detalle_sensibilidad$Mean)) {
  stop("Fallo de integridad en los conteos o intervalos de sensibilidad.")
}

# Tabla compatible con el bloque original.
result_table <- detalle_sensibilidad %>%
  select(Category, Mean, Lower_CI, Upper_CI)

calcular_especificidad <- function(clave, estado_lesion, fit = BaseSens$FIT_sorteado) {
  incluidos <- !is.na(estado_lesion) & estado_lesion == 0L & !is.na(fit)
  n <- sum(incluidos)
  verdaderos_negativos <- sum(incluidos & fit == "Negativo")
  falsos_positivos <- sum(incluidos & fit == "Positivo")
  intervalo <- wilson(verdaderos_negativos, n)

  tibble(
    clave = clave,
    Category = unname(etiquetas[[clave]]),
    True_Negatives = verdaderos_negativos,
    False_Positives = falsos_positivos,
    N = n,
    Unknown_reference = sum(is.na(estado_lesion) & !is.na(fit)),
    Missing_selected_test = sum(!is.na(estado_lesion) & estado_lesion == 0L & is.na(fit)),
    Mean = unname(intervalo[["estimacion"]]),
    Lower_CI = unname(intervalo[["inferior"]]),
    Upper_CI = unname(intervalo[["superior"]])
  )
}

detalle_especificidad <- bind_rows(
  Map(calcular_especificidad, names(estados_lesion), estados_lesion)
)

if (any(detalle_especificidad$True_Negatives < 0) ||
    any(detalle_especificidad$False_Positives < 0) ||
    any(detalle_especificidad$True_Negatives + detalle_especificidad$False_Positives != detalle_especificidad$N) ||
    any(detalle_especificidad$Lower_CI > detalle_especificidad$Mean) ||
    any(detalle_especificidad$Upper_CI < detalle_especificidad$Mean)) {
  stop("Fallo de integridad en los conteos o intervalos de especificidad.")
}

specificity_result_table <- detalle_especificidad %>%
  select(Category, Mean, Lower_CI, Upper_CI)

# Estimacion clinica complementaria: especificidad del FIT entre personas sin
# ningun adenoma y sin cancer invasor. Es una referencia unica, no especifica
# de cada subtipo de lesion.
referencia_sin_neoplasia <- BaseSens$adenomas == 0 &
  (is.na(BaseSens$cancer) | BaseSens$cancer != 1)
incluidos_sin_neoplasia <- referencia_sin_neoplasia & !is.na(BaseSens$FIT_sorteado)
tn_sin_neoplasia <- sum(incluidos_sin_neoplasia & BaseSens$FIT_sorteado == "Negativo")
fp_sin_neoplasia <- sum(incluidos_sin_neoplasia & BaseSens$FIT_sorteado == "Positivo")
n_sin_neoplasia <- tn_sin_neoplasia + fp_sin_neoplasia
ic_spec_sin_neoplasia <- wilson(tn_sin_neoplasia, n_sin_neoplasia)

specificity_no_neoplasia_table <- tibble(
  Category = "No adenoma or invasive cancer",
  True_Negatives = tn_sin_neoplasia,
  False_Positives = fp_sin_neoplasia,
  N = n_sin_neoplasia,
  Missing_selected_test = sum(referencia_sin_neoplasia & is.na(BaseSens$FIT_sorteado)),
  Mean = unname(ic_spec_sin_neoplasia[["estimacion"]]),
  Lower_CI = unname(ic_spec_sin_neoplasia[["inferior"]]),
  Upper_CI = unname(ic_spec_sin_neoplasia[["superior"]])
)

spec_no_neoplasia <- specificity_no_neoplasia_table$Mean
li_spec_no_neoplasia <- specificity_no_neoplasia_table$Lower_CI
ls_spec_no_neoplasia <- specificity_no_neoplasia_table$Upper_CI
a_spec_no_neoplasia <- tn_sin_neoplasia
b_spec_no_neoplasia <- fp_sin_neoplasia

result_table_diagnostic <- bind_rows(
  detalle_sensibilidad %>%
    transmute(
      Category,
      Measure = "Sensitivity",
      Numerator = Positives,
      Denominator = N,
      Mean,
      Lower_CI,
      Upper_CI
    ),
  detalle_especificidad %>%
    transmute(
      Category,
      Measure = "Specificity",
      Numerator = True_Negatives,
      Denominator = N,
      Mean,
      Lower_CI,
      Upper_CI
    )
)

extraer <- function(clave, columna) {
  detalle_sensibilidad[[columna]][match(clave, detalle_sensibilidad$clave)]
}

# Objetos conservados para que las simulaciones beta posteriores funcionen sin
# cambios. Ahora a_sens y b_sens representan personas, no dos tests por persona.
sens_adenoma_total <- extraer("adenoma_total", "Mean")
li_sens_adenoma_total <- extraer("adenoma_total", "Lower_CI")
ls_sens_adenoma_total <- extraer("adenoma_total", "Upper_CI")
a_sens_adenoma_total <- extraer("adenoma_total", "Positives")
b_sens_adenoma_total <- extraer("adenoma_total", "N") - a_sens_adenoma_total

sens_small <- extraer("small", "Mean")
li_sens_small <- extraer("small", "Lower_CI")
ls_sens_small <- extraer("small", "Upper_CI")
a_sens_small <- extraer("small", "Positives")
b_sens_small <- extraer("small", "N") - a_sens_small

sens_medium <- extraer("medium", "Mean")
li_sens_medium <- extraer("medium", "Lower_CI")
ls_sens_medium <- extraer("medium", "Upper_CI")
a_sens_medium <- extraer("medium", "Positives")
b_sens_medium <- extraer("medium", "N") - a_sens_medium

sens_small_medium <- extraer("small_medium", "Mean")
li_sens_small_medium <- extraer("small_medium", "Lower_CI")
ls_sens_small_medium <- extraer("small_medium", "Upper_CI")
a_sens_small_medium <- extraer("small_medium", "Positives")
b_sens_small_medium <- extraer("small_medium", "N") - a_sens_small_medium

sens_large <- extraer("large", "Mean")
li_sens_large <- extraer("large", "Lower_CI")
ls_sens_large <- extraer("large", "Upper_CI")
a_sens_large <- extraer("large", "Positives")
b_sens_large <- extraer("large", "N") - a_sens_large

sens_low_risk <- extraer("low_risk", "Mean")
li_sens_low_risk <- extraer("low_risk", "Lower_CI")
ls_sens_low_risk <- extraer("low_risk", "Upper_CI")
a_sens_low_risk <- extraer("low_risk", "Positives")
b_sens_low_risk <- extraer("low_risk", "N") - a_sens_low_risk

sens_high_risk <- extraer("high_risk", "Mean")
li_sens_high_risk <- extraer("high_risk", "Lower_CI")
ls_sens_high_risk <- extraer("high_risk", "Upper_CI")
a_sens_high_risk <- extraer("high_risk", "Positives")
b_sens_high_risk <- extraer("high_risk", "N") - a_sens_high_risk

sens_ca <- extraer("cancer", "Mean")
li_sens_ca <- extraer("cancer", "Lower_CI")
ls_sens_ca <- extraer("cancer", "Upper_CI")
a_sens_ca <- extraer("cancer", "Positives")
b_sens_ca <- extraer("cancer", "N") - a_sens_ca

extraer_especificidad <- function(clave, columna) {
  detalle_especificidad[[columna]][match(clave, detalle_especificidad$clave)]
}

# Objetos equivalentes para especificidad. Para una distribucion beta,
# a_spec corresponde a verdaderos negativos y b_spec a falsos positivos.
spec_adenoma_total <- extraer_especificidad("adenoma_total", "Mean")
li_spec_adenoma_total <- extraer_especificidad("adenoma_total", "Lower_CI")
ls_spec_adenoma_total <- extraer_especificidad("adenoma_total", "Upper_CI")
a_spec_adenoma_total <- extraer_especificidad("adenoma_total", "True_Negatives")
b_spec_adenoma_total <- extraer_especificidad("adenoma_total", "False_Positives")

spec_small <- extraer_especificidad("small", "Mean")
li_spec_small <- extraer_especificidad("small", "Lower_CI")
ls_spec_small <- extraer_especificidad("small", "Upper_CI")
a_spec_small <- extraer_especificidad("small", "True_Negatives")
b_spec_small <- extraer_especificidad("small", "False_Positives")

spec_medium <- extraer_especificidad("medium", "Mean")
li_spec_medium <- extraer_especificidad("medium", "Lower_CI")
ls_spec_medium <- extraer_especificidad("medium", "Upper_CI")
a_spec_medium <- extraer_especificidad("medium", "True_Negatives")
b_spec_medium <- extraer_especificidad("medium", "False_Positives")

spec_small_medium <- extraer_especificidad("small_medium", "Mean")
li_spec_small_medium <- extraer_especificidad("small_medium", "Lower_CI")
ls_spec_small_medium <- extraer_especificidad("small_medium", "Upper_CI")
a_spec_small_medium <- extraer_especificidad("small_medium", "True_Negatives")
b_spec_small_medium <- extraer_especificidad("small_medium", "False_Positives")

spec_large <- extraer_especificidad("large", "Mean")
li_spec_large <- extraer_especificidad("large", "Lower_CI")
ls_spec_large <- extraer_especificidad("large", "Upper_CI")
a_spec_large <- extraer_especificidad("large", "True_Negatives")
b_spec_large <- extraer_especificidad("large", "False_Positives")

spec_low_risk <- extraer_especificidad("low_risk", "Mean")
li_spec_low_risk <- extraer_especificidad("low_risk", "Lower_CI")
ls_spec_low_risk <- extraer_especificidad("low_risk", "Upper_CI")
a_spec_low_risk <- extraer_especificidad("low_risk", "True_Negatives")
b_spec_low_risk <- extraer_especificidad("low_risk", "False_Positives")

spec_high_risk <- extraer_especificidad("high_risk", "Mean")
li_spec_high_risk <- extraer_especificidad("high_risk", "Lower_CI")
ls_spec_high_risk <- extraer_especificidad("high_risk", "Upper_CI")
a_spec_high_risk <- extraer_especificidad("high_risk", "True_Negatives")
b_spec_high_risk <- extraer_especificidad("high_risk", "False_Positives")

spec_ca <- extraer_especificidad("cancer", "Mean")
li_spec_ca <- extraer_especificidad("cancer", "Lower_CI")
ls_spec_ca <- extraer_especificidad("cancer", "Upper_CI")
a_spec_ca <- extraer_especificidad("cancer", "True_Negatives")
b_spec_ca <- extraer_especificidad("cancer", "False_Positives")

n_ad <- sum(BaseSens$adenomas == 1, na.rm = TRUE)
n_adenoma_total <- sum(casos$adenoma_total, na.rm = TRUE)
n_small <- sum(casos$small, na.rm = TRUE)
n_medium <- sum(casos$medium, na.rm = TRUE)
n_large <- sum(casos$large, na.rm = TRUE)
n_low_risk <- sum(casos$low_risk, na.rm = TRUE)
n_high_risk <- sum(casos$high_risk, na.rm = TRUE)
n_ca <- sum(casos$cancer, na.rm = TRUE)

# Comparacion con cada test por separado y con el calculo anterior que agrupaba
# ambos tests. Esta tabla es diagnostica; result_table sigue siendo la salida
# utilizada por el resto del analisis.
calcular_metodo <- function(clave, indicador_caso, metodo, fits) {
  indicador_caso[is.na(indicador_caso)] <- FALSE
  x <- sum(vapply(
    fits,
    function(fit) sum(indicador_caso & !is.na(fit) & fit == "Positivo"),
    numeric(1)
  ))
  n <- sum(vapply(
    fits,
    function(fit) sum(indicador_caso & !is.na(fit)),
    numeric(1)
  ))
  intervalo <- wilson(x, n)
  tibble(
    clave = clave,
    Category = unname(etiquetas[[clave]]),
    Method = metodo,
    Positives = x,
    N = n,
    Mean = unname(intervalo[["estimacion"]]),
    Lower_CI = unname(intervalo[["inferior"]]),
    Upper_CI = unname(intervalo[["superior"]])
  )
}

comparacion_sensibilidad <- bind_rows(lapply(names(casos), function(clave) {
  bind_rows(
    calcular_metodo(clave, casos[[clave]], "Test_1", list(BaseSens$FIT_1)),
    calcular_metodo(clave, casos[[clave]], "Test_2", list(BaseSens$FIT_2)),
    calcular_metodo(clave, casos[[clave]], "Promedio_anterior_2_tests", list(BaseSens$FIT_1, BaseSens$FIT_2)),
    calcular_metodo(clave, casos[[clave]], "Test_aleatorio", list(BaseSens$FIT_sorteado))
  )
}))

calcular_metodo_especificidad <- function(clave, estado_lesion, metodo, fits) {
  no_enfermo <- !is.na(estado_lesion) & estado_lesion == 0L
  verdaderos_negativos <- sum(vapply(
    fits,
    function(fit) sum(no_enfermo & !is.na(fit) & fit == "Negativo"),
    numeric(1)
  ))
  falsos_positivos <- sum(vapply(
    fits,
    function(fit) sum(no_enfermo & !is.na(fit) & fit == "Positivo"),
    numeric(1)
  ))
  n <- verdaderos_negativos + falsos_positivos
  intervalo <- wilson(verdaderos_negativos, n)
  tibble(
    clave = clave,
    Category = unname(etiquetas[[clave]]),
    Method = metodo,
    True_Negatives = verdaderos_negativos,
    False_Positives = falsos_positivos,
    N = n,
    Mean = unname(intervalo[["estimacion"]]),
    Lower_CI = unname(intervalo[["inferior"]]),
    Upper_CI = unname(intervalo[["superior"]])
  )
}

comparacion_especificidad <- bind_rows(lapply(names(estados_lesion), function(clave) {
  bind_rows(
    calcular_metodo_especificidad(clave, estados_lesion[[clave]], "Test_1", list(BaseSens$FIT_1)),
    calcular_metodo_especificidad(clave, estados_lesion[[clave]], "Test_2", list(BaseSens$FIT_2)),
    calcular_metodo_especificidad(
      clave,
      estados_lesion[[clave]],
      "Promedio_anterior_2_tests",
      list(BaseSens$FIT_1, BaseSens$FIT_2)
    ),
    calcular_metodo_especificidad(
      clave,
      estados_lesion[[clave]],
      "Test_aleatorio",
      list(BaseSens$FIT_sorteado)
    )
  )
}))

resumen_sorteo_sensibilidad <- tibble(
  indicador = c(
    "n_cohorte",
    "ambos_tests_observados",
    "solo_test_1_observado",
    "solo_test_2_observado",
    "ningun_test_observado",
    "seleccion_test_1",
    "seleccion_test_2"
  ),
  n = c(
    nrow(BaseSens),
    sum(!is.na(BaseSens$FIT_1) & !is.na(BaseSens$FIT_2)),
    sum(!is.na(BaseSens$FIT_1) & is.na(BaseSens$FIT_2)),
    sum(is.na(BaseSens$FIT_1) & !is.na(BaseSens$FIT_2)),
    sum(is.na(BaseSens$FIT_1) & is.na(BaseSens$FIT_2)),
    sum(BaseSens$test_sorteado == 1L, na.rm = TRUE),
    sum(BaseSens$test_sorteado == 2L, na.rm = TRUE)
  )
)

print(result_table)
print(specificity_result_table)
print(specificity_no_neoplasia_table)
print(resumen_sorteo_sensibilidad)
