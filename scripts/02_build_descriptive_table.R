# -*- coding: UTF-8 -*-

# Tabla descriptiva independiente del modelo de prevalencia -------------------
#
# Objetivo:
#   Describir la cohorte PRENEC con highrisk == 0, total y por sexo, sin
#   modificar ni reutilizar las bases agregadas A/B del modelo de prevalencia.
#
# Definiciones heredadas del proyecto:
#   * highrisk == 0 corresponde a Familiar_CACO == "NO".
#   * La cohorte analitica se restringe a Edad_2 entre 50 y 75 anos.
#   * La colonoscopia calificante ocurre en o despues de Fecha_1er_Colon_check
#     y antes de Fecha_2do_Colon_check; si no existe segunda fecha, no se aplica
#     limite superior.
#   * Las categorias de tamano e histologia reproducen literalmente el script
#     nuevas_bases_prenec_rondas_aleatorias.R.
#
# Diferencia deliberada respecto del modelo de prevalencia:
#   Esta tabla aplica literalmente el pedido FIT > 100. El modelo define un
#   FIT positivo como FIT >= 100. Los casos exactamente iguales a 100 se
#   informan en el archivo de validaciones para hacer visible la diferencia.
#
# Entradas:
#   Ocho archivos regionales protegidos indicados por PRENEC_DATA_DIR.
# Salidas:
#   Tabla numerica, tabla LaTeX y controles en PRENEC_TABLE_OUTPUT_DIR.
# Independencia:
#   Este script reconstruye la tabla desde los archivos regionales; no utiliza
#   las bases agregadas A/B del modelo.

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(lubridate)
  library(stringr)
})

script_directory <- function() {
  arguments <- commandArgs(trailingOnly = FALSE)
  file_argument <- grep("^--file=", arguments, value = TRUE)
  if (length(file_argument) == 0L) {
    return(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
  }
  dirname(normalizePath(
    sub("^--file=", "", file_argument[[1]]),
    winslash = "/",
    mustWork = TRUE
  ))
}

project_directory <- Sys.getenv(
  "PRENEC_PROJECT_ROOT",
  unset = normalizePath(
    file.path(script_directory(), ".."),
    winslash = "/",
    mustWork = TRUE
  )
)

RUTA_DATOS <- Sys.getenv(
  "PRENEC_DATA_DIR",
  unset = file.path(project_directory, "data", "raw")
)

RUTA_SALIDA <- Sys.getenv(
  "PRENEC_TABLE_OUTPUT_DIR",
  unset = file.path(project_directory, "results", "descriptive_table")
)

EDAD_MINIMA <- 50L
EDAD_MAXIMA <- 75L
UMBRAL_FIT_ESTRICTO <- 100

dir.create(RUTA_SALIDA, recursive = TRUE, showWarnings = FALSE)

archivo_salida <- function(nombre) file.path(RUTA_SALIDA, nombre)


# Funciones de carga -----------------------------------------------------------

normalizar_texto <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  str_to_upper(str_squish(x))
}

fecha_flexible <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, c("POSIXct", "POSIXlt"))) {
    return(as.Date(x))
  }
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1899-12-30"))
  }

  texto <- str_squish(as.character(x))
  texto[texto %in% c("", "NA", "N/A", "NULL")] <- NA_character_
  salida <- rep(as.Date(NA), length(texto))

  es_serial <- !is.na(texto) & str_detect(texto, "^[0-9]+(?:\\.[0-9]+)?$")
  if (any(es_serial)) {
    salida[es_serial] <- as.Date(
      suppressWarnings(as.numeric(texto[es_serial])),
      origin = "1899-12-30"
    )
  }

  por_parsear <- !is.na(texto) & !es_serial
  if (any(por_parsear)) {
    fecha_parseada <- suppressWarnings(parse_date_time(
      texto[por_parsear],
      orders = c(
        "ymd HMS", "ymd HM", "ymd",
        "dmy HMS", "dmy HM", "dmy",
        "mdy HMS", "mdy HM", "mdy"
      ),
      quiet = TRUE
    ))
    salida[por_parsear] <- as.Date(fecha_parseada)
  }

  salida
}

numero_flexible <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  texto <- str_replace_all(str_squish(as.character(x)), ",", ".")
  suppressWarnings(as.numeric(texto))
}

primera_columna <- function(nombres, patron, etiqueta, archivo) {
  candidatas <- nombres[str_detect(nombres, regex(patron, ignore_case = TRUE))]
  if (length(candidatas) == 0L) {
    stop("No se encontro ", etiqueta, " en ", basename(archivo), ".")
  }
  candidatas[[1L]]
}

cargar_region <- function(archivo) {
  # Se conserva la inferencia de tipos de readxl utilizada por el script
  # original. Las advertencias de celdas atipicas se silencian aqui, pero las
  # validaciones al final verifican que la cohorte y los desenlaces coincidan.
  base <- suppressWarnings(suppressMessages(read_excel(
    archivo,
    .name_repair = "unique_quiet"
  )))
  names(base) <- str_replace_all(names(base), " ", "_")

  nombre_boston <- primera_columna(
    names(base),
    "^Preparacion_Escala_Boston(?:\\.\\.\\.[0-9]+)?$",
    "la escala de Boston de la primera colonoscopia",
    archivo
  )
  nombre_alcance <- primera_columna(
    names(base),
    "^Alcance(?:\\.\\.\\.[0-9]+)?$",
    "el alcance de la primera colonoscopia",
    archivo
  )
  nombre_tiempo_retiro <- primera_columna(
    names(base),
    "^Tiempo_Retiro(?:\\.\\.\\.[0-9]+)?$",
    "el tiempo de retiro de la primera colonoscopia",
    archivo
  )
  nombre_retiro_8 <- primera_columna(
    names(base),
    "^Tiempo_Retiro_>_8_Min",
    "el indicador de tiempo de retiro de la primera colonoscopia",
    archivo
  )

  requeridas <- c(
    "Codigo", "Sexo", "Fecha_Nacimiento", "Fecha_Enrolamiento",
    "Valor_TSDO_1_1", "Valor_TSDO_2_1", "Familiar_CACO",
    "Fecha_1er_Colon_check", "Fecha_2do_Colon_check",
    "Fecha_1era_Colono", "Tipo_Histologico_1",
    "Tamano_Polipo_Mayor_1"
  )
  faltantes <- setdiff(requeridas, names(base))
  if (length(faltantes) > 0L) {
    stop(
      "Faltan columnas en ", basename(archivo), ": ",
      paste(faltantes, collapse = ", ")
    )
  }

  base %>%
    transmute(
      Codigo = as.character(Codigo),
      Sexo = as.character(Sexo),
      Fecha_Nacimiento = fecha_flexible(Fecha_Nacimiento),
      Fecha_Enrolamiento = fecha_flexible(Fecha_Enrolamiento),
      Valor_TSDO_1_1 = numero_flexible(Valor_TSDO_1_1),
      Valor_TSDO_2_1 = numero_flexible(Valor_TSDO_2_1),
      Familiar_CACO = as.character(Familiar_CACO),
      Fecha_1er_Colon_check = fecha_flexible(Fecha_1er_Colon_check),
      Fecha_2do_Colon_check = fecha_flexible(Fecha_2do_Colon_check),
      Fecha_1era_Colono = fecha_flexible(Fecha_1era_Colono),
      Tipo_Histologico_1 = as.character(Tipo_Histologico_1),
      Tamano_Polipo_Mayor_1 = as.character(Tamano_Polipo_Mayor_1),
      Boston_primera_colonoscopia = numero_flexible(.data[[nombre_boston]]),
      Alcance_primera_colonoscopia = as.character(.data[[nombre_alcance]]),
      Tiempo_retiro_primera_colonoscopia = numero_flexible(
        .data[[nombre_tiempo_retiro]]
      ),
      Retiro_8_primera_colonoscopia = as.character(.data[[nombre_retiro_8]]),
      origen = tools::file_path_sans_ext(basename(archivo))
    )
}

archivos <- list.files(
  RUTA_DATOS,
  pattern = "^[^~].*\\.xlsx$",
  full.names = TRUE
)
if (length(archivos) == 0L) {
  stop("No se encontraron archivos .xlsx en ", RUTA_DATOS, ".")
}

prenec <- map_dfr(archivos, cargar_region)


# Transformaciones que reproducen el modelo original --------------------------

tipos_interes <- c(
  "Adenocarcinoma intramucoso",
  "Adenoma tubular con atipia de alto grado",
  "Adenoma tubular con atipia de bajo grado",
  "Adenoma tubulo-velloso con atipia de alto grado",
  "Adenoma tubulo-velloso con atipia de bajo grado",
  "Adenoma velloso con atipia de alto grado"
)

tipos_histologia_avanzada <- c(
  "Adenocarcinoma intramucoso",
  "Adenoma tubular con atipia de alto grado",
  "Adenoma tubulo-velloso con atipia de alto grado",
  "Adenoma tubulo-velloso con atipia de bajo grado",
  "Adenoma velloso con atipia de alto grado"
)

tipos_cancer <- c(
  "Adenocarcinoma bien diferenciado",
  "Adenocarcinoma moderadamente diferenciado",
  "Adenocarcinoma pobremente diferenciado",
  "Otros tipo de carcinoma",
  "Neoplasia No Epitelial"
)

prenec <- prenec %>%
  mutate(
    Fecha_Nacimiento_Corregida = case_when(
      Fecha_Nacimiento > Sys.Date() ~ as.Date(NA),
      TRUE ~ Fecha_Nacimiento
    ),
    Fecha_Nacimiento_Corregida = case_when(
      Fecha_Nacimiento_Corregida > as.Date("2013-01-01") |
        Fecha_Nacimiento_Corregida < as.Date("1922-01-01") ~ as.Date(NA),
      TRUE ~ Fecha_Nacimiento_Corregida
    ),
    Edad_2 = year(as.period(interval(
      Fecha_Nacimiento_Corregida,
      Fecha_Enrolamiento
    ))),
    Sexo_analisis = case_when(
      normalizar_texto(Sexo) %in% c("FEMENINO", "MUJER", "FEMALE", "F") ~ "Mujeres",
      normalizar_texto(Sexo) %in% c("MASCULINO", "HOMBRE", "MALE", "M") ~ "Hombres",
      TRUE ~ NA_character_
    ),
    highrisk = case_when(
      normalizar_texto(Familiar_CACO) == "NO" ~ 0L,
      normalizar_texto(Familiar_CACO) == "SI" ~ 1L,
      TRUE ~ NA_integer_
    ),
    Tamano_Polipo_Mayor_1 = str_replace_all(
      Tamano_Polipo_Mayor_1,
      "pequeÃ±o",
      "pequeño"
    ),
    Tamano_texto_pequeno = normalizar_texto(Tamano_Polipo_Mayor_1) %in%
      c("DIMINUTO", "DIMINUTOS", "PEQUENO", "MENOR DE 5 MM"),
    Tamano_Polipo_Mayor_1 = case_when(
      Tamano_texto_pequeno ~ "Pequeño",
      TRUE ~ Tamano_Polipo_Mayor_1
    ),
    Tamano_Polipo_Limpio = str_replace_all(Tamano_Polipo_Mayor_1, ",", "."),
    Tamano_Numerico = suppressWarnings(as.numeric(
      str_extract(Tamano_Polipo_Limpio, "\\d+(\\.\\d+)?")
    )),
    Unidad = case_when(
      str_detect(str_to_lower(Tamano_Polipo_Limpio), "cm") ~ "cm",
      str_detect(str_to_lower(Tamano_Polipo_Limpio), "mm") ~ "mm",
      TRUE ~ "desconocido"
    ),
    Tamano_mm = case_when(
      Unidad == "cm" ~ Tamano_Numerico * 10,
      Unidad == "mm" ~ Tamano_Numerico,
      Unidad == "desconocido" & !is.na(Tamano_Numerico) ~ Tamano_Numerico,
      TRUE ~ NA_real_
    ),
    Tamano_mm = case_when(
      Tamano_Polipo_Mayor_1 == "5x7 mm" ~ 7,
      Tamano_Polipo_Mayor_1 == "7x10 mm" ~ 10,
      TRUE ~ Tamano_mm
    ),
    adenoma_size = case_when(
      Tipo_Histologico_1 %in% tipos_interes & Tamano_mm <= 5 ~ "pequeño",
      Tipo_Histologico_1 %in% tipos_interes & Tamano_mm >= 6 & Tamano_mm <= 9 ~ "mediano",
      Tipo_Histologico_1 %in% tipos_interes & Tamano_mm >= 10 ~ "grande",
      Tipo_Histologico_1 %in% tipos_interes & Tamano_texto_pequeno ~ "pequeño",
      TRUE ~ NA_character_
    ),
    risk = case_when(
      Tamano_mm < 10 &
        Tipo_Histologico_1 == "Adenoma tubular con atipia de bajo grado" ~ "low_risk",
      Tamano_texto_pequeno &
        Tipo_Histologico_1 == "Adenoma tubular con atipia de bajo grado" ~ "low_risk",
      Tamano_mm >= 10 & Tipo_Histologico_1 %in% tipos_interes ~ "high_risk",
      Tamano_mm < 10 & Tipo_Histologico_1 %in% tipos_histologia_avanzada ~ "high_risk",
      Tipo_Histologico_1 %in% tipos_cancer ~ "cancer",
      TRUE ~ NA_character_
    ),
    Indicador_Fecha_Colono = case_when(
      !is.na(Fecha_1era_Colono) &
        Fecha_1era_Colono >= Fecha_1er_Colon_check &
        (is.na(Fecha_2do_Colon_check) |
           Fecha_1era_Colono < Fecha_2do_Colon_check) ~ Fecha_1era_Colono,
      TRUE ~ as.Date(NA)
    ),
    Ronda_1_modelo = case_when(
      Valor_TSDO_1_1 >= 100 ~ "Positivo",
      Valor_TSDO_1_1 < 100 ~ "Negativo",
      TRUE ~ NA_character_
    ),
    Ronda_2_modelo = case_when(
      Valor_TSDO_2_1 >= 100 ~ "Positivo",
      Valor_TSDO_2_1 < 100 ~ "Negativo",
      TRUE ~ NA_character_
    )
  )

cohorte_highrisk0_completa <- prenec %>% filter(highrisk == 0L)

cohorte <- cohorte_highrisk0_completa %>%
  filter(
    !is.na(Edad_2),
    Edad_2 >= EDAD_MINIMA,
    Edad_2 <= EDAD_MAXIMA
  ) %>%
  mutate(
    FIT1_mayor_100 = !is.na(Valor_TSDO_1_1) & Valor_TSDO_1_1 > UMBRAL_FIT_ESTRICTO,
    FIT2_mayor_100 = !is.na(Valor_TSDO_2_1) & Valor_TSDO_2_1 > UMBRAL_FIT_ESTRICTO,
    FIT_ambos_observados = !is.na(Valor_TSDO_1_1) & !is.na(Valor_TSDO_2_1),
    colonoscopia_ventana = !is.na(Indicador_Fecha_Colono),
    preparacion_adecuada = colonoscopia_ventana &
      Boston_primera_colonoscopia %in% c(8, 9),
    alcance_ciego = colonoscopia_ventana &
      normalizar_texto(Alcance_primera_colonoscopia) == "CIEGO",
    retiro_8_documentado = colonoscopia_ventana &
      normalizar_texto(Retiro_8_primera_colonoscopia) %in% c("SI", "NO"),
    retiro_8_o_mas = colonoscopia_ventana &
      normalizar_texto(Retiro_8_primera_colonoscopia) == "SI",
    FIT_cualquiera_positivo =
      Ronda_1_modelo == "Positivo" | Ronda_2_modelo == "Positivo",
    adenoma_pequeno = colonoscopia_ventana & adenoma_size == "pequeño",
    adenoma_mediano = colonoscopia_ventana & adenoma_size == "mediano",
    adenoma_grande = colonoscopia_ventana & adenoma_size == "grande",
    adenoma_no_avanzado = colonoscopia_ventana & risk == "low_risk",
    adenoma_avanzado = colonoscopia_ventana & risk == "high_risk",
    cancer = colonoscopia_ventana & risk == "cancer"
  )


# Estadisticas y formato -------------------------------------------------------

media_ic_t <- function(x, confianza = 0.95) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n == 0L) {
    return(c(n = 0, estimate = NA, lower = NA, upper = NA))
  }
  estimador <- mean(x)
  if (n == 1L) {
    return(c(n = n, estimate = estimador, lower = NA, upper = NA))
  }
  error <- qt(1 - (1 - confianza) / 2, df = n - 1) * sd(x) / sqrt(n)
  c(
    n = n,
    estimate = estimador,
    lower = estimador - error,
    upper = estimador + error
  )
}

wilson_ic <- function(numerador, denominador, confianza = 0.95) {
  if (is.na(denominador) || denominador <= 0) {
    return(c(estimate = NA, lower = NA, upper = NA))
  }
  z <- qnorm(1 - (1 - confianza) / 2)
  p <- numerador / denominador
  centro <- (p + z^2 / (2 * denominador)) / (1 + z^2 / denominador)
  semi <- z * sqrt(
    p * (1 - p) / denominador + z^2 / (4 * denominador^2)
  ) / (1 + z^2 / denominador)
  c(estimate = p, lower = max(0, centro - semi), upper = min(1, centro + semi))
}

registro_media <- function(grupo, seccion, indicador, x, unidad) {
  estad <- media_ic_t(x)
  tibble(
    grupo = grupo,
    seccion = seccion,
    indicador = indicador,
    tipo = "mean_t_ci95",
    numerador = NA_real_,
    denominador = unname(estad[["n"]]),
    estimador = unname(estad[["estimate"]]),
    ic95_inferior = unname(estad[["lower"]]),
    ic95_superior = unname(estad[["upper"]]),
    unidad = unidad
  )
}

registro_proporcion <- function(grupo, seccion, indicador, numerador, denominador) {
  estad <- wilson_ic(numerador, denominador)
  tibble(
    grupo = grupo,
    seccion = seccion,
    indicador = indicador,
    tipo = "proportion_wilson_ci95",
    numerador = as.numeric(numerador),
    denominador = as.numeric(denominador),
    estimador = unname(estad[["estimate"]]),
    ic95_inferior = unname(estad[["lower"]]),
    ic95_superior = unname(estad[["upper"]]),
    unidad = "proportion"
  )
}

registro_conteo <- function(grupo, seccion, indicador, valor) {
  tibble(
    grupo = grupo,
    seccion = seccion,
    indicador = indicador,
    tipo = "count",
    numerador = as.numeric(valor),
    denominador = NA_real_,
    estimador = as.numeric(valor),
    ic95_inferior = NA_real_,
    ic95_superior = NA_real_,
    unidad = "people"
  )
}

resumir_grupo <- function(datos, grupo) {
  n_total <- nrow(datos)
  n_fit1_observado <- sum(!is.na(datos$Valor_TSDO_1_1))
  n_fit2_observado <- sum(!is.na(datos$Valor_TSDO_2_1))
  n_colonoscopias <- sum(datos$colonoscopia_ventana, na.rm = TRUE)
  n_fit_cualquiera_positivo <- sum(datos$FIT_cualquiera_positivo, na.rm = TRUE)
  n_retiro_documentado <- sum(datos$retiro_8_documentado, na.rm = TRUE)

  bind_rows(
    registro_conteo(grupo, "Participants", "Participants, n", n_total),
    registro_media(
      grupo, "Participants", "Age, years, mean (95% CI)",
      datos$Edad_2, "years"
    ),
    registro_proporcion(
      grupo, "FIT", "FIT concentration >100 ng/mL, round 1, n/N (%)",
      sum(datos$FIT1_mayor_100, na.rm = TRUE), n_fit1_observado
    ),
    registro_proporcion(
      grupo, "FIT", "FIT concentration >100 ng/mL, round 2, n/N (%)",
      sum(datos$FIT2_mayor_100, na.rm = TRUE), n_fit2_observado
    ),
    registro_media(
      grupo, "FIT", "FIT concentration, round 1, ng/mL, mean (95% CI)",
      datos$Valor_TSDO_1_1, "ng/mL"
    ),
    registro_media(
      grupo, "FIT", "FIT concentration, round 2, ng/mL, mean (95% CI)",
      datos$Valor_TSDO_2_1, "ng/mL"
    ),
    registro_proporcion(
      grupo, "FIT", "Completion of both FIT rounds, n/N (%)",
      sum(datos$FIT_ambos_observados, na.rm = TRUE), n_total
    ),
    registro_proporcion(
      grupo,
      "Colonoscopy participation",
      "Colonoscopy participation after any positive FIT result, n/N (%)",
      sum(datos$FIT_cualquiera_positivo & datos$colonoscopia_ventana, na.rm = TRUE),
      n_fit_cualquiera_positivo
    ),
    registro_proporcion(
      grupo, "Colonoscopy quality", "Adequate bowel preparation (Boston score 8–9), n/N (%)",
      sum(datos$preparacion_adecuada, na.rm = TRUE), n_colonoscopias
    ),
    registro_proporcion(
      grupo, "Colonoscopy quality", "Caecal intubation, n/N (%)",
      sum(datos$alcance_ciego, na.rm = TRUE), n_colonoscopias
    ),
    registro_proporcion(
      grupo, "Colonoscopy quality", "Withdrawal time ≥8 min in the first colonoscopy, n/N (%)",
      sum(datos$retiro_8_o_mas, na.rm = TRUE), n_retiro_documentado
    ),
    registro_proporcion(
      grupo, "Lesion detection", "Small adenoma (≤5 mm), n/N (%)",
      sum(datos$adenoma_pequeno, na.rm = TRUE), n_colonoscopias
    ),
    registro_proporcion(
      grupo, "Lesion detection", "Medium-sized adenoma (6–9 mm), n/N (%)",
      sum(datos$adenoma_mediano, na.rm = TRUE), n_colonoscopias
    ),
    registro_proporcion(
      grupo, "Lesion detection", "Large adenoma (≥10 mm), n/N (%)",
      sum(datos$adenoma_grande, na.rm = TRUE), n_colonoscopias
    ),
    registro_proporcion(
      grupo, "Lesion detection", "Non-advanced adenoma, n/N (%)",
      sum(datos$adenoma_no_avanzado, na.rm = TRUE), n_colonoscopias
    ),
    registro_proporcion(
      grupo, "Lesion detection", "Advanced adenoma, n/N (%)",
      sum(datos$adenoma_avanzado, na.rm = TRUE), n_colonoscopias
    ),
    registro_proporcion(
      grupo, "Lesion detection", "Colorectal cancer, n/N (%)",
      sum(datos$cancer, na.rm = TRUE), n_colonoscopias
    )
  )
}

resultados_numericos <- bind_rows(
  resumir_grupo(cohorte, "Both sexes"),
  resumir_grupo(filter(cohorte, Sexo_analisis == "Mujeres"), "Women"),
  resumir_grupo(filter(cohorte, Sexo_analisis == "Hombres"), "Men")
) %>%
  mutate(
    grupo = factor(grupo, levels = c("Both sexes", "Women", "Men")),
    orden = match(
      indicador,
      unique(indicador[grupo == "Both sexes"])
    )
  ) %>%
  arrange(orden, grupo) %>%
  mutate(grupo = as.character(grupo))


# Publication-facing table in English -----------------------------------------

formato_entero_en <- function(x) {
  ifelse(
    is.na(x),
    "··",
    formatC(x, format = "f", digits = 0, decimal.mark = ".", big.mark = ",")
  )
}

formato_decimal_en <- function(x, digitos = 1) {
  ifelse(
    is.na(x),
    "··",
    formatC(
      x,
      format = "f",
      digits = digitos,
      decimal.mark = ".",
      big.mark = ","
    )
  )
}

formatear_resultado_en <- function(tipo, numerador, denominador,
                                   estimador, inferior, superior) {
  if (tipo == "count") {
    return(formato_entero_en(estimador))
  }
  if (tipo == "mean_t_ci95") {
    return(paste0(
      formato_decimal_en(estimador), " (",
      formato_decimal_en(inferior), "–",
      formato_decimal_en(superior), ")"
    ))
  }
  paste0(
    formato_entero_en(numerador), "/",
    formato_entero_en(denominador), " (",
    formato_decimal_en(100 * estimador), "%)"
  )
}

tabla_formateada_larga <- resultados_numericos %>%
  rowwise() %>%
  mutate(valor = formatear_resultado_en(
    tipo, numerador, denominador, estimador, ic95_inferior, ic95_superior
  )) %>%
  ungroup()

tabla_principal <- tabla_formateada_larga %>%
  select(orden, seccion, indicador, grupo, valor) %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  arrange(orden) %>%
  select(
    Section = seccion,
    Characteristic = indicador,
    `Both sexes`, Women, Men
  )


# Validaciones ----------------------------------------------------------------

n_highrisk0_completa <- nrow(cohorte_highrisk0_completa)
n_analitica <- nrow(cohorte)
n_mujeres <- sum(cohorte$Sexo_analisis == "Mujeres", na.rm = TRUE)
n_hombres <- sum(cohorte$Sexo_analisis == "Hombres", na.rm = TRUE)
n_sexo_no_clasificado <- sum(is.na(cohorte$Sexo_analisis))
n_colonoscopias <- sum(cohorte$colonoscopia_ventana, na.rm = TRUE)
n_boston_observado <- sum(
  cohorte$colonoscopia_ventana & !is.na(cohorte$Boston_primera_colonoscopia)
)
n_alcance_observado <- sum(
  cohorte$colonoscopia_ventana & !is.na(cohorte$Alcance_primera_colonoscopia)
)
n_retiro_observado <- sum(cohorte$retiro_8_documentado, na.rm = TRUE)
n_retiro_8_o_mas <- sum(cohorte$retiro_8_o_mas, na.rm = TRUE)
n_fit_cualquiera_positivo <- sum(cohorte$FIT_cualquiera_positivo, na.rm = TRUE)
n_colono_fit_cualquiera_positivo <- sum(
  cohorte$FIT_cualquiera_positivo & cohorte$colonoscopia_ventana,
  na.rm = TRUE
)
n_tamano <- sum(cohorte$adenoma_pequeno, na.rm = TRUE) +
  sum(cohorte$adenoma_mediano, na.rm = TRUE) +
  sum(cohorte$adenoma_grande, na.rm = TRUE)
n_riesgo <- sum(cohorte$adenoma_no_avanzado, na.rm = TRUE) +
  sum(cohorte$adenoma_avanzado, na.rm = TRUE)

validaciones <- tibble(
  validacion = c(
    "Regional files loaded",
    "highrisk=0 records before age restriction",
    "highrisk=0 records aged 50–75 years",
    "Women + men = analytic cohort",
    "Unclassified sex in analytic cohort",
    "Qualifying colonoscopies",
    "Recorded Boston score among qualifying colonoscopies",
    "Recorded caecal-intubation status among qualifying colonoscopies",
    "Recorded withdrawal-time criterion among qualifying colonoscopies",
    "Withdrawal time coded as at least 8 min",
    "FIT round 1 observed",
    "FIT round 2 observed",
    "Both FIT rounds observed",
    "FIT round 1 exactly 100",
    "FIT round 2 exactly 100",
    "At least one positive FIT result",
    "Qualifying colonoscopy after any positive FIT result",
    "Small adenomas detected",
    "Medium-sized adenomas detected",
    "Large adenomas detected",
    "Non-advanced adenomas detected",
    "Advanced adenomas detected",
    "Colorectal cancers detected",
    "Size partition = risk partition",
    "Maximum number of records per participant code"
  ),
  valor = c(
    length(archivos),
    n_highrisk0_completa,
    n_analitica,
    n_mujeres + n_hombres,
    n_sexo_no_clasificado,
    n_colonoscopias,
    n_boston_observado,
    n_alcance_observado,
    n_retiro_observado,
    n_retiro_8_o_mas,
    sum(!is.na(cohorte$Valor_TSDO_1_1)),
    sum(!is.na(cohorte$Valor_TSDO_2_1)),
    sum(cohorte$FIT_ambos_observados),
    sum(cohorte$Valor_TSDO_1_1 == 100, na.rm = TRUE),
    sum(cohorte$Valor_TSDO_2_1 == 100, na.rm = TRUE),
    n_fit_cualquiera_positivo,
    n_colono_fit_cualquiera_positivo,
    sum(cohorte$adenoma_pequeno, na.rm = TRUE),
    sum(cohorte$adenoma_mediano, na.rm = TRUE),
    sum(cohorte$adenoma_grande, na.rm = TRUE),
    sum(cohorte$adenoma_no_avanzado, na.rm = TRUE),
    sum(cohorte$adenoma_avanzado, na.rm = TRUE),
    sum(cohorte$cancer, na.rm = TRUE),
    n_tamano == n_riesgo,
    max(table(prenec$Codigo))
  ),
  esperado = c(
    "8",
    "34311",
    "34241",
    "34241",
    "0",
    "3434",
    "3434",
    "3434",
    "3434",
    "3099",
    "31102",
    "31097",
    "31095",
    "20",
    "16",
    "4087",
    "3433",
    "484",
    "386",
    "512",
    "623",
    "759",
    "83",
    "1",
    "1"
  )
) %>%
  mutate(
    valor = as.character(valor),
    estado = if_else(valor == esperado, "OK", "REVIEW")
  )

if (any(validaciones$estado != "OK")) {
  print(validaciones)
  stop("One or more validation checks do not match the current cohort.")
}


# Exportaciones tabulares ------------------------------------------------------

resultados_numericos_export <- resultados_numericos %>%
  transmute(
    group = grupo,
    section = seccion,
    characteristic = indicador,
    statistic_type = tipo,
    numerator = numerador,
    denominator = denominador,
    estimate = estimador,
    ci95_lower = ic95_inferior,
    ci95_upper = ic95_superior,
    unit = unidad,
    order = orden
  )

validaciones_export <- validaciones %>%
  transmute(
    check = validacion,
    value = valor,
    expected = esperado,
    status = estado
  )

write.table(
  tabla_principal,
  archivo_salida("tabla_descriptiva_highrisk0.tsv"),
  sep = "\t", row.names = FALSE, quote = FALSE, na = ""
)

write.table(
  resultados_numericos_export,
  archivo_salida("resultados_numericos_highrisk0.tsv"),
  sep = "\t", row.names = FALSE, quote = FALSE, na = ""
)

write.table(
  validaciones_export,
  archivo_salida("validaciones_highrisk0.tsv"),
  sep = "\t", row.names = FALSE, quote = FALSE, na = ""
)


# Codigo LaTeX estilo Lancet ---------------------------------------------------
# Este bloque se mantiene al final del script para que la salida .tex sea la
# ultima salida analitica generada, tal como se solicito.

formato_entero_tex <- function(x) {
  if (is.na(x)) {
    return("\\nodata")
  }
  gsub(",", "\\\\,", formatC(x, format = "f", digits = 0, big.mark = ","))
}

formato_decimal_tex <- function(x, digitos = 1) {
  if (is.na(x)) {
    return("\\nodata")
  }
  texto <- formatC(x, format = "f", digits = digitos, decimal.mark = ".")
  partes <- strsplit(texto, ".", fixed = TRUE)[[1L]]
  paste(partes, collapse = paste0("\\", "dec{}"))
}

formatear_resultado_tex <- function(tipo, numerador, denominador,
                                    estimador, inferior, superior) {
  if (tipo == "count") {
    return(formato_entero_tex(estimador))
  }
  if (tipo == "mean_t_ci95") {
    return(paste0(
      formato_decimal_tex(estimador), " (",
      formato_decimal_tex(inferior), "--",
      formato_decimal_tex(superior), ")"
    ))
  }
  paste0(
    formato_entero_tex(numerador), "/",
    formato_entero_tex(denominador), " (",
    formato_decimal_tex(100 * estimador), "\\%)"
  )
}

etiquetas_tex <- c(
  "Participants, n" = "Participants, n",
  "Age, years, mean (95% CI)" = "Age, years, mean (95\\% CI)",
  "FIT concentration >100 ng/mL, round 1, n/N (%)" = "FIT concentration $>100$ ng/mL, round 1, n/N (\\%)",
  "FIT concentration >100 ng/mL, round 2, n/N (%)" = "FIT concentration $>100$ ng/mL, round 2, n/N (\\%)",
  "FIT concentration, round 1, ng/mL, mean (95% CI)" = "FIT concentration, round 1, ng/mL, mean (95\\% CI)",
  "FIT concentration, round 2, ng/mL, mean (95% CI)" = "FIT concentration, round 2, ng/mL, mean (95\\% CI)",
  "Completion of both FIT rounds, n/N (%)" = "Completion of both FIT rounds, n/N (\\%)",
  "Colonoscopy participation after any positive FIT result, n/N (%)" = "Colonoscopy participation after any positive FIT result, n/N (\\%)",
  "Adequate bowel preparation (Boston score 8–9), n/N (%)" = "Adequate bowel preparation (Boston score 8--9), n/N (\\%)",
  "Caecal intubation, n/N (%)" = "Caecal intubation, n/N (\\%)",
  "Withdrawal time ≥8 min in the first colonoscopy, n/N (%)" = "Withdrawal time $\\geq8$ min in the first colonoscopy, n/N (\\%)",
  "Small adenoma (≤5 mm), n/N (%)" = "Small adenoma ($\\leq5$ mm), n/N (\\%)",
  "Medium-sized adenoma (6–9 mm), n/N (%)" = "Medium-sized adenoma (6--9 mm), n/N (\\%)",
  "Large adenoma (≥10 mm), n/N (%)" = "Large adenoma ($\\geq10$ mm), n/N (\\%)",
  "Non-advanced adenoma, n/N (%)" = "Non-advanced adenoma, n/N (\\%)",
  "Advanced adenoma, n/N (%)" = "Advanced adenoma, n/N (\\%)",
  "Colorectal cancer, n/N (%)" = "Colorectal cancer, n/N (\\%)"
)

tabla_tex_larga <- resultados_numericos %>%
  rowwise() %>%
  mutate(valor_tex = formatear_resultado_tex(
    tipo, numerador, denominador, estimador, ic95_inferior, ic95_superior
  )) %>%
  ungroup()

tabla_tex <- tabla_tex_larga %>%
  select(orden, seccion, indicador, grupo, valor_tex) %>%
  pivot_wider(names_from = grupo, values_from = valor_tex) %>%
  arrange(orden)

lineas_cuerpo <- character()
seccion_anterior <- NA_character_
for (i in seq_len(nrow(tabla_tex))) {
  seccion_actual <- tabla_tex$seccion[[i]]
  if (!identical(seccion_actual, seccion_anterior)) {
    if (length(lineas_cuerpo) > 0L) {
      lineas_cuerpo <- c(lineas_cuerpo, "\\addlinespace")
    }
    lineas_cuerpo <- c(
      lineas_cuerpo,
      paste0("\\textit{\\textbf{", seccion_actual, "}} & & & \\\\")
    )
  }
  lineas_cuerpo <- c(
    lineas_cuerpo,
    paste0(
      unname(etiquetas_tex[[tabla_tex$indicador[[i]]]]), " & ",
      tabla_tex[["Both sexes"]][[i]], " & ",
      tabla_tex[["Women"]][[i]], " & ",
      tabla_tex[["Men"]][[i]], " \\\\")
  )
  seccion_anterior <- seccion_actual
}

codigo_latex <- c(
  "\\documentclass[10pt]{article}",
  "\\usepackage[a4paper,margin=18mm]{geometry}",
  "\\usepackage[T1]{fontenc}",
  "\\usepackage{newtxtext,newtxmath}",
  "\\usepackage{microtype}",
  "\\usepackage{booktabs,threeparttable,tabularx,array}",
  "\\usepackage[font=small,labelfont=bf,justification=raggedright,singlelinecheck=false]{caption}",
  "\\newcolumntype{C}{>{\\centering\\arraybackslash}p{0.17\\linewidth}}",
  "\\newcommand{\\dec}{\\textperiodcentered}",
  "\\newcommand{\\nodata}{\\textperiodcentered\\textperiodcentered}",
  "",
  "\\begin{document}",
  "\\begin{table}[p]",
  "\\centering",
  paste0(
    "\\caption{Screening characteristics and colonoscopy outcomes among PRENEC participants aged ",
    EDAD_MINIMA, "--", EDAD_MAXIMA,
    " years without a reported family history of colorectal cancer}"
  ),
  "\\label{tab:screening_colonoscopy_no_family_history}",
  "\\begin{threeparttable}",
  "\\scriptsize",
  "\\setlength{\\tabcolsep}{3pt}",
  "\\renewcommand{\\arraystretch}{1.04}",
  "\\begin{tabularx}{\\linewidth}{@{}>{\\raggedright\\arraybackslash}X C C C@{}}",
  "\\toprule",
  " & \\textbf{Both sexes} & \\textbf{Women} & \\textbf{Men} \\\\",
  "\\midrule",
  lineas_cuerpo,
  "\\bottomrule",
  "\\end{tabularx}",
  "\\begin{tablenotes}[flushleft]",
  "\\scriptsize",
  paste0(
    "\\item[] Data are n/N (\\%) or mean (95\\% CI), unless otherwise indicated. ",
    "The analysis was restricted to participants aged ", EDAD_MINIMA, "--", EDAD_MAXIMA,
    " years without a reported family history of colorectal cancer."
  ),
  paste0(
    "\\item[] FIT concentration greater than ", UMBRAL_FIT_ESTRICTO,
    " ng/mL was used for the round-specific percentages; their denominator was the number ",
    "with a recorded result in that round. Completion of both FIT rounds required a recorded ",
    "value in each round and used the full analytic cohort as the denominator."
  ),
  paste0(
    "\\item[] Colonoscopy participation was the proportion with a qualifying first colonoscopy ",
    "among participants with at least one positive FIT result (concentration $\\geq100$ ng/mL) ",
    "in rounds 1 or 2. A qualifying colonoscopy occurred from the first colonoscopy-check date ",
    "(inclusive) to the second colonoscopy-check date (exclusive); when the second date was ",
    "missing, no upper time boundary was applied."
  ),
  paste0(
    "\\item[] Colonoscopy quality indicators refer to the first qualifying colonoscopy. Their ",
    "denominator was the number of qualifying colonoscopies with a recorded value. The source ",
    "withdrawal-time field is labelled $>8$ min, but records lasting exactly 8 min are coded as ",
    "meeting the criterion; the table therefore reports the observed coding as $\\geq8$ min. ",
    "Lesion-detection estimates used all qualifying colonoscopies as the denominator."
  ),
  paste0(
    "\\item[] Small, medium-sized, and large adenomas were defined as $\\leq5$ mm, 6--9 mm, ",
    "and $\\geq10$ mm, respectively. Size and risk classifications were based on the largest ",
    "recorded polyp and are overlapping classification systems; estimates should not be summed ",
    "across systems."
  ),
  "\\item[] FIT=faecal immunochemical test. CI=confidence interval.",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}",
  "\\end{document}"
)

writeLines(
  codigo_latex,
  con = archivo_salida("tabla_descriptiva_highrisk0_lancet.tex"),
  useBytes = TRUE
)
