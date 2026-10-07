# -*- coding: UTF-8 -*-

# Stage 05: colonoscopy-participation quality control -------------------------
# Reestima la adherencia a colonoscopia a partir de las bases A y B ya
# construidas. Este archivo NO modifica el modelo de prevalencia.
#
# Definicion:
#   adherencia = personas con FIT positivo y primera colonoscopia dentro de la
#                ventana del primer Colon Check / personas con FIT positivo
#
# La variable colonoscopy_n de las bases A/B ya incorpora exactamente la
# condicion !is.na(Indicador_Fecha_Colono), donde la primera colonoscopia debe
# ocurrir desde el primer Colon Check (inclusive) y antes del segundo Colon
# Check (exclusivo), cuando este existe.

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
})

obtener_directorio_script <- function() {
  argumentos <- commandArgs(trailingOnly = FALSE)
  argumento_archivo <- grep("^--file=", argumentos, value = TRUE)
  if (length(argumento_archivo) == 0L) {
    return(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
  }
  dirname(normalizePath(
    sub("^--file=", "", argumento_archivo[[1]]),
    winslash = "/",
    mustWork = TRUE
  ))
}

directorio_script <- obtener_directorio_script()
directorio_proyecto <- Sys.getenv(
  "PRENEC_PROJECT_ROOT",
  unset = normalizePath(
    file.path(directorio_script, ".."),
    winslash = "/",
    mustWork = TRUE
  )
)
semilla_bases <- Sys.getenv("PRENEC_BASE_SEED", unset = "20260923")
directorio_bases <- Sys.getenv(
  "PRENEC_BASE_DIR",
  unset = file.path(directorio_proyecto, "data", "intermediate")
)
directorio_salida <- Sys.getenv(
  "PRENEC_ADHERENCE_OUTPUT_DIR",
  unset = file.path(directorio_proyecto, "results", "quality_control", "adherence")
)
dir.create(directorio_salida, recursive = TRUE, showWarnings = FALSE)

archivos <- data.frame(
  Base = rep(c("A", "B"), each = 3L),
  Sexo = rep(c("Ambos sexos", "Hombres", "Mujeres"), times = 2L),
  sufijo = rep(c("", "_hombres", "_mujeres"), times = 2L),
  stringsAsFactors = FALSE
)
archivos$ruta <- file.path(
  directorio_bases,
  paste0(
    "Base_final", archivos$Base,
    "_N_aleatoria", archivos$sufijo,
    "_cohorte_actual_semilla_", semilla_bases, ".xlsx"
  )
)

faltantes <- archivos$ruta[!file.exists(archivos$ruta)]
if (length(faltantes) > 0L) {
  stop(
    "No se encontraron las bases requeridas:\n",
    paste(faltantes, collapse = "\n"),
    call. = FALSE
  )
}

columnas_requeridas <- c(
  "Edad_2", "pop", "fit_observed_n", "fit_positive_n", "colonoscopy_n"
)

leer_base <- function(ruta, base_id, sexo) {
  datos <- as.data.frame(readxl::read_excel(ruta), stringsAsFactors = FALSE)
  columnas_faltantes <- setdiff(columnas_requeridas, names(datos))
  if (length(columnas_faltantes) > 0L) {
    stop(
      basename(ruta), " no contiene: ",
      paste(columnas_faltantes, collapse = ", "),
      call. = FALSE
    )
  }

  datos %>%
    transmute(
      Base = base_id,
      Sexo = sexo,
      Edad = suppressWarnings(as.integer(Edad_2)),
      Poblacion = suppressWarnings(as.numeric(pop)),
      FIT_observado = suppressWarnings(as.numeric(fit_observed_n)),
      FIT_positivo = suppressWarnings(as.numeric(fit_positive_n)),
      Colonoscopia_ventana = suppressWarnings(as.numeric(colonoscopy_n)),
      Archivo_fuente = basename(ruta)
    )
}

datos_por_edad <- bind_rows(lapply(seq_len(nrow(archivos)), function(i) {
  leer_base(archivos$ruta[[i]], archivos$Base[[i]], archivos$Sexo[[i]])
}))

columnas_conteo <- c(
  "Poblacion", "FIT_observado", "FIT_positivo", "Colonoscopia_ventana"
)
if (anyNA(datos_por_edad[, columnas_conteo])) {
  stop("Existen conteos faltantes en las bases de entrada.", call. = FALSE)
}
if (any(as.matrix(datos_por_edad[, columnas_conteo]) < 0)) {
  stop("Existen conteos negativos en las bases de entrada.", call. = FALSE)
}
if (any(datos_por_edad$Colonoscopia_ventana > datos_por_edad$FIT_positivo) ||
    any(datos_por_edad$FIT_positivo > datos_por_edad$FIT_observado) ||
    any(datos_por_edad$FIT_observado > datos_por_edad$Poblacion)) {
  stop(
    "No se cumple Colonoscopia <= FIT positivo <= FIT observado <= poblacion.",
    call. = FALSE
  )
}

# Las personas sin edad no pueden asignarse a un grupo etario. Se documentan
# por separado y se excluyen solamente de los resúmenes por edad.
exclusiones_sin_edad <- datos_por_edad %>%
  filter(is.na(Edad)) %>%
  select(
    Base, Sexo, Poblacion, FIT_observado, FIT_positivo,
    Colonoscopia_ventana, Archivo_fuente
  ) %>%
  arrange(Base, factor(Sexo, c("Ambos sexos", "Hombres", "Mujeres")))

datos_por_edad <- datos_por_edad %>% filter(!is.na(Edad))

validar_sexos <- function(base_id) {
  total <- datos_por_edad %>%
    filter(Base == base_id, Sexo == "Ambos sexos") %>%
    select(Edad, all_of(columnas_conteo))

  suma_sexos <- datos_por_edad %>%
    filter(Base == base_id, Sexo %in% c("Hombres", "Mujeres")) %>%
    group_by(Edad) %>%
    summarise(across(all_of(columnas_conteo), sum), .groups = "drop")

  control <- full_join(
    total,
    suma_sexos,
    by = "Edad",
    suffix = c("_total", "_sexos")
  )
  for (columna in columnas_conteo) {
    if (any(
      control[[paste0(columna, "_total")]] !=
        control[[paste0(columna, "_sexos")]]
    )) {
      stop(
        "Hombres + mujeres no reproduce el total de la Base ", base_id,
        " para ", columna, ".",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

validar_sexos("A")
validar_sexos("B")

agregar_intervalo_wilson <- function(tabla, exitos, total, confianza = 0.95) {
  x <- tabla[[exitos]]
  n <- tabla[[total]]
  z <- qnorm(1 - (1 - confianza) / 2)
  p <- ifelse(n > 0, x / n, NA_real_)
  denominador <- 1 + z^2 / n
  centro <- (p + z^2 / (2 * n)) / denominador
  semiancho <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denominador

  tabla$Adherencia <- p
  tabla$IC95_inferior <- ifelse(n > 0, pmax(0, centro - semiancho), NA_real_)
  tabla$IC95_superior <- ifelse(n > 0, pmin(1, centro + semiancho), NA_real_)
  tabla
}

etiquetas_edad <- c("50-54", "55-59", "60-64", "65-69", "70-74", "75-79")

datos_50_79 <- datos_por_edad %>%
  filter(Edad >= 50, Edad <= 79) %>%
  mutate(
    Grupo_edad = cut(
      Edad,
      breaks = c(50, 55, 60, 65, 70, 75, 80),
      right = FALSE,
      labels = etiquetas_edad
    )
  )

adherencia_quinquenios <- datos_50_79 %>%
  group_by(Base, Sexo, Grupo_edad) %>%
  summarise(
    Poblacion = sum(Poblacion),
    FIT_observado = sum(FIT_observado),
    FIT_positivo = sum(FIT_positivo),
    Colonoscopia_ventana = sum(Colonoscopia_ventana),
    Sin_colonoscopia_ventana = FIT_positivo - Colonoscopia_ventana,
    .groups = "drop"
  ) %>%
  mutate(Grupo_edad = as.character(Grupo_edad)) %>%
  agregar_intervalo_wilson("Colonoscopia_ventana", "FIT_positivo")

adherencia_original <- data.frame(
  Grupo_edad = etiquetas_edad,
  Adherencia_original = c(0.9149, 0.9051, 0.8515, 0.8386, 0.8199, 0.8519),
  stringsAsFactors = FALSE
)

adherencia_quinquenios <- adherencia_quinquenios %>%
  left_join(adherencia_original, by = "Grupo_edad") %>%
  mutate(Diferencia_original_pp = 100 * (Adherencia - Adherencia_original)) %>%
  select(
    Base, Sexo, Grupo_edad,
    Poblacion, FIT_observado, FIT_positivo,
    Colonoscopia_ventana, Sin_colonoscopia_ventana,
    Adherencia, IC95_inferior, IC95_superior,
    Adherencia_original, Diferencia_original_pp
  )

# Versión que coincide exactamente con el rango etario del modelo. Los cinco
# primeros grupos son quinquenales; la última fila contiene solo la edad 75.
adherencia_grupos_modelo <- datos_por_edad %>%
  filter(Edad >= 50, Edad <= 75) %>%
  mutate(
    Grupo_edad = case_when(
      Edad >= 50 & Edad <= 54 ~ "50-54",
      Edad >= 55 & Edad <= 59 ~ "55-59",
      Edad >= 60 & Edad <= 64 ~ "60-64",
      Edad >= 65 & Edad <= 69 ~ "65-69",
      Edad >= 70 & Edad <= 74 ~ "70-74",
      Edad == 75 ~ "75"
    )
  ) %>%
  group_by(Base, Sexo, Grupo_edad) %>%
  summarise(
    Poblacion = sum(Poblacion),
    FIT_observado = sum(FIT_observado),
    FIT_positivo = sum(FIT_positivo),
    Colonoscopia_ventana = sum(Colonoscopia_ventana),
    Sin_colonoscopia_ventana = FIT_positivo - Colonoscopia_ventana,
    .groups = "drop"
  ) %>%
  agregar_intervalo_wilson("Colonoscopia_ventana", "FIT_positivo") %>%
  mutate(
    Adherencia_original = case_when(
      Grupo_edad == "50-54" ~ 0.9149,
      Grupo_edad == "55-59" ~ 0.9051,
      Grupo_edad == "60-64" ~ 0.8515,
      Grupo_edad == "65-69" ~ 0.8386,
      Grupo_edad == "70-74" ~ 0.8199,
      Grupo_edad == "75" ~ 0.8519
    ),
    Diferencia_original_pp = 100 * (Adherencia - Adherencia_original)
  ) %>%
  select(
    Base, Sexo, Grupo_edad,
    Poblacion, FIT_observado, FIT_positivo,
    Colonoscopia_ventana, Sin_colonoscopia_ventana,
    Adherencia, IC95_inferior, IC95_superior,
    Adherencia_original, Diferencia_original_pp
  ) %>%
  arrange(
    Base,
    factor(Sexo, c("Ambos sexos", "Hombres", "Mujeres")),
    factor(Grupo_edad, c("50-54", "55-59", "60-64", "65-69", "70-74", "75"))
  )

construir_estimador_central <- function(tabla) {
  columnas_clave <- c("Sexo", "Grupo_edad")
  columnas_utiles <- c(
    columnas_clave, "FIT_positivo", "Colonoscopia_ventana", "Adherencia"
  )

  tabla_A <- tabla %>%
    filter(Base == "A") %>%
    select(all_of(columnas_utiles)) %>%
    rename(
      FIT_positivo_A = FIT_positivo,
      Colonoscopia_ventana_A = Colonoscopia_ventana,
      Adherencia_A = Adherencia
    )

  tabla_B <- tabla %>%
    filter(Base == "B") %>%
    select(all_of(columnas_utiles)) %>%
    rename(
      FIT_positivo_B = FIT_positivo,
      Colonoscopia_ventana_B = Colonoscopia_ventana,
      Adherencia_B = Adherencia
    )

  full_join(tabla_A, tabla_B, by = columnas_clave) %>%
    mutate(
      Adherencia_central_AB =
        (Colonoscopia_ventana_A + Colonoscopia_ventana_B) /
        (FIT_positivo_A + FIT_positivo_B),
      Diferencia_A_B_pp = 100 * (Adherencia_A - Adherencia_B)
    ) %>%
    arrange(
      factor(Sexo, c("Ambos sexos", "Hombres", "Mujeres")),
      suppressWarnings(as.numeric(substr(Grupo_edad, 1, 2)))
    )
}

# A y B contienen a las mismas personas con asignaciones FIT complementarias.
# El cociente agrupado es un estimador puntual descriptivo de la expectativa
# del sorteo. No se calcula un Wilson central porque A y B no son observaciones
# independientes. Los intervalos Wilson se informan únicamente para A y B.
adherencia_central_quinquenios <- construir_estimador_central(
  adherencia_quinquenios
)
adherencia_central_modelo <- construir_estimador_central(
  adherencia_grupos_modelo
)

adherencia_por_edad <- datos_50_79 %>%
  mutate(
    Sin_colonoscopia_ventana = FIT_positivo - Colonoscopia_ventana
  ) %>%
  agregar_intervalo_wilson("Colonoscopia_ventana", "FIT_positivo") %>%
  select(
    Base, Sexo, Edad, Poblacion, FIT_observado, FIT_positivo,
    Colonoscopia_ventana, Sin_colonoscopia_ventana,
    Adherencia, IC95_inferior, IC95_superior, Archivo_fuente
  ) %>%
  arrange(Base, factor(Sexo, c("Ambos sexos", "Hombres", "Mujeres")), Edad)

resumir_periodo <- function(datos, edad_minima, edad_maxima, etiqueta) {
  datos %>%
    filter(Edad >= edad_minima, Edad <= edad_maxima) %>%
    group_by(Base, Sexo) %>%
    summarise(
      Periodo_edad = etiqueta,
      Poblacion = sum(Poblacion),
      FIT_observado = sum(FIT_observado),
      FIT_positivo = sum(FIT_positivo),
      Colonoscopia_ventana = sum(Colonoscopia_ventana),
      Sin_colonoscopia_ventana = FIT_positivo - Colonoscopia_ventana,
      .groups = "drop"
    )
}

adherencia_periodos <- bind_rows(
  resumir_periodo(datos_por_edad, 50, 75, "50-75 (modelo)"),
  resumir_periodo(datos_por_edad, 50, 79, "50-79 (quinquenios completos)")
) %>%
  agregar_intervalo_wilson("Colonoscopia_ventana", "FIT_positivo") %>%
  select(
    Base, Sexo, Periodo_edad,
    Poblacion, FIT_observado, FIT_positivo,
    Colonoscopia_ventana, Sin_colonoscopia_ventana,
    Adherencia, IC95_inferior, IC95_superior
  ) %>%
  arrange(
    Periodo_edad,
    Base,
    factor(Sexo, c("Ambos sexos", "Hombres", "Mujeres"))
  )

escribir_tsv <- function(datos, nombre) {
  ruta <- file.path(directorio_salida, nombre)
  write.table(
    datos,
    file = ruta,
    sep = "\t",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
  ruta
}

sufijo <- paste0("semilla_", semilla_bases)
rutas <- c(
  grupos_modelo = escribir_tsv(
    adherencia_grupos_modelo,
    paste0("adherencia_grupos_modelo_50_75_", sufijo, ".tsv")
  ),
  central_modelo = escribir_tsv(
    adherencia_central_modelo,
    paste0("adherencia_central_modelo_50_75_", sufijo, ".tsv")
  ),
  quinquenios = escribir_tsv(
    adherencia_quinquenios,
    paste0("adherencia_quinquenios_", sufijo, ".tsv")
  ),
  central_quinquenios = escribir_tsv(
    adherencia_central_quinquenios,
    paste0("adherencia_central_quinquenios_", sufijo, ".tsv")
  ),
  por_edad = escribir_tsv(
    adherencia_por_edad,
    paste0("adherencia_por_edad_", sufijo, ".tsv")
  ),
  periodos = escribir_tsv(
    adherencia_periodos,
    paste0("adherencia_periodos_", sufijo, ".tsv")
  ),
  exclusiones = escribir_tsv(
    exclusiones_sin_edad,
    paste0("adherencia_exclusiones_sin_edad_", sufijo, ".tsv")
  )
)

message("Resultados de adherencia:")
message(paste(normalizePath(rutas, winslash = "/", mustWork = FALSE), collapse = "\n"))

invisible(list(
  grupos_modelo = adherencia_grupos_modelo,
  central_modelo = adherencia_central_modelo,
  quinquenios = adherencia_quinquenios,
  central_quinquenios = adherencia_central_quinquenios,
  por_edad = adherencia_por_edad,
  periodos = adherencia_periodos,
  exclusiones_sin_edad = exclusiones_sin_edad,
  archivos = rutas
))
