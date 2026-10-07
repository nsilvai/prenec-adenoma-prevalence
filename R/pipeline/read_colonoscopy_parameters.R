# Carga y valida los parametros externos de sensibilidad y especificidad
# de colonoscopia. El archivo Excel guarda pseudoconteos antes de sumar
# el prior de Jeffreys (0.5, 0.5) utilizado en las simulaciones.

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop(
    "Se requiere el paquete 'readxl' para cargar los parametros de colonoscopia.",
    call. = FALSE
  )
}

normalizar_logico_colonoscopia <- function(x, campo) {
  if (is.logical(x)) {
    if (anyNA(x)) {
      stop(
        sprintf("La columna '%s' contiene valores logicos faltantes.", campo),
        call. = FALSE
      )
    }
    return(x)
  }

  texto <- tolower(trimws(as.character(x)))
  resultado <- rep(NA, length(texto))
  resultado[texto %in% c("true", "t", "1", "si", "yes")] <- TRUE
  resultado[texto %in% c("false", "f", "0", "no")] <- FALSE

  if (anyNA(resultado)) {
    stop(
      sprintf("La columna '%s' contiene valores logicos no reconocidos.", campo),
      call. = FALSE
    )
  }
  resultado
}

validar_parametros_colonoscopia <- function(tabla) {
  columnas_requeridas <- c(
    "lesion_key", "metric", "estimate", "beta_a", "beta_b",
    "n_effective", "parameter_method", "source_id", "proxy_from",
    "active", "used_current_model", "prior_added_in_R", "notes"
  )

  faltantes <- setdiff(columnas_requeridas, names(tabla))
  if (length(faltantes) > 0L) {
    stop(
      sprintf(
        "Faltan columnas requeridas en 'Parametros_modelo': %s.",
        paste(faltantes, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  tabla <- as.data.frame(tabla, stringsAsFactors = FALSE)
  tabla$lesion_key <- tolower(trimws(as.character(tabla$lesion_key)))
  tabla$metric <- tolower(trimws(as.character(tabla$metric)))
  tabla$parameter_method <- trimws(as.character(tabla$parameter_method))
  tabla$source_id <- trimws(as.character(tabla$source_id))
  tabla$proxy_from <- trimws(as.character(tabla$proxy_from))
  tabla$active <- normalizar_logico_colonoscopia(tabla$active, "active")
  tabla$used_current_model <- normalizar_logico_colonoscopia(
    tabla$used_current_model,
    "used_current_model"
  )

  columnas_numericas <- c(
    "estimate", "beta_a", "beta_b", "n_effective", "prior_added_in_R"
  )
  for (columna in columnas_numericas) {
    tabla[[columna]] <- suppressWarnings(as.numeric(tabla[[columna]]))
  }

  tabla <- tabla[tabla$active, , drop = FALSE]
  lesiones_esperadas <- c(
    "adenoma_total", "small", "medium", "large",
    "low_risk", "high_risk", "cancer"
  )
  metricas_esperadas <- c("sensitivity", "specificity")
  claves_esperadas <- as.vector(outer(
    lesiones_esperadas,
    metricas_esperadas,
    paste,
    sep = "::"
  ))
  claves_observadas <- paste(tabla$lesion_key, tabla$metric, sep = "::")

  duplicadas <- unique(claves_observadas[duplicated(claves_observadas)])
  if (length(duplicadas) > 0L) {
    stop(
      sprintf("Hay parametros duplicados: %s.", paste(duplicadas, collapse = ", ")),
      call. = FALSE
    )
  }

  desconocidas <- setdiff(claves_observadas, claves_esperadas)
  if (length(desconocidas) > 0L) {
    stop(
      sprintf("Hay parametros no reconocidos: %s.", paste(desconocidas, collapse = ", ")),
      call. = FALSE
    )
  }

  ausentes <- setdiff(claves_esperadas, claves_observadas)
  if (length(ausentes) > 0L) {
    stop(
      sprintf("Faltan parametros activos: %s.", paste(ausentes, collapse = ", ")),
      call. = FALSE
    )
  }

  valores_principales <- unlist(tabla[columnas_numericas], use.names = FALSE)
  if (anyNA(valores_principales) || any(!is.finite(valores_principales))) {
    stop("Los parametros numericos deben ser finitos y no pueden ser NA.", call. = FALSE)
  }
  if (any(tabla$estimate < 0 | tabla$estimate > 1)) {
    stop("Cada estimacion debe estar entre 0 y 1.", call. = FALSE)
  }
  if (any(tabla$beta_a < 0 | tabla$beta_b < 0)) {
    stop("Los parametros beta_a y beta_b no pueden ser negativos.", call. = FALSE)
  }
  if (any(tabla$prior_added_in_R < 0)) {
    stop("prior_added_in_R no puede ser negativo.", call. = FALSE)
  }
  if (any(tabla$n_effective <= 0 | (tabla$beta_a + tabla$beta_b) <= 0)) {
    stop("Cada fila debe tener un tamano efectivo y una concentracion beta positivos.", call. = FALSE)
  }

  tolerancia_n <- pmax(1e-8, abs(tabla$n_effective) * 1e-9)
  if (any(abs(tabla$n_effective - (tabla$beta_a + tabla$beta_b)) > tolerancia_n)) {
    stop("n_effective debe ser igual a beta_a + beta_b.", call. = FALSE)
  }

  estimacion_beta <- tabla$beta_a / (tabla$beta_a + tabla$beta_b)
  if (any(abs(tabla$estimate - estimacion_beta) > 1e-9)) {
    stop("estimate debe ser igual a beta_a / (beta_a + beta_b).", call. = FALSE)
  }

  if (any(is.na(tabla$source_id) | tabla$source_id == "")) {
    stop("Todas las filas activas deben identificar una fuente.", call. = FALSE)
  }

  es_proxy <- grepl("proxy", tabla$parameter_method, ignore.case = TRUE)
  proxy_vacio <- is.na(tabla$proxy_from) | tabla$proxy_from == ""
  if (any(es_proxy & proxy_vacio)) {
    stop("Las filas construidas como proxy deben completar proxy_from.", call. = FALSE)
  }

  notas_vacias <- is.na(tabla$notes) | trimws(as.character(tabla$notes)) == ""
  if (any(es_proxy & notas_vacias)) {
    stop("Las filas construidas como proxy deben justificar la decision en notes.", call. = FALSE)
  }

  cuantiles <- vapply(
    seq_len(nrow(tabla)),
    function(i) {
      valores <- qbeta(
        c(0.001, 0.5, 0.999),
        tabla$beta_a[[i]] + tabla$prior_added_in_R[[i]],
        tabla$beta_b[[i]] + tabla$prior_added_in_R[[i]]
      )
      all(is.finite(valores))
    },
    logical(1)
  )
  if (any(!cuantiles)) {
    stop("Todos los cuantiles beta deben ser finitos despues de sumar el prior 0,5.", call. = FALSE)
  }

  if (any(tabla$metric == "specificity" & tabla$used_current_model)) {
    stop(
      paste0(
        "La especificidad de colonoscopia aun no forma parte de las ecuaciones ",
        "de prevalencia; used_current_model debe permanecer FALSE."
      ),
      call. = FALSE
    )
  }

  tabla[match(claves_esperadas, claves_observadas), , drop = FALSE]
}

validar_fuentes_colonoscopia <- function(parametros, fuentes) {
  columnas_fuente <- c(
    "source_id", "citation", "test_evaluated", "original_stratum",
    "sample_or_basis", "use_in_model", "limitations"
  )
  faltantes <- setdiff(columnas_fuente, names(fuentes))
  if (length(faltantes) > 0L) {
    stop(
      sprintf(
        "Faltan columnas requeridas en 'Evidencia_fuente': %s.",
        paste(faltantes, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  fuentes <- as.data.frame(fuentes, stringsAsFactors = FALSE)
  fuentes$source_id <- trimws(as.character(fuentes$source_id))
  if (any(is.na(fuentes$source_id) | fuentes$source_id == "")) {
    stop("Cada fila de Evidencia_fuente debe tener source_id.", call. = FALSE)
  }
  if (anyDuplicated(fuentes$source_id)) {
    stop("Los source_id de Evidencia_fuente deben ser unicos.", call. = FALSE)
  }

  referencias_ausentes <- setdiff(unique(parametros$source_id), fuentes$source_id)
  if (length(referencias_ausentes) > 0L) {
    stop(
      sprintf(
        "Hay source_id sin respaldo en Evidencia_fuente: %s.",
        paste(referencias_ausentes, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  for (campo in setdiff(columnas_fuente, "source_id")) {
    vacio <- is.na(fuentes[[campo]]) | trimws(as.character(fuentes[[campo]])) == ""
    if (any(vacio)) {
      stop(
        sprintf("La columna '%s' de Evidencia_fuente no puede quedar vacia.", campo),
        call. = FALSE
      )
    }
  }

  invisible(fuentes)
}

ruta_parametros_colonoscopia <- Sys.getenv(
  "PRENEC_COLON_PARAMS",
  unset = file.path(
    Sys.getenv("PRENEC_PROJECT_ROOT", unset = normalizePath(getwd())),
    "data",
    "parameters",
    "colonoscopy_sensitivity_parameters.xlsx"
  )
)

if (!file.exists(ruta_parametros_colonoscopia)) {
  stop(
    sprintf(
      paste0(
        "No se encontro el archivo de parametros de colonoscopia: '%s'. ",
        "Defina PRENEC_COLON_PARAMS o ubique el Excel junto a los scripts."
      ),
      ruta_parametros_colonoscopia
    ),
    call. = FALSE
  )
}

hojas_colonoscopia <- readxl::excel_sheets(ruta_parametros_colonoscopia)
hojas_requeridas_colonoscopia <- c(
  "Parametros_modelo", "Evidencia_fuente", "Diccionario"
)
hojas_ausentes_colonoscopia <- setdiff(
  hojas_requeridas_colonoscopia,
  hojas_colonoscopia
)
if (length(hojas_ausentes_colonoscopia) > 0L) {
  stop(
    sprintf(
      "Faltan hojas requeridas en el Excel: %s.",
      paste(hojas_ausentes_colonoscopia, collapse = ", ")
    ),
    call. = FALSE
  )
}

parametros_colonoscopia <- validar_parametros_colonoscopia(
  readxl::read_excel(
    ruta_parametros_colonoscopia,
    sheet = "Parametros_modelo",
    na = c("", "NA")
  )
)

fuentes_colonoscopia <- readxl::read_excel(
  ruta_parametros_colonoscopia,
  sheet = "Evidencia_fuente",
  na = c("", "NA")
)
validar_fuentes_colonoscopia(parametros_colonoscopia, fuentes_colonoscopia)

sufijos_colonoscopia <- c(
  adenoma_total = "adenoma_total",
  small = "small",
  medium = "medium",
  large = "large",
  low_risk = "low_risk",
  high_risk = "high_risk",
  cancer = "ca"
)

for (lesion in names(sufijos_colonoscopia)) {
  sufijo <- unname(sufijos_colonoscopia[[lesion]])
  fila_sens <- parametros_colonoscopia[
    parametros_colonoscopia$lesion_key == lesion &
      parametros_colonoscopia$metric == "sensitivity",
    ,
    drop = FALSE
  ]
  fila_spec <- parametros_colonoscopia[
    parametros_colonoscopia$lesion_key == lesion &
      parametros_colonoscopia$metric == "specificity",
    ,
    drop = FALSE
  ]

  assign(paste0("sens_col_", sufijo), fila_sens$estimate[[1]], envir = .GlobalEnv)
  assign(paste0("a_sens_col_", sufijo), fila_sens$beta_a[[1]], envir = .GlobalEnv)
  assign(paste0("b_sens_col_", sufijo), fila_sens$beta_b[[1]], envir = .GlobalEnv)
  assign(paste0("n_sens_col_", sufijo), fila_sens$n_effective[[1]], envir = .GlobalEnv)

  assign(paste0("spec_col_", sufijo), fila_spec$estimate[[1]], envir = .GlobalEnv)
  assign(paste0("a_spec_col_", sufijo), fila_spec$beta_a[[1]], envir = .GlobalEnv)
  assign(paste0("b_spec_col_", sufijo), fila_spec$beta_b[[1]], envir = .GlobalEnv)
  assign(paste0("n_spec_col_", sufijo), fila_spec$n_effective[[1]], envir = .GlobalEnv)
}

rm(
  lesion, sufijo, fila_sens, fila_spec, sufijos_colonoscopia,
  hojas_colonoscopia, hojas_requeridas_colonoscopia,
  hojas_ausentes_colonoscopia
)
