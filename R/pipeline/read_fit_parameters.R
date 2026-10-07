# Carga y valida escenarios alternativos de sensibilidad FIT desde Excel.
# El escenario central no se toma de este archivo: se estima directamente en
# la cohorte PRENEC highrisk == 1 mediante sensibilidad_fit_test_aleatorio.R.
# Los escenarios externos pueden ser parciales: cada fila activa reemplaza
# solo la lesion indicada y los componentes ausentes conservan el estimador
# central de PRENEC.

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Se requiere el paquete 'readxl' para cargar parametros FIT.", call. = FALSE)
}

normalizar_logico_fit <- function(x, campo) {
  if (is.logical(x)) {
    if (anyNA(x)) {
      stop("La columna '", campo, "' contiene valores logicos faltantes.", call. = FALSE)
    }
    return(x)
  }
  texto <- tolower(trimws(as.character(x)))
  resultado <- rep(NA, length(texto))
  resultado[texto %in% c("true", "t", "1", "si", "yes")] <- TRUE
  resultado[texto %in% c("false", "f", "0", "no")] <- FALSE
  if (anyNA(resultado)) {
    stop("La columna '", campo, "' contiene valores logicos no reconocidos.", call. = FALSE)
  }
  resultado
}

derivar_beta_desde_ic_fit <- function(estimate, ci_lower, ci_upper) {
  if (any(!is.finite(c(estimate, ci_lower, ci_upper))) ||
      estimate <= 0 || estimate >= 1 ||
      ci_lower < 0 || ci_upper > 1 || ci_lower >= estimate || ci_upper <= estimate) {
    stop("estimate e IC95% no permiten derivar una distribucion beta valida.", call. = FALSE)
  }
  sd_aprox <- (ci_upper - ci_lower) / (2 * qnorm(0.975))
  var_aprox <- sd_aprox^2
  n_effective <- estimate * (1 - estimate) / var_aprox - 1
  if (!is.finite(n_effective) || n_effective <= 0) {
    stop("El IC95% produce una concentracion beta no valida.", call. = FALSE)
  }
  c(
    beta_a = estimate * n_effective,
    beta_b = (1 - estimate) * n_effective,
    n_effective = n_effective
  )
}

validar_parametros_fit_literatura <- function(tabla) {
  columnas_requeridas <- c(
    "scenario_id", "scenario_label", "lesion_key", "estimate",
    "ci_lower", "ci_upper", "beta_a", "beta_b", "n_effective",
    "distribution", "logit_mean", "logit_se", "prior_added_in_simulation",
    "active", "source_id", "citation", "notes"
  )
  faltantes <- setdiff(columnas_requeridas, names(tabla))
  if (length(faltantes) > 0L) {
    stop(
      "Faltan columnas en 'Escenarios_literatura': ",
      paste(faltantes, collapse = ", "),
      call. = FALSE
    )
  }

  tabla <- as.data.frame(tabla, stringsAsFactors = FALSE)
  tabla$active <- normalizar_logico_fit(tabla$active, "active")
  tabla <- tabla[tabla$active, , drop = FALSE]
  if (nrow(tabla) == 0L) {
    tabla$parameter_method <- character(0)
    return(tabla)
  }

  campos_texto <- c(
    "scenario_id", "scenario_label", "lesion_key", "distribution",
    "source_id", "citation", "notes"
  )
  for (campo in campos_texto) {
    tabla[[campo]] <- trimws(as.character(tabla[[campo]]))
    if (any(is.na(tabla[[campo]]) | tabla[[campo]] == "")) {
      stop("Las filas activas deben completar '", campo, "'.", call. = FALSE)
    }
  }
  if (any(tabla$scenario_id == "PRENEC_central")) {
    stop("'PRENEC_central' es un scenario_id reservado para la cohorte PRENEC.", call. = FALSE)
  }

  tabla$lesion_key <- tolower(tabla$lesion_key)
  lesiones_permitidas <- c(
    "adenoma_total", "small", "medium", "large",
    "low_risk", "high_risk", "cancer"
  )
  desconocidas <- setdiff(unique(tabla$lesion_key), lesiones_permitidas)
  if (length(desconocidas) > 0L) {
    stop("Lesiones FIT no reconocidas: ", paste(desconocidas, collapse = ", "), call. = FALSE)
  }

  claves <- paste(tabla$scenario_id, tabla$lesion_key, sep = "::")
  if (anyDuplicated(claves)) {
    stop("Hay filas FIT duplicadas por escenario y lesion.", call. = FALSE)
  }

  numericas <- c(
    "estimate", "ci_lower", "ci_upper", "beta_a", "beta_b", "n_effective",
    "logit_mean", "logit_se", "prior_added_in_simulation"
  )
  for (campo in numericas) {
    tabla[[campo]] <- suppressWarnings(as.numeric(tabla[[campo]]))
  }
  tabla$parameter_method <- NA_character_
  tabla$distribution <- tolower(tabla$distribution)
  if (any(!tabla$distribution %in% c("beta", "logit_normal"))) {
    stop("distribution debe ser 'beta' o 'logit_normal'.", call. = FALSE)
  }

  for (i in seq_len(nrow(tabla))) {
    if (!is.finite(tabla$prior_added_in_simulation[[i]])) {
      tabla$prior_added_in_simulation[[i]] <- 0
    }
    if (tabla$prior_added_in_simulation[[i]] < 0) {
      stop("prior_added_in_simulation no puede ser negativo.", call. = FALSE)
    }

    if (tabla$distribution[[i]] == "logit_normal") {
      if (!is.finite(tabla$logit_mean[[i]]) ||
          !is.finite(tabla$logit_se[[i]]) || tabla$logit_se[[i]] <= 0) {
        stop("Las filas logit_normal deben completar logit_mean y logit_se > 0.", call. = FALSE)
      }
      tabla$estimate[[i]] <- plogis(tabla$logit_mean[[i]])
      if (!is.finite(tabla$ci_lower[[i]]) || !is.finite(tabla$ci_upper[[i]])) {
        tabla$ci_lower[[i]] <- plogis(tabla$logit_mean[[i]] - qnorm(0.975) * tabla$logit_se[[i]])
        tabla$ci_upper[[i]] <- plogis(tabla$logit_mean[[i]] + qnorm(0.975) * tabla$logit_se[[i]])
      } else if (tabla$ci_lower[[i]] < 0 || tabla$ci_upper[[i]] > 1 ||
                 tabla$ci_lower[[i]] > tabla$estimate[[i]] ||
                 tabla$ci_upper[[i]] < tabla$estimate[[i]] ||
                 tabla$ci_lower[[i]] >= tabla$ci_upper[[i]]) {
        stop("El IC95% logit_normal debe contener estimate y estar dentro de [0,1].", call. = FALSE)
      }
      tabla$beta_a[[i]] <- NA_real_
      tabla$beta_b[[i]] <- NA_real_
      tabla$n_effective[[i]] <- NA_real_
      tabla$prior_added_in_simulation[[i]] <- 0
      tabla$parameter_method[[i]] <- "logit_normal_meta_analysis"
      next
    }

    tiene_beta <- is.finite(tabla$beta_a[[i]]) && is.finite(tabla$beta_b[[i]])
    if (tiene_beta) {
      if (tabla$beta_a[[i]] <= 0 || tabla$beta_b[[i]] < 0) {
        stop("beta_a debe ser positiva y beta_b no negativa en filas FIT activas.", call. = FALSE)
      }
      n_beta <- tabla$beta_a[[i]] + tabla$beta_b[[i]]
      estimacion_beta <- tabla$beta_a[[i]] / n_beta
      if (is.finite(tabla$n_effective[[i]]) && abs(tabla$n_effective[[i]] - n_beta) > max(1e-8, n_beta * 1e-9)) {
        stop("n_effective no coincide con beta_a + beta_b.", call. = FALSE)
      }
      if (is.finite(tabla$estimate[[i]]) && abs(tabla$estimate[[i]] - estimacion_beta) > 1e-9) {
        stop("estimate no coincide con beta_a/(beta_a+beta_b).", call. = FALSE)
      }
      tabla$n_effective[[i]] <- n_beta
      tabla$estimate[[i]] <- estimacion_beta
      tabla$parameter_method[[i]] <- "reported_or_pooled_counts"
      if (is.finite(tabla$ci_lower[[i]]) && is.finite(tabla$ci_upper[[i]]) &&
          tabla$ci_lower[[i]] < tabla$estimate[[i]] &&
          tabla$ci_upper[[i]] > tabla$estimate[[i]]) {
        beta_ic <- derivar_beta_desde_ic_fit(
          tabla$estimate[[i]], tabla$ci_lower[[i]], tabla$ci_upper[[i]]
        )
        coincide_ic <- all(abs(
          c(tabla$beta_a[[i]], tabla$beta_b[[i]], tabla$n_effective[[i]]) -
            unname(beta_ic)
        ) <= pmax(1e-8, abs(unname(beta_ic)) * 1e-9))
        if (coincide_ic) {
          tabla$parameter_method[[i]] <- "beta_moment_match_from_95ci"
        }
      }
    } else {
      beta <- derivar_beta_desde_ic_fit(
        tabla$estimate[[i]], tabla$ci_lower[[i]], tabla$ci_upper[[i]]
      )
      tabla$beta_a[[i]] <- unname(beta[["beta_a"]])
      tabla$beta_b[[i]] <- unname(beta[["beta_b"]])
      tabla$n_effective[[i]] <- unname(beta[["n_effective"]])
      tabla$parameter_method[[i]] <- "beta_moment_match_from_95ci"
    }
    if (tabla$beta_a[[i]] + tabla$prior_added_in_simulation[[i]] <= 0 ||
        tabla$beta_b[[i]] + tabla$prior_added_in_simulation[[i]] <= 0) {
      stop(
        "Los shapes beta despues del prior deben ser estrictamente positivos.",
        call. = FALSE
      )
    }
  }

  if (any(tabla$estimate < 0 | tabla$estimate > 1)) {
    stop("Las sensibilidades FIT deben estar entre 0 y 1.", call. = FALSE)
  }

  # Un escenario de literatura puede reemplazar uno o mas componentes. Esto
  # permite evaluar evidencia externa especifica para un desenlace sin
  # inventar parametros para las lesiones no informadas por la fuente.
  for (escenario in unique(tabla$scenario_id)) {
    filas_escenario <- tabla$scenario_id == escenario
    if (length(unique(tabla$scenario_label[filas_escenario])) != 1L ||
        length(unique(tabla$source_id[filas_escenario])) != 1L) {
      stop(
        "Las filas del escenario FIT '", escenario,
        "' deben compartir scenario_label y source_id.",
        call. = FALSE
      )
    }
  }

  tabla
}

ruta_parametros_fit <- Sys.getenv(
  "PRENEC_FIT_PARAMS",
  unset = file.path(
    Sys.getenv("PRENEC_PROJECT_ROOT", unset = normalizePath(getwd())),
    "data",
    "parameters",
    "external_fit_sensitivity_parameters.xlsx"
  )
)
if (!file.exists(ruta_parametros_fit)) {
  stop(
    "No se encontro el archivo de escenarios FIT: '", ruta_parametros_fit, "'.",
    call. = FALSE
  )
}

hojas_fit <- readxl::excel_sheets(ruta_parametros_fit)
if (!("Escenarios_literatura" %in% hojas_fit)) {
  stop("Falta la hoja 'Escenarios_literatura' en el archivo FIT.", call. = FALSE)
}

parametros_fit_literatura <- validar_parametros_fit_literatura(
  readxl::read_excel(
    ruta_parametros_fit,
    sheet = "Escenarios_literatura",
    na = c("", "NA")
  )
)

if (nrow(parametros_fit_literatura) == 0L) {
  message("No hay escenarios FIT de literatura activos; se ejecutara solo PRENEC central.")
}

rm(hojas_fit)
