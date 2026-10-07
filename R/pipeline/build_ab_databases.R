# -*- coding: UTF-8 -*-
# Stage 01 implementation: PRENEC database construction -----------------------
#
# Purpose:
#   Import and harmonise regional workbooks; derive age, FIT rounds, lesion
#   classifications, family-history group, and qualifying colonoscopy; select
#   one FIT per participant; and create A/B aggregates by age and sex.
# Population:
#   A/B analytical outputs use highrisk == 0.
# Inputs and outputs:
#   Protected paths are supplied through PRENEC_* environment variables.
#   Restricted derived workbooks are written under PRENEC_OUTPUT_DIR.
# Invariants:
#   No correction for FIT coverage is introduced. Lesion counts require a
#   positive selected FIT and a qualifying colonoscopy. A/B share participants
#   and are complementary assignments, not independent samples.
##### Cargar paquetes ----

library(readxl)
library(janitor)
library(purrr)
library(dplyr)
library(writexl)
library(tidyr)
library(lubridate)
library(stringr)
library(rlang)

project_directory <- Sys.getenv(
  "PRENEC_PROJECT_ROOT",
  unset = normalizePath(getwd(), winslash = "/", mustWork = TRUE)
)
project_directory <- normalizePath(
  project_directory,
  winslash = "/",
  mustWork = TRUE
)

##### Configuración de la copia aleatorizada ----

SEMILLA_SORTEO <- suppressWarnings(as.integer(
  Sys.getenv("PRENEC_SEED", unset = "20260923")
))
if (is.na(SEMILLA_SORTEO)) {
  stop("PRENEC_SEED debe ser un número entero.")
}

ruta_salida <- Sys.getenv(
  "PRENEC_OUTPUT_DIR",
  unset = file.path(project_directory, "data", "intermediate")
)
dir.create(ruta_salida, recursive = TRUE, showWarnings = FALSE)

ruta_baseA_actual <- Sys.getenv(
  "PRENEC_BASE_A_ACTUAL",
  unset = file.path(project_directory, "data", "private", "Base_finalA_N.xlsx")
)
ruta_baseB_actual <- Sys.getenv(
  "PRENEC_BASE_B_ACTUAL",
  unset = file.path(project_directory, "data", "private", "Base_finalB_N.xlsx")
)

ruta_base_transformada <- Sys.getenv(
  "PRENEC_BASE_TRANSFORMADA",
  unset = file.path(project_directory, "data", "private", "Base_prenec.xlsx")
)

usar_base_transformada <- tolower(
  Sys.getenv("PRENEC_USAR_BASE_HISTORICA", unset = "1")
) %in% c("1", "true", "si", "yes")

ruta_archivos <- Sys.getenv(
  "PRENEC_DATA_DIR",
  unset = file.path(project_directory, "data", "raw")
)

archivo_salida <- function(nombre) file.path(ruta_salida, nombre)


if (!usar_base_transformada) {

##### Cargar y limpiar las bases de datos ----

cargar_bases_limpias <- function(ruta) {

  # Archivos Excel
  archivos <- list.files(ruta, pattern = "^[^~].*\\.xlsx$", full.names = TRUE)
  if (length(archivos) == 0L) {
    stop("No se encontraron archivos .xlsx en: ", normalizePath(ruta, winslash = "/", mustWork = FALSE))
  }
  nombres <- tools::file_path_sans_ext(basename(archivos))
  names(archivos) <- nombres  # clave para que funcione bien el nombre en imap

  # Definir bases con conflicto por columna
  conflicto_fechas <- list(
    Fecha_Nacimiento = c("Magallanes", "Valparaiso", "Valdivia"),
    Fecha_2do_Colon_check = c("Biobio"),
    Fecha_3er_Colon_check = c("Antofagasta", "Biobio", "Coquimbo", "Valparaiso", "Valdivia")
  )

  # Función para procesar una base
  procesar_base <- function(path, nombre_base) {
    df <- read_excel(path)

    # Limpiar nombres de columnas
    names(df) <- str_replace_all(names(df), " ", "_")

    # Detectar columnas conflictivas para esta base
    columnas_conflictivas <- names(conflicto_fechas)[
      map_lgl(conflicto_fechas, ~ nombre_base %in% .x)
    ]

    # Convertir a numérico → luego a Date
    for (col in columnas_conflictivas) {
      if (col %in% names(df)) {
        suppressWarnings({
          df[[col]] <- as.numeric(df[[col]])
          df[[col]] <- as.Date(df[[col]], origin = "1899-12-30")
        })
      }
    }

    # Agregar nombre de origen
    df$origen <- nombre_base

    return(df)
  }

  # Cargar y procesar todas las bases
  bases_limpias <- imap(archivos, procesar_base)
  return(bases_limpias)
}

bases_limpias <- cargar_bases_limpias(ruta_archivos)

# extraer los nombres de las variables

columnas_interes <- c("Codigo", "Centro", "RUT", "Sexo", "Fecha_Nacimiento", "Edad", "Fecha_Enrolamiento", "Valor_TSDO_1_1","Valor_TSDO_2_1",
                      "Resultado_TSDO_1", "Valor_TSDO_1_2", "Valor_TSDO_2_2","Resultado_TSDO_2", "Valor_TSDO_1_3", "Valor_TSDO_2_3",
                      "Resultado_TSDO_3","Tipo_Histologico_1", "Tamano_Polipo_Mayor_1", "Familiar_CACO", "N_Polipos_1", "Fecha_1era_Colono",
                      "Fecha_1er_Colon_check","Fecha_2do_Colon_check", "Fecha_3er_Colon_check", "origen")

# seleccionar solo las variables de interés de las bases de datos

bases_reducidas <- map(bases_limpias, ~ select(.x, any_of(columnas_interes)))

# comparar los nombres de las columnas

columnas <- exec(compare_df_cols, !!!bases_reducidas, return = "all")

# Segundo nivel de procesamiento antes de unir. Se requiere armonizar variables, se usará como plantilla
# la base de datos de Santiago

armonizar_bases_reducidas <- function(bases_reducidas, nombre_plantilla = "Santiago") {
  plantilla <- bases_reducidas[[nombre_plantilla]]

  # Extraer clases objetivo de la plantilla
  clases_objetivo <- sapply(plantilla, function(col) class(col)[1])

  # Función para armonizar una base individual
  armonizar_base <- function(df) {
    comunes <- intersect(names(df), names(clases_objetivo))

    for (col in comunes) {
      clase_target <- clases_objetivo[[col]]

      df[[col]] <- switch(
        clase_target,
        character = as.character(df[[col]]),
        numeric   = suppressWarnings(as.numeric(df[[col]])),
        integer   = suppressWarnings(as.integer(df[[col]])),
        logical   = as.logical(df[[col]]),
        factor    = as.factor(df[[col]]),
        Date = {
          if (inherits(df[[col]], "Date")) {
            df[[col]]
          } else if (inherits(df[[col]], "POSIXct") | inherits(df[[col]], "POSIXlt")) {
            as.Date(df[[col]])
          } else {
            suppressWarnings(as.Date(df[[col]], origin = "1899-12-30"))
          }
        },
        df[[col]]  # default: no transformar
      )
    }

    return(df)
  }

  # Aplicar armonización a todas las bases

  bases_armonizadas <- map(bases_reducidas, armonizar_base)
  return(bases_armonizadas)
}

bases_armonizadas <- armonizar_bases_reducidas(bases_reducidas, nombre_plantilla = "Santiago")

columnas <- exec(compare_df_cols, !!!bases_armonizadas, return = "all")


# Unir todas las bases

prenec <- bind_rows(bases_armonizadas)

# write_xlsx(prenec, "Productos/merge_prenec_020725.xlsx")


##### Análisis ----

# variables de identificación

prenec %>%
  group_by(Codigo) %>%
  summarise(Nro = n()) %>%
  summarise(maximo = max(Nro)) # No existen códigos duplicados

# Revisar sexo

table(prenec$Sexo)

class(prenec$Sexo)
# La normalizacion robusta se aplica despues de unir ambas ramas de carga.

# Fecha de nacimiento

table(is.na(prenec$Fecha_Nacimiento)) # tengo 40 fechas de nacimiento perdidas

summary(prenec$Fecha_Nacimiento) # hay fechas del futuro


prenec <- prenec %>%
  mutate(Fecha_Nacimiento_Corregida = case_when(
    Fecha_Nacimiento > Sys.Date() ~ as.Date(NA),
    TRUE ~ Fecha_Nacimiento
  ))

summary(prenec$Fecha_Nacimiento_Corregida) # hay fechas de 2013 que no tienen sentido y de 1900 tampoco tiene sentido según RUT

prenec <- prenec %>%
  mutate(Fecha_Nacimiento_Corregida = case_when(
    Fecha_Nacimiento_Corregida > as.Date("2013-01-01") | Fecha_Nacimiento_Corregida < as.Date("1922-01-01") ~ as.Date(NA),
    TRUE ~ Fecha_Nacimiento_Corregida
  ))

# Revisión de la edad

# names(table(prenec$Edad)) # existen edades negativas, 11, 123 y 2014 ~ transformar a NA
#
# prenec <- prenec %>%
#   mutate(Edad = case_when(
#     Edad <= 11 | Edad >= 123 ~ NA,
#     TRUE ~ Edad
#   ))

# Revisar fecha de enrolamiento

summary(prenec$Fecha_Enrolamiento) # mínimo 31-05-2012 y máximo 29-11-2022

# Calcular nuevamente edad (Edad_2) y comparar con la columna Edad

prenec <- prenec %>%
  mutate(Edad_2 = year(as.period(interval(Fecha_Nacimiento_Corregida, Fecha_Enrolamiento)))) # edad es una variable que se va actualizando con el tiempo

names(table(prenec$Edad_2)) # Edad_2 es la edad al momento del enrolamiento

# Esta nueva base de datos está estructurada de forma diferente. La variable Resultado_TSDO_1 corresponde a dos rondas
# de exámenes (lo que en la base de datos anterior sería Resultado_TSDO_1 y Resultado_TSDO_2). Se debe trabajar
# solamente con las columnas Valor_TSDO_1 _1y Valor_TSDO_2_1, para crear las variables Ronda_1 y Ronda_2, respectivamente

# Definición de rondas ----

prenec <- prenec %>%
  mutate(
    Ronda_1 = case_when(
      Valor_TSDO_1_1 >= 100 ~ "Positivo",
      Valor_TSDO_1_1 < 100 ~ "Negativo",
      TRUE ~ NA
    ),
    Ronda_2 = case_when(
      Valor_TSDO_2_1 >= 100 ~ "Positivo",
      Valor_TSDO_2_1 < 100 ~ "Negativo",
      TRUE ~ NA
    ),
    Ronda_3 = case_when(
      Valor_TSDO_1_2 >= 100 ~ "Positivo",
      Valor_TSDO_1_2 < 100 ~ "Negativo",
      TRUE ~ NA
    ),
    Ronda_4 = case_when(
      Valor_TSDO_2_2 >= 100 ~ "Positivo",
      Valor_TSDO_2_2 < 100 ~ "Negativo",
      TRUE ~ NA
    )
  )

# Tipo histológico, utilizar el Tipo_Histologico_1

names(table(prenec$Tipo_Histologico_1, exclude = "always"))


# Tamaño: Tamano_Polipo_Mayor_1

class(prenec$Tamano_Polipo_Mayor_1)

names(table(prenec$Tamano_Polipo_Mayor_1))

prenec <- prenec %>%
  mutate(Tamano_Polipo_Mayor_1 = str_replace_all(Tamano_Polipo_Mayor_1, "pequeÃ±o", "pequeño"))

# Manejar las observaciones que deben quedar como texto

prenec <- prenec %>%          # MODIFICACIÓN DB
  mutate(
    Tamano_Polipo_Mayor_1 = case_when(
      str_to_lower(Tamano_Polipo_Mayor_1) %in% c("diminuto", "diminutos", "pequeño", "menor de 5 mm") ~ "Pequeño",
      TRUE ~ Tamano_Polipo_Mayor_1 # Mantener el resto de valores sin cambios
    )
  )

# Manejar las observaciones que deben queda como número

prenec <- prenec %>%                           # MODIFICACION BD
  mutate(
    # Reemplazar comas por puntos para los decimales
    Tamano_Polipo_Limpio = str_replace_all(Tamano_Polipo_Mayor_1, ",", "."),

    # Extraer números (permitiendo decimales con puntos o comas convertidos)
    Tamano_Numerico = as.numeric(str_extract(Tamano_Polipo_Limpio, "\\d+(\\.\\d+)?")),

    # Detectar la unidad (cm, mm) sin importar mayúsculas/minúsculas
    Unidad = case_when(
      str_detect(tolower(Tamano_Polipo_Limpio), "cm") ~ "cm",
      str_detect(tolower(Tamano_Polipo_Limpio), "mm") ~ "mm",
      TRUE ~ "desconocido"
    ),

    # Mejorar la conversión a mm
    Tamano_mm = case_when(
      Unidad == "cm" ~ Tamano_Numerico * 10, # Convertir cm a mm
      Unidad == "mm" ~ Tamano_Numerico,     # Mantener en mm
      Unidad == "desconocido" & !is.na(Tamano_Numerico) ~ Tamano_Numerico, # Usar valor directo si no hay unidad pero hay número
      TRUE ~ NA_real_                       # Mantener NA en casos problemáticos
    )
  )

revision_polipo <- prenec %>%
  filter(!is.na(Tamano_Polipo_Mayor_1)) %>%
  select(Tamano_Polipo_Mayor_1, Tamano_Polipo_Limpio, Tamano_Numerico,Unidad, Tamano_mm)

# write_xlsx(revision_polipo, "Productos/revision_polipo.xlsx")

# Se necesita hacer una corrección puntual en la observación de Tamano_Polipo_Mayor_1 que es igual a "5x7 mm"
# ya que quedará al aplicar la función como 5 mm y tamaño pequeño siendo que su mayor dimensión es 7 mm y pertenece
# a la categoría "mediano"

prenec <- prenec %>%
  mutate(Tamano_mm = case_when(
    Tamano_Polipo_Mayor_1 == "5x7 mm" ~ 7,
    Tamano_Polipo_Mayor_1 == "7x10 mm" ~ 10,
    TRUE ~ Tamano_mm
  ))

prenec %>%
  filter(Tamano_Polipo_Mayor_1 == c("5x7 mm", "7x10 mm")) %>%
  select(Tamano_Polipo_Mayor_1, Tamano_mm)

# Definir los valores de Tipo Histologico que son de interés

tipos_interes <- c("Adenocarcinoma intramucoso",
                   "Adenoma tubular con atipia de alto grado",
                   "Adenoma tubular con atipia de bajo grado",
                   "Adenoma tubulo-velloso con atipia de alto grado",
                   "Adenoma tubulo-velloso con atipia de bajo grado",
                   "Adenoma velloso con atipia de alto grado")

# Revisión de Tipo_Histologico_1

class(prenec$Tipo_Histologico_1)
names(table(prenec$Tipo_Histologico_1))

# Crear la variable adenoma_size

prenec <- prenec %>%                                   # MODIFICACION BD
  mutate(adenoma_size = case_when(
    Tipo_Histologico_1 %in% tipos_interes & Tamano_mm <= 5 ~ "pequeño",
    Tipo_Histologico_1 %in% tipos_interes & Tamano_mm >= 6 & Tamano_mm <= 9 ~ "mediano",
    Tipo_Histologico_1 %in% tipos_interes & Tamano_mm >= 10 ~ "grande",
    Tipo_Histologico_1 %in% tipos_interes & Tamano_Polipo_Mayor_1 == "Pequeño" ~ "pequeño",
    TRUE ~ NA
  ))

names(table(prenec$adenoma_size, exclude = "always"))

# Familiar con cáncer de colon, esta variable está asociada a high risk

table(prenec$Familiar_CACO, exclude = "always") # No existen valores NA

prenec <- prenec %>%                 # MODIFICACIÓN BD
  mutate(highrisk = case_when(
    Familiar_CACO == "NO" ~ 0,
    Familiar_CACO == "SI" ~ 1,
    TRUE ~ NA
  ))

# N_Polipos_1 : esta variable es necesaria para construir  n_ad_1, n_ad_2, n_ad_3 y n_ad_4, con valores SI y NO, de acuerdo
# con los siguientes criterios
#   - n_ad_1 = SI, si la variable "n" es igual a 1 y NO en otro caso
#   - n_ad_2 = SI, si la variable "n" es igual a 2 y NO en otro caso
#   - n_ad_3 = SI, si la variable "n" es mayor o igual a 3 y NO en otro caso
#   - n_ad_4 = SI, si la variable "n" es mayor o igual a 1 y NO en otro caso
# Nota: En cualquiera de los casos anteriores se asignará NA si la variable "n" es NA

# Además, se debe crear la variable risk, que combina tamaño y tipo histológico para crear una categoría de low_risk, high_risk y cáncer

class(prenec$N_Polipos_1)
summary(prenec$N_Polipos_1) # Existen 33356 NA y el rango de valores se encuentra entre 0 y 150

prenec <- prenec %>%                                            # MODIFICACIÓN BD
  mutate(
    n_ad_1 = case_when(
      is.na(N_Polipos_1) ~ NA,
      N_Polipos_1 == 1 ~ "SI",
      TRUE ~ "NO"
    ),
    n_ad_2 = case_when(
      is.na(N_Polipos_1) ~ NA,
      N_Polipos_1 == 2 ~ "SI",
      TRUE ~ "NO"
    ),
    n_ad_3 = case_when(
      is.na(N_Polipos_1) ~ NA,
      N_Polipos_1 >= 3 ~ "SI",
      TRUE ~ "NO"
    ),
    n_ad_4 = case_when(
      is.na(N_Polipos_1) ~ NA,
      N_Polipos_1 >= 1 ~ "SI",
      TRUE ~ "NO"
    ),
    risk = case_when(
      Tamano_mm < 10 & Tipo_Histologico_1 == "Adenoma tubular con atipia de bajo grado" ~ "low_risk", # ok
      Tamano_Polipo_Mayor_1 == "Pequeño" & Tipo_Histologico_1 == "Adenoma tubular con atipia de bajo grado" ~ "low_risk", # ok
      Tamano_mm >= 10 & Tipo_Histologico_1 %in% c("Adenoma tubular con atipia de bajo grado",
                                                  "Adenocarcinoma intramucoso",
                                                  "Adenoma tubular con atipia de alto grado",
                                                  "Adenoma tubulo-velloso con atipia de alto grado",
                                                  "Adenoma tubulo-velloso con atipia de bajo grado",
                                                  "Adenoma velloso con atipia de alto grado") ~ "high_risk",
      Tamano_mm < 10 & Tipo_Histologico_1 %in% c("Adenocarcinoma intramucoso",
                                                 "Adenoma tubular con atipia de alto grado",
                                                 "Adenoma tubulo-velloso con atipia de alto grado",
                                                 "Adenoma tubulo-velloso con atipia de bajo grado",
                                                 "Adenoma velloso con atipia de alto grado") ~ "high_risk",
      Tipo_Histologico_1 %in% c("Adenocarcinoma bien diferenciado",
                                "Adenocarcinoma moderadamente diferenciado",
                                "Adenocarcinoma pobremente diferenciado",
                                "Otros tipo de carcinoma",
                                "Neoplasia No Epitelial") ~ "cancer",
      TRUE ~ NA
    )
  )

# Crear variable adenoma:

prenec <- prenec %>%                                                                  # MODIFICACIÓN BD
  mutate(adenoma = case_when(
    Tipo_Histologico_1 %in% c(
      "Adenocarcinoma intramucoso",
      "Adenoma tubular con atipia de alto grado",
      "Adenoma tubular con atipia de bajo grado",
      "Adenoma tubulo-velloso con atipia de alto grado",
      "Adenoma tubulo-velloso con atipia de bajo grado",
      "Adenoma velloso con atipia de alto grado"
    ) ~ 1,
    TRUE ~ 0
  ))

# Variable 1° fecha colonoscopía: Fecha_1era_Colono

summary(prenec$Fecha_1era_Colono) # mínimo 06-07-2012 y máximo 31-03-2023

# Es posible que los pacientes se hayan realizado una colonoscopía pero no asociada a su primer colon
# check. Para poder estimar la adherencia, es necesario utilizar  a los pacientes que tuvieron colonoscopía
# luego del primer colon check y antes del segundo.

class(prenec$Fecha_1er_Colon_check)
class(prenec$Fecha_2do_Colon_check)
class(prenec$Fecha_3er_Colon_check)

# Crear la variable indicadora considerando valores NA en Fecha_2do_Colon_check

prenec <- prenec %>%
  mutate(
    Indicador_Fecha_Colono = case_when(
      !is.na(Fecha_1era_Colono) &
        Fecha_1era_Colono >= Fecha_1er_Colon_check &
        (is.na(Fecha_2do_Colon_check) | Fecha_1era_Colono < Fecha_2do_Colon_check) ~ Fecha_1era_Colono,
      TRUE ~ as.Date(NA)
    )
  )

# write_xlsx(prenec, "Productos/prenec_transformaciones_020725.xlsx")

# Nota: para cálculo de la adherencia utilizar la variable Indicador_Fecha_Colono_Colono

# Crear tres bases de datos en función de la base prenec:

} else {
  if (!file.exists(ruta_base_transformada)) {
    stop("No existe la base transformada histórica: ", ruta_base_transformada)
  }
  fecha_excel_a_date <- function(x) {
    as.Date(suppressWarnings(as.numeric(as.character(x))), origin = "1899-12-30")
  }

  prenec <- read_excel(ruta_base_transformada) %>%
    mutate(
      Fecha_1era_Colono_analisis = as.Date(Fecha_1era_Colono_formato),
      Fecha_1er_Colon_check_analisis = fecha_excel_a_date(Fecha_1er_Colon_check),
      Fecha_2do_Colon_check_analisis = fecha_excel_a_date(Fecha_2do_Colon_check),
      Indicador_Fecha_Colono = case_when(
        !is.na(Fecha_1era_Colono_analisis) &
          Fecha_1era_Colono_analisis >= Fecha_1er_Colon_check_analisis &
          (is.na(Fecha_2do_Colon_check_analisis) |
             Fecha_1era_Colono_analisis < Fecha_2do_Colon_check_analisis) ~ Fecha_1era_Colono_analisis,
        TRUE ~ as.Date(NA)
      )
    )
}

# Normalizacion comun del sexo para ambas fuentes. La base historica usa
# Femenino/Masculino, mientras las bases regionales quedan como Mujer/Hombre.
# Se conserva Sexo original y se usa Sexo_analisis solo para las subbases.
sexo_texto <- str_to_lower(str_squish(as.character(prenec$Sexo)), locale = "es")
prenec <- prenec %>%
  mutate(
    Sexo_analisis = case_when(
      sexo_texto %in% c("femenino", "mujer", "female", "f") ~ "Mujer",
      sexo_texto %in% c("masculino", "hombre", "male", "m") ~ "Hombre",
      TRUE ~ NA_character_
    )
  )

sexo_no_clasificado <- prenec %>%
  filter(highrisk == 0, is.na(Sexo_analisis))
valores_no_clasificados <- character(0)
if (nrow(sexo_no_clasificado) > 0L) {
  valores_no_clasificados <- unique(as.character(sexo_no_clasificado$Sexo))
  valores_no_clasificados[is.na(valores_no_clasificados)] <- "<NA>"
  stop(
    paste0(
      "No se puede particionar toda la cohorte analitica por sexo. ",
      "Valores no reconocidos en Sexo: ",
      paste(valores_no_clasificados, collapse = ", "),
      "."
    )
  )
}
rm(sexo_texto, sexo_no_clasificado, valores_no_clasificados)

# Sorteo complementario por individuo ----
# Para personas con ambas rondas observadas, una moneda justa decide si A usa
# Ronda_1 y B Ronda_2, o viceversa. Si solo existe una ronda, ambas bases usan
# el único resultado disponible; si no existe ninguna, ambas quedan en NA.
# El orden por Codigo hace reproducible la asignación mientras no cambie la cohorte.

if (anyNA(prenec$Codigo) || anyDuplicated(prenec$Codigo) > 0L) {
  stop("Codigo debe ser único y no faltante para sortear por individuo.")
}

orden_codigo <- order(as.character(prenec$Codigo), method = "radix")
RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
set.seed(SEMILLA_SORTEO)
u_sorteo <- numeric(nrow(prenec))
u_sorteo[orden_codigo] <- runif(nrow(prenec))

prenec <- prenec %>%
  mutate(
    ambas_rondas_observadas = !is.na(Ronda_1) & !is.na(Ronda_2),
    ronda_sorteada_A = case_when(
      ambas_rondas_observadas & u_sorteo < 0.5 ~ 1L,
      ambas_rondas_observadas                 ~ 2L,
      !is.na(Ronda_1)                         ~ 1L,
      !is.na(Ronda_2)                         ~ 2L,
      TRUE                                    ~ NA_integer_
    ),
    ronda_sorteada_B = case_when(
      ambas_rondas_observadas & u_sorteo < 0.5 ~ 2L,
      ambas_rondas_observadas                 ~ 1L,
      !is.na(Ronda_1)                         ~ 1L,
      !is.na(Ronda_2)                         ~ 2L,
      TRUE                                    ~ NA_integer_
    ),
    FIT_A = case_when(
      ronda_sorteada_A == 1L ~ Ronda_1,
      ronda_sorteada_A == 2L ~ Ronda_2,
      TRUE                   ~ NA_character_
    ),
    FIT_B = case_when(
      ronda_sorteada_B == 1L ~ Ronda_1,
      ronda_sorteada_B == 2L ~ Ronda_2,
      TRUE                   ~ NA_character_
    )
  )

# Controles de integridad del sorteo. Se comparan valores y NA de forma
# explícita para que una falla no quede oculta por la lógica trivaluada de R.
son_iguales_incluyendo_na <- function(x, y) {
  (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y)
}

indice_ambas <- prenec$ambas_rondas_observadas
asignacion_complementaria <-
  (prenec$ronda_sorteada_A == 1L & prenec$ronda_sorteada_B == 2L) |
  (prenec$ronda_sorteada_A == 2L & prenec$ronda_sorteada_B == 1L)
if (any(indice_ambas & !asignacion_complementaria)) {
  stop("Fallo de integridad: A y B no recibieron rondas opuestas en todos los casos con ambas rondas.")
}

indice_una <- xor(!is.na(prenec$Ronda_1), !is.na(prenec$Ronda_2))
resultado_unico <- coalesce(prenec$Ronda_1, prenec$Ronda_2)
if (any(indice_una & !son_iguales_incluyendo_na(prenec$FIT_A, resultado_unico)) ||
    any(indice_una & !son_iguales_incluyendo_na(prenec$FIT_B, resultado_unico))) {
  stop("Fallo de integridad: no se copió la única ronda observada en ambas bases.")
}

indice_ninguna <- is.na(prenec$Ronda_1) & is.na(prenec$Ronda_2)
if (any(indice_ninguna & (!is.na(prenec$FIT_A) | !is.na(prenec$FIT_B)))) {
  stop("Fallo de integridad: se asignó un resultado a una persona sin rondas observadas.")
}

fuente_analisis <- if (usar_base_transformada) ruta_base_transformada else ruta_archivos

##### Base Resumen A ----

#   a) BaseResumenA: esta base debe ser un subset de todas las personas con highrisk = 0 y:
#
#   i.    Debe crearse una nueva variable llamada "pop" que contenga la suma de las personas por edad
#   ii.   Debe crearse también una nueva variable "ad_small" que contenga la suma de personas por edad, cuya variable
#         "adenoma_size" = pequeño. Esta suma debe ser solo para las personas cuyo valor "Resultado TSO 1" = Positivo.
#   iii.  Debe crearse también una nueva variable "ad_medium" que contenga la suma de personas por edad, cuya variable
#         "adenoma_size" = mediano. Esta suma debe ser solo para las personas cuyo valor "Resultado TSDO 1" = Positivo.
#   iv.   Debe crearse también una nueva variable "ad_large" que contenga la suma de personas por edad, cuya variable
#         "adenoma_size" = grande. Esta suma debe ser solo para las personas cuyo valor "Resultado TSO 1" = Positivo.
#   v.    Debe crearse una nueva variable "1_ad" que contenga la suma de personas por edad cuya variable "n_ad_1" = SI.
#         Esta suma debe ser solo para las personas cuyo valor "Resultado TSDO 1" = Positivo.
#   vi.   Debe crearse una nueva variable "2_ad" que contenga la suma de personas por edad cuya variable "n_ad_2" = SI.
#         Esta suma debe ser solo para las personas cuyo valor "Resultado TSDO 1" = Positivo.
#   vii.  Debe crearse una nueva variable "3_ad" que contenga la suma de personas por edad cuya variable "n_ad_3" = SI.
#         Esta suma debe ser solo para las personas cuyo valor "Resultado TSDO 1" = Positivo.
#   viii. Debe crearse una nueva variable "1_adormore" que contenga la suma de personas por edad cuya variable "n_ad_4" = SI.
#         Esta suma debe ser solo para las personas cuyo valor "Resultado TSDO 1" = Positivo.

# Crear la variable "pop"

BaseResumenA <- prenec %>%
  filter(highrisk == 0) %>%
  group_by(Edad_2) %>%
  mutate(pop = n()) %>%
  ungroup()

# Crear la variable "ad_small". Nota: la variable "adenoma_size" contiene NA que deben ser ignorados para que se apliquen los
# códigos correctamente
# En la construcción de las bases A y B tenemos que considerar cuandoe estimamos adenomas solo aquellos que provienen
# de una colonoscopía con fecha anterior a la segunda colon check.

BaseResumenA <- BaseResumenA %>%
  group_by(Edad_2) %>%
  mutate(
    ad_small = ifelse(adenoma_size == "pequeño" & FIT_A == "Positivo" & !is.na(Indicador_Fecha_Colono),
                      sum(adenoma_size == "pequeño" & FIT_A == "Positivo" & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                      NA
    ),
    ad_medium = ifelse(adenoma_size == "mediano" & FIT_A == "Positivo" & !is.na(Indicador_Fecha_Colono),
                       sum(adenoma_size == "mediano" & FIT_A == "Positivo" & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                       NA
    ),
    ad_large = ifelse(adenoma_size == "grande" & FIT_A == "Positivo" & !is.na(Indicador_Fecha_Colono),
                      sum(adenoma_size == "grande" & FIT_A == "Positivo" & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                      NA
    ),
    `1_ad` = ifelse(n_ad_1 == "SI" & FIT_A == "Positivo" & !is.na(Indicador_Fecha_Colono),
                    sum(n_ad_1 == "SI" & FIT_A == "Positivo" & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                    NA
    ),
    `2_ad` = ifelse(n_ad_2 == "SI" & FIT_A == "Positivo" & !is.na(Indicador_Fecha_Colono),
                    sum(n_ad_2 == "SI" & FIT_A == "Positivo" & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                    NA
    ),
    `3_adormore` = ifelse(n_ad_3 == "SI" & FIT_A == "Positivo" & !is.na(Indicador_Fecha_Colono),
                          sum(n_ad_3 == "SI" & FIT_A == "Positivo" & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                          NA
    ),
    `1_adormore` = ifelse(n_ad_4 == "SI" & FIT_A == "Positivo" & !is.na(Indicador_Fecha_Colono),
                          sum(n_ad_4 == "SI" & FIT_A == "Positivo" & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                          NA
    ),
    grupo_edad = ifelse(FIT_A == "Positivo", as.character(cut(Edad_2,
                                                                breaks = c(seq(0, 80, by = 5), Inf),
                                                                labels = c(paste(seq(0, 75, by = 5), seq(4, 79, by = 5), sep = "-"), "80 o más"),
                                                                right = FALSE)),
                        NA
    )
  ) %>%
  ungroup()


BaseResumenA <- BaseResumenA %>%
  group_by(grupo_edad) %>%
  mutate(
    adherencia_nro = ifelse(FIT_A == "Positivo",
                            sum(!is.na(Indicador_Fecha_Colono) & FIT_A == "Positivo"),
                            NA),
    adherencia_porcentaje = ifelse(FIT_A == "Positivo",
                                   round((adherencia_nro / sum(FIT_A == "Positivo", na.rm = TRUE)) * 100, 2),
                                   NA)
  ) %>%
  ungroup()

# write_xlsx(BaseResumenA, "Productos/BaseResumenA_15.04.2025.xlsx")


BaseResumenA_N <- BaseResumenA %>%
  group_by(Edad_2) %>%
  mutate(
    ad_small = ifelse(adenoma_size == "pequeño" & FIT_A == "Positivo" & !is.na(Indicador_Fecha_Colono),
                      sum(adenoma_size == "pequeño" & FIT_A == "Positivo" & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                      NA
    ),
    ad_medium = ifelse(adenoma_size == "mediano" & FIT_A == "Positivo" & !is.na(Indicador_Fecha_Colono),
                       sum(adenoma_size == "mediano" & FIT_A == "Positivo" & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                       NA
    ),
    ad_large = ifelse(adenoma_size == "grande" & FIT_A == "Positivo" & !is.na(Indicador_Fecha_Colono),
                      sum(adenoma_size == "grande" & FIT_A == "Positivo" & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                      NA
    ),
    low_risk = ifelse(risk == "low_risk" & FIT_A == "Positivo" & !is.na(Indicador_Fecha_Colono),
                      sum(risk == "low_risk" & FIT_A == "Positivo" & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                      NA
    ),
    high_risk = ifelse(risk == "high_risk" & FIT_A == "Positivo" & !is.na(Indicador_Fecha_Colono),
                       sum(risk == "high_risk" & FIT_A == "Positivo" & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                       NA
    ),
    cancer = ifelse(risk == "cancer" & FIT_A == "Positivo" & !is.na(Indicador_Fecha_Colono),
                    sum(risk == "cancer" & FIT_A == "Positivo" & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                    NA
    ),
    ads1 = ifelse(adenoma_size == "pequeño" & FIT_A == "Positivo" & N_Polipos_1 == 1 & !is.na(Indicador_Fecha_Colono),
                  sum(adenoma_size == "pequeño" & FIT_A == "Positivo" & N_Polipos_1 == 1 & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                  NA
    ),
    adm1 = ifelse(adenoma_size == "mediano" & FIT_A == "Positivo" & N_Polipos_1 == 1 & !is.na(Indicador_Fecha_Colono),
                  sum(adenoma_size == "mediano" & FIT_A == "Positivo" & N_Polipos_1 == 1 & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                  NA
    ),
    adl1 = ifelse(adenoma_size == "grande" & FIT_A == "Positivo" & N_Polipos_1 == 1 & !is.na(Indicador_Fecha_Colono),
                  sum(adenoma_size == "grande" & FIT_A == "Positivo" & N_Polipos_1 == 1 & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                  NA
    ),
    ads2 = ifelse(adenoma_size == "pequeño" & FIT_A == "Positivo" & N_Polipos_1 == 2 & !is.na(Indicador_Fecha_Colono),
                  sum(adenoma_size == "pequeño" & FIT_A == "Positivo" & N_Polipos_1 == 2 & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                  NA
    ),
    adm2 = ifelse(adenoma_size == "mediano" & FIT_A == "Positivo" & N_Polipos_1 == 2 & !is.na(Indicador_Fecha_Colono),
                  sum(adenoma_size == "mediano" & FIT_A == "Positivo" & N_Polipos_1 == 2 & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                  NA
    ),
    adl2 = ifelse(adenoma_size == "grande" & FIT_A == "Positivo" & N_Polipos_1 == 2 & !is.na(Indicador_Fecha_Colono),
                  sum(adenoma_size == "grande" & FIT_A == "Positivo" & N_Polipos_1 == 2 & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                  NA
    ),
    ads3m = ifelse(adenoma_size == "pequeño" & FIT_A == "Positivo" & N_Polipos_1 >= 3 & !is.na(Indicador_Fecha_Colono),
                   sum(adenoma_size == "pequeño" & FIT_A == "Positivo" & N_Polipos_1 >= 3 & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                   NA
    ),
    adm3m = ifelse(adenoma_size == "mediano" & FIT_A == "Positivo" & N_Polipos_1 >= 3 & !is.na(Indicador_Fecha_Colono),
                   sum(adenoma_size == "mediano" & FIT_A == "Positivo" & N_Polipos_1 >= 3 & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                   NA
    ),
    adl3m = ifelse(adenoma_size == "grande" & FIT_A == "Positivo" & N_Polipos_1 >= 3 & !is.na(Indicador_Fecha_Colono),
                   sum(adenoma_size == "grande" & FIT_A == "Positivo" & N_Polipos_1 >= 3 & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                   NA
    )
  ) %>%
  ungroup()

# write_xlsx(BaseResumenA_N, "Productos/BaseResumenA_N_15.04.2025.xlsx")

# colapso de las bases de datos

# BaseResumenA

edad_poblacionA <- BaseResumenA %>%
  group_by(Edad_2) %>%
  summarise(pop = first(na.omit(pop)))

resumen_edad_unicaA <- BaseResumenA %>%
  filter(FIT_A == "Positivo") %>%
  group_by(Edad_2) %>%
  summarise(across(c(ad_small, ad_medium, ad_large, `1_ad`, `2_ad`, `3_adormore`, `1_adormore`),
                   ~ ifelse(all(is.na(.)), NA, first(na.omit(.))), .names = "unique_{col}")) %>%
  complete(Edad_2 = unique(BaseResumenA$Edad_2))

# Paso 3: Unir ambas bases (edad_poblacion y resumen_edad_unica) por la columna Edad

base_finalA <- resumen_edad_unicaA %>%
  left_join(edad_poblacionA, by = "Edad_2") %>%
  select(Edad_2, pop, unique_ad_small, unique_ad_medium, unique_ad_large, unique_1_ad, unique_2_ad, unique_3_adormore, unique_1_adormore) %>%
  rename_with(~ str_remove(., "^unique_"), starts_with("unique_"))

# write_xlsx(base_finalA, "Productos/base_finalA_15.04.2025.xlsx")

# BaseResumenA_N

edad_poblacionA_N <- BaseResumenA %>%
  group_by(Edad_2) %>%
  summarise(pop = first(na.omit(pop)))

resumen_edad_unicaA_N <- BaseResumenA_N %>%
  filter(FIT_A == "Positivo") %>%
  group_by(Edad_2) %>%
  summarise(across(c(ad_small, ad_medium, ad_large, `low_risk`, `high_risk`,`cancer`,`ads1`,`adm1`,
                     `adl1`,`ads2`,`adm2`,`adl2`,`ads3m`,`adm3m`,`adl3m`),
                   ~ ifelse(all(is.na(.)), NA, first(na.omit(.))), .names = "unique_{col}")) %>%
  complete(Edad_2 = unique(BaseResumenA$Edad_2))

# Paso 3: Unir ambas bases (edad_poblacion y resumen_edad_unica) por la columna Edad

base_finalA_N <- resumen_edad_unicaA_N %>%
  left_join(edad_poblacionA_N, by = "Edad_2") %>%
  select(Edad_2, pop, unique_ad_small, unique_ad_medium, unique_ad_large, unique_low_risk, unique_high_risk, unique_cancer, unique_ads1,
         unique_adm1, unique_adl1, unique_ads2, unique_adm2, unique_adl2, unique_ads3m, unique_adm3m, unique_adl3m) %>%
  rename_with(~ str_remove(., "^unique_"), starts_with("unique_"))


# write_xlsx(base_finalA_N, "Productos/Base_finalA_N_15.04.2025.xlsx")

##### Base Resumen B ----

# b) BaseResumenB: esta base debe ser un subset de todas las personas con highrisk = 0 y:
#
# i.    Debe crearse una nueva variable llamada "pop" que contenga la suma de las personas por edad
# ii.   Debe crearse también una nueva variable "ad_small" que contenga la suma de personas por edad, cuya variable
#       "adenoma_size" = pequeño. Esta suma debe ser solo para las personas cuyo valor "Resultado TSO 2" = Positivo.
# iii.  Debe crearse también una nueva variable "ad_medium" que contenga la suma de personas por edad, cuya variable
#       "adenoma_size" = mediano. Esta suma debe ser solo para las personas cuyo valor "Resultado TSDO 2" = Positivo.
# iv.   Debe crearse también una nueva variable "ad_large" que contenga la suma de personas por edad, cuya variable
#       "adenoma_size" = grande. Esta suma debe ser solo para las personas cuyo valor "Resultado TSO 2" = Positivo.
# v.    Debe crearse una nueva variable "1_ad" que contenga la suma de personas por edad cuya variable "n_ad_1" = SI.
#       Esta suma debe ser solo para las personas cuyo valor "Resultado TSDO 2" = Positivo.
# vi.   Debe crearse una nueva variable "2_ad" que contenga la suma de personas por edad cuya variable "n_ad_2" = SI.
#       Esta suma debe ser solo para las personas cuyo valor "Resultado TSDO 2" = Positivo.
# vii.  Debe crearse una nueva variable "3_ad" que contenga la suma de personas por edad cuya variable "n_ad_3" = SI.
#       Esta suma debe ser solo para las personas cuyo valor "Resultado TSDO 2" = Positivo.
# viii. Debe crearse una nueva variable "1_adormore" que contenga la suma de personas por edad cuya variable "n_ad_4" = SI.
#       Esta suma debe ser solo para las personas cuyo valor "Resultado TSDO 2" = Positivo.


# Crear la variable "pop"

BaseResumenB <- prenec %>%
  filter(highrisk == 0) %>%
  group_by(Edad_2) %>%
  mutate(pop = n()) %>%
  ungroup()

# Crear la variable "ad_small". Nota: la variable "adenoma_size" contiene NA que deben ser ignorados para que se apliquen los
# códigos correctamente

BaseResumenB <- BaseResumenB %>%
  group_by(Edad_2) %>%
  mutate(
    ad_small = ifelse(adenoma_size == "pequeño" & FIT_B == "Positivo" & !is.na(Indicador_Fecha_Colono),
                      sum(adenoma_size == "pequeño" & FIT_B == "Positivo" & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                      NA
    ),
    ad_medium = ifelse(adenoma_size == "mediano" & FIT_B == "Positivo" & !is.na(Indicador_Fecha_Colono),
                       sum(adenoma_size == "mediano" & FIT_B == "Positivo" & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                       NA
    ),
    ad_large = ifelse(adenoma_size == "grande" & FIT_B == "Positivo" & !is.na(Indicador_Fecha_Colono),
                      sum(adenoma_size == "grande" & FIT_B == "Positivo" & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                      NA
    ),
    `1_ad` = ifelse(n_ad_1 == "SI" & FIT_B == "Positivo" & !is.na(Indicador_Fecha_Colono),
                    sum(n_ad_1 == "SI" & FIT_B == "Positivo" & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                    NA
    ),
    `2_ad` = ifelse(n_ad_2 == "SI" & FIT_B == "Positivo" & !is.na(Indicador_Fecha_Colono),
                    sum(n_ad_2 == "SI" & FIT_B == "Positivo" & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                    NA
    ),
    `3_adormore` = ifelse(n_ad_3 == "SI" & FIT_B == "Positivo" & !is.na(Indicador_Fecha_Colono),
                          sum(n_ad_3 == "SI" & FIT_B == "Positivo" & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                          NA
    ),
    `1_adormore` = ifelse(n_ad_4 == "SI" & FIT_B == "Positivo" & !is.na(Indicador_Fecha_Colono),
                          sum(n_ad_4 == "SI" & FIT_B == "Positivo" & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                          NA
    ),
    grupo_edad = ifelse(FIT_B == "Positivo", as.character(cut(Edad_2,
                                                                breaks = c(seq(0, 80, by = 5), Inf),
                                                                labels = c(paste(seq(0, 75, by = 5), seq(4, 79, by = 5), sep = "-"), "80 o más"),
                                                                right = FALSE)),
                        NA
    )) %>%
  ungroup()

BaseResumenB <- BaseResumenB %>%
  group_by(grupo_edad) %>%
  mutate(
    adherencia_nro = ifelse(FIT_B == "Positivo",
                            sum(!is.na(Indicador_Fecha_Colono) & FIT_B == "Positivo"),
                            NA),
    adherencia_porcentaje = ifelse(FIT_B == "Positivo",
                                   round((adherencia_nro / sum(FIT_B == "Positivo", na.rm = TRUE)) * 100, 2),
                                   NA)
  ) %>%
  ungroup()


# write_xlsx(BaseResumenB, "Productos/BaseResumenB_15.04.2025.xlsx")

BaseResumenB_N <- BaseResumenB %>%
  group_by(Edad_2) %>%
  mutate(
    ad_small = ifelse(adenoma_size == "pequeño" & FIT_B == "Positivo" & !is.na(Indicador_Fecha_Colono),
                      sum(adenoma_size == "pequeño" & FIT_B == "Positivo" & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                      NA
    ),
    ad_medium = ifelse(adenoma_size == "mediano" & FIT_B == "Positivo" & !is.na(Indicador_Fecha_Colono),
                       sum(adenoma_size == "mediano" & FIT_B == "Positivo" & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                       NA
    ),
    ad_large = ifelse(adenoma_size == "grande" & FIT_B == "Positivo" & !is.na(Indicador_Fecha_Colono),
                      sum(adenoma_size == "grande" & FIT_B == "Positivo" & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                      NA
    ),
    low_risk = ifelse(risk == "low_risk" & FIT_B == "Positivo" & !is.na(Indicador_Fecha_Colono),
                      sum(risk == "low_risk" & FIT_B == "Positivo" & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                      NA
    ),
    high_risk = ifelse(risk == "high_risk" & FIT_B == "Positivo" & !is.na(Indicador_Fecha_Colono),
                       sum(risk == "high_risk" & FIT_B == "Positivo" & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                       NA
    ),
    cancer = ifelse(risk == "cancer" & FIT_B == "Positivo" & !is.na(Indicador_Fecha_Colono),
                    sum(risk == "cancer" & FIT_B == "Positivo" & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                    NA
    ),
    ads1 = ifelse(adenoma_size == "pequeño" & FIT_B == "Positivo" & N_Polipos_1 == 1 & !is.na(Indicador_Fecha_Colono),
                  sum(adenoma_size == "pequeño" & FIT_B == "Positivo" & N_Polipos_1 == 1 & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                  NA
    ),
    adm1 = ifelse(adenoma_size == "mediano" & FIT_B == "Positivo" & N_Polipos_1 == 1 & !is.na(Indicador_Fecha_Colono),
                  sum(adenoma_size == "mediano" & FIT_B == "Positivo" & N_Polipos_1 == 1 & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                  NA
    ),
    adl1 = ifelse(adenoma_size == "grande" & FIT_B == "Positivo" & N_Polipos_1 == 1 & !is.na(Indicador_Fecha_Colono),
                  sum(adenoma_size == "grande" & FIT_B == "Positivo" & N_Polipos_1 == 1 & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                  NA
    ),
    ads2 = ifelse(adenoma_size == "pequeño" & FIT_B == "Positivo" & N_Polipos_1 == 2 & !is.na(Indicador_Fecha_Colono),
                  sum(adenoma_size == "pequeño" & FIT_B == "Positivo" & N_Polipos_1 == 2 & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                  NA
    ),
    adm2 = ifelse(adenoma_size == "mediano" & FIT_B == "Positivo" & N_Polipos_1 == 2 & !is.na(Indicador_Fecha_Colono),
                  sum(adenoma_size == "mediano" & FIT_B == "Positivo" & N_Polipos_1 == 2 & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                  NA
    ),
    adl2 = ifelse(adenoma_size == "grande" & FIT_B == "Positivo" & N_Polipos_1 == 2 & !is.na(Indicador_Fecha_Colono),
                  sum(adenoma_size == "grande" & FIT_B == "Positivo" & N_Polipos_1 == 2 & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                  NA
    ),
    ads3m = ifelse(adenoma_size == "pequeño" & FIT_B == "Positivo" & N_Polipos_1 >= 3 & !is.na(Indicador_Fecha_Colono),
                   sum(adenoma_size == "pequeño" & FIT_B == "Positivo" & N_Polipos_1 >= 3 & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                   NA
    ),
    adm3m = ifelse(adenoma_size == "mediano" & FIT_B == "Positivo" & N_Polipos_1 >= 3 & !is.na(Indicador_Fecha_Colono),
                   sum(adenoma_size == "mediano" & FIT_B == "Positivo" & N_Polipos_1 >= 3 & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                   NA
    ),
    adl3m = ifelse(adenoma_size == "grande" & FIT_B == "Positivo" & N_Polipos_1 >= 3 & !is.na(Indicador_Fecha_Colono),
                   sum(adenoma_size == "grande" & FIT_B == "Positivo" & N_Polipos_1 >= 3 & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
                   NA
    )
  ) %>%
  ungroup()

# write_xlsx(BaseResumenB_N, "Productos/BaseResumenB_N_15.04.2025.xlsx")


# colapso de la base de datos

# BaseResumenB

edad_poblacionB <- BaseResumenB %>%
  group_by(Edad_2) %>%
  summarise(pop = first(na.omit(pop)))

resumen_edad_unicaB <- BaseResumenB %>%
  filter(FIT_B == "Positivo") %>%
  group_by(Edad_2) %>%
  summarise(across(c(ad_small, ad_medium, ad_large, `1_ad`, `2_ad`, `3_adormore`, `1_adormore`),
                   ~ ifelse(all(is.na(.)), NA, first(na.omit(.))), .names = "unique_{col}")) %>%
  complete(Edad_2 = unique(BaseResumenB$Edad_2))

# Paso 3: Unir ambas bases (edad_poblacion y resumen_edad_unica) por la columna Edad

base_finalB <- resumen_edad_unicaB %>%
  left_join(edad_poblacionB, by = "Edad_2") %>%
  select(Edad_2, pop, unique_ad_small, unique_ad_medium, unique_ad_large, unique_1_ad, unique_2_ad, unique_3_adormore, unique_1_adormore) %>%
  rename_with(~ str_remove(., "^unique_"), starts_with("unique_"))

# write_xlsx(base_finalB,"Productos/Base_finalB_15.04.2025.xlsx")

# BaseResumenB_N

edad_poblacionB_N <- BaseResumenB %>%
  group_by(Edad_2) %>%
  summarise(pop = first(na.omit(pop)))

resumen_edad_unicaB_N <- BaseResumenB_N %>%
  filter(FIT_B == "Positivo") %>%
  group_by(Edad_2) %>%
  summarise(across(c(ad_small, ad_medium, ad_large, `low_risk`, `high_risk`,`cancer`,`ads1`,`adm1`,
                     `adl1`,`ads2`,`adm2`,`adl2`,`ads3m`,`adm3m`,`adl3m`),
                   ~ ifelse(all(is.na(.)), NA, first(na.omit(.))), .names = "unique_{col}")) %>%
  complete(Edad_2 = unique(BaseResumenB$Edad_2))

# Paso 3: Unir ambas bases (edad_poblacion y resumen_edad_unica) por la columna Edad

base_finalB_N <- resumen_edad_unicaB_N %>%
  left_join(edad_poblacionB_N, by = "Edad_2") %>%
  select(Edad_2, pop, unique_ad_small, unique_ad_medium, unique_ad_large, unique_low_risk, unique_high_risk, unique_cancer, unique_ads1,
         unique_adm1, unique_adl1, unique_ads2, unique_adm2, unique_adl2, unique_ads3m, unique_adm3m, unique_adl3m) %>%
  rename_with(~ str_remove(., "^unique_"), starts_with("unique_"))

# write_xlsx(base_finalB_N, "Productos/Base_finalB_N_15.04.2025.xlsx")


##### Exportación y comparación de las bases aleatorizadas ----

variables_resultado <- c(
  "adenoma_total", "ad_small", "ad_medium", "ad_large",
  "low_risk", "high_risk", "cancer",
  "ads1", "adm1", "adl1", "ads2", "adm2", "adl2", "ads3m", "adm3m", "adl3m"
)

variables_denominador <- c(
  "fit_observed_n", "fit_positive_n", "colonoscopy_n"
)

variables_diagnostico <- c(
  "adenoma_sin_clasificacion_tamano",
  "adenoma_sin_clasificacion_riesgo"
)

variables_comparacion <- c(variables_resultado, variables_diagnostico)

# Construye una referencia por edad usando una ronda FIT fija sobre exactamente
# la misma cohorte analítica que las bases aleatorizadas. Los ceros se dejan
# como NA para mantener el formato de las bases históricas.
construir_base_referencia <- function(datos, variable_fit) {
  datos %>%
    filter(highrisk == 0) %>%
    group_by(Edad_2) %>%
    summarise(
      pop = n(),
      fit_observed_n = sum(!is.na(.data[[variable_fit]])),
      fit_positive_n = sum(.data[[variable_fit]] == "Positivo", na.rm = TRUE),
      colonoscopy_n = sum(
        .data[[variable_fit]] == "Positivo" & !is.na(Indicador_Fecha_Colono),
        na.rm = TRUE
      ),
      adenoma_total = sum(
        adenoma == 1 &
          .data[[variable_fit]] == "Positivo" &
          !is.na(Indicador_Fecha_Colono),
        na.rm = TRUE
      ),
      ad_small = sum(adenoma_size == "pequeño" & .data[[variable_fit]] == "Positivo" & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
      ad_medium = sum(adenoma_size == "mediano" & .data[[variable_fit]] == "Positivo" & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
      ad_large = sum(adenoma_size == "grande" & .data[[variable_fit]] == "Positivo" & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
      low_risk = sum(risk == "low_risk" & .data[[variable_fit]] == "Positivo" & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
      high_risk = sum(risk == "high_risk" & .data[[variable_fit]] == "Positivo" & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
      cancer = sum(risk == "cancer" & .data[[variable_fit]] == "Positivo" & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
      ads1 = sum(adenoma_size == "pequeño" & .data[[variable_fit]] == "Positivo" & N_Polipos_1 == 1 & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
      adm1 = sum(adenoma_size == "mediano" & .data[[variable_fit]] == "Positivo" & N_Polipos_1 == 1 & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
      adl1 = sum(adenoma_size == "grande" & .data[[variable_fit]] == "Positivo" & N_Polipos_1 == 1 & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
      ads2 = sum(adenoma_size == "pequeño" & .data[[variable_fit]] == "Positivo" & N_Polipos_1 == 2 & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
      adm2 = sum(adenoma_size == "mediano" & .data[[variable_fit]] == "Positivo" & N_Polipos_1 == 2 & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
      adl2 = sum(adenoma_size == "grande" & .data[[variable_fit]] == "Positivo" & N_Polipos_1 == 2 & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
      ads3m = sum(adenoma_size == "pequeño" & .data[[variable_fit]] == "Positivo" & N_Polipos_1 >= 3 & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
      adm3m = sum(adenoma_size == "mediano" & .data[[variable_fit]] == "Positivo" & N_Polipos_1 >= 3 & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
      adl3m = sum(adenoma_size == "grande" & .data[[variable_fit]] == "Positivo" & N_Polipos_1 >= 3 & !is.na(Indicador_Fecha_Colono), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      across(
        all_of(variables_resultado),
        ~ replace(as.numeric(.x), .x == 0, NA_real_)
      ),
      adenoma_sin_clasificacion_tamano =
        replace_na(adenoma_total, 0) -
        replace_na(ad_small, 0) -
        replace_na(ad_medium, 0) -
        replace_na(ad_large, 0),
      adenoma_sin_clasificacion_riesgo =
        replace_na(adenoma_total, 0) -
        replace_na(low_risk, 0) -
        replace_na(high_risk, 0)
    ) %>%
    arrange(Edad_2)
}

# Las bases totales se reconstruyen con el mismo helper que las subbases por
# sexo. Esto evita diferencias de esquema y de definición entre bases.
base_finalA_N <- construir_base_referencia(prenec, "FIT_A")
base_finalB_N <- construir_base_referencia(prenec, "FIT_B")

base_referencia_R1 <- construir_base_referencia(prenec, "Ronda_1")
base_referencia_R2 <- construir_base_referencia(prenec, "Ronda_2")

# Subbases por sexo. Se calculan desde los individuos ya sorteados, por lo que
# no se realiza un segundo sorteo y cada persona conserva FIT_A/FIT_B.
base_finalA_N_hombres <- construir_base_referencia(
  prenec %>% filter(Sexo_analisis == "Hombre"),
  "FIT_A"
)
base_finalA_N_mujeres <- construir_base_referencia(
  prenec %>% filter(Sexo_analisis == "Mujer"),
  "FIT_A"
)
base_finalB_N_hombres <- construir_base_referencia(
  prenec %>% filter(Sexo_analisis == "Hombre"),
  "FIT_B"
)
base_finalB_N_mujeres <- construir_base_referencia(
  prenec %>% filter(Sexo_analisis == "Mujer"),
  "FIT_B"
)

validar_base_final <- function(base, nombre_base, n_esperado) {
  if (sum(base$pop, na.rm = TRUE) != n_esperado) {
    stop(nombre_base, ": la suma de pop no coincide con la cohorte analítica.")
  }

  denominadores <- as.matrix(base[, variables_denominador])
  if (anyNA(denominadores) || any(denominadores < 0)) {
    stop(nombre_base, ": existen denominadores faltantes o negativos.")
  }
  if (any(base$colonoscopy_n > base$fit_positive_n) ||
      any(base$fit_positive_n > base$fit_observed_n) ||
      any(base$fit_observed_n > base$pop)) {
    stop(
      nombre_base,
      paste0(
        ": no se cumple 0 <= colonoscopy_n <= fit_positive_n <= ",
        "fit_observed_n <= pop."
      )
    )
  }

  matriz_resultados <- as.matrix(base[, variables_resultado])
  if (any(matriz_resultados < 0, na.rm = TRUE)) {
    stop(nombre_base, ": existen conteos negativos.")
  }
  if (any(matriz_resultados > base$pop, na.rm = TRUE)) {
    stop(nombre_base, ": existe un conteo mayor que pop en alguna edad.")
  }

  conteo_cero <- function(x) replace(as.numeric(x), is.na(x), 0)
  total_directo <- conteo_cero(base$adenoma_total)
  total_tamano <- conteo_cero(base$ad_small) + conteo_cero(base$ad_medium) + conteo_cero(base$ad_large)
  total_riesgo <- conteo_cero(base$low_risk) + conteo_cero(base$high_risk)
  if (any(total_tamano > total_directo)) {
    stop(
      nombre_base,
      ": la suma small + medium + large supera adenoma_total."
    )
  }
  if (any(total_riesgo > total_directo)) {
    stop(nombre_base, ": la suma low_risk + high_risk supera adenoma_total.")
  }

  if (any(base$adenoma_sin_clasificacion_tamano != total_directo - total_tamano) ||
      any(base$adenoma_sin_clasificacion_riesgo != total_directo - total_riesgo)) {
    stop(nombre_base, ": los diagnósticos de clasificación no reconcilian.")
  }
}

n_cohorte_analitica <- sum(prenec$highrisk == 0, na.rm = TRUE)
validar_base_final(base_finalA_N, "Base A aleatorizada", n_cohorte_analitica)
validar_base_final(base_finalB_N, "Base B aleatorizada", n_cohorte_analitica)
validar_base_final(base_referencia_R1, "Referencia Ronda 1", n_cohorte_analitica)
validar_base_final(base_referencia_R2, "Referencia Ronda 2", n_cohorte_analitica)

n_hombres_analiticos <- sum(
  prenec$highrisk == 0 & prenec$Sexo_analisis == "Hombre",
  na.rm = TRUE
)
n_mujeres_analiticas <- sum(
  prenec$highrisk == 0 & prenec$Sexo_analisis == "Mujer",
  na.rm = TRUE
)
if (n_hombres_analiticos + n_mujeres_analiticas != n_cohorte_analitica) {
  stop("Las subcohortes de hombres y mujeres no cubren toda la cohorte analitica.")
}

validar_base_final(
  base_finalA_N_hombres,
  "Base A aleatorizada - hombres",
  n_hombres_analiticos
)
validar_base_final(
  base_finalA_N_mujeres,
  "Base A aleatorizada - mujeres",
  n_mujeres_analiticas
)
validar_base_final(
  base_finalB_N_hombres,
  "Base B aleatorizada - hombres",
  n_hombres_analiticos
)
validar_base_final(
  base_finalB_N_mujeres,
  "Base B aleatorizada - mujeres",
  n_mujeres_analiticas
)

validar_particion_sexo <- function(base_total, base_hombres, base_mujeres, nombre_base) {
  columnas_conteo <- c(
    "pop", variables_denominador, variables_resultado, variables_diagnostico
  )
  total_normalizado <- base_total %>%
    select(Edad_2, all_of(columnas_conteo)) %>%
    mutate(across(all_of(columnas_conteo), ~ replace_na(as.numeric(.x), 0)))

  suma_sexos <- bind_rows(base_hombres, base_mujeres) %>%
    select(Edad_2, all_of(columnas_conteo)) %>%
    mutate(across(all_of(columnas_conteo), ~ replace_na(as.numeric(.x), 0))) %>%
    group_by(Edad_2) %>%
    summarise(across(all_of(columnas_conteo), sum), .groups = "drop")

  comparacion <- full_join(
    total_normalizado,
    suma_sexos,
    by = "Edad_2",
    suffix = c("_total", "_sexos")
  )

  for (columna in columnas_conteo) {
    valor_total <- comparacion[[paste0(columna, "_total")]]
    valor_sexos <- comparacion[[paste0(columna, "_sexos")]]
    valor_total[is.na(valor_total)] <- 0
    valor_sexos[is.na(valor_sexos)] <- 0
    if (any(valor_total != valor_sexos)) {
      stop(
        nombre_base,
        ": la suma de hombres y mujeres no reproduce la base total en ",
        columna,
        "."
      )
    }
  }
  invisible(TRUE)
}

validar_particion_sexo(
  base_finalA_N,
  base_finalA_N_hombres,
  base_finalA_N_mujeres,
  "Base A aleatorizada"
)
validar_particion_sexo(
  base_finalB_N,
  base_finalB_N_hombres,
  base_finalB_N_mujeres,
  "Base B aleatorizada"
)

validar_poblacion_AB_sexo <- function(base_A, base_B, sexo) {
  poblacion_A <- base_A %>% select(Edad_2, pop_A = pop)
  poblacion_B <- base_B %>% select(Edad_2, pop_B = pop)
  comparacion <- full_join(poblacion_A, poblacion_B, by = "Edad_2")
  if (any(is.na(comparacion$pop_A)) ||
      any(is.na(comparacion$pop_B)) ||
      any(comparacion$pop_A != comparacion$pop_B)) {
    stop("Las bases A y B no conservan la misma poblacion por edad para ", sexo, ".")
  }
  invisible(TRUE)
}

validar_poblacion_AB_sexo(
  base_finalA_N_hombres,
  base_finalB_N_hombres,
  "hombres"
)
validar_poblacion_AB_sexo(
  base_finalA_N_mujeres,
  base_finalB_N_mujeres,
  "mujeres"
)

columnas_faltantes <- function(base, nombre_base) {
  faltantes <- setdiff(
    c(
      "Edad_2", "pop", variables_denominador,
      variables_resultado, variables_diagnostico
    ),
    names(base)
  )
  if (length(faltantes) > 0L) {
    stop(nombre_base, " no contiene: ", paste(faltantes, collapse = ", "))
  }
}

columnas_faltantes(base_finalA_N, "base_finalA_N aleatorizada")
columnas_faltantes(base_finalB_N, "base_finalB_N aleatorizada")
columnas_faltantes(base_finalA_N_hombres, "base_finalA_N hombres")
columnas_faltantes(base_finalA_N_mujeres, "base_finalA_N mujeres")
columnas_faltantes(base_finalB_N_hombres, "base_finalB_N hombres")
columnas_faltantes(base_finalB_N_mujeres, "base_finalB_N mujeres")
columnas_faltantes(base_referencia_R1, "Referencia Ronda 1 de la misma cohorte")
columnas_faltantes(base_referencia_R2, "Referencia Ronda 2 de la misma cohorte")

if (!file.exists(ruta_baseA_actual)) {
  stop("No existe la referencia A actual: ", ruta_baseA_actual)
}
if (!file.exists(ruta_baseB_actual)) {
  stop("No existe la referencia B actual: ", ruta_baseB_actual)
}

base_A_historica <- read_excel(ruta_baseA_actual)
base_B_historica <- read_excel(ruta_baseB_actual)

# Las bases agregadas históricas no contienen los nuevos denominadores. Se
# conserva su compatibilidad agregando sólo adenoma_total desde la partición
# por tamaño, donde NA representa un conteo igual a cero.
normalizar_base_historica <- function(base, nombre_base) {
  columnas_base <- c(
    "Edad_2", "pop", "ad_small", "ad_medium", "ad_large",
    "low_risk", "high_risk", "cancer",
    "ads1", "adm1", "adl1", "ads2", "adm2", "adl2",
    "ads3m", "adm3m", "adl3m"
  )
  faltantes <- setdiff(columnas_base, names(base))
  if (length(faltantes) > 0L) {
    stop(nombre_base, " no contiene: ", paste(faltantes, collapse = ", "))
  }

  if (!"adenoma_total" %in% names(base)) {
    conteos_tamano <- as.data.frame(
      base[, c("ad_small", "ad_medium", "ad_large")]
    )
    conteos_tamano[is.na(conteos_tamano)] <- 0
    total_tamano <- rowSums(conteos_tamano)
    base$adenoma_total <- replace(total_tamano, total_tamano == 0, NA_real_)
  }

  conteo_historico <- function(x) replace(as.numeric(x), is.na(x), 0)
  total_directo <- conteo_historico(base$adenoma_total)
  total_tamano <- conteo_historico(base$ad_small) +
    conteo_historico(base$ad_medium) +
    conteo_historico(base$ad_large)
  total_riesgo <- conteo_historico(base$low_risk) +
    conteo_historico(base$high_risk)
  base$adenoma_sin_clasificacion_tamano <- total_directo - total_tamano
  base$adenoma_sin_clasificacion_riesgo <- total_directo - total_riesgo

  base
}

base_A_historica <- normalizar_base_historica(base_A_historica, "Base A histórica")
base_B_historica <- normalizar_base_historica(base_B_historica, "Base B histórica")

totales_largos <- function(base, nombre, cohorte) {
  n_personas <- sum(base$pop, na.rm = TRUE)
  total <- vapply(
    variables_comparacion,
    function(variable) sum(base[[variable]], na.rm = TRUE),
    numeric(1)
  )

  tibble(
    indicador = variables_comparacion,
    base = nombre,
    cohorte = cohorte,
    n_personas = n_personas,
    total = total,
    tasa_10000 = ifelse(n_personas == 0, NA_real_, 10000 * total / n_personas)
  )
}

etiqueta_cohorte <- ifelse(
  usar_base_transformada,
  "cohorte_historica_transformada",
  "cohorte_regional_vigente"
)

resultados_largos <- bind_rows(
  totales_largos(base_finalA_N, "A_aleatoria", etiqueta_cohorte),
  totales_largos(base_finalB_N, "B_aleatoria", etiqueta_cohorte),
  totales_largos(base_referencia_R1, "R1_misma_cohorte", etiqueta_cohorte),
  totales_largos(base_referencia_R2, "R2_misma_cohorte", etiqueta_cohorte),
  totales_largos(base_A_historica, "A_historica_20250104", "cohorte_historica_20250104"),
  totales_largos(base_B_historica, "B_historica_20250104", "cohorte_historica_20250104")
)

comparacion_totales <- resultados_largos %>%
  select(indicador, base, total, tasa_10000) %>%
  pivot_wider(
    names_from = base,
    values_from = c(total, tasa_10000),
    names_glue = "{.value}_{base}"
  ) %>%
  mutate(
    diferencia_A_vs_R2_misma_cohorte = total_A_aleatoria - total_R2_misma_cohorte,
    diferencia_B_vs_R2_misma_cohorte = total_B_aleatoria - total_R2_misma_cohorte,
    diferencia_promedio_AB_vs_R2_misma_cohorte =
      ((total_A_aleatoria + total_B_aleatoria) / 2) - total_R2_misma_cohorte,
    porcentaje_A_vs_R2_misma_cohorte = ifelse(
      total_R2_misma_cohorte == 0,
      NA_real_,
      100 * diferencia_A_vs_R2_misma_cohorte / total_R2_misma_cohorte
    ),
    porcentaje_B_vs_R2_misma_cohorte = ifelse(
      total_R2_misma_cohorte == 0,
      NA_real_,
      100 * diferencia_B_vs_R2_misma_cohorte / total_R2_misma_cohorte
    ),
    diferencia_tasa_A_vs_B_historica =
      tasa_10000_A_aleatoria - tasa_10000_B_historica_20250104,
    diferencia_tasa_B_vs_B_historica =
      tasa_10000_B_aleatoria - tasa_10000_B_historica_20250104
  )

calcular_distancia <- function(base_nueva, referencia, comparabilidad) {
  nueva <- resultados_largos %>%
    filter(base == base_nueva, indicador %in% variables_resultado) %>%
    select(indicador, total_nuevo = total, tasa_nueva = tasa_10000)
  ref <- resultados_largos %>%
    filter(base == referencia, indicador %in% variables_resultado) %>%
    select(indicador, total_referencia = total, tasa_referencia = tasa_10000)

  left_join(nueva, ref, by = "indicador") %>%
    summarise(
      base_nueva = base_nueva,
      referencia = referencia,
      comparabilidad = comparabilidad,
      distancia_L1_conteos = sum(abs(total_nuevo - total_referencia)),
      RMSE_conteos = sqrt(mean((total_nuevo - total_referencia)^2)),
      distancia_L1_tasa_10000 = sum(abs(tasa_nueva - tasa_referencia)),
      RMSE_tasa_10000 = sqrt(mean((tasa_nueva - tasa_referencia)^2)),
      .groups = "drop"
    )
}

resumen_distancias <- bind_rows(
  calcular_distancia("A_aleatoria", "R1_misma_cohorte", "misma_cohorte"),
  calcular_distancia("A_aleatoria", "R2_misma_cohorte", "misma_cohorte"),
  calcular_distancia("B_aleatoria", "R1_misma_cohorte", "misma_cohorte"),
  calcular_distancia("B_aleatoria", "R2_misma_cohorte", "misma_cohorte"),
  calcular_distancia("A_aleatoria", "B_historica_20250104", "cohorte_distinta_usar_tasas"),
  calcular_distancia("B_aleatoria", "B_historica_20250104", "cohorte_distinta_usar_tasas")
)

prenec_analisis <- prenec %>% filter(highrisk == 0)

resumen_sorteo <- tibble(
  base = c("A_aleatoria", "B_aleatoria", "Ronda_1_original", "Ronda_2_original", "Positivo_en_alguna_ronda"),
  n_cohorte = nrow(prenec_analisis),
  positivos = c(
    sum(prenec_analisis$FIT_A == "Positivo", na.rm = TRUE),
    sum(prenec_analisis$FIT_B == "Positivo", na.rm = TRUE),
    sum(prenec_analisis$Ronda_1 == "Positivo", na.rm = TRUE),
    sum(prenec_analisis$Ronda_2 == "Positivo", na.rm = TRUE),
    sum(prenec_analisis$Ronda_1 == "Positivo" | prenec_analisis$Ronda_2 == "Positivo", na.rm = TRUE)
  ),
  negativos = c(
    sum(prenec_analisis$FIT_A == "Negativo", na.rm = TRUE),
    sum(prenec_analisis$FIT_B == "Negativo", na.rm = TRUE),
    sum(prenec_analisis$Ronda_1 == "Negativo", na.rm = TRUE),
    sum(prenec_analisis$Ronda_2 == "Negativo", na.rm = TRUE),
    NA_integer_
  ),
  sin_resultado = c(
    sum(is.na(prenec_analisis$FIT_A)),
    sum(is.na(prenec_analisis$FIT_B)),
    sum(is.na(prenec_analisis$Ronda_1)),
    sum(is.na(prenec_analisis$Ronda_2)),
    sum(is.na(prenec_analisis$Ronda_1) & is.na(prenec_analisis$Ronda_2))
  )
)

patrones_rondas <- prenec_analisis %>%
  summarise(
    n_cohorte = n(),
    ambas_rondas_observadas = sum(!is.na(Ronda_1) & !is.na(Ronda_2)),
    solo_Ronda_1_observada = sum(!is.na(Ronda_1) & is.na(Ronda_2)),
    solo_Ronda_2_observada = sum(is.na(Ronda_1) & !is.na(Ronda_2)),
    ninguna_ronda_observada = sum(is.na(Ronda_1) & is.na(Ronda_2)),
    A_recibe_Ronda_1 = sum(ronda_sorteada_A == 1L, na.rm = TRUE),
    A_recibe_Ronda_2 = sum(ronda_sorteada_A == 2L, na.rm = TRUE),
    B_recibe_Ronda_1 = sum(ronda_sorteada_B == 1L, na.rm = TRUE),
    B_recibe_Ronda_2 = sum(ronda_sorteada_B == 2L, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "indicador", values_to = "n")

resumen_sexo <- tibble(
  Sexo = c("Hombre", "Mujer"),
  n_cohorte = c(n_hombres_analiticos, n_mujeres_analiticas),
  pop_base_A = c(
    sum(base_finalA_N_hombres$pop, na.rm = TRUE),
    sum(base_finalA_N_mujeres$pop, na.rm = TRUE)
  ),
  pop_base_B = c(
    sum(base_finalB_N_hombres$pop, na.rm = TRUE),
    sum(base_finalB_N_mujeres$pop, na.rm = TRUE)
  ),
  positivos_FIT_A = c(
    sum(prenec_analisis$Sexo_analisis == "Hombre" & prenec_analisis$FIT_A == "Positivo", na.rm = TRUE),
    sum(prenec_analisis$Sexo_analisis == "Mujer" & prenec_analisis$FIT_A == "Positivo", na.rm = TRUE)
  ),
  positivos_FIT_B = c(
    sum(prenec_analisis$Sexo_analisis == "Hombre" & prenec_analisis$FIT_B == "Positivo", na.rm = TRUE),
    sum(prenec_analisis$Sexo_analisis == "Mujer" & prenec_analisis$FIT_B == "Positivo", na.rm = TRUE)
  )
)

comparacion_por_edad <- bind_rows(
  base_finalA_N %>% mutate(base = "A_aleatoria"),
  base_finalB_N %>% mutate(base = "B_aleatoria"),
  base_referencia_R1 %>% mutate(base = "R1_misma_cohorte"),
  base_referencia_R2 %>% mutate(base = "R2_misma_cohorte"),
  base_A_historica %>% mutate(base = "A_historica_20250104"),
  base_B_historica %>% mutate(base = "B_historica_20250104")
) %>%
  select(base, Edad_2, pop, all_of(variables_comparacion)) %>%
  pivot_longer(
    cols = all_of(variables_comparacion),
    names_to = "indicador",
    values_to = "conteo"
  ) %>%
  arrange(indicador, Edad_2, base)

parametros <- tibble(
  parametro = c(
    "semilla",
    "regla_ambas_rondas",
    "regla_una_ronda",
    "fuente_analisis",
    "modo_fuente",
    "n_total_fuente",
    "n_highrisk_0",
    "n_hombres_highrisk_0",
    "n_mujeres_highrisk_0",
    "comparacion_principal",
    "comparacion_historica",
    "base_A_historica",
    "base_B_historica"
  ),
  valor = c(
    as.character(SEMILLA_SORTEO),
    "Moneda 50/50: A y B reciben rondas opuestas",
    "Ambas bases usan la unica ronda observada",
    fuente_analisis,
    ifelse(usar_base_transformada, "base_historica_transformada", "bases_regionales_actuales"),
    as.character(nrow(prenec)),
    as.character(nrow(prenec_analisis)),
    as.character(n_hombres_analiticos),
    as.character(n_mujeres_analiticas),
    "Conteos: A/B aleatorias versus R1/R2 de la misma cohorte",
    "Tasas por 10.000: A/B aleatorias versus bases históricas por distinto denominador",
    ruta_baseA_actual,
    ruta_baseB_actual
  )
)

etiqueta_archivo_cohorte <- ifelse(
  usar_base_transformada,
  "cohorte_historica",
  "cohorte_actual"
)
sufijo_semilla <- paste0("_", etiqueta_archivo_cohorte, "_semilla_", SEMILLA_SORTEO, ".xlsx")
ruta_salida_A <- archivo_salida(paste0("Base_finalA_N_aleatoria", sufijo_semilla))
ruta_salida_B <- archivo_salida(paste0("Base_finalB_N_aleatoria", sufijo_semilla))
ruta_salida_A_hombres <- archivo_salida(paste0("Base_finalA_N_aleatoria_hombres", sufijo_semilla))
ruta_salida_A_mujeres <- archivo_salida(paste0("Base_finalA_N_aleatoria_mujeres", sufijo_semilla))
ruta_salida_B_hombres <- archivo_salida(paste0("Base_finalB_N_aleatoria_hombres", sufijo_semilla))
ruta_salida_B_mujeres <- archivo_salida(paste0("Base_finalB_N_aleatoria_mujeres", sufijo_semilla))
ruta_salida_R1 <- archivo_salida(paste0("Base_finalR1_N_referencia_", etiqueta_archivo_cohorte, ".xlsx"))
ruta_salida_R2 <- archivo_salida(paste0("Base_finalR2_N_referencia_", etiqueta_archivo_cohorte, ".xlsx"))
ruta_salida_comparacion <- archivo_salida(
  paste0("Comparacion_bases_aleatorias", sufijo_semilla)
)

write_xlsx(base_finalA_N, ruta_salida_A)
write_xlsx(base_finalB_N, ruta_salida_B)
write_xlsx(base_finalA_N_hombres, ruta_salida_A_hombres)
write_xlsx(base_finalA_N_mujeres, ruta_salida_A_mujeres)
write_xlsx(base_finalB_N_hombres, ruta_salida_B_hombres)
write_xlsx(base_finalB_N_mujeres, ruta_salida_B_mujeres)
write_xlsx(base_referencia_R1, ruta_salida_R1)
write_xlsx(base_referencia_R2, ruta_salida_R2)
write_xlsx(
  list(
    Comparacion_totales = comparacion_totales,
    Resultados_largos = resultados_largos,
    Resumen_distancias = resumen_distancias,
    Resumen_sorteo = resumen_sorteo,
    Resumen_sexo = resumen_sexo,
    Patrones_rondas = patrones_rondas,
    Comparacion_por_edad = comparacion_por_edad,
    Parametros = parametros
  ),
  ruta_salida_comparacion
)

message("Base A aleatorizada: ", normalizePath(ruta_salida_A, winslash = "/", mustWork = FALSE))
message("Base B aleatorizada: ", normalizePath(ruta_salida_B, winslash = "/", mustWork = FALSE))
message("Base A - hombres: ", normalizePath(ruta_salida_A_hombres, winslash = "/", mustWork = FALSE))
message("Base A - mujeres: ", normalizePath(ruta_salida_A_mujeres, winslash = "/", mustWork = FALSE))
message("Base B - hombres: ", normalizePath(ruta_salida_B_hombres, winslash = "/", mustWork = FALSE))
message("Base B - mujeres: ", normalizePath(ruta_salida_B_mujeres, winslash = "/", mustWork = FALSE))
message("Referencia Ronda 1: ", normalizePath(ruta_salida_R1, winslash = "/", mustWork = FALSE))
message("Referencia Ronda 2: ", normalizePath(ruta_salida_R2, winslash = "/", mustWork = FALSE))
message("Comparación: ", normalizePath(ruta_salida_comparacion, winslash = "/", mustWork = FALSE))
print(comparacion_totales)

if (identical(Sys.getenv("PRENEC_EJECUTAR_DESCRIPTIVOS", unset = "0"), "1")) {

##### Estadísticas descriptivas ----

# crear decenios de edad en la base prenec

prenec <- prenec %>%
  mutate(Decenio_Edad = as.character(cut(Edad_2,
                                         breaks = c(seq(0, 80, by = 10), Inf),
                                         labels = c(paste(seq(0,70, by = 10), seq(9, 79, by = 10), sep = "-"), "80 o más"),
                                         right = FALSE)))

# a.- Número de personas enroladas en cada año (Fecha_Enrolamiento), por decenios de edad

table(is.na(prenec$Fecha_Enrolamiento), exclude = "always") # no tiene NA
class(prenec$Fecha_Enrolamiento)

enrolamiento_decenios <- prenec %>%
  filter(highrisk == 0) %>%
  mutate(Anio_Enrolamiento = year(Fecha_Enrolamiento)) %>%
  group_by(Decenio_Edad, Anio_Enrolamiento) %>%
  summarise(Nro = n()) %>%
  pivot_wider(names_from = Anio_Enrolamiento, values_from = Nro)

# b.- Número de personas y cobertura de colon check (Fecha_1er_Colon_check_formato), por año de enrolamiento sin edad

resumen_cobertura_anio <- prenec %>%
  filter(highrisk == 0) %>%
  mutate(Anio_Enrolamiento = year(Fecha_Enrolamiento)) %>%
  group_by(Anio_Enrolamiento) %>%
  summarise(
    total = n(),
    examen = sum(!is.na(Fecha_1er_Colon_check)),
    cobertura = round((examen / total * 100),2)
  )

# write_xlsx(resumen_cobertura_año, "Productos/resumen_cobertura_año15.04.25.xlsx")

# c.- Número de personas sin colonoscopía, que tienen solo 1, 2 o 3 colon check

prenec <- prenec %>%
  mutate(Indicador_Check_1 = case_when(
    Ronda_1 == "Positivo" | Ronda_2 == "Positivo" ~ 1,
    TRUE ~ 0
  ),
  Indicador_Check_2 = case_when(
    Ronda_3 == "Positivo" | Ronda_4 == "Positivo" ~ 1,
    TRUE ~ 0
  ))

adherencia_check <- prenec %>%
  filter(highrisk == 0) %>%
  mutate(
    A = case_when( # número de pacientes con colon check negativo en el primer colon check (es decir, ronda 1 y 2)
      Indicador_Check_1 == 0 ~ 1,
      TRUE ~ 0
    ),
    B = case_when( # Número de pacientes con segundo colon check de aquellos pacientes que tienen resultado negativo en ronda 1 y 2
      !is.na(Fecha_2do_Colon_check) & Indicador_Check_1 == 0 ~ 1,
      TRUE ~ 0
    ),
    C = case_when( # Número de pacientes con tercer colon check de aquellos pacientes negativos en el primer y segundo colon check
      !is.na(Fecha_3er_Colon_check) & Indicador_Check_1 == 0 & Indicador_Check_2 == 0 ~ 1,
      TRUE ~ 0
    )) %>%
  summarise(
    numerador_2 = sum(B),
    numerador_3 = sum(C),
    denominador = sum(A),
    Adherencia_2 = round(sum(B)/sum(A) * 100, 2),
    Adherencia_3 = round(sum(C)/sum(A) * 100, 2)
  )

# write_xlsx(adherencia_check, "Productos/adherencia_check15.04.25.xlsx")

# d.- Positividad de 1 test o de los dos test en el primer colon check, por decenios de edad y sexo

# d.1 Positividad 1 test: Ronda_1 positivo sobre el total de personas que se hicieron colon check, por edad

positividad_Ronda_1 <- prenec %>%
  filter(highrisk == 0 & !is.na(Fecha_1er_Colon_check)) %>%
  group_by(Sexo, Decenio_Edad) %>%
  summarise(
    Positivos = sum(Ronda_1 == "Positivo", na.rm = TRUE),
    Colon_check = n(),
    Positividad_porcentaje = round((Positivos / Colon_check * 100), 2)
  ) %>%
  pivot_wider(names_from = Sexo, values_from = c(Positivos, Colon_check, Positividad_porcentaje)) %>%
  select(Decenio_Edad, Positivos_Mujer, Colon_check_Mujer, Positividad_porcentaje_Mujer,
         Positivos_Hombre, Colon_check_Hombre, Positividad_porcentaje_Hombre)

#    d.2 Positividad 2 test: Ronda_2 positivo sobre el total de personas que se hicieron colon check, por edad

positividad_Ronda_2 <- prenec %>%
  filter(highrisk == 0 & !is.na(Fecha_1er_Colon_check)) %>%
  group_by(Sexo, Decenio_Edad) %>%
  summarise(
    Positivos = sum(Ronda_2 == "Positivo", na.rm = TRUE),
    Colon_check = n(),
    Positividad_porcentaje = round((Positivos / Colon_check * 100), 2)
  ) %>%
  pivot_wider(names_from = Sexo, values_from = c(Positivos, Colon_check, Positividad_porcentaje)) %>%
  select(Decenio_Edad, Positivos_Mujer, Colon_check_Mujer, Positividad_porcentaje_Mujer,
         Positivos_Hombre, Colon_check_Hombre, Positividad_porcentaje_Hombre)


#    d.3 Positividad 2 test conjuntos: Ronda 1 o 2 positivo, sobre el total de personas que se hicieron colon check, por edad

positividad_conjunta <- prenec %>%
  filter(highrisk == 0 & !is.na(Fecha_1er_Colon_check)) %>%
  group_by(Sexo, Decenio_Edad) %>%
  summarise(
    Positivos = sum(Indicador_Check_1 == 1),
    Colon_check = n(),
    Positividad_porcentaje = round((Positivos / Colon_check * 100), 2)
  ) %>%
  pivot_wider(names_from = Sexo, values_from = c(Positivos, Colon_check, Positividad_porcentaje)) %>%
  select(Decenio_Edad, Positivos_Mujer, Colon_check_Mujer, Positividad_porcentaje_Mujer,
         Positivos_Hombre, Colon_check_Hombre, Positividad_porcentaje_Hombre)


# e.- Construir tasa de adenomas pequeños, medianos y grandes por decenios de edad

adenomas_decenios <- prenec %>%
  filter(highrisk == 0) %>%
  mutate(Indicador_adenoma = case_when(
    !is.na(adenoma_size) ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(Decenio_Edad) %>%
  summarise(
    pequenos_numero = sum(adenoma_size == "pequeño", na.rm = TRUE),
    pequenos_porcentaje = round(sum(adenoma_size == "pequeño", na.rm = TRUE) / sum(Indicador_adenoma == 1, na.rm = TRUE) * 100, 2),
    medianos_numero = sum(adenoma_size == "mediano", na.rm = TRUE),
    medianos_porcentaje = round(sum(adenoma_size == "mediano", na.rm = TRUE) / sum(Indicador_adenoma == 1, na.rm = TRUE) * 100, 2),
    grandes_numero = sum(adenoma_size == "grande", na.rm = TRUE),
    grandes_porcentaje = round(sum(adenoma_size == "grande", na.rm = TRUE) / sum(Indicador_adenoma == 1, na.rm = TRUE) * 100, 2),
    denominador = sum(Indicador_adenoma == 1, na.rm = TRUE)
  )


# construir la base high risk: esta base debe ser un subset de todas las personas con highrisk = 1.

high_risk <- prenec %>%
  filter(highrisk == 1)

# Crear categorías de adenomas según histología y tamaño: low_risk, high_risk y cancer. Esta variable ya fue creada antes en
# la base PRENEC, es risk

# Crear combinaciones para 1_ad, 2_ad o 3_adormore. Individuos que tienen valor = 1 si cumplen cada una de estas categorías. Esto es lo mismo que
# en la BaseResumenX, solo que en vez de sumar los individuos, asignar valor = 1

high_risk <- high_risk %>%
  mutate(
    `1_ad` = case_when(
      n_ad_1 == "SI" & Ronda_1 == "Positivo" & !is.na(Indicador_Fecha_Colono) ~ 1,
      TRUE ~ NA),

    `2_ad` = case_when(
      n_ad_2 == "SI" & Ronda_1 == "Positivo" & !is.na(Indicador_Fecha_Colono) ~ 1,
      TRUE ~ NA),

    `3_adormore` = case_when(
      n_ad_3 == "SI" & Ronda_1 == "Positivo" & !is.na(Indicador_Fecha_Colono) ~ 1,
      TRUE ~ NA),

    `1_adormore` = case_when(
      n_ad_4 == "SI" & Ronda_1 == "Positivo" & !is.na(Indicador_Fecha_Colono) ~ 1,
      TRUE ~ NA

    ))

# Luego hay que encontrar:

# - ads1: El número de personas por edad con adenoma pequeño, Ronda_1 = Positivo y 1_ad = 1
# - adm1: El número de personas por edad con adenoma mediano, Ronda_1 = Positivo y 1_ad = 1
# - adl1: El número de personas por edad con adenoma grande, Ronda_1 = Positivo y 1_ad = 1
# - ads2: El número de personas por edad con adenoma pequeño, Ronda_1 = Positivo y 2_ad = 1
# - adm2: El número de personas por edad con adenoma mediano, Ronda_1 = Positivo y 2_ad = 1
# - adl2: El número de personas por edad con adenoma grande, Ronda_1 = Positivo y 2_ad = 1
# - ads3m: El número de personas por edad con adenoma pequeño, Ronda_1 = Positivo y 3_adormore = 1
# - adm3m: El número de personas por edad con adenoma mediano, Ronda_1 = Positivo y 3_adormore = 1
# - adl3m: El número de personas por edad con adenoma grande, Ronda_1 = Positivo y 3_adormore = 1

high_risk <- high_risk %>%
  group_by(Edad_2) %>%
  mutate(ads1 = ifelse(adenoma_size == "pequeño" & `1_ad` == 1, sum(adenoma_size == "pequeño" & `1_ad` == 1, na.rm = TRUE), NA),
         adm1 = ifelse(adenoma_size == "mediano" & `1_ad` == 1, sum(adenoma_size == "mediano" & `1_ad` == 1, na.rm = TRUE), NA),
         adl1 = ifelse(adenoma_size == "grande" & `1_ad` == 1, sum(adenoma_size == "grande" & `1_ad` == 1, na.rm = TRUE), NA),
         ads2 = ifelse(adenoma_size == "pequeño" & `2_ad` == 1, sum(adenoma_size == "pequeño" & `2_ad` == 1, na.rm = TRUE), NA),
         adm2 = ifelse(adenoma_size == "mediano" & `2_ad` == 1, sum(adenoma_size == "mediano" & `2_ad` == 1, na.rm = TRUE), NA),
         adl2 = ifelse(adenoma_size == "grande" & `2_ad` == 1, sum(adenoma_size == "grande" & `2_ad` == 1, na.rm = TRUE), NA),
         ads3m = ifelse(adenoma_size == "pequeño" & `3_adormore` == 1, sum(adenoma_size == "pequeño" & `3_adormore` == 1, na.rm = TRUE), NA),
         adm3m = ifelse(adenoma_size == "mediano" & `3_adormore` == 1, sum(adenoma_size == "mediano" & `3_adormore` == 1, na.rm = TRUE),NA),
         adl3m = ifelse(adenoma_size == "grande" & `3_adormore` == 1, sum(adenoma_size == "grande" & `3_adormore` == 1, na.rm = TRUE), NA)) %>%
  ungroup()

high_risk_resumen <- high_risk %>%
  group_by(Edad_2) %>%
  summarise(across(c(ads1, adm1, adl1, ads2, adm2, adl2, ads3m, adm3m, adl3m),
                   ~ ifelse(length(unique(na.omit(.))) == 1, unique(na.omit(.)), NA),
                   .names = "unique_{col}"),
            .groups = "drop")


# write_xlsx(high_risk, "Productos/base_high_risk_15.04.2025.xlsx")
# write_xlsx(high_risk_resumen, "Productos/base_high_risk_resumen_15.04.2025.xlsx")

# write_xlsx(prenec,"Productos/Base_prenec_transformada_15.04.2025.xlsx")


##### Tarea del 21.03.25 ----

# Considerando la variable Resultado_TSDO_1, filtrar a los positivos y responder ¿cuántos tienen fecha de 1era colonoscopía por quinquenios
# de edad??

prenec <- prenec %>%
  mutate(Quinquenio_Edad = as.character(cut(Edad_2,
                                            breaks = c(seq(40, 80, by = 5), Inf),
                                            labels = c(paste(seq(40, 75, by = 5), seq(44, 79, by = 5), sep = "-"), "80 y más"),
                                            right = FALSE)))

quinquenio_colono <- prenec %>%
  filter(Resultado_TSDO_1 == "Positivo") %>%
  select(Quinquenio_Edad, Resultado_TSDO_1, Fecha_1era_Colono) %>%
  mutate(Indicador = case_when(
    !is.na(Fecha_1era_Colono) ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(Quinquenio_Edad) %>%
  summarise(Nro_positivos = n(),
            Nro_colono = sum(Indicador))

# write_xlsx(prenec, "prenec_con_quinquenio20250415.xlsx")

##### FIN -----

}

