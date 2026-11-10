
# ==============================================================
## 10. ANÁLISIS ADICIONAL: CONTRIBUCIONES AL CRECIMIENTO DEL PBI
# ==============================================================
#
# Pregunta que responde: de cada punto de crecimiento (o caída) del PBI,
# ¿cuánto se explica por consumo de los hogares, gasto del gobierno,
# inversión, exportaciones e importaciones?
#
# Por qué complementa la regresión: inversión y exportaciones son componentes
# del PBI (PBI = C + G + I + X − M). En la regresión, parte de sus coeficientes
# reproduce esa identidad contable. La descomposición por contribuciones usa
# la identidad de forma explícita y mide, sin estimar nada, qué componente
# empujó el crecimiento en cada trimestre.
#
# Definición. La contribución del componente j al crecimiento interanual es
#
#     contribución_j,t = 100 × (X_j,t − X_j,t−4) / PBI_t−4
#
# Es la variación del componente medida en relación con el tamaño del PBI de
# un año atrás. La suma de las contribuciones da exactamente el crecimiento
# del PBI (el control de 10.1 lo verifica). Esto es válido porque la serie está a precios constantes de 2016 (año base
# fijo), donde los componentes son aditivos.
#
# Nota sobre importaciones: en el Excel del BCU vienen con signo negativo.
# Por eso un aumento de importaciones da una contribución negativa, que es lo
# correcto: más importaciones, a igual demanda, implican menos producción local.
#
# Esta sección es autocontenida: lee el Excel de nuevo para no depender del
# resto del script. Si se pega dentro de proyecto.R, puede partir de
# datos_macro (corregido) y saltear el bloque 10.1.

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)


# --------------------------------------------------------------
# 10.1 Datos del BCU por componente
# --------------------------------------------------------------

componentes <- read_excel("BCU_datos.xlsx", sheet = "Valores_K", skip = 7)
names(componentes)[2] <- "variable"

componentes <- componentes %>%
  filter(variable %in% c(
    "PRODUCTO INTERNO BRUTO",
    "Hogares",
    "Gobierno e ISFLSH",
    "Formación Bruta de Capital",
    "Exportaciones de bienes y servicios",
    "(-) Importaciones de bienes y servicios"
  )) %>%
  pivot_longer(
    cols = starts_with(c("I ", "II ", "III ", "IV ")),
    names_to = "trimestre",
    values_to = "valor"
  ) %>%
  mutate(
    trimestre = gsub("\\*", "", trimestre),
    # El BCU rotula 2021 como "202I" (letra I). Sin esta corrección, 2021
    # queda sin año y los lag(4) comparan contra el trimestre equivocado.
    trimestre = sub("([0-9]{3})I$", "\\11", trimestre)
  ) %>%
  separate(trimestre, into = c("trim", "año"), sep = " ", remove = FALSE) %>%
  mutate(
    año = as.integer(año),
    trimestre_num = match(trim, c("I", "II", "III", "IV"))
  ) %>%
  select(trimestre, año, trimestre_num, variable, valor) %>%
  pivot_wider(names_from = variable, values_from = valor) %>%
  rename(
    pbi           = `PRODUCTO INTERNO BRUTO`,
    consumo       = `Hogares`,
    gasto_publico = `Gobierno e ISFLSH`,
    inversion     = `Formación Bruta de Capital`,
    exportaciones = `Exportaciones de bienes y servicios`,
    importaciones = `(-) Importaciones de bienes y servicios`
  ) %>%
  arrange(año, trimestre_num)

stopifnot(!any(is.na(componentes$año)))
stopifnot(all(diff(componentes$año * 4 + componentes$trimestre_num) == 1))

# Control de la identidad: C + G + I + X + M (M ya negativa) debe dar el PBI.
# Si la diferencia no fuera ~0, las contribuciones no sumarían el crecimiento.
discrepancia <- with(componentes,
  max(abs(consumo + gasto_publico + inversion + exportaciones + importaciones - pbi) / pbi))
stopifnot("Los componentes no suman el PBI" = discrepancia < 1e-6)


# --------------------------------------------------------------
# 10.2 Contribuciones trimestrales (interanuales)
# --------------------------------------------------------------

nombres_componentes <- c(
  consumo       = "Consumo de los hogares",
  gasto_publico = "Gasto del gobierno",
  inversion     = "Inversión (FBC)",
  exportaciones = "Exportaciones",
  importaciones = "Importaciones"
)

contribuciones <- componentes %>%
  mutate(
    across(
      c(consumo, gasto_publico, inversion, exportaciones, importaciones),
      ~ 100 * (.x - lag(.x, 4)) / lag(pbi, 4),
      .names = "contrib_{.col}"
    ),
    crecimiento_pbi = 100 * (pbi / lag(pbi, 4) - 1),
    fecha = as.Date(paste(año, 3 * trimestre_num - 2, 1, sep = "-"))
  ) %>%
  filter(!is.na(crecimiento_pbi))

# Tabla: cada fila es un trimestre, cada columna la contribución en p.p.
contribuciones %>%
  select(trimestre, crecimiento_pbi, starts_with("contrib_")) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  print(n = Inf)


# --------------------------------------------------------------
# 10.3 Gráfico: barras apiladas de contribuciones + línea del PBI
# --------------------------------------------------------------
# Cada barra se divide en la contribución de cada componente. Las partes
# positivas se apilan hacia arriba y las negativas hacia abajo; el punto
# negro es el crecimiento del PBI, que es la suma de las partes.

contrib_largo <- contribuciones %>%
  select(fecha, trimestre, crecimiento_pbi,
         contrib_consumo, contrib_gasto_publico, contrib_inversion,
         contrib_exportaciones, contrib_importaciones) %>%
  pivot_longer(starts_with("contrib_"), names_to = "componente", values_to = "contribucion") %>%
  mutate(
    componente = sub("contrib_", "", componente),
    componente = factor(nombres_componentes[componente], levels = nombres_componentes)
  )

grafico_contribuciones <- ggplot(contrib_largo, aes(x = fecha)) +
  geom_col(aes(y = contribucion, fill = componente), width = 80) +
  geom_line(data = contribuciones, aes(y = crecimiento_pbi), linewidth = 0.6) +
  geom_point(data = contribuciones, aes(y = crecimiento_pbi), size = 1.3) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_fill_manual(values = c("#4C72B0", "#55A868", "#C44E52", "#8172B2", "#CCB974")) +
  labs(
    title = "Contribuciones al crecimiento interanual del PBI",
    subtitle = "Puntos porcentuales; la línea es el crecimiento del PBI",
    x = NULL, y = "Puntos porcentuales", fill = NULL,
    caption = "Fuente: elaboración propia con datos del BCU (precios constantes de 2016)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(grafico_contribuciones)
ggsave("contribuciones_pbi.png", grafico_contribuciones, width = 10, height = 5.5, dpi = 150)


# --------------------------------------------------------------
# 10.4 Contribuciones anuales
# --------------------------------------------------------------
# Se suman los cuatro trimestres de cada año y se aplica la misma fórmula
# sobre el año anterior. Solo años completos.

contribuciones_anuales <- componentes %>%
  group_by(año) %>%
  filter(n() == 4) %>%
  summarise(across(c(pbi, consumo, gasto_publico, inversion, exportaciones, importaciones), sum),
            .groups = "drop") %>%
  arrange(año) %>%
  mutate(
    crecimiento_pbi = 100 * (pbi / lag(pbi) - 1),
    across(
      c(consumo, gasto_publico, inversion, exportaciones, importaciones),
      ~ 100 * (.x - lag(.x)) / lag(pbi),
      .names = "contrib_{.col}"
    )
  ) %>%
  filter(!is.na(crecimiento_pbi)) %>%
  select(año, crecimiento_pbi, starts_with("contrib_")) %>%
  mutate(across(-año, ~ round(.x, 2)))

print(contribuciones_anuales)


# --------------------------------------------------------------
# 10.5 Promedios por período
# --------------------------------------------------------------
# Compara qué sostuvo el crecimiento antes, durante y después de la pandemia.
# Los cortes son una elección del análisis y conviene justificarlos en el
# informe: 2020-2021 incluye la caída y el rebote por efecto base.

contribuciones %>%
  mutate(periodo = case_when(
    año <= 2019 ~ "1. Prepandemia (2017-2019)",
    año <= 2021 ~ "2. Pandemia y rebote (2020-2021)",
    TRUE        ~ "3. Pospandemia (2022 en adelante)"
  )) %>%
  group_by(periodo) %>%
  summarise(
    trimestres = n(),
    across(c(crecimiento_pbi, contrib_consumo, contrib_gasto_publico,
             contrib_inversion, contrib_exportaciones, contrib_importaciones),
           ~ round(mean(.x), 2))
  )

# Lectura sugerida para el informe: en cada período, identificar qué
# componente aportó más al crecimiento y si el aporte de las exportaciones se
# compensó con mayores importaciones (contribución externa neta = X + M).
