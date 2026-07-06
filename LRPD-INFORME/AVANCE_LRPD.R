# ================================================================
# ANALISIS ESTADISTICO - ENFERMEDAD HEMORRAGICA VIRAL DEL CONEJO
# Base teorica basada en: Shorunke et al. (2022)
# Curso: Sistematizacion y Metodos Estadisticos
# Grupo: 9
# Integrantes:
# Marca lobo, Smith
# Pariona Ucharima, Edson
# Ripas, David
# Soto Straub, Said
# ================================================================

# ----------------------------------------------------------------
# 0. OBJETIVO DEL SCRIPT
# ----------------------------------------------------------------
# Este script realiza un analisis completo de una base teorica sobre
# Enfermedad Hemorragica Viral del Conejo (RHD).
#
# Analisis incluidos:
# 1. Importacion y revision de datos.
# 2. Estadistica descriptiva.
# 3. Tablas de frecuencia.
# 4. Graficos utiles para el informe.
# 5. Pruebas Chi-cuadrado o Fisher.
# 6. Odds Ratio crudos.
# 7. Regresion logistica multivariada.
# 8. Exportacion de tablas y graficos.
# 9. Respuestas automaticas a los objetivos.

# ----------------------------------------------------------------
# 1. INSTALAR Y CARGAR PAQUETES
# ----------------------------------------------------------------

library(readxl)
library(dplyr)
library(ggplot2)
library(janitor)
library(epitools)
library(broom)
library(gtsummary)
library(writexl)
library(forcats)

# ----------------------------------------------------------------
# 2. CREAR CARPETA DE RESULTADOS
# ----------------------------------------------------------------
if(!dir.exists("resultados_RHD_conejos")){
  dir.create("resultados_RHD_conejos")
}

# ----------------------------------------------------------------
# 3. IMPORTAR BASE DE DATOS
# ----------------------------------------------------------------
# IMPORTANTE:
# Guardar este script en la misma carpeta donde se encuentra el archivo:
# base_RHD_conejos_teorica.xlsx

datos <- read_excel("base_RHD_conejos_teorica.xlsx",
                    sheet = "base_RHD_conejos")

# Revisar primeras filas
head(datos)

# Revisar estructura de variables
str(datos)

# Limpiar nombres de variables por seguridad
datos <- datos %>%
  clean_names()

# Convertir variables categoricas a factor
datos <- datos %>%
  mutate(
    lga = as.factor(lga),
    tamano_granja = factor(tamano_granja, levels = c(">20", "<=20")),
    origen_animales_ado = factor(origen_animales_ado, levels = c("No", "Si")),
    agua_lluvia = factor(agua_lluvia, levels = c("No", "Si")),
    alimentacion_comercial = factor(alimentacion_comercial, levels = c("No", "Si")),
    desinfecta_herramientas = factor(desinfecta_herramientas, levels = c("No", "Si")),
    lavado_manos = factor(lavado_manos, levels = c("No", "Si")),
    presencia_corral_enfermos = factor(presencia_corral_enfermos, levels = c("No", "Si")),
    granja_cercada = factor(granja_cercada, levels = c("No", "Si")),
    capacitacion_cunicultura = factor(capacitacion_cunicultura, levels = c("No", "Si")),
    enfermedad_hemorragica = factor(enfermedad_hemorragica, levels = c("No", "Si"))
  )

# ----------------------------------------------------------------
# 4. OBJETIVOS DEL TRABAJO
# ----------------------------------------------------------------
# Objetivo general:
# Determinar los factores asociados a la enfermedad hemorrhagica viral
# del conejo en granjas cunicolas.
#
# Objetivos especificos:
# OE1: Describir las caracteristicas de las granjas evaluadas.
# OE2: Determinar la prevalencia de enfermedad hemorrhagica viral.
# OE3: Evaluar la asociacion entre tamano de granja y enfermedad.
# OE4: Evaluar la asociacion entre agua de lluvia y enfermedad.
# OE5: Evaluar la asociacion entre desinfeccion de herramientas y enfermedad.
# OE6: Evaluar la asociacion entre alimentacion comercial y enfermedad.
# OE7: Identificar factores asociados mediante regresion logistica.

# ----------------------------------------------------------------
# 5. ANALISIS DESCRIPTIVO GENERAL
# ----------------------------------------------------------------

# Resumen de variable numerica
resumen_numerico <- datos %>%
  summarise(
    n = n(),
    media_antiguedad = mean(antiguedad_granja_anios, na.rm = TRUE),
    sd_antiguedad = sd(antiguedad_granja_anios, na.rm = TRUE),
    mediana_antiguedad = median(antiguedad_granja_anios, na.rm = TRUE),
    minimo_antiguedad = min(antiguedad_granja_anios, na.rm = TRUE),
    maximo_antiguedad = max(antiguedad_granja_anios, na.rm = TRUE)
  )

resumen_numerico

# Tablas de frecuencia para variables categoricas
tabla_lga <- tabyl(datos, lga) %>% adorn_pct_formatting()
tabla_tamano <- tabyl(datos, tamano_granja) %>% adorn_pct_formatting()
tabla_agua <- tabyl(datos, agua_lluvia) %>% adorn_pct_formatting()
tabla_desinfeccion <- tabyl(datos, desinfecta_herramientas) %>% adorn_pct_formatting()
tabla_alimentacion <- tabyl(datos, alimentacion_comercial) %>% adorn_pct_formatting()
tabla_enfermedad <- tabyl(datos, enfermedad_hemorragica) %>% adorn_pct_formatting()

tabla_lga
tabla_tamano
tabla_agua
tabla_desinfeccion
tabla_alimentacion
tabla_enfermedad

# Exportar tablas descriptivas
write_xlsx(
  list(
    resumen_numerico = resumen_numerico,
    lga = tabla_lga,
    tamano_granja = tabla_tamano,
    agua_lluvia = tabla_agua,
    desinfeccion = tabla_desinfeccion,
    alimentacion = tabla_alimentacion,
    enfermedad = tabla_enfermedad
  ),
  "resultados_RHD_conejos/01_tablas_descriptivas.xlsx"
)

# ----------------------------------------------------------------
# 6. GRAFICOS DESCRIPTIVOS
# ----------------------------------------------------------------

# Grafico 1: Distribucion por localidad/zona
p1 <- ggplot(datos, aes(x = fct_infreq(lga))) +
  geom_bar() +
  coord_flip() +
  labs(
    title = "Distribucion de granjas segun localidad",
    x = "Localidad",
    y = "Numero de granjas"
  ) +
  theme_minimal()

p1
ggsave("resultados_RHD_conejos/grafico_01_lga.png", p1, width = 8, height = 5, dpi = 300)

# Grafico 2: Tamano de granja
p2 <- ggplot(datos, aes(x = tamano_granja)) +
  geom_bar() +
  labs(
    title = "Distribucion del tamano de granja",
    x = "Tamano de granja",
    y = "Numero de granjas"
  ) +
  theme_minimal()

p2
ggsave("resultados_RHD_conejos/grafico_02_tamano_granja.png", p2, width = 7, height = 5, dpi = 300)

# Grafico 3: Prevalencia de enfermedad hemorragica
p3 <- ggplot(datos, aes(x = enfermedad_hemorragica)) +
  geom_bar() +
  labs(
    title = "Frecuencia de enfermedad hemorragica viral del conejo",
    x = "Enfermedad hemorragica",
    y = "Numero de granjas"
  ) +
  theme_minimal()

p3
ggsave("resultados_RHD_conejos/grafico_03_prevalencia_RHD.png", p3, width = 7, height = 5, dpi = 300)

# Grafico 4: Agua de lluvia y enfermedad
p4 <- ggplot(datos, aes(x = agua_lluvia, fill = enfermedad_hemorragica)) +
  geom_bar(position = "fill") +
  labs(
    title = "Proporcion de RHD segun uso de agua de lluvia",
    x = "Uso de agua de lluvia",
    y = "Proporcion",
    fill = "RHD"
  ) +
  theme_minimal()

p4
ggsave("resultados_RHD_conejos/grafico_04_RHD_agua_lluvia.png", p4, width = 7, height = 5, dpi = 300)

# Grafico 5: Desinfeccion y enfermedad
p5 <- ggplot(datos, aes(x = desinfecta_herramientas, fill = enfermedad_hemorragica)) +
  geom_bar(position = "fill") +
  labs(
    title = "Proporcion de RHD segun desinfeccion de herramientas",
    x = "Desinfecta herramientas",
    y = "Proporcion",
    fill = "RHD"
  ) +
  theme_minimal()

p5
ggsave("resultados_RHD_conejos/grafico_05_RHD_desinfeccion.png", p5, width = 7, height = 5, dpi = 300)

# Grafico 6: Tamano de granja y enfermedad
p6 <- ggplot(datos, aes(x = tamano_granja, fill = enfermedad_hemorragica)) +
  geom_bar(position = "fill") +
  labs(
    title = "Proporcion de RHD segun tamano de granja",
    x = "Tamano de granja",
    y = "Proporcion",
    fill = "RHD"
  ) +
  theme_minimal()

p6
ggsave("resultados_RHD_conejos/grafico_06_RHD_tamano_granja.png", p6, width = 7, height = 5, dpi = 300)

# Grafico 7: Antiguedad de granja segun enfermedad
p7 <- ggplot(datos, aes(x = enfermedad_hemorragica, y = antiguedad_granja_anios)) +
  geom_boxplot() +
  labs(
    title = "Antiguedad de la granja segun presencia de RHD",
    x = "Enfermedad hemorragica",
    y = "Antiguedad de la granja (anios)"
  ) +
  theme_minimal()

p7
ggsave("resultados_RHD_conejos/grafico_07_antiguedad_RHD.png", p7, width = 7, height = 5, dpi = 300)

# ----------------------------------------------------------------
# 7. FUNCION PARA ANALISIS BIVARIADO
# ----------------------------------------------------------------
# Esta funcion crea tabla cruzada, prueba Chi-cuadrado/Fisher y OR.
# Si alguna celda esperada es pequena, se usa Fisher automaticamente.

analisis_bivariado <- function(variable){
  tabla <- table(datos[[variable]], datos$enfermedad_hemorragica)
  print(tabla)
  
  prueba_chi <- suppressWarnings(chisq.test(tabla))
  
  if(any(prueba_chi$expected < 5)){
    prueba <- fisher.test(tabla)
    metodo <- "Fisher exact test"
  } else {
    prueba <- prueba_chi
    metodo <- "Chi-cuadrado"
  }
  
  # Odds ratio solo funciona directamente para tablas 2x2.
  if(nrow(tabla) == 2 & ncol(tabla) == 2){
    or <- oddsratio(tabla, method = "wald")$measure
    or_resultado <- data.frame(
      categoria = rownames(or),
      OR = or[,1],
      IC95_inf = or[,2],
      IC95_sup = or[,3]
    )
  } else {
    or_resultado <- data.frame(
      categoria = NA,
      OR = NA,
      IC95_inf = NA,
      IC95_sup = NA
    )
  }
  
  resultado <- data.frame(
    variable = variable,
    metodo = metodo,
    p_valor = prueba$p.value
  )
  
  return(list(tabla = tabla, prueba = resultado, or = or_resultado))
}

# ----------------------------------------------------------------
# 8. ANALISIS BIVARIADO POR OBJETIVOS
# ----------------------------------------------------------------

# OE3: Tamano de granja
biv_tamano <- analisis_bivariado("tamano_granja")
biv_tamano$prueba
biv_tamano$or

# OE4: Agua de lluvia
biv_agua <- analisis_bivariado("agua_lluvia")
biv_agua$prueba
biv_agua$or

# OE5: Desinfeccion de herramientas
biv_desinf <- analisis_bivariado("desinfecta_herramientas")
biv_desinf$prueba
biv_desinf$or

# OE6: Alimentacion comercial
biv_alim <- analisis_bivariado("alimentacion_comercial")
biv_alim$prueba
biv_alim$or

# Otros factores exploratorios
biv_origen <- analisis_bivariado("origen_animales_ado")
biv_lavado <- analisis_bivariado("lavado_manos")
biv_corral <- analisis_bivariado("presencia_corral_enfermos")
biv_cercada <- analisis_bivariado("granja_cercada")
biv_capacitacion <- analisis_bivariado("capacitacion_cunicultura")

# Consolidar p-valores
resultados_bivariados <- bind_rows(
  biv_tamano$prueba,
  biv_agua$prueba,
  biv_desinf$prueba,
  biv_alim$prueba,
  biv_origen$prueba,
  biv_lavado$prueba,
  biv_corral$prueba,
  biv_cercada$prueba,
  biv_capacitacion$prueba
)

resultados_bivariados

# Consolidar OR crudos
or_crudos <- bind_rows(
  tamano_granja = biv_tamano$or,
  agua_lluvia = biv_agua$or,
  desinfecta_herramientas = biv_desinf$or,
  alimentacion_comercial = biv_alim$or,
  origen_animales_ado = biv_origen$or,
  lavado_manos = biv_lavado$or,
  presencia_corral_enfermos = biv_corral$or,
  granja_cercada = biv_cercada$or,
  capacitacion_cunicultura = biv_capacitacion$or,
  .id = "variable"
)

or_crudos

write_xlsx(
  list(
    p_valores_bivariados = resultados_bivariados,
    OR_crudos = or_crudos
  ),
  "resultados_RHD_conejos/02_resultados_bivariados_OR.xlsx"
)

# ----------------------------------------------------------------
# 9. REGRESION LOGISTICA MULTIVARIADA
# ----------------------------------------------------------------
# La variable respuesta es enfermedad_hemorragica.
# R interpreta el primer nivel como referencia. Aquí: No = referencia, Si = evento.

modelo <- glm(
  enfermedad_hemorragica ~ tamano_granja +
    agua_lluvia +
    origen_animales_ado +
    alimentacion_comercial +
    desinfecta_herramientas +
    lavado_manos +
    presencia_corral_enfermos +
    granja_cercada,
  family = binomial,
  data = datos
)

summary(modelo)

# Convertir coeficientes a OR ajustados
or_ajustados <- tidy(modelo, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate(
    interpretacion = case_when(
      estimate > 1 & p.value < 0.05 ~ "Factor asociado a mayor odds de RHD",
      estimate < 1 & p.value < 0.05 ~ "Factor protector asociado a menor odds de RHD",
      TRUE ~ "No significativo al 5%"
    )
  )

or_ajustados

write_xlsx(
  list(
    OR_ajustados_modelo = or_ajustados
  ),
  "resultados_RHD_conejos/03_regresion_logistica_OR_ajustados.xlsx"
)

# Tabla bonita del modelo para visualizar en RStudio
tabla_modelo <- tbl_regression(
  modelo,
  exponentiate = TRUE,
  label = list(
    tamano_granja ~ "Tamano de granja",
    agua_lluvia ~ "Uso de agua de lluvia",
    origen_animales_ado ~ "Animales procedentes de Ado-Ekiti",
    alimentacion_comercial ~ "Alimentacion comercial",
    desinfecta_herramientas ~ "Desinfecta herramientas",
    lavado_manos ~ "Lavado de manos",
    presencia_corral_enfermos ~ "Corral para enfermos",
    granja_cercada ~ "Granja cercada"
  )
)

tabla_modelo

# ----------------------------------------------------------------
# 10. GRAFICO FOREST PLOT DE OR AJUSTADOS
# ----------------------------------------------------------------
# Se excluye el intercepto porque no se interpreta como factor de riesgo.

forest_data <- or_ajustados %>%
  filter(term != "(Intercept)") %>%
  mutate(term = recode(term,
                       "tamano_granja<=20" = "Granja <=20 conejos",
                       "agua_lluviaSi" = "Uso de agua de lluvia",
                       "origen_animales_adoSi" = "Origen Ado-Ekiti",
                       "alimentacion_comercialSi" = "Alimentacion comercial",
                       "desinfecta_herramientasSi" = "Desinfecta herramientas",
                       "lavado_manosSi" = "Lavado de manos",
                       "presencia_corral_enfermosSi" = "Corral para enfermos",
                       "granja_cercadaSi" = "Granja cercada"))

p8 <- ggplot(forest_data,
             aes(x = estimate, y = reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_x_log10() +
  labs(
    title = "Odds Ratio ajustados para RHD",
    x = "OR ajustado (escala logaritmica)",
    y = "Variable"
  ) +
  theme_minimal()

p8
ggsave("resultados_RHD_conejos/grafico_08_forest_OR_ajustados.png", p8, width = 9, height = 6, dpi = 300)

# ----------------------------------------------------------------
# 11. RESPUESTAS AUTOMATICAS A LOS OBJETIVOS
# ----------------------------------------------------------------

prev <- datos %>%
  summarise(
    total = n(),
    casos = sum(enfermedad_hemorragica == "Si"),
    prevalencia = round(casos / total * 100, 1)
  )

respuesta_objetivos <- data.frame(
  objetivo = c(
    "OE1: Caracteristicas de las granjas",
    "OE2: Prevalencia de RHD",
    "OE3: Tamano de granja y RHD",
    "OE4: Agua de lluvia y RHD",
    "OE5: Desinfeccion de herramientas y RHD",
    "OE6: Alimentacion comercial y RHD",
    "OE7: Regresion logistica"
  ),
  resultado = c(
    paste0("Se evaluaron ", nrow(datos), " granjas cunicolas. La antiguedad promedio fue ",
           round(mean(datos$antiguedad_granja_anios), 1), " anios."),
    paste0("La prevalencia de enfermedad hemorragica viral fue ", prev$prevalencia,
           "% (", prev$casos, "/", prev$total, ")."),
    paste0("La asociacion entre tamano de granja y RHD presento p = ",
           round(biv_tamano$prueba$p_valor, 4), "."),
    paste0("La asociacion entre uso de agua de lluvia y RHD presento p = ",
           round(biv_agua$prueba$p_valor, 4), "."),
    paste0("La asociacion entre desinfeccion de herramientas y RHD presento p = ",
           round(biv_desinf$prueba$p_valor, 4), "."),
    paste0("La asociacion entre alimentacion comercial y RHD presento p = ",
           round(biv_alim$prueba$p_valor, 4), "."),
    "En la regresion logistica se deben interpretar los OR ajustados. OR > 1 indica mayor odds de RHD; OR < 1 indica posible factor protector."
  )
)

respuesta_objetivos

write_xlsx(
  list(
    respuestas_objetivos = respuesta_objetivos
  ),
  "resultados_RHD_conejos/04_respuestas_objetivos.xlsx"
)

# ----------------------------------------------------------------
# 12. INTERPRETACION GENERAL PARA EL INFORME
# ----------------------------------------------------------------
# Guia para interpretar:
# - Si p < 0.05: existe asociacion estadisticamente significativa.
# - Si OR > 1: la categoria evaluada tiene mayor odds de presentar RHD.
# - Si OR < 1: la categoria evaluada tiene menor odds de presentar RHD.
# - Si el intervalo de confianza del OR incluye el 1, el resultado no es significativo.
#
# Ejemplo de redaccion:
# "Las granjas que usaron agua de lluvia presentaron mayor probabilidad de
# enfermedad hemorragica viral en comparacion con las que no usaron agua de lluvia.
# Esta asociacion fue estadisticamente significativa (p < 0.05)."
#
# Comparacion con el articulo:
# El articulo de Shorunke et al. identifico como factores asociados el tamano de
# granja <=20 conejos, uso de agua de lluvia, origen de animales desde Ado-Ekiti,
# falta de desinfeccion de herramientas y uso de alimento comercial.

# ----------------------------------------------------------------
# 13. MENSAJE FINAL
# ----------------------------------------------------------------
cat("\nAnalisis completado. Revisa la carpeta 'resultados_RHD_conejos'.\n")
cat("Se generaron tablas, graficos, OR crudos, regresion logistica y respuestas a los objetivos.\n")
