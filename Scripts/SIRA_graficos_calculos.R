## Textiles ## 
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(showtext)
library(ggtext)
library(writexl)

font_add_google("Montserrat", "montserrat")
showtext_auto()

dir <- "direccion"

# Empalme de las bases de datos del IPC para CABA # 
base_1 <- read_excel(paste0(dir,"ipc_ambos.xlsx"), sheet = "Hoja1")
base_1$fecha <- as.Date(base_1$fecha)
base_2 <- read_excel(paste0(dir,"ipc_ambos.xlsx"), sheet = "Hoja2")
base_2$fecha <- as.Date(base_2$fecha)
base_2 <- base_2[base_2$fecha!="2022-02-01",]

columnas_iguales <- intersect(names(base_1), names(base_2))
base_1 <- base_1[, columnas_iguales]
base_2 <- base_2[, columnas_iguales]

base_full <- rbind(base_1, base_2)

ipc_columns <- names(base_full)
ipc_columns <- ipc_columns[!ipc_columns %in% c("fecha")]

multipliers <- list()

for (col in ipc_columns) {
  value <- base_full[base_full$fecha == as.Date("2021-12-01"), col]
  value <- as.numeric(value)
  if (length(value) == 1 && is.numeric(value)) {
    multipliers[[col]] <- value / 100
  } else {
    multipliers[[col]] <- NA  
  }
}

base_full$empalmar <- ifelse(base_full$fecha >= "2022-03-01", 1, 0)

for (col in ipc_columns) {
  multiplier <- multipliers[[col]]
  if (!is.na(multiplier)) {
    base_full[base_full$empalmar == 1, col] <- base_full[base_full$empalmar == 1, col] * multiplier
  }
}

ipc_columns <- ipc_columns[-1]
for (col in ipc_columns) {
  base_full[, col] <- ((base_full[, col]/base_full[,"Nivel General"])-1)*100
}


base_long <- base_full %>%
  pivot_longer(cols = all_of(ipc_columns), names_to = "ipc_type", values_to = "value")

base_long_1 <- base_long
table(base_long_1$ipc_type)


# Se guarda la base de datos para realizar las regresiones en Stata # 
base_long_1 <- base_long_1[base_long_1$fecha>="2022-01-01",]
base_long_1 <- base_long_1[base_long_1$ipc_type%in%c("Prendas de vestir",
                                                     "Seguros",
                                                     "Servicios de recreación",
                                                     "Seguros médicos",
                                                     "Artículos textiles para el hogar",
                                                     "Bebidas alcoholicas",
                                                     "Cristaleria, vajilla y utensilios para el hogar", 
                                                     "Electrodomesticos", 
                                                     "Funcionamiento de equipos de transporte personal", 
                                                     "Muebles, accesorios y alfombras", 
                                                     "Productos de recreacion"),]

# Normalizamos las variables para hacer el gráfico con varias categorias # 
primer_mes <- base_long_1 %>%
  dplyr::filter(fecha == as.Date("2022-01-01")) %>%
  dplyr::select(ipc_type, value) %>%
  dplyr::rename(primer_valor = value)

base_normalizada <- base_long_1 %>%
  dplyr::left_join(primer_mes, by = "ipc_type") %>%
  dplyr::mutate(value_norm = value - primer_valor)

write_xlsx(base_normalizada,paste0(dir,"base_ipc_clean_final_norm.xlsx"))


base_normalizada_1 <- base_normalizada[base_normalizada$ipc_type%in%c("Prendas de vestir",
                                                                      "Seguros",
                                                                      "Servicios de recreación",
                                                                      "Seguros médicos", 
                                                                      "Artículos textiles para el hogar", 
                                                                      "Electrodomesticos", 
                                                                      "Productos de recreacion"),]

#### Productos expuestos y no al comercio internacional ####
# opcion 1 
ipc_evolucion_plot_1 <- ggplot(base_normalizada_1, aes(x = fecha, y = value_norm, color = ipc_type, group = ipc_type)) +
  geom_line(data = base_normalizada_1 %>% filter(ipc_type %in% c("Seguros", "Servicios de recreación", "Seguros médicos")), linewidth = 0.7, linetype = "dashed") +
  geom_line(data = base_normalizada_1 %>% filter(ipc_type %in% c("Prendas de vestir", "Artículos textiles para el hogar", "Electrodomesticos", "Productos de recreacion")), linewidth = 1.2) +
  labs(title = "Evolución del IPC por rubro respecto IPC general",
       subtitle = "",
       x = "Fecha",
       y = "Puntos porcentuales",
       color = "Rubro",
       caption = "Fuente: Elaboración propia en base a datos del IPC CABA\n Las variables fueron normalizadas en 2021-01") +
  theme_minimal() +
  theme(
    text = element_text(size = 14, family = "montserrat"),
    legend.position = "bottom"
  ) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
  scale_color_manual(values = c(
    "Seguros" = "#D96C75",   
    "Prendas de vestir" = "#2E1B57",     
    "Seguros médicos" = "#7A4F7A", 
    "Servicios de recreación" = "#8D7B3A", 
    "Artículos textiles para el hogar" = "#7A869A", 
    "Electrodomesticos" = "#CCD4E1", 
    "Productos de recreacion" = "#89AEDD"), 
    labels = c(
      "Seguros" = "Seguros",
      "Prendas de vestir" = "Vestimenta y Calzado",
      "Seguros médicos" = "Seguros médicos", 
      "Servicios de recreación" = "Servicios de recreación", 
      "Artículos textiles para el hogar" = "Blanquería", 
      "Electrodomesticos" = "Electrodomésticos", 
      "Productos de recreacion" = "Productos de recreación"
    )) +
  geom_vline(xintercept = as.Date("2023-12-01"), linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey", linewidth = 1.5, alpha = 0.2) +
  geom_label(aes(x = as.Date("2023-07-01"), 
                 y = max(45, na.rm = TRUE), 
                 label = "Eliminación del SIRA\n2023-12"),
             color = "black", fill = "white", size = 4, 
             family = "montserrat", label.size = 0.5, label.padding = unit(0.25, "lines")) 
ipc_evolucion_plot_1

# opcion 2 
base_normalizada_1$exposicion <- ifelse(base_normalizada_1$ipc_type %in% c("Seguros", "Servicios de recreación", "Seguros médicos"), "No expuestos", "Expuestos")
# Dividir los datos en dos subconjuntos
base_expuestos <- base_normalizada_1 %>% filter(exposicion == "Expuestos")
base_no_expuestos <- base_normalizada_1 %>% filter(exposicion == "No expuestos")

# Crear el gráfico para "No expuestos"
plot_no_expuestos <- ggplot(base_no_expuestos, aes(x = fecha, y = value_norm, color = ipc_type, group = ipc_type)) +
  geom_line(linewidth = 1.2, linetype = "dashed") +
  labs(title = "Evolución del IPC relativo - No expuestos",
       x = "Fecha",
       y = "Puntos porcentuales",
       color = NULL) +  # Eliminamos el título de la leyenda
  theme_minimal() +
  theme(
    text = element_text(size = 14, family = "montserrat"),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_blank(),  # Eliminamos el título repetido
    legend.box = "horizontal",
    legend.key.width = unit(1.5, "cm")
  ) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
  scale_color_manual(values = c(
    "Seguros" = "#D96C75",   
    "Seguros médicos" = "#7A4F7A", 
    "Servicios de recreación" = "#8D7B3A"), 
    labels = c(
      "Seguros" = "Seguros",
      "Seguros médicos" = "Seguros médicos", 
      "Servicios de recreación" = "Servicios de recreación"
    )) +
  geom_vline(xintercept = as.Date("2023-12-01"), linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey", linewidth = 1.5, alpha = 0.2) +
  geom_label(aes(x = as.Date("2023-08-01"), 
                 y = max(20, na.rm = TRUE), 
                 label = "Eliminación del SIRA\n2023-12"),
             color = "black", fill = "white", size = 4, 
             family = "montserrat", label.size = 0.5, label.padding = unit(0.25, "lines")) +
  guides(color = guide_legend(nrow = 2))  # Ajuste para mostrar leyenda en dos renglones


#### Productos expuestos al comercio internacional ####
# Calcular el valor del último mes para cada categoría

base_normalizada_1 <- base_normalizada_1 %>%
  mutate(ipc_type = case_when(
    ipc_type == "Artículos textiles para el hogar" ~ "Blanquería",
    TRUE ~ ipc_type
  ))

valor_referencia <- base_normalizada_1 %>%
  dplyr::filter(fecha == as.Date("2024-04-01")) %>%
  dplyr::select(ipc_type, valor_referencia = value_norm)

sort(base_normalizada_1$ipc_type)

base_normalizada_2 <- base_normalizada_1[base_normalizada_1$ipc_type%in%c("Prendas de vestir", "Blanquería", 
                                                                          "Electrodomesticos", "Productos de recreacion"),]

# Calcular la diferencia entre el último valor y el valor en 2023-12-01
diferencia_valor <- base_normalizada_2 %>%
  dplyr::group_by(ipc_type) %>%
  dplyr::filter(fecha == max(fecha)) %>%
  dplyr::summarize(ultimo_valor = round(value_norm, 1)) %>%
  dplyr::left_join(valor_referencia, by = "ipc_type") %>%
  dplyr::mutate(diferencia = round(ultimo_valor - valor_referencia, 1)) %>%
  ungroup()
sort(diferencia_valor$ipc_type)


# Crear un vector con las etiquetas actualizadas
labels_actualizadas <- paste(
  c("Blanquería", 
    "Electrodomesticos", "Prendas de vestir","Productos de recreacion")
)

names(labels_actualizadas) <- c("Blanquería", 
                                "Electrodomesticos", "Prendas de vestir", "Productos de recreacion")


ipc_evolucion_plot_1 <- ggplot(base_normalizada_2, aes(x = fecha, y = value_norm, color = ipc_type, group = ipc_type)) +
  geom_line(data = base_normalizada_1 %>% filter(ipc_type %in% c("Prendas de vestir", "Blanquería", "Electrodomesticos", "Productos de recreacion")), linewidth = 1.2) +
  labs(title = "Evolución del IPC relativo - Expuestos",
       subtitle = "",
       x = "Fecha",
       y = "Puntos porcentuales",
       color = "Rubro",
       caption = "Fuente: Elaboración propia en base a datos del IPC CABA\n Las variables fueron normalizadas en 2021-01\n Productos de recreación incluye juguetes y equipos para deporte") +
  theme_minimal() +
  theme(
    text = element_text(size = 14, family = "montserrat"),
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm")
  ) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
  scale_color_manual(values = c(
    "Prendas de vestir" = "#2E1B57",     
    "Blanquería" = "#7A869A", 
    "Electrodomesticos" = "#CCD4E1", 
    "Productos de recreacion" = "#89AEDD"), 
    labels = labels_actualizadas) +
  guides(color = guide_legend(nrow = 2)) +  # Especificar 2 filas para la leyenda
  geom_vline(xintercept = as.Date("2023-12-01"), linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey", linewidth = 1.5, alpha = 0.2) +
  geom_label(aes(x = as.Date("2023-09-01"), 
                 y = max(45, na.rm = TRUE), 
                 label = "Eliminación del SIRA\n2023-12"),
             color = "black", fill = "white", size = 4, 
             family = "montserrat", label.size = 0.5, label.padding = unit(0.25, "lines"))

ipc_evolucion_plot_1

## segunda opcion 

# Crear un vector con las etiquetas actualizadas
labels_actualizadas <- paste(
  diferencia_valor$ipc_type,
  "[", diferencia_valor$diferencia, " pp]"
)

names(labels_actualizadas) <- c("Prendas de vestir", "Artículos textiles para el hogar", 
                                "Electrodomesticos", "Productos de recreacion")


ipc_evolucion_plot_1 <- ggplot(base_normalizada_2, aes(x = fecha, y = value_norm, color = ipc_type, group = ipc_type)) +
  geom_line(data = base_normalizada_1 %>% filter(ipc_type %in% c("Prendas de vestir", "Artículos textiles para el hogar", "Electrodomesticos", "Productos de recreacion")), linewidth = 1.2) +
  labs(title = "Evolución del IPC por rubro respecto IPC general",
       subtitle = "",
       x = "Fecha",
       y = "Puntos porcentuales",
       color = "Rubro",
       caption = "Fuente: Elaboración propia en base a datos del IPC CABA\nLas variables fueron normalizadas en 2021-01\nSe reporta entre corchetes la diferencia porcentual entre 2024-07 y 2023-12") +
  theme_minimal() +
  theme(
    text = element_text(size = 14, family = "montserrat"),
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm")
  ) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
  scale_color_manual(values = c(
    "Prendas de vestir" = "#2E1B57",     
    "Artículos textiles para el hogar" = "#7A869A", 
    "Electrodomesticos" = "#CCD4E1", 
    "Productos de recreacion" = "#89AEDD"), 
    labels = labels_actualizadas) +
  guides(color = guide_legend(nrow = 2)) +  # Especificar 2 filas para la leyenda
  geom_vline(xintercept = as.Date("2023-12-01"), linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey", linewidth = 1.5, alpha = 0.2) +
  geom_label(aes(x = as.Date("2023-08-01"), 
                 y = max(45, na.rm = TRUE), 
                 label = "Eliminación del SIRA\n2023-12"),
             color = "black", fill = "white", size = 4, 
             family = "montserrat", label.size = 0.5, label.padding = unit(0.25, "lines"))

ipc_evolucion_plot_1


## calculo de inflación ## 
base_1 <- read_excel(paste0(dir,"ipc_ambos.xlsx"), sheet = "Hoja1")
base_1$fecha <- as.Date(base_1$fecha)
base_2 <- read_excel(paste0(dir,"ipc_ambos.xlsx"), sheet = "Hoja2")
base_2$fecha <- as.Date(base_2$fecha)
base_2 <- base_2[base_2$fecha!="2022-02-01",]

columnas_iguales <- intersect(names(base_1), names(base_2))
base_1 <- base_1[, columnas_iguales]
base_2 <- base_2[, columnas_iguales]

base_full <- rbind(base_1, base_2)

ipc_columns <- names(base_full)
ipc_columns <- ipc_columns[!ipc_columns %in% c("fecha")]

multipliers <- list()

for (col in ipc_columns) {
  value <- base_full[base_full$fecha == as.Date("2021-12-01"), col]
  value <- as.numeric(value)
  if (length(value) == 1 && is.numeric(value)) {
    multipliers[[col]] <- value / 100
  } else {
    multipliers[[col]] <- NA  
  }
}

base_full$empalmar <- ifelse(base_full$fecha >= "2022-03-01", 1, 0)

for (col in ipc_columns) {
  multiplier <- multipliers[[col]]
  if (!is.na(multiplier)) {
    base_full[base_full$empalmar == 1, col] <- base_full[base_full$empalmar == 1, col] * multiplier
  }
}

base_full <- base_full[base_full$fecha>="2018-01-01",]

base_long <- base_full %>%
  pivot_longer(cols = all_of(ipc_columns), names_to = "ipc_type", values_to = "value")

base_long_1 <- base_long
table(base_long_1$ipc_type)
# Se guarda la base de datos para realizar las regresiones en Stata # 
base_long_1 <- base_long_1[base_long_1$fecha>="2022-01-01",]
base_long_1 <- base_long_1[base_long_1$ipc_type%in%c("Prendas de vestir",
                                                     "Artículos textiles para el hogar",
                                                     "Electrodomesticos", 
                                                     "Productos de recreacion", "Nivel General"),]

base_long_2 <- base_long_1[base_long_1$fecha%in%c("2023-12-01","2024-07-01"),]
base_long_2 <- base_long_2 %>%
  dplyr::group_by(ipc_type) %>%
  dplyr::mutate(variacion_porcentual = (value[fecha == "2024-07-01"] - value[fecha == "2023-12-01"]) / 
                  value[fecha == "2023-12-01"] * 100)

