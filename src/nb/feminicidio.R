library(tidyverse)
library(dplyr)

# import datasets from:
# https://www.datosabiertos.gob.pe/dataset/mimp-n%C3%BAmero-de-casos-de-v%C3%ADctimas-de-feminicidio-atendidas-por-los-cem-seg%C3%BAn-grupo-de-edad
# https://www.datosabiertos.gob.pe/dataset/mimp-n%C3%BAmero-de-casos-de-tentativa-de-feminicidio-atendidos-por-los-cem-seg%C3%BAn-grupo-de-edad
feminicidio <- "data/input/2.4.1 BdD_Feminicidio_5.csv"
tentativa <- "data/input/2.4.2 BdD_Tentativa_Feminicidio_6.csv"

raw1_df <- read_csv2(feminicidio)
raw2_df <- read_csv2(tentativa, locale = locale(encoding = "latin1"))
# remove column, estandarize column names and reorder columns
raw2_df <- subset(raw2_df, select = - UBIGEO)
names(raw2_df) <- sub(" DE TENTATIVA", "", names(raw2_df))
raw1_df$CATEGORY <- "FEMINICIDIO"
raw2_df$CATEGORY <- "TENTATIVA FEMINICIDIO"
columns <- c("AÑO", "NOMBRE SERVICIO", "DEPARTAMENTO", "PROVINCIA",
             "DISTRITO", "CATEGORY",
             "NOMBRE DE CENTRO DE ATENCIÓN", "N° DE CASOS - MUJERES",
             "N° CASOS - VINCULO RELACIONAL - PAREJA",
             "N° CASO - VINCULO RELACIONAL - EX PAREJA",
             "N° CASOS - VINCULO RELACIONAL - FAMILIAR",
             "N° CASOS - VINCULO RELACIONAL - CONOCIDO",
             "N° CASOS  - VINCULO RELACIONAL - DESCONOCIDO",
             "N° CASOS - VINCULO RELACIONAL - OTROS",
             "N° CASOS FEMINICIDIO - 0_5 - MUJERES",
             "N° CASOS FEMINICIDIO - 6_11 - MUJERES",
             "N° CASOS FEMINICIDIO - 12_17 - MUJERES",
             "N° CASOS FEMINICIDIO - 18_29 - MUJERES",
             "N° CASOS FEMINICIDIO - 30_59 - MUJERES",
             "N° CASOS FEMINICIDIO - 60_MÁS - MUJERES")
raw1_df <- raw1_df %>% select(all_of(columns))
raw2_df <- raw2_df %>% select(all_of(columns))
# check order columns names before append. All items must be TRUE
check_column_names <- identical(names(raw1_df), names(raw2_df))
# append, rename columns and filter >= 2019
df <- rbind(raw1_df, raw2_df)
new_columns <- c("year", "service_name", "department", "province",
                 "district", "category",
                 "care_center_name", "cases", "PAREJA", "EX_PAREJA", "FAMILIAR",
                 "CONOCIDO", "DESCONOCIDO", "OTROS",
                 "0 A 5 AÑOS", "6 A 11 AÑOS",
                 "12 A 17 AÑOS", "18 A 29 AÑOS", "30 A 59 AÑOS", "60 A MAS")
names(df) <- new_columns
df <- df %>% filter(year > 2018)
# fill numerical columns with NA values with 0
df[9:length(df)][is.na(df[9:length(df)])] <- 0
# split datasets and transform wide to long
df_bond <- df %>% select(year, service_name, department, province,
                         district, category, care_center_name,
                         PAREJA, EX_PAREJA, FAMILIAR, CONOCIDO,
                         DESCONOCIDO, OTROS)
df_agegroup <- df %>% select(year, service_name, department,
                             province, district, category,
                             care_center_name, "0 A 5 AÑOS", "6 A 11 AÑOS",
                             "12 A 17 AÑOS", "18 A 29 AÑOS",
                             "30 A 59 AÑOS", "60 A MAS")
df_bond <- df_bond %>% pivot_longer(c("PAREJA", "EX_PAREJA",
                                      "FAMILIAR", "CONOCIDO",
                                      "DESCONOCIDO", "OTROS"),
                                    names_to = "relational_bond",
                                    values_to = "cases")
df_agegroup <- df_agegroup %>% pivot_longer(c("0 A 5 AÑOS", "6 A 11 AÑOS",
                                              "12 A 17 AÑOS", "18 A 29 AÑOS",
                                              "30 A 59 AÑOS", "60 A MAS"),
                                            names_to = "age_group",
                                            values_to = "cases")
# test total dataframes
t1 <- df %>% group_by(year) %>% summarise(sum = sum(cases), .groups = "drop")
t2 <- df_agegroup %>%
  group_by(year) %>%
  summarise(sum = sum(cases), .groups = "drop")
t3 <- df_bond %>%
  group_by(year) %>%
  summarise(sum = sum(cases), .groups = "drop")
all.equal(t1, t2)
all.equal(t1, t3)
# write the list of dataframe
data <- list(df_bond, df_agegroup)
write.csv(data, "data/output/data_mimp_prep.csv", row.names = FALSE)
