# ---- Libraries ----
library(tidyverse)
library(openxlsx)
library(writexl)
library(highcharter)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(readxl)
library(haven)
library(sf)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(rmapshaper)

options(warn = -1)

source("custom_styles.R")                                                                       

# --Excel datacube:
datacube_url <- "https://www.abs.gov.au/articles/geospatial-dietary-indicators/4316DO002_202324_ESTIMATES.xlsx"
datacube <- tempfile(fileext = ".xlsx")
download.file(datacube_url, datacube, mode = "wb")

# ----Load SA3 & SA4 map polygons ----
SA3_sp_simpl_med <- readRDS("sa3_med.rds")
SA3_sp_simpl_med <- sf::st_as_sf(SA3_sp_simpl_med)
SA3_sp_simpl_med <- sf::st_transform(SA3_sp_simpl_med, 4326)
SA3_sp_simpl_med <- as(SA3_sp_simpl_med, "Spatial")
SA3_sp_simpl_med@data$SA3_code <- as.character(SA3_sp_simpl_med@data$SA3_code)

SA4_sp_simpl_med <- readRDS("sa4_med.rds")
SA4_sp_simpl_med <- sf::st_as_sf(SA4_sp_simpl_med)
SA4_sp_simpl_med <- sf::st_transform(SA4_sp_simpl_med, 4326)
SA4_sp_simpl_med <- as(SA4_sp_simpl_med, "Spatial")
SA4_sp_simpl_med@data$SA4_code <- as.character(SA4_sp_simpl_med@data$SA4_code)


# ---- Geography lookup (for SA3/SA4 cross tabs) ----
geog <- read_excel("geog.xlsx", sheet = 1) %>% 
  select(SA3_code, SA4_code, SA4_name, GCCSA_code, GCCSA_name, STATE_code, RA_code, state) %>%
  mutate(PoS = case_when(
    STATE_code == "08" ~ "cc",
    substr(GCCSA_name, 1, 4) == "Grea" ~ "cc",
    TRUE ~ "bal"
  )
  ) %>%
  distinct()                                                                                     


# ------------------------------------------
# ---- Read in & format tables
# -------------------------------------------

Tab1  <- read_excel(datacube, sheet = 2,  range = "A7:O142")
Tab2  <- read_excel(datacube, sheet = 3,  range = "A7:O142")
Tab3  <- read_excel(datacube, sheet = 4,  range = "A7:H51")
Tab4  <- read_excel(datacube, sheet = 5,  range = "A7:M18")
Tab5  <- read_excel(datacube, sheet = 6,  range = "A7:I31")
Tab6  <- read_excel(datacube, sheet = 7,  range = "A7:O142")
Tab7  <- read_excel(datacube, sheet = 8,  range = "A7:O142")
Tab8  <- read_excel(datacube, sheet = 9,  range = "A7:H51")
Tab9  <- read_excel(datacube, sheet = 10, range = "A7:M18")
Tab10 <- read_excel(datacube, sheet = 11, range = "A7:I31")
Tab11 <- read_excel(datacube, sheet = 12, range = "A7:U142")
Tab12 <- read_excel(datacube, sheet = 13, range = "A7:U142")
Tab13 <- read_excel(datacube, sheet = 14, range = "A7:K51")
Tab14 <- read_excel(datacube, sheet = 15, range = "A7:S18")
Tab15 <- read_excel(datacube, sheet = 16, range = "A7:L31")

# SA4 tables
Tab16 <- read_excel(datacube, sheet = 17, range = "A5:G11885")
Tab17 <- read_excel(datacube, sheet = 18, range = "A5:G11885")
Tab18 <- read_excel(datacube, sheet = 19, range = "A5:E3877")
Tab19 <- read_excel(datacube, sheet = 20, range = "A5:E973")
Tab20 <- read_excel(datacube, sheet = 21, range = "A5:F2117")

# SA3 tables
Tab21 <- read_excel(datacube, sheet = 22, range = "A5:G44555")
Tab22 <- read_excel(datacube, sheet = 23, range = "A5:G44555")
Tab23 <- read_excel(datacube, sheet = 24, range = "A5:E14525")
Tab24 <- read_excel(datacube, sheet = 25, range = "A5:E3635")
Tab25 <- read_excel(datacube, sheet = 26, range = "A5:F7925")

# Assign suitable colnames
T11_name  <- c("Class_level", "AUSNUT_code",  "AUSNUT_descr", "g_NSW", "g_Vic", "g_Qld", "g_SA", "g_WA", "g_Tas", "g_NT", "g_ACT", "g_Aust.",
              "pcg_NSW", "pcg_Vic", "pcg_Qld", "pcg_SA", "pcg_WA", "pcg_Tas", "pcg_NT", "pcg_ACT", "pcg_Aust.")
T12_name  <- c("Class_level", "AUSNUT_code",  "AUSNUT_descr", "kJ_NSW", "kJ_Vic", "kJ_Qld", "kJ_SA", "kJ_WA", "kJ_Tas", "kJ_NT", "kJ_ACT", "kJ_Aust.",
              "pckJ_NSW", "pckJ_Vic", "pckJ_Qld", "pckJ_SA", "pckJ_WA", "pckJ_Tas", "pckJ_NT", "pckJ_ACT", "pckJ_Aust.")
T13_name  <- c("Nutrient", "Unit", "NSW", "Vic", "Qld", "SA", "WA", "Tas", "NT", "ACT", "Aust.")
T14_name  <- c("Macronutrient", "kJ_NSW", "kJ_Vic", "kJ_Qld", "kJ_SA", "kJ_WA", "kJ_Tas", "kJ_NT", "kJ_ACT", "kJ_Aust.",
              "pckJ_NSW", "pckJ_Vic", "pckJ_Qld", "pckJ_SA", "pckJ_WA", "pckJ_Tas", "pckJ_NT", "pckJ_ACT", "pckJ_Aust.")
T15_name  <- c("Disc_status", "ADG_group", "Unit", "NSW", "Vic", "Qld", "SA", "WA", "Tas", "NT", "ACT", "Aust.")

# SEIFA
T1_name  <- c("Class_level", "AUSNUT_code", "AUSNUT_descr", "g_Q1", "g_Q2", "g_Q3", "g_Q4", "g_Q5", "g_Total Q",
              "pcg_Q1", "pcg_Q2", "pcg_Q3", "pcg_Q4", "pcg_Q5", "pcg_Total Q")
T2_name  <- c("Class_level", "AUSNUT_code", "AUSNUT_descr", "kJ_Q1", "kJ_Q2", "kJ_Q3", "kJ_Q4", "kJ_Q5", "kJ_Total Q",
              "pckJ_Q1", "pckJ_Q2", "pckJ_Q3", "pckJ_Q4", "pckJ_Q5", "pckJ_Total Q")
T3_name  <- c("Nutrient", "Unit", "Q1", "Q2", "Q3", "Q4", "Q5", "Total Q")
T4_name  <- c("Macronutrient","kJ_Q1", "kJ_Q2", "kJ_Q3", "kJ_Q4", "kJ_Q5", "kJ_Total Q",
              "pckJ_Q1", "pckJ_Q2", "pckJ_Q3", "pckJ_Q4", "pckJ_Q5", "pckJ_Total Q")
T5_name <- c("Disc_status", "ADG_group", "Unit", "Q1", "Q2", "Q3", "Q4", "Q5", "Total Q")

# Remoteness
T6_name <- c("Class_level", "AUSNUT_code", "AUSNUT_descr",
              "g_Maj. Cities", "g_Inner Reg.", "g_Outer Reg.", "g_Remote", "g_Very Remote", "g_Total RA",
              "pcg_Maj. Cities", "pcg_Inner Reg.", "pcg_Outer Reg.", "pcg_Remote", "pcg_Very Remote", "pcg_Total RA")
T7_name <- c("Class_level", "AUSNUT_code", "AUSNUT_descr",
              "kJ_Maj. Cities", "kJ_Inner Reg.", "kJ_Outer Reg.", "kJ_Remote", "kJ_Very Remote", "kJ_Total RA",
              "pckJ_Maj. Cities", "pckJ_Inner Reg.", "pckJ_Outer Reg.", "pckJ_Remote", "pckJ_Very Remote", "pckJ_Total RA")
T8_name <- c("Nutrient", "Unit",  "Maj. Cities", "Inner Reg.", "Outer Reg.", "Remote", "Very Remote", "Total RA")
T9_name <- c("Macronutrient",  "kJ_Maj. Cities", "kJ_Inner Reg.", "kJ_Outer Reg.", "kJ_Remote", "kJ_Very Remote", "kJ_Total RA",
              "pckJ_Maj. Cities", "pckJ_Inner Reg.", "pckJ_Outer Reg.", "pckJ_Remote", "pckJ_Very Remote", "pckJ_Total RA")
T10_name <- c("Disc_status", "ADG_group", "Unit",  "Maj. Cities", "Inner Reg.", "Outer Reg.", "Remote", "Very Remote", "Total RA")

# SA4 tables
T16_name <- c("SA4_code", "SA4_name", "Class_level", "AUSNUT_code",  "AUSNUT_descr", "g", "pcg")
T17_name <- c("SA4_code", "SA4_name", "Class_level", "AUSNUT_code",  "AUSNUT_descr", "kJ", "pckJ")
T18_name <- c("SA4_code", "SA4_name", "Nutrient", "Unit", "val")
T19_name <- c("SA4_code", "SA4_name", "Macronutrient", "kJ", "Percent of kJ")
T20_name <- c("SA4_code", "SA4_name", "Disc_status", "ADG_group", "Unit", "val")

# SA3 tables
T21_name <- c("SA3_code", "SA3_name", "Class_level", "AUSNUT_code",  "AUSNUT_descr", "g", "pcg")
T22_name <- c("SA3_code", "SA3_name", "Class_level", "AUSNUT_code",  "AUSNUT_descr", "kJ", "pckJ")
T23_name <- c("SA3_code", "SA3_name", "Nutrient", "Unit", "val")
T24_name <- c("SA3_code", "SA3_name", "Macronutrient", "kJ", "Percent of kJ")
T25_name <- c("SA3_code", "SA3_name", "Disc_status", "ADG_group", "Unit", "val")



#---------------------------
# Assign newnames & format:
#---------------------------

Tab1 <- Tab1 %>%
  setNames(T1_name) %>%
  pivot_longer(4:15, names_to = "Geog_cat", values_to = "val", values_transform = list(val = as.numeric)) %>%
  separate(col = "Geog_cat", into = c("Unit", "Geog_cat"), sep = "_") %>%
  mutate(Geog_type = "SEIFA") %>%
  mutate(AUSNUT_code = as.character(AUSNUT_code))

Tab2 <- Tab2 %>%
  setNames(T2_name) %>%
  pivot_longer(4:15, names_to = "Geog_cat", values_to = "val", values_transform = list(val = as.numeric)) %>%
  separate(col = "Geog_cat", into = c("Unit", "Geog_cat"), sep = "_") %>%
  mutate(Geog_type = "SEIFA") %>%
  mutate(AUSNUT_code = as.character(AUSNUT_code))

Tab3 <- Tab3 %>%
  setNames(T3_name) %>%
  pivot_longer(3:8, names_to = "Geog_cat", values_to = "val", values_transform = list(val = as.numeric)) %>%
  mutate(Geog_type = "SEIFA")

Tab4 <- Tab4 %>%
  setNames(T4_name) %>%
  pivot_longer(2:13, names_to = "Geog_cat", values_to = "val", values_transform = list(val = as.numeric)) %>%
  separate(col = "Geog_cat", into = c("Unit", "Geog_cat"), sep = "_") %>%
  mutate(Geog_type = "SEIFA")

Tab5 <- Tab5 %>%
  setNames(T5_name) %>%
  pivot_longer(4:9, names_to = "Geog_cat", values_to = "val", values_transform = list(val = as.numeric)) %>%
  mutate(Geog_type = "SEIFA")

Tab6 <- Tab6 %>%
  setNames(T6_name) %>%
  pivot_longer(4:15, names_to = "Geog_cat", values_to = "val", values_transform = list(val = as.numeric)) %>%
  separate(col = "Geog_cat", into = c("Unit", "Geog_cat"), sep = "_") %>%
  mutate(Geog_type = "RA") %>%
  mutate(AUSNUT_code = as.character(AUSNUT_code))

Tab7 <- Tab7 %>%
  setNames(T7_name) %>%
  pivot_longer(4:15, names_to = "Geog_cat", values_to = "val", values_transform = list(val = as.numeric)) %>%
  separate(col = "Geog_cat", into = c("Unit", "Geog_cat"), sep = "_") %>%
  mutate(Geog_type = "RA") %>%
  mutate(AUSNUT_code = as.character(AUSNUT_code))

Tab8 <- Tab8 %>%
  setNames(T8_name) %>%
  pivot_longer(3:8, names_to = "Geog_cat", values_to = "val", values_transform = list(val = as.numeric)) %>%
  mutate(Geog_type = "RA")

Tab9 <- Tab9 %>%
  setNames(T9_name) %>%
  pivot_longer(2:13, names_to = "Geog_cat", values_to = "val", values_transform = list(val = as.numeric)) %>%
  separate(col = "Geog_cat", into = c("Unit", "Geog_cat"), sep = "_") %>%
  mutate(Geog_type = "RA")

Tab10 <- Tab10 %>%
  setNames(T10_name) %>%
  pivot_longer(4:9, names_to = "Geog_cat", values_to = "val", values_transform = list(val = as.numeric)) %>%
  mutate(Geog_type = "RA")

Tab11 <- Tab11 %>%
  setNames(T11_name) %>%
  pivot_longer(4:21, names_to = "Geog_cat", values_to = "val", values_transform = list(val = as.numeric)) %>%
  separate(col = "Geog_cat", into = c("Unit", "Geog_cat"), sep = "_") %>%
  mutate(Geog_type = "State") %>%
  mutate(AUSNUT_code = as.character(AUSNUT_code))

Tab12 <- Tab12 %>%
  setNames(T12_name) %>%
  pivot_longer(4:21, names_to = "Geog_cat", values_to = "val", values_transform = list(val = as.numeric)) %>%
  separate(col = "Geog_cat", into = c("Unit", "Geog_cat"), sep = "_") %>%
  mutate(Geog_type = "State") %>%
  mutate(AUSNUT_code = as.character(AUSNUT_code))

Tab13 <- Tab13 %>%
  setNames(T13_name) %>%
  pivot_longer(3:11, names_to = "Geog_cat", values_to = "val", values_transform = list(val = as.numeric)) %>%
  mutate(Geog_type = "State")

Tab14 <- Tab14 %>%
  setNames(T14_name) %>%
  pivot_longer(2:19, names_to = "Geog_cat", values_to = "val", values_transform = list(val = as.numeric)) %>%
  separate(col = "Geog_cat", into = c("Unit", "Geog_cat"), sep = "_") %>%
  mutate(Geog_type = "State")

Tab15 <- Tab15 %>%
  setNames(T15_name) %>%
  pivot_longer(4:12, names_to = "Geog_cat", values_to = "val", values_transform = list(val = as.numeric)) %>%
  mutate(Geog_type = "State")

Tab16 <- Tab16 %>%
  setNames(T16_name) %>%
  pivot_longer(6:7, names_to = "Unit", values_to = "val", values_transform = list(val = as.numeric)) %>%
  mutate(SA4_code = as.character(SA4_code), AUSNUT_code = as.character(AUSNUT_code))

Tab17 <- Tab17 %>%
  setNames(T17_name) %>%
  pivot_longer(6:7, names_to = "Unit", values_to = "val", values_transform = list(val = as.numeric)) %>%
  mutate(SA4_code = as.character(SA4_code), AUSNUT_code = as.character(AUSNUT_code))

Tab18 <- Tab18 %>%
  setNames(T18_name) %>%
  mutate(SA4_code = as.character(SA4_code),
         val = as.numeric(val))

Tab19 <- Tab19 %>%
  setNames(T19_name) %>%
  mutate(SA4_code = as.character(SA4_code))

Tab20 <- Tab20 %>%
  setNames(T20_name) %>%
  mutate(SA4_code = as.character(SA4_code),
         val = as.numeric(val))

Tab21 <- Tab21 %>%
  setNames(T21_name) %>%
  pivot_longer(6:7, names_to = "Unit", values_to = "val", values_transform = list(val = as.numeric)) %>%
  mutate(SA3_code = as.character(SA3_code), AUSNUT_code = as.character(AUSNUT_code))

Tab22 <- Tab22 %>%
  setNames(T22_name) %>%
  pivot_longer(6:7, names_to = "Unit", values_to = "val", values_transform = list(val = as.numeric)) %>%
  mutate(SA3_code = as.character(SA3_code), AUSNUT_code = as.character(AUSNUT_code))

Tab23 <- Tab23 %>%
  setNames(T23_name) %>%
  mutate(SA3_code = as.character(SA3_code),
         val = as.numeric(val))

Tab24 <- Tab24 %>%
  setNames(T24_name) %>%
  mutate(SA3_code = as.character(SA3_code))

Tab25 <- Tab25 %>%
  setNames(T25_name) %>%
  mutate(SA3_code = as.character(SA3_code),
         val = as.numeric(val))


#----------------------------
# Consolidate common tables 
#----------------------------

# Load user-friendly short Ausnut labels
labels <- read_xlsx_from_url("https://raw.githubusercontent.com/Atyepa/AUSNUT/master/AUSNUT_maj_subma_shortlabels.xlsx") %>% 
  mutate(code = as.character(code))

# 1.1) AUSNUT x aggregate geog 
Ausnut_tab <- Tab1 %>% 
  bind_rows(Tab2) %>% 
  bind_rows(Tab6, Tab7, Tab11, Tab12) %>% 
  mutate(Class_level = case_when(AUSNUT_code == ". ." ~ "Summary", TRUE ~ Class_level)) %>% 
  apply_labels(labels, by = c("code" = "AUSNUT_code")) 

# Calculate a row for each series called "Total food (excl. beverages)"
# --- 1) Slice the two components ---
totals  <- Ausnut_tab %>% filter(AUSNUT_descr == "Total foods and beverages")
nonalc  <- Ausnut_tab %>% filter(AUSNUT_descr == "Non-alcoholic beverages") %>%
  select(Unit, Geog_type, Geog_cat, val_nonalc = val)

# --- 2) Join and compute the difference ---
new_rows <- totals %>%
  inner_join(nonalc, by = c("Unit", "Geog_type", "Geog_cat")) %>%
  mutate(
    val = val - val_nonalc,
    # Rename descriptor(s) for the new category
    AUSNUT_descr = "Total foods (excl. beverages)"
  ) %>%
  # If you have code/name columns tied to AUSNUT, you can blank or custom-code them here.
  # e.g., AUSNUT_code = NA_character_
  select(names(Ausnut_tab))  # keep columns in the original order

# Slice AUSNUT non_alc  for summary
non_alc  <- Ausnut_tab %>% filter(AUSNUT_descr == "Non-alcoholic beverages") %>% 
  mutate(Class_level = "Summary", 
         AUSNUT_descr = "Total non-alcoholic beverages",
         AUSNUT_code = ". .", 
         cLabel = "Total non-alcoholic beverages")

# --- 3) Append back to original ---
Ausnut_tab <- bind_rows(Ausnut_tab, new_rows, non_alc) %>% 
  mutate(cLabel = case_when(AUSNUT_code != ". ." ~ paste0(AUSNUT_code, ", ", AUSNUT_descr),
                            TRUE ~ AUSNUT_descr))   

Ausnut_tab$Geog_cat <- factor(Ausnut_tab$Geog_cat, 
                              levels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Total Q", 
                                         "Maj. Cities", "Inner Reg.", "Outer Reg.", 
                                         "Remote", "Very Remote", "Total RA", 
                                         "NSW", "Vic", "Qld", "SA", "WA", 
                                         "Tas", "NT", "ACT", "Aust."))

# Helper to add "Total foods (excl. beverages)" rows
add_total_excl_bev <- function(.df, by_keys) {
  totals <- .df %>%
    filter(AUSNUT_descr == "Total foods and beverages")
  
  nonalc <- .df %>%
    filter(AUSNUT_descr == "Non-alcoholic beverages") %>%
    select(all_of(by_keys), val_nonalc = val)
  
  new_rows <- totals %>%
    inner_join(nonalc, by = by_keys) %>%
    mutate(
      val = val - val_nonalc,
      AUSNUT_descr = "Total foods (excl. beverages)"
    ) %>%
    # Keep label column consistent with descriptor change
    mutate(
      cLabel = case_when(
        AUSNUT_code != ". ." ~ paste0(AUSNUT_code, ", ", AUSNUT_descr),
        TRUE ~ AUSNUT_descr
      )
    ) %>%
    select(names(.df))  # preserve original column order
  
  bind_rows(.df, new_rows)
}


# 1.2) AUSNUT x SA3
AusnutSA3_tab <- Tab21 %>%
  bind_rows(Tab22) %>%
  mutate(Class_level = case_when(AUSNUT_code == ". ." ~ "Summary", TRUE ~ Class_level)) %>%
  apply_labels(labels, by = c("code" = "AUSNUT_code")) %>%
  mutate(cLabel = case_when(AUSNUT_code != ". ." ~ paste0(AUSNUT_code, ", ", AUSNUT_descr),
                            TRUE ~ AUSNUT_descr)) %>%
  mutate(SA3 = paste0(SA3_code, ", ", SA3_name)) %>%
  left_join(select(geog, -RA_code), by = "SA3_code") %>%
  distinct()

# Add the "Total foods (excl. beverages)" rows; join key is Unit + SA3_code
AusnutSA3_tab <- add_total_excl_bev(AusnutSA3_tab, by_keys = c("Unit", "SA3_code"))


# 1.3) AUSNUT x SA4
AusnutSA4_tab <- Tab16 %>%
  bind_rows(Tab17) %>%
  mutate(Class_level = case_when(AUSNUT_code == ". ." ~ "Summary", TRUE ~ Class_level)) %>%
  apply_labels(labels, by = c("code" = "AUSNUT_code")) %>%
  mutate(cLabel = case_when(AUSNUT_code != ". ." ~ paste0(AUSNUT_code, ", ", AUSNUT_descr),
                            TRUE ~ AUSNUT_descr)) %>%
  mutate(SA4 = paste0(SA4_code, ", ", SA4_name)) %>%
  left_join(select(geog, -SA4_name, -SA3_code, -RA_code), by = "SA4_code") %>%
  distinct()

# Add the "Total foods (excl. beverages)" rows; join key is Unit + SA4_code
AusnutSA4_tab <- add_total_excl_bev(AusnutSA4_tab, by_keys = c("Unit", "SA4_code"))



# 2.1) Nutrient x aggr geog
Nutrient_tab <- Tab3 %>% 
  bind_rows(Tab8, Tab13) %>%  
  mutate(Geog_cat = factor(Geog_cat, 
                           levels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Total Q", 
                                      "Maj. Cities", "Inner Reg.", "Outer Reg.", 
                                      "Remote", "Very Remote", "Total RA", 
                                      "NSW", "Vic", "Qld", "SA", "WA", 
                                      "Tas", "NT", "ACT", "Aust.")))
# 2.2) Nutrient x SA3
Nutrient_tabSA3 <- Tab23 %>%
  mutate(SA3 = paste0(SA3_code, ", ", SA3_name))%>%
  left_join(select(geog,-RA_code), by = "SA3_code") %>%
  distinct()

# 2.3) Nutrient x SA4
Nutrient_tabSA4 <- Tab18 %>%
  mutate(SA4 = paste0(SA4_code, ", ", SA4_name))%>%
  left_join(select(geog,-RA_code, -SA3_code, -SA4_name), by = "SA4_code") %>%
  distinct()

# 3.1 Macronutrient x aggr geog
Macro_tab <- Tab4 %>% 
  bind_rows(Tab9, Tab14) %>%  
  mutate(Geog_cat = factor(Geog_cat, 
                           levels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Total Q", 
                                      "Maj. Cities", "Inner Reg.", "Outer Reg.", 
                                      "Remote", "Very Remote", "Total RA", 
                                      "NSW", "Vic", "Qld", "SA", "WA", 
                                      "Tas", "NT", "ACT", "Aust.")))

Macro_tab$Unit <- gsub("pckJ", "Percent of kJ", Macro_tab$Unit)

# 3.2 Macronutrient x SA3
Macro_tabSA3 <- Tab24 %>%
  pivot_longer(4:5, names_to = "Unit", values_to = "val", values_transform = list(val = as.numeric)) %>%
  mutate(SA3 = paste0(SA3_code, ", ", SA3_name))%>%
  left_join(select(geog,-RA_code), by = "SA3_code") %>%
  distinct()

# 3.3 Macronutrient x SA4
Macro_tabSA4 <- Tab19 %>%
  pivot_longer(4:5, names_to = "Unit", values_to = "val", values_transform = list(val = as.numeric)) %>%
  mutate(SA4 = paste0(SA4_code, ", ", SA4_name))%>%
  left_join(select(geog,-RA_code, -SA3_code, -SA4_name), by = "SA4_code") %>%
  distinct()

# 4.1 ADG x aggr geog
ADG_tab <- Tab5 %>%
  bind_rows(Tab10, Tab15) %>%
  mutate(Geog_cat = factor(Geog_cat,
                           levels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Total Q", 
                                      "Maj. Cities", "Inner Reg.", "Outer Reg.", 
                                      "Remote", "Very Remote", "Total RA", 
                                      "NSW", "Vic", "Qld", "SA", "WA", 
                                      "Tas", "NT", "ACT", "Aust.")))
# 4.2 ADG x SA3
ADG_tabSA3 <- Tab25 %>%
  mutate(SA3 = paste0(SA3_code, ", ", SA3_name))%>%
  left_join(select(geog,-RA_code), by = "SA3_code") %>%
  distinct()

# 4.3 ADG x SA4
ADG_tabSA4 <- Tab20 %>%
  mutate(SA4 = paste0(SA4_code, ", ", SA4_name))%>%
  left_join(select(geog,-RA_code, -SA3_code, -SA4_name), by = "SA4_code") %>%
  distinct()

#-----------------------------
# Generate label lists for UI
#-----------------------------
Two_dig <- Ausnut_tab %>%
  filter(Class_level %in% c("Major", "Selected beverages", "Total", "Discretionary")) %>% 
  select(AUSNUT_code, AUSNUT_descr, cLabel) %>%
  distinct() %>% 
  group_by(cLabel) %>%
  tally()

Two_dig <- as.list(as.character(Two_dig$cLabel))

Thr_dig <- Ausnut_tab %>%
  filter(Class_level == "Sub-major") %>% 
  select(AUSNUT_code, AUSNUT_descr, cLabel) %>%
  distinct() %>% 
  group_by(cLabel) %>%
  tally()

Thr_dig <- as.list(as.character(Thr_dig$cLabel))


ADG <- ADG_tab %>%
  select(ADG_group) %>%
  distinct() 

ADG <- as.list(as.character(ADG$ADG_group))

Nut_unit <- Nutrient_tab %>% 
  select(Nutrient, Unit)%>% 
  distinct() %>% 
  dplyr::mutate(order = row_number())

Nutrient <- as.list(as.character(Nut_unit$Nutrient))

Macro <- Macro_tab %>%
  select(Macronutrient) %>%
  distinct() 

Macro <- as.list(as.character(Macro$Macronutrient))


# SA3 list 
SA3 <- AusnutSA3_tab %>% 
  select(SA3_code, SA3) %>% 
  distinct() 

SA3list <- as.list(SA3$SA3)

# SA4 list 
SA4 <- AusnutSA4_tab %>% 
  select(SA4_code, SA4) %>% 
  distinct() 

SA4list <- as.list(SA4$SA4)


# ---- Helper: safe join of values onto polygons (no row duplication) ----
attach_layer_to_sp <- function(SA3_sp, layer_df) {
  stopifnot(inherits(SA3_sp, "SpatialPolygonsDataFrame"))
  
  if (!"SA3_code" %in% names(layer_df)) {
    SA3_sp@data$val   <- NA_real_
    SA3_sp@data$Unit  <- NA_character_
    SA3_sp@data$label <- NA_character_
    return(SA3_sp)
  }
  
  layer_df <- layer_df %>%
    dplyr::group_by(SA3_code) %>%
    dplyr::summarise(
      val   = sum(as.numeric(val), na.rm = TRUE),
      Unit  = dplyr::first(Unit),
      label = dplyr::first(label),
      .groups = "drop"
    )
  
  idx <- match(SA3_sp@data$SA3_code, layer_df$SA3_code)
  
  SA3_sp@data$val   <- layer_df$val  [idx]
  SA3_sp@data$Unit  <- layer_df$Unit [idx]
  SA3_sp@data$label <- layer_df$label[idx]
  
  SA3_sp
}             


# ---- Helper: safe join of values onto polygons (SA4; no row duplication) ----
attach_layer_to_sp_sa4 <- function(SA4_sp, layer_df) {
  stopifnot(inherits(SA4_sp, "SpatialPolygonsDataFrame"))
  
  if (!"SA4_code" %in% names(layer_df)) {
    SA4_sp@data$val   <- NA_real_
    SA4_sp@data$Unit  <- NA_character_
    SA4_sp@data$label <- NA_character_
    return(SA4_sp)
  }
  
  layer_df <- layer_df %>%
    dplyr::group_by(SA4_code) %>%
    dplyr::summarise(
      val   = sum(as.numeric(val), na.rm = TRUE),
      Unit  = dplyr::first(Unit),
      label = dplyr::first(label),
      .groups = "drop"
    )
  
  idx <- match(SA4_sp@data$SA4_code, layer_df$SA4_code)
  
  SA4_sp@data$val   <- layer_df$val  [idx]
  SA4_sp@data$Unit  <- layer_df$Unit [idx]
  SA4_sp@data$label <- layer_df$label[idx]
  
  SA4_sp
}

#=============================================



