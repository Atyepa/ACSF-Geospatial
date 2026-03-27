# ACSF Geospatial Dietary Indicators

An interactive R Shiny application for exploring Australian dietary data across geographic regions, built using the 
[Geospatial dietary indicators data cube (xlsx)](https://www.abs.gov.au/articles/geospatial-dietary-indicators/4316DO002_202324_ESTIMATES.xlsx) from Geospatial dietary indicators - Apparent consumption-based dietary indicators for sub-national areas including SEIFA, Remoteness, SA3 and SA4.

**ABS article:** [www.abs.gov.au/articles/geospatial-dietary-indicators](www.abs.gov.au/articles/geospatial-dietary-indicators)

**Live app:** [atyepa.shinyapps.io/ACSF_Geospatial_2023_24](https://atyepa.shinyapps.io/ACSF_Geospatial_2023_24/)

---

## Overview

This tool allows users to compare dietary intakes across:

- **States and territories**
- **SEIFA quintiles** (Index of Relative Socio-economic Advantage and Disadvantage)
- **Remoteness Areas** (Major Cities through Very Remote)
- **SA3 and SA4 statistical areas**

Users can explore nutrient and food group data across multiple classification systems and visualise results as interactive charts or choropleth maps.

---

## Features

### Data

Dietary data are sourced from the [ABS Geospatial Dietary Indicators](https://www.abs.gov.au/articles/geospatial-dietary-indicators) data product and cover four classification systems:

| Table | Description |
|-------|-------------|
| AUSNUT food groups | Major, Sub-major, and Summary levels |
| Nutrients | Key macro- and micronutrients |
| Macronutrient energy | Energy contributions by macronutrient |
| ADG food groups | Australian Dietary Guidelines food groups |

### Geographic levels

| Level | Description |
|-------|-------------|
| State/territory | All states and territories (except Tas. - sorry!) |
| SEIFA quintiles | Socioeconomic disadvantage deciles (IRSD) |
| Remoteness Area | RA0–RA4 classifications |
| SA3 | ~330 Statistical Area Level 3 regions |
| SA4 | ~88 Statistical Area Level 4 regions |

### Visualisation modes

- **Bar / column charts** with optional stacking
- **Comparison charts**: percentage difference, absolute difference, range, and dumbbell plots
- **Interactive choropleth maps** (Leaflet) for SA3 and SA4 regions
- **Data tables** with Excel export

---

## Repository structure

```
├── app.R                        # Shiny entry point
├── App UI.R                     # UI layout and controls
├── App Server.R                 # Server-side reactive logic
├── App data processing.R        # Data download, cleaning, and preparation
├── custom_styles.R              # Custom CSS
├── geog.xlsx                    # Geographic reference lookup (SA3/SA4/state/RA)
├── sa3_med.rds                  # Simplified SA3 spatial boundaries (WGS84)
├── sa4_med.rds                  # Simplified SA4 spatial boundaries (WGS84)
└── rsconnect/                   # ShinyApps.io deployment config
```

---

## Requirements

Install the required R packages before running:

```r
install.packages(c(
  "shiny", "shinydashboard", "bslib",
  "tidyverse", "openxlsx",
  "highcharter",
  "leaflet", "sf",
  "DT", "shinyWidgets"
))
```

---

## Running locally

```r
shiny::runApp()
```

The data processing script (`App data processing.R`) downloads the ABS datacube on first run and applies geographic lookups and label mappings. This may take a minute on the initial load.

---

## Data sources

- **Dietary data:** [ABS Geospatial Dietary Indicators](https://www.abs.gov.au/articles/geospatial-dietary-indicators)
- **Geographic boundaries:** ABS ASGS (simplified for web performance)
- **Food/nutrient labels:** Derived from external label mapping hosted on GitHub

---

## Notes

- All dietary estimates are amounts per 10,000 kJ.
- SA3/SA4 spatial polygons are pre-simplified to reduce file size and improve map rendering performance.

**Data suppresssion** was applied to a number of areas to uphold confidentialisation. These include:
- 76 out of 330 SA3s 
- 7 out of 88 SA4s
- 1 out of 8 States

