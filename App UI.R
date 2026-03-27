# =========================
#           UI
# =========================
ui <- fluidPage(
  theme = shinytheme("darkly"),
  custom_styles(),
  headerPanel("ACSF, Geospatial Dietary Indicators, 2023-24"),
  tags$style(HTML("
  table.dataTable {
    table-layout: fixed !important;
    width: 100% !important;
  }
")),
  
  sidebarPanel(
    pickerInput(
      "choosetable", "Select table:",
      choices = c(
        "AUSNUT foodgroups" = "AUSNUT",
        "Nutrients" = "Nutrients",
        "Macronutrient kJ" = "Macro",
        "ADG food groups" = "ADG"
      ),
      selected = "AUSNUT", 
      multiple = FALSE,
      width = "300px"),
    
    # AUSNUT options
    conditionalPanel(
      condition = "input.choosetable == 'AUSNUT'",
      radioButtons(
        "Class1","Classification level:",
        choices = c("Major","Sub-major", "Summary"),
        selected = "Major", inline = TRUE
      ),
      
      conditionalPanel(
        condition = "input.Class1 == 'Major'",
        pickerInput(
          "Majgrp1","AUSNUT major food groups:", choices = c(Two_dig),
          selected = "11, Non-alcoholic beverages", multiple = TRUE,
          options = list(`actions-box` = TRUE), width = "300px"
        )
      ),
      conditionalPanel(
        condition = "input.Class1 == 'Sub-major'",
        pickerInput(
          "Mingrp1","AUSNUT sub-major food groups:", choices = c(Thr_dig),
          selected = "115, Soft drinks", multiple = TRUE,
          options = list(`actions-box` = TRUE), width = "300px"
        )
      ),
      conditionalPanel(
        condition = "input.Class1 == 'Summary'",
        pickerInput(
          "Summary","Summary groupings:", choices = c(
            "Discretionary foods",
            "Non-discretionary foods",
            "Total sugar sweetened beverages",
            "Total other non-alcoholic beverages",
            "Total non-alcoholic beverages",
            "Total foods (excl. beverages)",
            "Total foods and beverages"
          ),
          selected = "Discretionary foods", multiple = TRUE,
          options = list(`actions-box` = TRUE), width = "300px"
        )
      )),
    conditionalPanel(
      condition = "input.choosetable == 'AUSNUT' && input.chooseSA != 'SA3' && input.chooseSA != 'SA4'",
      pickerInput(
        "A_Nutrient","Grams or Energy (kJ):",
        choices = c("Grams" = "g",
                    "% grams" = "pcg",
                    "Energy" = "kJ",
                    "% energy" = "pckJ"),
        selected = "g", multiple = FALSE, width = "300px" )
    ),
    
    conditionalPanel(
      condition = "
    input.choosetable === 'AUSNUT' &&
    (
      (input.Class1 === 'Major'     && Array.isArray(input.Majgrp1) && input.Majgrp1.length > 5) ||
      (input.Class1 === 'Sub-major' && Array.isArray(input.Mingrp1) && input.Mingrp1.length > 5) ||
      (input.Class1 === 'Summary'   && Array.isArray(input.Summary)  && input.Summary.length  > 5)
    )
  ",
      tags$div(
        style = "margin-top: 10px; margin-bottom: 2px;",
        sliderInput(
          inputId = "ausnut_cutoff",
          label   = "Min. val (to exclude low-consumption foods):",
          min     = 0,
          max     = 50,      # updated server-side via updateSliderInput()
          value   = 4,
          step    = 1,
          width   = "300px"
        )
      )
    ),
    
    # ADG options
    conditionalPanel(
      condition = "input.choosetable == 'ADG'",
      pickerInput(
        "ADG","ADG group", choices = c(ADG),
        selected = "Vegetables and legumes/beans", multiple = TRUE, width = "300px"
      ),
      radioButtons(
        "discstat","Discretionary status:",
        choices = c("Non-discretionary","Total"), selected = "Non-discretionary",
        inline = TRUE
      ),
      radioButtons(
        "adg_unit","ADG unit:",
        choices = c("Serves","Grams"), selected = "Serves",
        inline = TRUE
      )
    ),
    
    # Nutrient options
    conditionalPanel(
      condition = "input.choosetable == 'Nutrients'",
      pickerInput(
        "Macronut","Select from macronutrients:",
        choices = c(
          "Energy","Protein","Carbohydrate","Total sugars","Added sugars","Free sugars",
          "Starch","Total Fat","Polyunsaturated fat","Monounsaturated fat","Saturated fat",
          "Linoleic acid","Alpha-Linolenic acid","Total long chain omega 3 fatty acids","Trans fatty acids",
          "Dietary Fibre"
        ),
        selected = "Protein", multiple = TRUE, options = list(`actions-box` = TRUE), width = "300px"
      ),
      pickerInput(
        "Micronut","or micronutrients:",
        choices = c(
          "Preformed Vitamin A","Pro Vitamin A","Vitamin A retinol equivalent","Riboflavin (B2)","Thiamin (B1)",
          "Niacin (B3)","Niacin equivalent","Vitamin B6","Vitamin B12","Folate, natural","Folic acid","Total Folates",
          "Folate equivalent","Vitamin C","Vitamin E","Calcium","Iodine","Iron","Magnesium","Phosphorus",
          "Potassium","Selenium","Sodium","Zinc","Cholesterol","Caffeine","Moisture"
        ),
        selected = "none", multiple = TRUE, options = list(`actions-box` = TRUE), width = "300px"
      )
    ),
    
    # Macro options
    conditionalPanel(
      condition = "input.choosetable == 'Macro'",
      radioButtons(
        "kj_unit","Dietary energy unit:",
        choices = c("kJ","Percent of kJ"), selected = "Percent of kJ", inline = TRUE
      ),
      pickerInput(
        "MacrokJ","Macronutrients:", choices = c(Macro),
        selected = c("Protein","Total Fat","Carbohydrate","Dietary Fibre"),
        multiple = TRUE, options = list(`actions-box` = TRUE), width = "300px"
      )
    ),
    
    # Spread / difference from Australia (all tables, non-SA mode only)
    conditionalPanel(
      condition = "input.chooseSA != 'SA3' && input.chooseSA != 'SA4'",
      radioButtons("spread", "Plot difference from Australia overall:",
                   choices = c("Off", "Percent" = "pc", "Absolute" = "g",
                               "Range comparison" = "range", "Dumbbell" = "dumbbell"),
                   selected = "Off", inline = TRUE)
    ),

    # Dumbbell endpoints — rendered server-side based on table + geography selection
    uiOutput("dumbbell_endpoints_ui"),

    #--- Multi-select: aggregate geographies---
    checkboxGroupInput(
      inputId = "choosegeog",
      label   = "Aggregate geography:",
      choices = c("State", "SEIFA", "RA"),
      selected = "SEIFA",
      inline  = TRUE
    ),
    
    #
    # ---- SEIFA block ----
    conditionalPanel(
      condition = "input.choosegeog && (input.choosegeog === 'SEIFA' || (Array.isArray(input.choosegeog) && input.choosegeog.includes('SEIFA')))",
      checkboxGroupInput(
        "seifa","SEIFA quintile:",
        choices  = c("Q1", "Q2", "Q3", "Q4", "Q5", "Total Q"),
        selected = c("Q1", "Q2", "Q3", "Q4", "Q5"),
        inline = TRUE
      )
    ),
    
    # ---- RA block ----
    conditionalPanel(
      condition = "input.choosegeog && (input.choosegeog === 'RA' || (Array.isArray(input.choosegeog) && input.choosegeog.includes('RA')))",
      checkboxGroupInput(
        "ra","Remoteness Area:",
        choices  = c("Maj. Cities", "Inner Reg.", "Outer Reg.", "Remote", "Very Remote", "Total RA"),
        selected = c("Maj. Cities", "Inner Reg.", "Outer Reg.", "Remote", "Very Remote"),
        inline = TRUE
      )
    ), 
    
    # ---- State block ----
    conditionalPanel(
      condition = "input.choosegeog && (input.choosegeog === 'State' || (Array.isArray(input.choosegeog) && input.choosegeog.includes('State')))",
      checkboxGroupInput(
        "state","State:",
        choices = c("NSW","Vic","Qld","SA","WA","Tas","NT","ACT","Aust."),
        selected = c("NSW","Vic","Qld","SA","WA","Tas","NT","ACT"),
        inline = TRUE
      )
    ),
    
    
    #--- Single-select: SA levels ---
    radioButtons(
      inputId = "chooseSA",
      label   = "Statistical Areas (exclusive):",
      choices = c("SA3", "SA4"),
      selected = character(0),
      inline  = TRUE
    ),
    
    #--- SA3 block ---
    conditionalPanel(
      condition = "input.chooseSA == 'SA3'",
      pickerInput(
        "SA3", "SA3:", choices = c(SA3list), selected = c(SA3list),
        multiple = TRUE, options = list(`actions-box` = TRUE), width = "300px"
      ),
      sliderInput("SA3range","SA3 range:", min = 1, max = 332, value = c(1,332), step = 10),
      prettySwitch(
        inputId = "subSA3",
        label = "Subset SA3s by state, part of state or remoteness:",
        value = FALSE,
        status = "primary",
        fill = TRUE,
        bigger = FALSE,
        inline = TRUE
      ),
      conditionalPanel(
        condition = "input.subSA3 == true",
        shinyWidgets::pickerInput(
          inputId = "SA3_state",
          label   = "Subset SA3s by state:",
          choices = c("NSW","Vic","Qld","SA","WA","Tas","NT","ACT"),
          selected = c("NSW","Vic","Qld","SA","WA","Tas","NT","ACT"),
          multiple = TRUE,
          options  = list(`actions-box` = TRUE), width = "300px"
        ),
        checkboxGroupInput(
          "SA3_pos","By capital city or state bal:",
          choices = c("cc","bal"),
          selected = c("cc","bal"), inline = TRUE
        )
      )
    ),
    
    #--- SA4 block ---
    conditionalPanel(
      condition = "input.chooseSA == 'SA4'",
      pickerInput(
        "SA4","SA4:", choices = c(SA4list), selected = c(SA4list),
        multiple = TRUE, options = list(`actions-box` = TRUE), width = "300px"
      ),
      sliderInput("SA4range","SA4 range:", min = 1, max = 89, value = c(1,89), step = 5),
      prettySwitch(
        inputId = "subSA4",
        label = "Subset SA4s by state, part of state or remoteness:",
        value = FALSE,
        status = "primary",
        fill = TRUE,
        bigger = FALSE,
        inline = TRUE
      ),
      conditionalPanel(
        condition = "input.subSA4 == true",
        shinyWidgets::pickerInput(
          inputId = "SA4_state",
          label   = "Subset SA4s by state:",
          choices = c("NSW","Vic","Qld","SA","WA","Tas","NT","ACT"),
          selected = c("NSW","Vic","Qld","SA","WA","Tas","NT","ACT"),
          multiple = TRUE,
          options  = list(`actions-box` = TRUE), width = "300px"
        ),
        checkboxGroupInput(
          "SA4_pos","By capital city or state bal:",
          choices = c("cc","bal"),
          selected = c("cc","bal"), inline = TRUE
        )
      )
    ),
    
    # Plot options here:
    tags$div(
      style = "font-weight: bold;
     font-size: 100%;
    color: #80B1D3;
    text-transform: uppercase;
    margin-top: 14px;
    margin-bottom: 6px;",
      "Plot options:"), 
    
    # One colbar control outside (applies globally)
    conditionalPanel(
      condition = "input.chooseSA != 'SA3' && input.chooseSA != 'SA4'",
      radioButtons(
        "colbar","Plot to column or bar",
        choices = c("bar","column"),
        selected = "column",
        inline = TRUE ) ),
    
    checkboxInput("showDataLabels", "Show Data Labels", value = FALSE),
    
    conditionalPanel(
      condition = "input.chooseSA != 'SA3' && input.chooseSA != 'SA4'",
      
      tags$div(
        # The visible button the user clicks
        tags$button(
          id = "swap_group",
          type = "button",
          class = "btn btn-primary",   # use Bootstrap styling
          "Swap x-axis with series group"
        ),
        
        
        # Client-side logic to set initial value when Shiny is ready, then toggle on click
        tags$script(HTML("
  (function() {
    var current = 'food_group';
    var btn = document.getElementById('swap_group');

    function sendVal() {
      if (window.Shiny && Shiny.setInputValue) {
        Shiny.setInputValue('swap_group', current, { priority: 'event' });
      }
    }

    function updateAppearance() {
      if (!btn) return;
      if (current === 'food_group') {
        btn.classList.add('active');
        btn.setAttribute('aria-pressed', 'true');
      } else {
        btn.classList.remove('active');
        btn.setAttribute('aria-pressed', 'false');
      }
    }

    // Ensure initial value is sent after Shiny connects
    document.addEventListener('shiny:connected', function() {
      sendVal();
      updateAppearance();
    });

    // Fallback: if already connected (e.g., dynamic re-render), send immediately
    if (document.readyState !== 'loading' && window.Shiny && Shiny.shinyapp) {
      sendVal();
      updateAppearance();
    }

    // Toggle on click and notify Shiny
    if (btn) {
      btn.addEventListener('click', function() {
        current = (current === 'food_group') ? 'geog_group' : 'food_group';
        sendVal();
        updateAppearance();
      });
    }
  })();
"))
        
        
      )
    ), # closes conditional
    
    
    conditionalPanel(
      # Treat undefined swap_group as 'food_group' so it shows on first render
      condition = "(input.chooseSA != 'SA3' && input.chooseSA != 'SA4') && ((input.swap_group || 'food_group') === 'food_group')",
      radioButtons(
        "ordercat", "x-axis ordered by:",
        choices  = c("Value" = "value", "Classification order" = "classorder"),
        selected = "value",
        inline   = TRUE
      ) ),
    
    
    conditionalPanel(
      condition = "
    input.choosetable == 'AUSNUT' &&
    ((input.swap_group || 'food_group') === 'food_group') &&
    (
      (Array.isArray(input.Majgrp1) ? input.Majgrp1.length : 0) > 1 ||
      (Array.isArray(input.Mingrp1) ? input.Mingrp1.length : 0) > 1 ||
      (Array.isArray(input.Summary) ? input.Summary.length : 0) > 1
    )
  ",
      radioButtons(
        'Ausnstack', 'Stacking:',
        choices  = c('Stacked', 'Normal'),
        selected = 'Normal',
        inline   = TRUE
      )
    ),
    
    conditionalPanel(
      condition = "input.choosetable == 'Macro'",
      radioButtons(
        'Macrostack', 'Stacking:',
        choices  = c('Stacked', 'Normal'),
        selected = 'Stacked',
        inline   = TRUE
      )
    )
    
  ),
  
  mainPanel(
    # Scrollable region that contains the tab headers and their content
    div(
      class = "mp-scroll",
      
      tabsetPanel(
        type = "tabs",
        
        tabPanel("Graph", 
                 uiOutput("pwarning"),
                 highchartOutput("hcontainer", height = "680px"),
                 conditionalPanel(
                   condition = "input.spread == 'range'",
                   div(
                     style = "
    width: 65%;
    margin-left: auto;
    margin-right: auto;
    margin-top: 10px;
  ",
                     DT::DTOutput("range_table"))
                 )),
        tabPanel(
          "Table",
          verbatimTextOutput("message"),
          DT::dataTableOutput("table")
        ),
        
        tabPanel(
          "Map", uiOutput("mwarning"),
          uiOutput("map_group_ui"),
          actionButton("update_map", "Update map", icon = icon("sync"), class = "btn-primary"),
          br(), br(),
          conditionalPanel(
            condition = "input.chooseSA == 'SA3'",
            leafletOutput("sa3_map", height = "650px")
          ),
          conditionalPanel(
            condition = "input.chooseSA == 'SA4'",
            leafletOutput("sa4_map", height = "650px")
          )
        )
      ) 
    ),  
    
    # Footer applies to ALL tabs (bottom of mainPanel)
    div(
      class = "graph-footer",
      tags$p(
        "Source: ",
        tags$a(
          href = "https://www.abs.gov.au/articles/geospatial-dietary-indicators",
          target = "_blank",
          "ACSF Geospatial Dietary Indicators, 2023-24"
        )
      ),
      tags$p(paste0("Data retrieved from ABS, ", format(lubridate::now(), "%d-%m-%Y"))),
      downloadButton("downloadTb", "Download graph/table selection:")
    )
  )
)


