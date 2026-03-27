# =========================
#         SERVER
# =========================
server <- function(input, output, session) {
  
  # --- Enforce mutual exclusivity between choosegeog (multi) and chooseSA (single) ---
  
  # If user selects SA3/SA4, clear the aggregates
  observeEvent(input$chooseSA, ignoreInit = TRUE, {
    sa <- input$chooseSA
    if (length(sa) == 1 && sa %in% c("SA3", "SA4")) {
      updateCheckboxGroupInput(session, "choosegeog", selected = character(0))
    }
  })
  
  # If user selects any aggregates, clear SA3/SA4
  observeEvent(input$choosegeog, ignoreInit = TRUE, {
    agg <- input$choosegeog
    if (length(agg) > 0) {
      # Clear radio selection. Some Shiny versions need NULL/character(0)
      updateRadioButtons(session, "chooseSA", selected = character(0))
    }
  })
  
  # --- Unified geography reactive (replacement for input$choosegeog) ---
  # Returns:
  #   - c("State", "SEIFA", ...)  when aggregates are selected
  #   - "SA3" or "SA4"            when an SA is selected
  #   - character(0)              when nothing is selected (startup; guard with req() where needed)
  chosen_geog <- reactive({
    agg <- input$choosegeog
    sa  <- input$chooseSA
    
    if (length(agg) > 0) {
      agg
    } else if (length(sa) == 1 && sa %in% c("SA3","SA4")) {
      sa
    } else {
      character(0)
    }
  })
  
  # ---- Scalar SA mode: returns "SA3", "SA4", or NA ----
  geog_mode <- reactive({
    g <- chosen_geog()
    
    if (any(g %in% c("SA3", "SA4"))) {
      g[g %in% c("SA3", "SA4")][1]   # always length-1
    } else {
      NA_character_
    }
  })
  
  # Returns TRUE if SA3 or SA4 is selected
  geog_is_exclusive <- function() {
    any(chosen_geog() %in% c("SA3", "SA4"))
  }
  
  # A pretty label for aggregate selections (e.g., "State + SEIFA")
  geog_label <- function() {
    g <- setdiff(chosen_geog(), c("SA3","SA4"))
    if (length(g) > 0) paste(g, collapse = " + ") else "Geography"
  }
  
  
  # ---- Reactives ----
  SA3s    <- reactive({ list(FoodGROUP = input$SA3) })
  SA4s    <- reactive({ list(FoodGROUP = input$SA4) })
  Table   <- reactive({ list(Table = input$SA3) })
  TableSA4<- reactive({ list(TableSA4 = input$SA4) })
  start_SA3 <- reactive({ list(start_SA3 = input$SA3range[1]) })
  end_SA3   <- reactive({ list(end_SA3   = input$SA3range[2]) })
  start_SA4 <- reactive({ list(start_SA4 = input$SA4range[1]) })
  end_SA4   <- reactive({ list(end_SA4   = input$SA4range[2]) })
  Maj1    <- reactive({ list(Majgrp1 = input$Majgrp1) })
  Min1    <- reactive({ list(Mingrp1 = input$Mingrp1) })
  Summ    <- reactive({ list(Summary = input$Summary) })
  Class1  <- reactive({ list(Class1 = input$Class1) })
  NutA    <- reactive({ list(A_Nutrient = input$A_Nutrient) })
  Nutrients <- reactive({ list(Nutrients = input$Nutrients) })
  geog      <- reactive({ list(geog = input$choosegeog) })
  Macronut  <- reactive({ list(Macronut = input$Macronut) })
  Micronut  <- reactive({ list(Micronut = input$Micronut) })
  MacrokJ   <- reactive({ list(MacrokJ = input$MacrokJ) })
  kj_unit   <- reactive({ list(kj_unit = input$kj_unit) })
  colbar    <- reactive({ list(colbar = input$colbar) })
  
  chg_unit <- reactive({
    if (input$spread == "pc")    return("%")
    if (input$spread == "range") return("%")
    if (input$spread == "g") {
      switch(input$choosetable,
             "AUSNUT"    = input$A_Nutrient,
             "Nutrients" = "",           # units vary by nutrient
             "Macro"     = input$kj_unit,
             "ADG"       = input$adg_unit,
             "grams"
      )
    }
  })

  # Update spread choices when table changes:
  # Dumbbell is only meaningful for AUSNUT; reset to "Off" if switching away with dumbbell selected.
  observeEvent(input$choosetable, ignoreInit = TRUE, {
    if (input$choosetable == "AUSNUT") {
      updateRadioButtons(session, "spread",
                         choices  = c("Off", "Percent" = "pc", "Absolute" = "g",
                                      "Range comparison" = "range", "Dumbbell" = "dumbbell"),
                         selected = input$spread, inline = TRUE)
    } else {
      new_sel <- if (input$spread == "dumbbell") "Off" else input$spread
      updateRadioButtons(session, "spread",
                         choices  = c("Off", "Percent" = "pc", "Absolute" = "g",
                                      "Range comparison" = "range"),
                         selected = new_sel, inline = TRUE)
    }
  })

  # Render dumbbell endpoint selectors appropriate to the active geography
  output$dumbbell_endpoints_ui <- renderUI({
    req(input$spread == "dumbbell", input$choosetable == "AUSNUT")
    geog_types <- input$choosegeog

    active_geog_type <- if ("SEIFA" %in% geog_types) "SEIFA"
                        else if ("RA"    %in% geog_types) "RA"
                        else if ("State" %in% geog_types) "State"
                        else return(NULL)

    ep_choices <- switch(active_geog_type,
      SEIFA = c("Q1", "Q2", "Q3", "Q4", "Q5"),
      RA    = c("Maj. Cities", "Inner Reg.", "Outer Reg.", "Remote", "Very Remote"),
      State = c("NSW", "Vic", "Qld", "SA", "WA", "Tas", "NT", "ACT")
    )
    def_low  <- switch(active_geog_type, SEIFA = "Q1", RA = "Maj. Cities", State = "NSW")
    def_high <- switch(active_geog_type, SEIFA = "Q5", RA = "Very Remote",  State = "Vic")
    ep_label <- switch(active_geog_type,
      SEIFA = "Select the two SEIFA quintiles to compare:",
      RA    = "Select the two remoteness areas to compare:",
      State = "Select the two states to compare:"
    )

    tagList(
      tags$div(
        style = "margin-top: 6px; margin-bottom: 4px; font-size: 90%; color: #aaaaaa; font-style: italic;",
        ep_label
      ),
      fluidRow(
        column(6, selectInput("dumbbell_low",  label = "Low endpoint:",
                              choices = ep_choices, selected = def_low,  width = "130px")),
        column(6, selectInput("dumbbell_high", label = "High endpoint:",
                              choices = ep_choices, selected = def_high, width = "130px"))
      )
    )
  })

  swap_group_raw <- reactive({ list(swap_group = input$swap_group) })  # keep old
  swap_group     <- reactive(input$swap_group)                         # new scalar
  
  ordercat <- reactive({ list(ordercat = input$ordercat) })
  
  # Filtering values (for select all)
  # When the user changes the unit picker, update the slider max and reset
  # the value to 0 so a stale threshold from a previous unit doesn't linger.
  observeEvent(input$A_Nutrient, {
    new_max <- switch(
      input$A_Nutrient,
      "g"    = 500,
      "kJ"   = 2000,
      "pcg"  = 100,
      "pckJ" = 100,
      500          # fallback
    )
    updateSliderInput(
      session,
      inputId = "ausnut_cutoff",
      max     = new_max,
      value   = 0         # reset on unit change
    )
  }, ignoreInit = TRUE)
  
  
  # ---- Filters  ----
  Ausnut_tab_filtered <- reactive({
    Ausnut_tab %>%
      drop_na() %>%
      filter(Geog_type %in% geog()$geog) %>%
      filter(Geog_cat %in% input$state |
               Geog_cat %in% input$seifa |
               Geog_cat %in% input$ra) %>%
      filter(Class_level %in% Class1()$Class1) %>%
      filter(Unit %in% NutA()$A_Nutrient) %>%
      filter(cLabel %in% input$Majgrp1 | cLabel %in% input$Mingrp1 | cLabel %in% input$Summary) %>%
      # ── cutoff filter ──────
      left_join(aust_baseline(), by = c("Class_level", "AUSNUT_descr", "Unit")) %>%
      filter(is.na(aust_val) | aust_val >= input$ausnut_cutoff) %>%
      # ── final select ──────
      select(Geog_type, Geog_cat, Class_level, AUSNUT_code, AUSNUT_descr, val, Unit)
  })                                                                                             
  
  # Make a percent change and absolute change reactive dataframe
  Ausnut_valspread <- reactive({
    
    req(input$spread != "Off")
    
    df <- Ausnut_tab %>%
      drop_na() %>%
      filter(
        Geog_type %in% chosen_geog() |
          Geog_cat  == "Aust."
      ) %>%
      filter(
        Geog_cat %in% input$state |
          Geog_cat %in% input$seifa |
          Geog_cat %in% input$ra |
          Geog_cat == "Aust."
      ) %>%
      filter(Class_level %in% Class1()$Class1) %>%
      filter(Unit %in% NutA()$A_Nutrient) %>%
      filter(
        cLabel %in% input$Majgrp1 |
          cLabel %in% input$Mingrp1 |
          cLabel %in% input$Summary
      ) %>%
      select(Geog_type, Geog_cat, Class_level,
             AUSNUT_code, AUSNUT_descr, val, Unit)
    
    # --- Extract Australian baseline ---
    aust_vals <- df %>%
      filter(Geog_cat == "Aust.") %>%
      filter(!Geog_cat %in% c("Total Q", "Total RA")) %>%
      select(Class_level, AUSNUT_descr, Unit, aust_val = val)
    
    df_joined <- df %>%
      filter(Geog_cat != "Aust.") %>%
      left_join(
        aust_vals,
        by = c("Class_level","AUSNUT_descr","Unit")
      )
    
    # =====================================================
    # RANGE COMPARISON MODE
    # =====================================================
    if (input$spread == "range") {
      
      summary_vals <- df_joined %>%
        summarise(
          highest = max(val, na.rm = TRUE),
          lowest  = min(val, na.rm = TRUE),
          aust    = first(aust_val)
        )
      
      tibble::tibble(
        Geog_type   = "Range",
        Geog_cat = c(
          "Highest vs Lowest",
          "Highest vs Australia",
          "Lowest vs Australia"
        ),
        Class_level = NA,
        AUSNUT_code = NA,
        AUSNUT_descr = unique(df_joined$AUSNUT_descr)[1],
        change_value = round(c(
          (summary_vals$highest / summary_vals$lowest - 1) * 100,
          (summary_vals$highest / summary_vals$aust   - 1) * 100,
          (summary_vals$lowest  / summary_vals$aust   - 1) * 100
        ), 1),
        Unit = "%"
      )
      
    } else {
      
      # =====================================================
      # EXISTING pc / g MODES
      # =====================================================
      df_joined %>%
        mutate(
          change_value = if (input$spread == "g") round(val - aust_val, 1)
                         else round((val - aust_val) / aust_val * 100, 1)
        ) %>%
        select(
          Geog_type, Geog_cat, Class_level,
          AUSNUT_code, AUSNUT_descr,
          change_value, Unit
        )
    }
  })
  
  Ausnut_rangesummary <- reactive({
    
    req(input$spread == "range")
    
    df <- Ausnut_tab %>%
      drop_na() %>%
      filter(
        Geog_type %in% chosen_geog() |
          Geog_cat  == "Aust."
      ) %>%
      filter(
        Geog_cat %in% input$state |
          Geog_cat %in% input$seifa |
          Geog_cat %in% input$ra |
          Geog_cat == "Aust."
      ) %>%
      filter(Class_level %in% Class1()$Class1) %>%
      filter(Unit %in% NutA()$A_Nutrient) %>%
      filter(
        cLabel %in% input$Majgrp1 |
          cLabel %in% input$Mingrp1 |
          cLabel %in% input$Summary
      ) %>%
      select(Geog_cat, AUSNUT_descr, val) %>%
      mutate(Geog_cat = as.character(Geog_cat))
    
    if (!any(df$Geog_cat != "Aust.")) {
      return(
        tibble::tibble(
          Food = character(0),
          Label = character(0),
          `Geog. group` = character(0),
          Difference = character(0),
          Val = numeric(0),
          `% difference` = numeric(0)
        )
      )
    }
    
    # ---- Identify extrema ----
    high_row <- df %>%
      filter(Geog_cat != "Aust.") %>%
      arrange(desc(val)) %>%
      slice(1)
    
    low_row <- df %>%
      filter(Geog_cat != "Aust.") %>%
      arrange(val) %>%
      slice(1)
    
    avg_val <- df %>%
      filter(Geog_cat == "Aust.") %>%
      pull(val) %>%
      dplyr::first()
    
    spread_abs <- round(high_row$val - low_row$val, 1)
    spread_pc  <- round((high_row$val / low_row$val - 1) * 100, 1)
    
    # =====================================================
    # CRITICAL FIX:
    # Column order defined EXACTLY as rendered in DT
    # =====================================================
    tibble::tibble(
      Food = unique(df$AUSNUT_descr)[1],
      
      Label = c(
        "High",
        "Low",
        "Avg",
        "High–Low spread"
      ),
      
      `Geog. group` = c(
        high_row$Geog_cat,
        low_row$Geog_cat,
        "Aust.",
        "—"
      ),
      
      # ---- TEXT COLUMN FIRST ----
      Difference = c(
        "From avg",
        "From avg",
        "—",
        "High–low"
      ),
      
      # ---- NUMERIC COLUMN SECOND ----
      Val = c(
        high_row$val,
        low_row$val,
        avg_val,
        spread_abs
      ),
      
      `% difference` = c(
        round((high_row$val/avg_val - 1)*100, 2),
        round((low_row$val/avg_val - 1)*100, 2),
        NA,
        round(spread_pc, 1)
      )
    )
    
  })
  
  # ---- Helper: build range-summary table (used by all tables) ----
  build_range_tbl <- function(df, food_col) {
    # df requires columns: Geog_cat (char), <food_col> (char), val (numeric)
    if (!any(as.character(df$Geog_cat) != "Aust.")) {
      return(tibble::tibble(Food = character(), Label = character(),
                            `Geog. group` = character(), Difference = character(),
                            Val = numeric(), `% difference` = numeric()))
    }
    food_name <- unique(df[[food_col]])[1]
    high_row  <- df %>% filter(Geog_cat != "Aust.") %>% dplyr::arrange(desc(val)) %>% dplyr::slice(1)
    low_row   <- df %>% filter(Geog_cat != "Aust.") %>% dplyr::arrange(val)       %>% dplyr::slice(1)
    avg_val   <- df %>% filter(Geog_cat == "Aust.") %>% dplyr::pull(val)
    spread_abs <- round(high_row$val - low_row$val, 1)
    spread_pc  <- round((high_row$val / low_row$val - 1) * 100, 1)
    tibble::tibble(
      Food           = food_name,
      Label          = c("High", "Low", "Avg", "High\u2013Low spread"),
      `Geog. group`  = c(high_row$Geog_cat, low_row$Geog_cat, "Aust.", "\u2014"),
      Difference     = c("From avg", "From avg", "\u2014", "High\u2013low"),
      Val            = c(high_row$val, low_row$val, avg_val, spread_abs),
      `% difference` = c(
        round((high_row$val / avg_val - 1) * 100, 2),
        round((low_row$val  / avg_val - 1) * 100, 2),
        NA,
        round(spread_pc, 1)
      )
    )
  }

  # ---- Nutrient valspread ----
  Nutrient_valspread <- reactive({
    req(input$spread != "Off", input$choosetable == "Nutrients")
    geog_types <- chosen_geog()
    req(length(geog_types) > 0, !any(geog_types %in% c("SA3", "SA4")))

    wanted <- setdiff(unique(c(Macronut()$Macronut, Micronut()$Micronut)), "none")

    df <- Nutrient_tab %>%
      drop_na() %>%
      filter(Geog_type %in% geog_types | Geog_cat == "Aust.") %>%
      filter(Geog_cat %in% input$state | Geog_cat %in% input$seifa |
               Geog_cat %in% input$ra  | Geog_cat == "Aust.") %>%
      filter(Nutrient %in% wanted) %>%
      select(Geog_type, Geog_cat, Nutrient, val, Unit) %>%
      mutate(Geog_cat = as.character(Geog_cat))

    aust_vals <- df %>%
      filter(Geog_cat == "Aust.") %>%
      select(Nutrient, Unit, aust_val = val)

    df_joined <- df %>%
      filter(Geog_cat != "Aust.") %>%
      left_join(aust_vals, by = c("Nutrient", "Unit"))

    if (input$spread == "range") {
      summary_vals <- df_joined %>%
        summarise(highest = max(val, na.rm = TRUE), lowest = min(val, na.rm = TRUE),
                  aust = dplyr::first(aust_val))
      tibble::tibble(
        Geog_type    = "Range",
        Geog_cat     = c("Highest vs Lowest", "Highest vs Australia", "Lowest vs Australia"),
        Nutrient     = unique(df_joined$Nutrient)[1],
        change_value = round(c(
          (summary_vals$highest / summary_vals$lowest - 1) * 100,
          (summary_vals$highest / summary_vals$aust   - 1) * 100,
          (summary_vals$lowest  / summary_vals$aust   - 1) * 100
        ), 1),
        Unit = "%"
      )
    } else {
      df_joined %>%
        mutate(change_value = if (input$spread == "g") round(val - aust_val, 1)
                              else round((val - aust_val) / aust_val * 100, 1)) %>%
        select(Geog_type, Geog_cat, Nutrient, change_value, Unit)
    }
  })

  Nutrient_rangesummary <- reactive({
    req(input$spread == "range", input$choosetable == "Nutrients")
    geog_types <- chosen_geog()
    req(length(geog_types) > 0, !any(geog_types %in% c("SA3", "SA4")))

    wanted <- setdiff(unique(c(Macronut()$Macronut, Micronut()$Micronut)), "none")

    df <- Nutrient_tab %>%
      drop_na() %>%
      filter(Geog_type %in% geog_types | Geog_cat == "Aust.") %>%
      filter(Geog_cat %in% input$state | Geog_cat %in% input$seifa |
               Geog_cat %in% input$ra  | Geog_cat == "Aust.") %>%
      filter(Nutrient %in% wanted) %>%
      select(Geog_cat, Nutrient, val) %>%
      mutate(Geog_cat = as.character(Geog_cat))

    build_range_tbl(df, food_col = "Nutrient")
  })

  # ---- Macro valspread ----
  Macro_valspread <- reactive({
    req(input$spread != "Off", input$choosetable == "Macro")
    geog_types <- chosen_geog()
    req(length(geog_types) > 0, !any(geog_types %in% c("SA3", "SA4")))

    df <- Macro_tab %>%
      drop_na() %>%
      filter(Geog_type %in% geog_types | Geog_cat == "Aust.") %>%
      filter(Geog_cat %in% input$state | Geog_cat %in% input$seifa |
               Geog_cat %in% input$ra  | Geog_cat == "Aust.") %>%
      filter(Macronutrient %in% MacrokJ()$MacrokJ) %>%
      filter(Unit == kj_unit()$kj_unit) %>%
      select(Geog_type, Geog_cat, Macronutrient, val, Unit) %>%
      mutate(Geog_cat = as.character(Geog_cat))

    aust_vals <- df %>%
      filter(Geog_cat == "Aust.") %>%
      select(Macronutrient, Unit, aust_val = val)

    df_joined <- df %>%
      filter(Geog_cat != "Aust.") %>%
      left_join(aust_vals, by = c("Macronutrient", "Unit"))

    if (input$spread == "range") {
      summary_vals <- df_joined %>%
        summarise(highest = max(val, na.rm = TRUE), lowest = min(val, na.rm = TRUE),
                  aust = dplyr::first(aust_val))
      tibble::tibble(
        Geog_type     = "Range",
        Geog_cat      = c("Highest vs Lowest", "Highest vs Australia", "Lowest vs Australia"),
        Macronutrient = unique(df_joined$Macronutrient)[1],
        change_value  = round(c(
          (summary_vals$highest / summary_vals$lowest - 1) * 100,
          (summary_vals$highest / summary_vals$aust   - 1) * 100,
          (summary_vals$lowest  / summary_vals$aust   - 1) * 100
        ), 1),
        Unit = "%"
      )
    } else {
      df_joined %>%
        mutate(change_value = if (input$spread == "g") round(val - aust_val, 1)
                              else round((val - aust_val) / aust_val * 100, 1)) %>%
        select(Geog_type, Geog_cat, Macronutrient, change_value, Unit)
    }
  })

  Macro_rangesummary <- reactive({
    req(input$spread == "range", input$choosetable == "Macro")
    geog_types <- chosen_geog()
    req(length(geog_types) > 0, !any(geog_types %in% c("SA3", "SA4")))

    df <- Macro_tab %>%
      drop_na() %>%
      filter(Geog_type %in% geog_types | Geog_cat == "Aust.") %>%
      filter(Geog_cat %in% input$state | Geog_cat %in% input$seifa |
               Geog_cat %in% input$ra  | Geog_cat == "Aust.") %>%
      filter(Macronutrient %in% MacrokJ()$MacrokJ) %>%
      filter(Unit == kj_unit()$kj_unit) %>%
      select(Geog_cat, Macronutrient, val) %>%
      mutate(Geog_cat = as.character(Geog_cat))

    build_range_tbl(df, food_col = "Macronutrient")
  })

  # ---- ADG valspread ----
  ADG_valspread <- reactive({
    req(input$spread != "Off", input$choosetable == "ADG")
    geog_types <- chosen_geog()
    req(length(geog_types) > 0, !any(geog_types %in% c("SA3", "SA4")))

    df <- ADG_tab %>%
      drop_na() %>%
      filter(Geog_type %in% geog_types | Geog_cat == "Aust.") %>%
      filter(Geog_cat %in% input$state | Geog_cat %in% input$seifa |
               Geog_cat %in% input$ra  | Geog_cat == "Aust.") %>%
      filter(Disc_status == input$discstat) %>%
      filter(ADG_group %in% input$ADG) %>%
      filter(Unit == input$adg_unit) %>%
      select(Geog_type, Geog_cat, ADG_group, val, Unit) %>%
      mutate(Geog_cat = as.character(Geog_cat))

    aust_vals <- df %>%
      filter(Geog_cat == "Aust.") %>%
      select(ADG_group, Unit, aust_val = val)

    df_joined <- df %>%
      filter(Geog_cat != "Aust.") %>%
      left_join(aust_vals, by = c("ADG_group", "Unit"))

    if (input$spread == "range") {
      summary_vals <- df_joined %>%
        summarise(highest = max(val, na.rm = TRUE), lowest = min(val, na.rm = TRUE),
                  aust = dplyr::first(aust_val))
      tibble::tibble(
        Geog_type    = "Range",
        Geog_cat     = c("Highest vs Lowest", "Highest vs Australia", "Lowest vs Australia"),
        ADG_group    = unique(df_joined$ADG_group)[1],
        change_value = round(c(
          (summary_vals$highest / summary_vals$lowest - 1) * 100,
          (summary_vals$highest / summary_vals$aust   - 1) * 100,
          (summary_vals$lowest  / summary_vals$aust   - 1) * 100
        ), 1),
        Unit = "%"
      )
    } else {
      df_joined %>%
        mutate(change_value = if (input$spread == "g") round(val - aust_val, 1)
                              else round((val - aust_val) / aust_val * 100, 1)) %>%
        select(Geog_type, Geog_cat, ADG_group, change_value, Unit)
    }
  })

  ADG_rangesummary <- reactive({
    req(input$spread == "range", input$choosetable == "ADG")
    geog_types <- chosen_geog()
    req(length(geog_types) > 0, !any(geog_types %in% c("SA3", "SA4")))

    df <- ADG_tab %>%
      drop_na() %>%
      filter(Geog_type %in% geog_types | Geog_cat == "Aust.") %>%
      filter(Geog_cat %in% input$state | Geog_cat %in% input$seifa |
               Geog_cat %in% input$ra  | Geog_cat == "Aust.") %>%
      filter(Disc_status == input$discstat) %>%
      filter(ADG_group %in% input$ADG) %>%
      filter(Unit == input$adg_unit) %>%
      select(Geog_cat, ADG_group, val) %>%
      mutate(Geog_cat = as.character(Geog_cat))

    build_range_tbl(df, food_col = "ADG_group")
  })

  # Build lookup of the Australia-overall val per food group + unit.
  # Used by the filtered reactives below.
  aust_baseline <- reactive({
    Ausnut_tab %>%
      filter(Geog_cat == "Aust.") %>%
      filter(Unit %in% NutA()$A_Nutrient) %>%
      filter(Class_level %in% Class1()$Class1) %>%
      select(Class_level, AUSNUT_descr, Unit, aust_val = val) %>%
      distinct()
  })
  
  
  dumbbell_data <- reactive({

    req(
      input$spread      == "dumbbell",
      input$choosetable == "AUSNUT",
      length(input$choosegeog) > 0
    )

    geog_types <- input$choosegeog

    # Determine which geography type drives the dumbbell
    active_geog_type <- if ("SEIFA" %in% geog_types) "SEIFA"
                        else if ("RA"    %in% geog_types) "RA"
                        else if ("State" %in% geog_types) "State"
                        else return(NULL)

    req(!is.null(input$dumbbell_low), !is.null(input$dumbbell_high))

    default_low  <- switch(active_geog_type, SEIFA = "Q1", RA = "Maj. Cities", State = "NSW")
    default_high <- switch(active_geog_type, SEIFA = "Q5", RA = "Very Remote",  State = "Vic")
    low_ep  <- input$dumbbell_low  %||% default_low
    high_ep <- input$dumbbell_high %||% default_high

    # Pull the two endpoints + the national total from the full (unfiltered) table
    df <- Ausnut_tab %>%
      drop_na() %>%
      filter(
        (Geog_type == active_geog_type & Geog_cat %in% c(low_ep, high_ep)) |
          Geog_cat == "Aust."
      ) %>%
      filter(Class_level %in% Class1()$Class1) %>%
      filter(Unit        %in% NutA()$A_Nutrient) %>%
      filter(
        cLabel %in% input$Majgrp1 |
          cLabel %in% input$Mingrp1 |
          cLabel %in% input$Summary
      ) %>%
      select(Geog_cat, Class_level, AUSNUT_descr, val, Unit)

    # Apply cutoff: keep only foods where Aust. val meets the threshold
    keep_foods <- df %>%
      filter(Geog_cat == "Aust.") %>%
      filter(val >= input$ausnut_cutoff) %>%
      pull(AUSNUT_descr) %>%
      unique()

    df <- df %>% filter(AUSNUT_descr %in% keep_foods)

    # Extract national baseline
    aust_vals <- df %>%
      filter(Geog_cat == "Aust.") %>%
      select(Class_level, AUSNUT_descr, Unit, aust_val = val)

    # Compute % deviation for the two endpoint categories
    db <- df %>%
      filter(Geog_cat != "Aust.") %>%
      left_join(aust_vals, by = c("Class_level", "AUSNUT_descr", "Unit")) %>%
      mutate(
        pct_dev = round((val - aust_val) / aust_val * 100, 1)
      )

    # Pivot to one row per food group, columns: low_dev, high_dev
    db_wide <- db %>%
      select(AUSNUT_descr, Geog_cat, pct_dev) %>%
      tidyr::pivot_wider(
        names_from  = Geog_cat,
        values_from = pct_dev
      )

    # Rename dynamically so downstream code always uses low_dev / high_dev
    db_wide <- db_wide %>%
      rename(
        low_dev  = !!rlang::sym(low_ep),
        high_dev = !!rlang::sym(high_ep)
      )

    # Order: descending by high-endpoint deviation
    db_wide <- db_wide %>%
      arrange(desc(high_dev)) %>%
      mutate(
        AUSNUT_descr = factor(
          AUSNUT_descr,
          levels = rev(AUSNUT_descr)   # rev() because Highcharter dumbbell reads bottom-up
        )
      )

    list(
      data      = db_wide,
      low_q     = low_ep,
      high_q    = high_ep,
      geog_type = active_geog_type,
      unit      = "% vs Australia"
    )
  })
  
  
  AusnutSA4_filtered <- reactive({
    AusnutSA4_tab %>%
      drop_na() %>%
      filter(SA4 %in% input$SA4) %>%
      filter(state %in% input$SA4_state) %>%
      filter(PoS %in% input$SA4_pos) %>%
      filter(Unit == NutA()$A_Nutrient) %>%
      filter(Class_level == Class1()$Class1) %>%
      filter(cLabel %in% input$Majgrp1 | cLabel %in% input$Mingrp1 | cLabel %in% input$Summary) %>%
      group_by(cLabel) %>%
      arrange(desc(val)) %>%
      mutate(a = 1, c = cumsum(a)) %>%
      ungroup() %>%
      filter(c >= start_SA4()$start_SA4, c <= end_SA4()$end_SA4) %>%
      select(STATE_code, GCCSA_code, SA4_code, SA4_name, AUSNUT_code, AUSNUT_descr, val, Unit)
  })                                                                                              
  
  U  <- reactive({
    Ausnut_tab %>%
      filter(Unit %in% NutA()$A_Nutrient) %>%
      group_by(Unit) %>%
      summarise(unit = max(Unit))
  })                                                                                              
  
  Nutrient_tab_filtered <- reactive({
    Nutrient_tab %>%
      drop_na() %>%
      filter(Geog_type %in% geog()$geog) %>%
      filter(Geog_cat %in% input$state |
               Geog_cat %in% input$seifa |
               Geog_cat %in% input$ra) %>%
      filter(Nutrient %in% Macronut()$Macronut | Nutrient %in% Micronut()$Micronut) %>% 
      select(Geog_type, Geog_cat, Nutrient, val, Unit)
  })                                                                                              
  
  NutrientSA4_filtered <- reactive({
    Nutrient_tabSA4 %>%
      drop_na() %>%
      filter(SA4 %in% input$SA4) %>%
      filter(state %in% input$SA4_state) %>%
      filter(PoS %in% input$SA4_pos) %>%
      filter(Nutrient %in% Macronut()$Macronut | Nutrient %in% Micronut()$Micronut) %>%
      arrange(desc(val)) %>%
      mutate(a = 1, c = cumsum(a)) %>%
      ungroup() %>%
      filter(c >= start_SA4()$start_SA4, c <= end_SA4()$end_SA4) %>%
      select(STATE_code, GCCSA_code, SA4_code, SA4_name, Nutrient, val, Unit)
  })                                                                                               
  
  Un <- reactive({
    Nutrient_tab %>%
      filter(Nutrient %in% Macronut()$Macronut | Nutrient %in% Micronut()$Micronut) %>%
      group_by(Unit) %>%
      summarise(unit = max(Unit))
  })                                                                                               
  
  Macro_tab_filtered <- reactive({
    Macro_tab %>%
      drop_na() %>%
      mutate(Macronutrient = factor(Macronutrient,
                                    levels = c("Alcohol","Dietary Fibre","Protein","Saturated fat","Total Fat",
                                               "Starch","Free sugars","Added sugars","Total sugars","Carbohydrate","Energy"))) %>%
      filter(Geog_type == geog()$geog) %>%
      filter(Geog_cat %in% input$state |
               Geog_cat %in% input$seifa |
               Geog_cat %in% input$ra) %>%
      filter(Macronutrient %in% MacrokJ()$MacrokJ) %>%
      filter(Unit == kj_unit()$kj_unit) %>% 
      select(Geog_type, Geog_cat, Macronutrient, val, Unit)
  })                                                                                              
  
  MacroSA4_filtered <- reactive({
    ranks <- Macro_tabSA4 %>%
      drop_na() %>%
      filter(SA4 %in% input$SA4,
             state %in% input$SA4_state,
             PoS %in% input$SA4_pos,
             Macronutrient %in% MacrokJ()$MacrokJ,
             Unit == kj_unit()$kj_unit) %>%
      group_by(SA4) %>% summarise(m = max(val), .groups = "drop") %>%
      arrange(desc(m), SA4) %>%
      mutate(c = dense_rank(order(-m, SA4)))
    Macro_tabSA4 %>%
      drop_na() %>%
      mutate(Macronutrient = factor(Macronutrient,
                                    levels = c("Alcohol","Dietary Fibre","Protein","Saturated fat","Total Fat",
                                               "Starch","Free sugars","Added sugars","Total sugars","Carbohydrate","Energy"))) %>%
      filter(SA4 %in% input$SA4,
             state %in% input$SA4_state,
             PoS %in% input$SA4_pos,
             Macronutrient %in% MacrokJ()$MacrokJ,
             Unit == kj_unit()$kj_unit) %>%
      left_join(ranks, by = "SA4") %>%
      filter(c >= start_SA4()$start_SA4, c <= end_SA4()$end_SA4) %>%
      arrange(c, SA4)
  })                                                                                              
  
  Um <- reactive({
    Macro_tab %>%
      filter(Unit == input$kj_unit) %>%
      group_by(Unit) %>%
      summarise(unit = max(Unit))
  })                                                                                              
  
  ADG_tab_filtered <- reactive({
    ADG_tab %>%
      filter(Unit == input$adg_unit) %>%
      filter(Geog_type %in% geog()$geog) %>%
      filter(Geog_cat %in% input$state |
               Geog_cat %in% input$seifa |
               Geog_cat %in% input$ra) %>%
      filter(Disc_status == input$discstat) %>%
      filter(ADG_group %in% input$ADG) %>% 
      select(Geog_type, Geog_cat, ADG_group, Disc_status, val, Unit)
  })                                                                                              
  
  ADGSA4_filtered <- reactive({
    ADG_tabSA4 %>%
      drop_na() %>%
      filter(Unit == input$adg_unit) %>%
      filter(SA4 %in% input$SA4) %>%
      filter(state %in% input$SA4_state) %>%
      filter(PoS %in% input$SA4_pos) %>%
      filter(Disc_status == input$discstat) %>%
      filter(ADG_group == input$ADG) %>%
      arrange(desc(val)) %>%
      mutate(a = 1, c = cumsum(a)) %>%
      filter(c >= start_SA4()$start_SA4, c <= end_SA4()$end_SA4) %>%
      select(STATE_code, GCCSA_code, GCCSA_name, SA4_code, SA4_name,
             Disc_status, ADG_group, val, Unit)
  })                                                                                        
  
  # ---- SA3-specific filters ----
  AusnutSA3_filtered <- reactive({
    df <- AusnutSA3_tab %>%
      drop_na() %>%
      filter(SA3 %in% input$SA3) %>%
      filter(state %in% input$SA3_state) %>%
      filter(PoS %in% input$SA3_pos) %>%
      filter(Unit == NutA()$A_Nutrient) %>%
      filter(Class_level == Class1()$Class1)
    
    if (Class1()$Class1 == "Major") {
      df <- df %>% filter(cLabel %in% Maj1()$Majgrp1)
    } 
    else if (Class1()$Class1 == "Sub-major") {
      df <- df %>% filter(cLabel %in% Min1()$Mingrp1)
    }
    else {
      df <- df %>% filter(cLabel %in% Summ()$Summary)
    }
    
    
    df %>%
      group_by(cLabel) %>%
      arrange(desc(val)) %>%
      mutate(a = 1, c = cumsum(a)) %>%
      ungroup() %>%
      filter(c >= start_SA3()$start_SA3, c <= end_SA3()$end_SA3) %>%
      select(STATE_code, GCCSA_code, SA4_code, SA4_name, SA3_code, SA3_name,
             AUSNUT_code, AUSNUT_descr, val, Unit)
  })                                                                                               
  
  
  NutrientSA3_filtered <- reactive({
    wanted <- unique(c(Macronut()$Macronut, Micronut()$Micronut))
    wanted <- setdiff(wanted, "none")
    Nutrient_tabSA3 %>%
      drop_na() %>%
      filter(SA3 %in% input$SA3) %>%
      filter(state %in% input$SA3_state) %>%
      filter(PoS %in% input$SA3_pos) %>%
      filter(Nutrient %in% wanted) %>%
      arrange(desc(val)) %>%
      mutate(a = 1, c = cumsum(a)) %>%
      filter(c >= start_SA3()$start_SA3, c <= end_SA3()$end_SA3) %>%
      select(STATE_code, GCCSA_code, SA4_code, SA4_name,
             SA3_code, SA3_name, Nutrient, val, Unit)

  })                                                                                               
  
  Un_SA3 <- reactive({
    Nutrient_tabSA3 %>%
      filter(Nutrient %in% Macronut()$Macronut | Nutrient %in% Micronut()$Micronut) %>%
      group_by(Unit) %>% summarise(unit = max(Unit))
  })                                                                                               
  
  MacroSA3_filtered <- reactive({
    ranks <- Macro_tabSA3 %>%
      drop_na() %>%
      filter(SA3 %in% input$SA3,
             state %in% input$SA3_state,
             PoS %in% input$SA3_pos,
             Macronutrient %in% MacrokJ()$MacrokJ,
             Unit == kj_unit()$kj_unit) %>%
      group_by(SA3) %>%
      summarise(m = max(val), .groups = "drop") %>%
      arrange(desc(m), SA3) %>%
      mutate(c = dense_rank(order(-m, SA3)))

    Macro_tabSA3 %>%
      drop_na() %>%
      mutate(Macronutrient = factor(Macronutrient,
                                    levels = c("Alcohol","Dietary Fibre","Protein","Saturated fat","Total Fat",
                                               "Starch","Free sugars","Added sugars","Total sugars","Carbohydrate","Energy"))) %>%
      filter(SA3 %in% input$SA3,
             state %in% input$SA3_state,
             PoS %in% input$SA3_pos,
             Macronutrient %in% MacrokJ()$MacrokJ,
             Unit == kj_unit()$kj_unit) %>%
      left_join(ranks, by = "SA3") %>%
      filter(c >= start_SA3()$start_SA3, c <= end_SA3()$end_SA3) %>%
      arrange(c, SA3) %>%
      select(STATE_code, GCCSA_code, GCCSA_name, SA4_code, SA4_name, SA3_code, SA3_name,
             Macronutrient, val, Unit)
  })                                                                                               
  
  Um_SA3 <- reactive({
    Macro_tabSA3 %>%
      filter(Unit == input$kj_unit) %>%
      group_by(Unit) %>% summarise(unit = max(Unit))
  })                                                                                              
  
  ADGSA3_filtered <- reactive({
    ADG_tabSA3 %>%
      drop_na() %>%
      filter(Unit == input$adg_unit) %>%
      filter(SA3 %in% input$SA3) %>%
      filter(state %in% input$SA3_state) %>%
      filter(PoS %in% input$SA3_pos) %>%
      filter(Disc_status == input$discstat) %>%
      filter(ADG_group == input$ADG) %>%
      arrange(desc(val)) %>%
      mutate(a = 1, c = cumsum(a)) %>%
      filter(c >= start_SA3()$start_SA3, c <= end_SA3()$end_SA3) %>%
      select(STATE_code, GCCSA_code, GCCSA_name, SA4_code, SA4_name, SA3_code, SA3_name,
             Disc_status, ADG_group, val, Unit)

  })                                                                                              
  
  
  # ---- Unified tab() for charts ----
  
  tab <- reactive({
    req(input$choosetable)
    
    # 1) Pick the label column per table
    label_col <- switch(
      input$choosetable,
      "AUSNUT"    = "AUSNUT_descr",
      "Nutrients" = "Nutrient",
      "Macro"     = "Macronutrient",
      "ADG"       = "ADG_group",
      stop("Unknown table: ", input$choosetable)
    )
    
    if (length(chosen_geog()) == 0) {
      return(
        tibble::tibble(
          val = numeric(0),
          !!label_col := character(0),
          Geog_cat = character(0)
        )
      )
    }
    
    # ---- Exclusive selection (SA3/SA4) based on unified chosen_geog() ----
    excl <- intersect(chosen_geog(), c("SA3","SA4"))
    excl_choice <- if (length(excl) >= 1) excl[[length(excl)]] else NULL  # last one if ever both appear
    is_excl <- !is.null(excl_choice)
    
    # 3) Choose the correct data source
    df <- if (identical(excl_choice, "SA3")) {
      switch(input$choosetable,
             "AUSNUT"    = AusnutSA3_filtered(),
             "Nutrients" = NutrientSA3_filtered(),
             "Macro"     = MacroSA3_filtered(),
             "ADG"       = ADGSA3_filtered()
      )
    } else if (identical(excl_choice, "SA4")) {
      switch(input$choosetable,
             "AUSNUT"    = AusnutSA4_filtered(),
             "Nutrients" = NutrientSA4_filtered(),
             "Macro"     = MacroSA4_filtered(),
             "ADG"       = ADGSA4_filtered()
      )
    } else {
      switch(input$choosetable,
             "AUSNUT"    = Ausnut_tab_filtered(),
             "Nutrients" = Nutrient_tab_filtered(),
             "Macro"     = Macro_tab_filtered(),
             "ADG"       = ADG_tab_filtered()
      )
    }
    
    if (is.null(df)) return(NULL)
    
    # 4) Preserve distinct() for Macro @ SA3
    if (identical(input$choosetable, "Macro") && identical(excl_choice, "SA3")) {
      df <- dplyr::distinct(df)
    }
    
    # 5) Determine which column names to use as x and group based on swap_group()
    #    Robust: pick the first existing geography column from a candidate list
    candidate_geog_cols <- if (is_excl) {
      if (identical(excl_choice, "SA3")) {
        c("SA3_name", "SA3")
      } else { # SA4
        c("SA4_name", "SA4")
      }
    } else {
      # Aggregate-mode; normally "Geog_cat", but fall back to any of these if needed
      c("Geog_cat", "State", "SEIFA", "RA")
    }
    geog_col <- intersect(candidate_geog_cols, names(df))[1]
    validate(need(!is.null(geog_col) && !is.na(geog_col),
                  paste0("No suitable geography column found in df. Candidates were: ",
                         paste(candidate_geog_cols, collapse = ", "),
                         " | Available: ",
                         paste(names(df), collapse = ", "))))
    
    # Scalar-safe flag for swap_group()
    is_geog_group <- isTRUE(swap_group() == "geog_group")
    
    x_col_name   <- if (is_geog_group) label_col else geog_col
    grp_col_name <- if (is_geog_group) geog_col  else label_col
    
    
    x_sym   <- rlang::sym(x_col_name)
    grp_sym <- rlang::sym(grp_col_name)
    
    
    # 6) Order levels for x-axis
    if (identical(input$ordercat, "value")) {
      # Order categories by descending sum of val
      order_levels <- df %>%
        dplyr::group_by(!!x_sym) %>%
        dplyr::summarise(.ord = sum(.data$val, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(.ord)) %>%
        dplyr::pull(!!x_sym) %>%
        unique()
    } else {
      
      if ("classorder" %in% names(df)) {
        # Explicit numeric ordering always wins
        order_levels <- df %>%
          dplyr::distinct(!!x_sym, classorder) %>%
          dplyr::arrange(classorder) %>%
          dplyr::pull(!!x_sym)
        
      } else {
        # Stable first-appearance order (NOT alphabetical)
        order_levels <- df %>%
          dplyr::distinct(!!x_sym) %>%
          dplyr::pull(!!x_sym)
      }
    }
    
    x_axis_categories <- order_levels
    
    df <- df %>%
      dplyr::mutate(
        !!x_sym := factor(.data[[x_col_name]], levels = order_levels)
      ) %>%
      dplyr::group_by(!!grp_sym) %>%
      {
        # Row ordering here doesn't affect x-axis order (factor levels do).
        # Keep only if your downstream rendering benefits from desc(val) rows.
        dplyr::arrange(., dplyr::desc(.data$val))
      } %>%
      dplyr::ungroup()
    
    df
  })
  
   
  # ---- Legend title helper (keeps map/plot consistent) ----
  legend_title <- reactive({
    g <- chosen_geog()
    
    # Reduce geography to ONE scalar
    mode <- if (any(g %in% c("SA3","SA4"))) {
      g[g %in% c("SA3","SA4")][1]
    } else if (length(g) > 0) {
      paste(g, collapse = " + ")
    } else {
      "Geography"
    }
    
    if (identical(input$choosetable, "AUSNUT")) {
      paste0("AUSNUT – ", mode)
    } else if (identical(input$choosetable, "Nutrients")) {
      paste0("Nutrients – ", mode)
    } else if (identical(input$choosetable, "Macro")) {
      paste0("Macronutrients – ", mode)
    } else if (identical(input$choosetable, "ADG")) {
      paste0("ADG – ", mode)
    } else {
      mode
    }
  })
  
  #
  
  # 1) Define this once (e.g., at the top of server.R or inside server() before renderers)
  geog_order_tbl <- dplyr::bind_rows(
    tibble::tibble(
      Geog_type = "RA",
      Geog_cat  = c("Maj. Cities", "Inner Reg.", "Outer Reg.", "Remote", "Very Remote", "Total RA"),
      ord       = 1:6
    ),
    tibble::tibble(
      Geog_type = "SEIFA",
      Geog_cat  = c("Q1", "Q2", "Q3", "Q4", "Q5", "Total Q"),
      ord       = 1:6
    ),
    tibble::tibble(
      Geog_type = "State",
      Geog_cat  = c("NSW", "Vic", "Qld", "SA", "WA", "Tas", "NT", "ACT", "Aust."),
      ord       = 1:9
    )
  )
  
  # A small helper: build ordered categories for Geog_cat given the data in hand
  make_geog_categories <- function(dat) {
    # dat must have: Geog_cat, Geog_type
    # Join to lookup; unknown categories get NA 'ord'
    tmp <- dat %>%
      dplyr::distinct(Geog_type, Geog_cat) %>%
      dplyr::left_join(geog_order_tbl, by = c("Geog_type", "Geog_cat")) %>%
      dplyr::mutate(
        # preserve first-appearance order for any categories not in the lookup
        .first_seen = dplyr::row_number()
      )
    
    # Order by 'ord' if known, otherwise by first appearance after all known ones
    tmp <- tmp %>%
      dplyr::mutate(
        .ord_final = ifelse(!is.na(.data$ord), .data$ord, max(dplyr::coalesce(ord, 0)) + .data$.first_seen)
      ) %>%
      dplyr::arrange(.data$Geog_type, .data$.ord_final)
    
    tmp$Geog_cat
  }
  
  
  # Plot warning
  output$pwarning <- renderUI({
    
    req(tab())
    
    pwarnings <- tagList()
    
    food_col_name <- switch(input$choosetable,
      "AUSNUT"    = "AUSNUT_descr",
      "Nutrients" = "Nutrient",
      "Macro"     = "Macronutrient",
      "ADG"       = "ADG_group",
      NULL
    )
    if (input$spread %in% c("pc", "g", "range") &&      # "dumbbell" intentionally absent
        !is.null(food_col_name)                  &&
        food_col_name %in% names(tab())          &&
        dplyr::n_distinct(tab()[[food_col_name]]) > 1) {
      
      pwarnings <- tagAppendChild(
        pwarnings,
        tags$div(
          class = "alert alert-warning",
          "Ensure you have only one food selected for this comparison."
        )
      )
    }
    
    pwarnings
  })
  
  
  output$mwarning <- renderUI({
    choose <- input$chooseSA
    nSA3    <- input$SA3
    nSA4    <- input$SA4
    
    mwarnings <- tagList()
    n_sel <- function(x) if (is.null(x)) 0L else length(x)
    if (is.null(choose) || !choose %in% c("SA3", "SA4")) {
      mwarnings <- tagAppendChild(
        mwarnings,
        tags$div(
          class = "alert alert-warning",
          "Map is available only for SA3 or SA4. ",
          "Switch 'Geography' to SA3 or SA4 to view the map."
        )
      )
    }
    return(mwarnings)
  } )
    
  #--------------------  
  # ---- Plotting  ----
  #--------------------
  # --Helpers---
  # Collapse labels, preserving factor order when present.
  collapse_in_order <- function(x, max_n = 6) {
    if (inherits(x, "factor")) {
      lev <- levels(x)
      present <- lev[lev %in% as.character(x)]
      ux <- present
    } else {
      ux <- unique(as.character(stats::na.omit(x)))
    }
    if (length(ux) == 0) return("(none)")
    if (length(ux) <= max_n) return(paste(ux, collapse = " + "))
    paste0(paste(ux[seq_len(max_n)], collapse = " + "),
           " + ", length(ux) - max_n, " more")
  }
  
  
  # Build <left> and <right> parts of the title so the "focus" variable
  # (e.g., AUSNUT_descr, Nutrient, Macronutrient, ADG_group) appears
  # appropriately regardless of whether it is on x or in the series.
  # - favor_left_threshold: 2 means 1–2 focus cats → focus on left; >2 → Geog on left
  build_title_labels <- function(
    df_plot, x_var, group_var,
    geog_label, x_label, group_label,
    focus_var,               
    favor_left_threshold = 2,
    max_n = 6
  ) {
    # Identify where focus_var lives and extract the vector
    focus_vec <- NULL
    if (identical(x_var, focus_var) && focus_var %in% names(df_plot)) {
      focus_vec <- df_plot[[x_var]]
    } else if (identical(group_var, focus_var) && focus_var %in% names(df_plot)) {
      focus_vec <- df_plot[[group_var]]
    }
    
    # If focus_var not on either axis/series, fallback to current labels
    if (is.null(focus_vec)) {
      return(list(left = group_label, right = x_label))
    }
    
    # Count distinct focus categories and build the collapsed label
    n_focus     <- length(unique(as.character(stats::na.omit(focus_vec))))
    focus_label <- collapse_in_order(focus_vec, max_n = max_n)
    
    if (n_focus <= favor_left_threshold) {
      # 1–2 focus categories → focus on left, Geog on right
      list(left = focus_label, right = geog_label)
    } else {
      # >2 focus categories → Geog on left, focus on right
      list(left = geog_label, right = focus_label)
    }
  }
  
  
  
  output$hcontainer <- renderHighchart({
    req(tab())
    
    validate(
      need("val" %in% names(tab()), "tab() is missing 'val'"),
      need(
        any(c("Geog_cat", "SA3_name", "SA4_name", "SA3", "SA4") %in% names(tab())),
        paste("tab() has no geography column. Columns are:", paste(names(tab()), collapse = ", "))
      )
    )
    
    df <- tab()
    
    # If no rows, show a friendly placeholder chart instead of error
    if (is.null(df) || nrow(df) == 0) {
      highchart() %>%
        #  hc_chart(backgroundColor = "transparent") %>%
        hc_title(text = "No data selected", style = list(color = "#333331",   
                                                         fontSize = "18px", fontWeight = "600"), align = "center", useHTML = TRUE) %>%
        hc_subtitle(text = "Adjust your selections to see a chart",
                    style = list(color = "#4D4D4D", fontSize = "14px", fontWeight = "400"), align = "center",useHTML = TRUE) %>%
        hc_xAxis(visible = FALSE) %>%
        hc_yAxis(visible = FALSE) %>%
        hc_add_theme(hc_theme_economist()) 
    } else {
      
      
      mode <- chosen_geog()
      
      mode <- if (any(mode %in% c("SA3","SA4"))) {
        mode[mode %in% c("SA3","SA4")][1]
      } else {
        NA_character_
      }
      
      # Define one canonical mode
      geog_mode <- chosen_geog()[1]   # "SA3", "SA4", or aggregate label(s)
      is_SA3    <- identical(geog_mode, "SA3")
      is_SA4    <- identical(geog_mode, "SA4")
      is_agg    <- !is_SA3 && !is_SA4
      
      hc <- NULL
      
      # ---- Shared helpers for all branches ----
      # Nice label for multi-select (State/SEIFA/RA)
      g <- chosen_geog()
      geog_label <- if (any(g %in% c("SA3","SA4"))) g[g %in% c("SA3","SA4")][1]
      else paste(g, collapse = " + ")
      
      # Scalar-safe flag for swap_group()
      is_geog_group <- isTRUE(swap_group() == "geog_group")
      
      # Defensively ensure chart type is scalar (in case colbar() can be vector-ish)
      chart_type <- as.character(colbar())[1]
      
      # ---- Non-exclusive geogs (State/SEIFA/RA) ----
      
      if (identical(input$choosetable, 'AUSNUT') && is_agg) {
        
        x_var       <- if (is_geog_group) "AUSNUT_descr" else "Geog_cat"
        group_var   <- if (is_geog_group) "Geog_cat" else "AUSNUT_descr"
        x_label     <- if (is_geog_group) "AUSNUT food groups" else geog_label
        group_label <- if (is_geog_group) geog_label else "AUSNUT food groups"
        
        # ---- PRE-ORDER DATA ----
        df_plot <- tab()
        
        if (input$ordercat == "value") {
          x_order <- df_plot %>%
            dplyr::group_by(.data[[x_var]]) %>%
            dplyr::summarise(.ord = sum(val, na.rm = TRUE), .groups = "drop") %>%
            dplyr::arrange(desc(.ord)) %>%
            dplyr::pull(.data[[x_var]]) %>%
            as.character()
        } else {
          x_vals  <- df_plot[[x_var]]
          x_order <- if (is.factor(x_vals)) as.character(levels(x_vals)) else unique(as.character(x_vals))
        }
        
        df_plot <- df_plot %>%
          dplyr::mutate(!!rlang::sym(x_var) := factor(.data[[x_var]], levels = x_order)) %>%
          dplyr::arrange(!!rlang::sym(x_var))
        
        # ---- Build title via helper ----
        tl <- build_title_labels(
          df_plot   = df_plot,
          x_var     = x_var,
          group_var = group_var,
          geog_label = geog_label,
          x_label    = x_label,
          group_label = group_label,
          focus_var = "AUSNUT_descr",
          favor_left_threshold = 2,
          max_n = 6
        )
        
        # stacking: "normal" when input$Ausnstack == "Stacked", else NULL
        stack_val <- if (identical(input$Ausnstack, "Stacked")) "normal" else NULL
        
        
        # ---- Plot ----
        hc <- df_plot %>%
          hchart(
            type = chart_type,
            hcaes(x = !!rlang::sym(x_var), y = val, group = !!rlang::sym(group_var))
          ) %>%
          hc_xAxis(
            type  = "category",
            title = list(text = x_label),
            labels = list(formatter = JS(
              "function(){ return (this.value || '').replace(/^\\s*\\d+\\s*,\\s*/, ''); }"
            ))
          ) %>%
          hc_yAxis(title = list(text = paste0(input$A_Nutrient))) %>%
          hc_title(text = paste0(tl$left, " by ", tl$right)) %>%
          hc_add_theme(hc_theme_economist()) %>%
          hc_colors(abscol) %>%
          hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(" ", U()$unit)) %>%
          hc_plotOptions(column = list(stacking = stack_val)) %>%  
          hc_plotOptions(bar    = list(stacking = stack_val)) %>%  
          
          apply_font_styles(input$showDataLabels)
      }
      
      # Difference plot for AUSNUT 
      if (identical(input$choosetable, 'AUSNUT') && 
          is_agg && 
          input$spread %in% c("pc", "g", "range")) {
        
        hc <- Ausnut_valspread() %>%
          arrange(desc(change_value)) %>%
          hchart(.,
                 type = "bar",
                 hcaes(
                   x = Geog_cat,
                   y = change_value
                 )) %>%
          hc_xAxis(title = list(text = paste(chosen_geog(), collapse = " + "))) %>%
          hc_yAxis(
            title = list(
              text = paste0("Difference from Australia, ", chg_unit()),
              style = list(
                color = "#333331",    
                fontSize = "14px"
              )
            ),
            plotLines = list(list(
              color = "white",
              width = 1,
              value = 0
            ))) %>%
          hc_title(
            text = paste0(
              tl$left, " by ", tl$right,
              "<br> difference from overall Australia level (",
              chg_unit(), ")"
            )
          ) %>%
          hc_add_theme(hc_theme_economist()) %>%
          hc_colors(abscol) %>%
          hc_tooltip(
            crosshairs = TRUE,
            valueSuffix = paste0(" ", chg_unit())
          ) %>%
          apply_font_styles(input$showDataLabels)
      }
      
      if (identical(input$choosetable, "AUSNUT") &&
           is_agg &&
           identical(input$spread, "dumbbell")) {
        
        db      <- dumbbell_data()
        df_db   <- db$data
        low_q   <- db$low_q
        high_q  <- db$high_q
        
        validate(need(nrow(df_db) > 0, "No data for dumbbell — check your food group and unit selections."))
        
        # Strip leading AUSNUT code ("11, Non-alcoholic beverages" → "Non-alcoholic beverages")
        clean_label <- function(x) {
          sub("^\\s*\\d+(?:\\.\\d+)*\\s*,\\s*", "", as.character(x))
        }
        
        df_db <- df_db %>%
          mutate(food_label = clean_label(AUSNUT_descr))
        
        # Category order for y-axis (factor levels set during ordering above, reversed for display)
        cats <- levels(df_db$AUSNUT_descr)
        cats_clean <- clean_label(cats)
        
        # Build the chart using two scatter series on a shared categorical y-axis,
        # connected by a line — Highcharter's native "dumbbell" type.
        #
        # Highcharter dumbbell: each row = list(low = <x1>, high = <x2>, name = <label>)
        # The chart is rendered as a horizontal dumbbell (inverted = TRUE).
        
        db_series <- df_db %>%
          arrange(AUSNUT_descr) %>%      # must match cats order
          mutate(food_label = clean_label(AUSNUT_descr)) %>%
          select(food_label, low_dev, high_dev)
        
        # Convert to the named-list format Highcharter expects for dumbbell
        point_list <- purrr::pmap(
          list(db_series$low_dev, db_series$high_dev, db_series$food_label),
          function(lo, hi, nm) list(low = lo, high = hi, name = nm)
        )
        
        hc <- highchart() %>%
          hc_chart(type = "dumbbell", inverted = TRUE) %>%
          hc_add_series(
            data        = point_list,
            name        = paste(low_q, "vs", high_q),
            lowColor    = "#E15759",      # Q1/low endpoint colour (red)
            color       = "#4E79A7",      # Q5/high endpoint colour (blue)
            connectorColor = "#999999",
            connectorWidth = 2,
            marker      = list(enabled = TRUE, radius = 6),
            dataLabels  = list(
              enabled = isTRUE(input$showDataLabels),
              format  = "{point.low}% / {point.high}%"
            )
          ) %>%
          hc_xAxis(
            categories = cats_clean,
            title      = list(text = "Food group"),
            labels     = list(style = list(fontSize = "12px"))
          ) %>%
          hc_yAxis(
            title      = list(text = "% difference from Australia overall"),
            plotLines  = list(list(
              color = "black",
              width = 1.5,
              value = 0,
              zIndex = 5,
              label  = list(text = "Australia avg", style = list(color = "#cccccc"))
            ))
          ) %>%
          hc_title(
            text = paste0(
              high_q, " vs ", low_q,
              " — % difference from Australia overall"
            )
          ) %>%
          hc_subtitle(
            text = paste0(
              "Positive = above national average  |  Negative = below  |  ",
              "● ", high_q, " (blue)   ● ", low_q, " (red)"
            )
          ) %>%
          hc_tooltip(
            useHTML  = TRUE,
            formatter = JS(paste0("
        function() {
          var lo  = this.point.low  !== undefined ? this.point.low  : 'N/A';
          var hi  = this.point.high !== undefined ? this.point.high : 'N/A';
          return '<b>' + this.point.name + '</b><br/>' +
                 '<span style=\"color:#E15759\">&#9679;</span> ", low_q,  ": <b>' + lo + '%</b><br/>' +
                 '<span style=\"color:#4E79A7\">&#9679;</span> ", high_q, ": <b>' + hi + '%</b>';
        }
      "))
          ) %>%
          hc_legend(enabled = FALSE) %>%
          hc_add_theme(hc_theme_economist()) %>%
          apply_font_styles(input$showDataLabels)
      }
      
      
      
      if (identical(input$choosetable, 'Nutrients') && is_agg) {
        
        x_var       <- if (is_geog_group) "Nutrient" else "Geog_cat"
        group_var   <- if (is_geog_group) "Geog_cat" else "Nutrient"
        x_label     <- if (is_geog_group) "Nutrients" else geog_label
        group_label <- if (is_geog_group) geog_label  else "Nutrients"
        
        # ---- PRE-ORDER DATA ----
        df_plot <- tab()
        
        if (input$ordercat == "value") {
          x_order <- df_plot %>%
            dplyr::group_by(.data[[x_var]]) %>%
            dplyr::summarise(.ord = sum(val, na.rm = TRUE), .groups = "drop") %>%
            dplyr::arrange(desc(.ord)) %>%
            dplyr::pull(.data[[x_var]]) %>%
            as.character()
        } else {
          x_vals  <- df_plot[[x_var]]
          x_order <- if (is.factor(x_vals)) as.character(levels(x_vals)) else unique(as.character(x_vals))
        }
        
        df_plot <- df_plot %>%
          dplyr::mutate(!!sym(x_var) := factor(.data[[x_var]], levels = x_order)) %>%
          dplyr::arrange(!!sym(x_var))
        
        # ---- NEW: Dynamic title via shared helper ----
        tl <- build_title_labels(
          df_plot     = df_plot,
          x_var       = x_var,
          group_var   = group_var,
          geog_label  = geog_label,
          x_label     = x_label,
          group_label = group_label,
          focus_var  = "Nutrient",
          favor_left_threshold = 2,
          max_n = 6
        )
        
        # ---- PLOT ----
        hc <- df_plot %>%
          hchart(
            type = chart_type,
            hcaes(
              x     = !!sym(x_var),
              y     = val,
              group = !!sym(group_var)
            )
          ) %>%
          hc_xAxis(
            type  = "category",
            title = list(text = x_label),
            labels = list(
              formatter = JS(
                "function(){
            return (this.value || '').replace(/^\\s*\\d+\\s*,\\s*/, '');
          }"
              )
            )
          ) %>%
          hc_yAxis(title = list(text = paste0(input$Nutrients))) %>%
          hc_title(text = paste0(tl$left, " by ", tl$right)) %>%
          hc_add_theme(hc_theme_economist()) %>%
          hc_colors(abscol) %>%
          hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(' ', U()$unit)) %>%
          hc_plotOptions(column = list(stacking = NULL)) %>%
          hc_plotOptions(bar    = list(stacking  = NULL)) %>%
          apply_font_styles(input$showDataLabels)
      }
      
      
      #  modified helper for Geog_cat ordering
      if (identical(input$choosetable, 'Macro') && is_agg) {
        x_var       <- if (is_geog_group) "Macronutrient" else "Geog_cat"
        group_var   <- if (is_geog_group) "Geog_cat"      else "Macronutrient"
        x_label     <- if (is_geog_group) "Macronutrients" else geog_label
        group_label <- if (is_geog_group) geog_label        else "Macronutrients"
        
        dat <- tab()  
        
        order_levels <- NULL
        if (identical(x_var, "Geog_cat")) {
          if (input$ordercat == "value") {
            order_levels <- dat %>%
              dplyr::group_by(Geog_cat) %>%
              dplyr::summarise(.ord = sum(val, na.rm = TRUE), .groups = "drop") %>%
              dplyr::arrange(dplyr::desc(.ord)) %>%
              dplyr::pull(Geog_cat) %>%
              as.character()
          } else {
            order_levels <- make_geog_categories(dat)
          }
          dat <- dat %>%
            dplyr::mutate(Geog_cat = factor(.data$Geog_cat, levels = order_levels))
        }
        
        # ---- NEW: Dynamic title via shared helper ----
        tl <- build_title_labels(
          df_plot     = dat,
          x_var       = x_var,
          group_var   = group_var,
          geog_label  = geog_label,
          x_label     = x_label,
          group_label = group_label,
          focus_var  = "Macronutrient",
          favor_left_threshold = 2,
          max_n = 6
        )
        
        hc <- dat %>%
          hchart(
            type = chart_type,
            hcaes(x = !!rlang::sym(x_var), y = val, group = !!rlang::sym(group_var))
          ) %>%
          hc_plotOptions(series = list(dataSorting = list(enabled = FALSE))) %>%
          {
            if (!is.null(order_levels)) {
              hc_xAxis(., categories = order_levels, title = list(text = x_label))
            } else {
              hc_xAxis(., title = list(text = x_label))
            }
          } %>%
          hc_yAxis(title = list(text = paste0(Um()$unit))) %>%
          # Dynamic title (e.g., "Fat by State/SEIFA/RA" or "Protein + 2 more by Selected SA3s")
          hc_title(text = paste0(tl$left, " by ", tl$right)) %>%
          hc_add_theme(hc_theme_economist()) %>%
          hc_colors(abscol) %>%
          hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(" ", Um()$unit)) %>%
          hc_plotOptions(bar    = list(stacking = if (identical(input$Macrostack, "Stacked")) "normal" else NULL)) %>%
          hc_plotOptions(column = list(stacking = if (identical(input$Macrostack, "Stacked")) "normal" else NULL)) %>%
          apply_font_styles(input$showDataLabels)
      }
      
      
      if (identical(input$choosetable, 'ADG') && is_agg) {
        x_var       <- if (is_geog_group) "ADG_group" else "Geog_cat"
        group_var   <- if (is_geog_group) "Geog_cat"  else "ADG_group"
        x_label     <- if (is_geog_group) "ADG groups" else geog_label
        group_label <- if (is_geog_group) geog_label    else "ADG groups"

        df_plot <- tab()

        # ---- PRE-ORDER DATA (same pattern as AUSNUT/Nutrients blocks) ----
        order_levels_adg <- NULL
        if (identical(x_var, "Geog_cat")) {
          # Geography on x-axis: respect ordercat
          if (input$ordercat == "value") {
            order_levels_adg <- df_plot %>%
              dplyr::group_by(Geog_cat) %>%
              dplyr::summarise(.ord = sum(val, na.rm = TRUE), .groups = "drop") %>%
              dplyr::arrange(dplyr::desc(.ord)) %>%
              dplyr::pull(Geog_cat) %>%
              as.character()
          } else {
            order_levels_adg <- make_geog_categories(df_plot)
          }
          df_plot <- df_plot %>%
            dplyr::mutate(Geog_cat = factor(.data$Geog_cat, levels = order_levels_adg))
        } else {
          # ADG_group on x-axis: respect ordercat
          if (input$ordercat == "value") {
            x_order_adg <- df_plot %>%
              dplyr::group_by(.data[[x_var]]) %>%
              dplyr::summarise(.ord = sum(val, na.rm = TRUE), .groups = "drop") %>%
              dplyr::arrange(desc(.ord)) %>%
              dplyr::pull(.data[[x_var]]) %>%
              as.character()
          } else {
            x_vals_adg  <- df_plot[[x_var]]
            x_order_adg <- if (is.factor(x_vals_adg)) as.character(levels(x_vals_adg)) else unique(as.character(x_vals_adg))
          }
          order_levels_adg <- x_order_adg
          df_plot <- df_plot %>%
            dplyr::mutate(!!rlang::sym(x_var) := factor(.data[[x_var]], levels = x_order_adg)) %>%
            dplyr::arrange(!!rlang::sym(x_var))
        }

        # ---- Dynamic title ----
        tl <- build_title_labels(
          df_plot     = df_plot,
          x_var       = x_var,
          group_var   = group_var,
          geog_label  = geog_label,
          x_label     = x_label,
          group_label = group_label,
          focus_var   = "ADG_group",
          favor_left_threshold = 2,
          max_n = 6
        )

        hc <- df_plot %>%
          hchart(type = chart_type, hcaes(x = !!sym(x_var), y = val, group = !!sym(group_var))) %>%
          hc_plotOptions(series = list(dataSorting = list(enabled = FALSE))) %>%
          {
            if (!is.null(order_levels_adg)) {
              hc_xAxis(., categories = order_levels_adg, title = list(text = x_label))
            } else {
              hc_xAxis(., title = list(text = x_label))
            }
          } %>%
          hc_yAxis(title = list(text = input$adg_unit)) %>%
          hc_title(text = paste0(tl$left, " by ", tl$right)) %>%
          hc_add_theme(hc_theme_economist()) %>%
          hc_colors(abscol) %>%
          hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(" ", input$adg_unit)) %>%
          apply_font_styles(input$showDataLabels)
      }

      # ---- Difference plots for non-AUSNUT tables ----

      if (identical(input$choosetable, 'Nutrients') &&
          is_agg &&
          input$spread %in% c("pc", "g", "range")) {

        vs <- Nutrient_valspread()
        hc <- vs %>%
          dplyr::arrange(desc(change_value)) %>%
          hchart(type = "bar", hcaes(x = Geog_cat, y = change_value)) %>%
          hc_xAxis(title = list(text = paste(chosen_geog(), collapse = " + "))) %>%
          hc_yAxis(
            title = list(text = paste0("Difference from Australia, ", chg_unit())),
            plotLines = list(list(color = "white", width = 1, value = 0))
          ) %>%
          hc_title(text = paste0(
            unique(vs$Nutrient)[1], " \u2014 difference from Australia (",
            chg_unit(), ")"
          )) %>%
          hc_add_theme(hc_theme_economist()) %>%
          hc_colors(abscol) %>%
          hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(" ", chg_unit())) %>%
          apply_font_styles(input$showDataLabels)
      }

      if (identical(input$choosetable, 'Macro') &&
          is_agg &&
          input$spread %in% c("pc", "g", "range")) {

        vs <- Macro_valspread()
        hc <- vs %>%
          dplyr::arrange(desc(change_value)) %>%
          hchart(type = "bar", hcaes(x = Geog_cat, y = change_value)) %>%
          hc_xAxis(title = list(text = paste(chosen_geog(), collapse = " + "))) %>%
          hc_yAxis(
            title = list(text = paste0("Difference from Australia, ", chg_unit())),
            plotLines = list(list(color = "white", width = 1, value = 0))
          ) %>%
          hc_title(text = paste0(
            unique(vs$Macronutrient)[1], " \u2014 difference from Australia (",
            chg_unit(), ")"
          )) %>%
          hc_add_theme(hc_theme_economist()) %>%
          hc_colors(abscol) %>%
          hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(" ", chg_unit())) %>%
          apply_font_styles(input$showDataLabels)
      }

      if (identical(input$choosetable, 'ADG') &&
          is_agg &&
          input$spread %in% c("pc", "g", "range")) {

        vs <- ADG_valspread()
        hc <- vs %>%
          dplyr::arrange(desc(change_value)) %>%
          hchart(type = "bar", hcaes(x = Geog_cat, y = change_value)) %>%
          hc_xAxis(title = list(text = paste(chosen_geog(), collapse = " + "))) %>%
          hc_yAxis(
            title = list(text = paste0("Difference from Australia, ", chg_unit())),
            plotLines = list(list(color = "white", width = 1, value = 0))
          ) %>%
          hc_title(text = paste0(
            unique(vs$ADG_group)[1], " \u2014 difference from Australia (",
            chg_unit(), ")"
          )) %>%
          hc_add_theme(hc_theme_economist()) %>%
          hc_colors(abscol) %>%
          hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(" ", chg_unit())) %>%
          apply_font_styles(input$showDataLabels)
      }

      #---------------- Exclusive geogs (SA3) ----------------
      
      if (identical(input$choosetable, "AUSNUT") && is_SA3) {
        # Use current plot data
        df_plot <- tab()
        
        # Build dynamic title using the same helper you defined above
        # x_var is SA3_name (x-axis), group_var is AUSNUT_descr (series)
        # geog_label is what you want to appear on the right side when many AUSNUT cats
        # (use "SA3" to get "... by SA3" as requested)
        tl <- build_title_labels(
          df_plot    = df_plot,
          x_var      = "SA3_name",
          group_var  = "AUSNUT_descr",
          geog_label = "SA3",
          x_label    = "SA3",
          group_label= "AUSNUT food groups",
          focus_var = "AUSNUT_descr",
          favor_left_threshold = 2,  # 1–2 AUSNUT cats → AUSNUT on left
          max_n = 6
        )
        
        hc <- df_plot %>%
          hchart(type = "bar", hcaes(x = SA3_name, y = val, group = AUSNUT_descr)) %>%
          hc_xAxis(title = list(text = "Selected SA3s")) %>%   # keep your existing x-axis label
          hc_yAxis(title = list(text = paste0(input$A_Nutrient, " per 10k kJ"))) %>%
          # Dynamic title: e.g., "Soft drinks by SA3" or "Fruit + Vegetables + 3 more by SA3"
          hc_title(text = paste0(tl$left, " by ", tl$right)) %>%
          hc_add_theme(hc_theme_economist()) %>%
          hc_colors(abscol) %>%
          hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(" ", U()$unit)) %>%
          hc_plotOptions(column = list(stacking = "normal")) %>%
          apply_font_styles(input$showDataLabels)
      }
      
      
      if (identical(input$choosetable, 'Nutrients') && is_SA3) {
        df_plot <- tab()
        
        tl <- build_title_labels(
          df_plot     = df_plot,
          x_var       = "SA3_name",
          group_var   = "Nutrient",
          geog_label  = "SA3",
          x_label     = "SA3",
          group_label = "Nutrients",
          focus_var  = "Nutrient",
          favor_left_threshold = 2,
          max_n = 6
        )
        
        hc <- df_plot %>%
          hchart(type = "bar", hcaes(x = SA3_name, y = val, group = Nutrient)) %>%
          hc_xAxis(title = list(text = "SA3")) %>%  # label is SA3 in exclusive mode
          hc_yAxis(title = list(text = paste0(Un()$unit))) %>%
          # Dynamic title: e.g., "Protein by SA3" or "Fibre + 2 more by SA3"
          hc_title(text = paste0(tl$left, " by ", tl$right)) %>%
          hc_add_theme(hc_theme_economist()) %>%
          hc_colors(abscol) %>%
          hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(" ", Un()$unit)) %>%
          apply_font_styles(input$showDataLabels)
      }
      
      
      if (identical(input$choosetable, 'Macro') && is_SA3) {
        df_plot <- tab()
        
        # Dynamic title via the shared helper
        # x_var: SA3_name (x-axis), group_var: Macronutrient (series)
        tl <- build_title_labels(
          df_plot     = df_plot,
          x_var       = "SA3_name",
          group_var   = "Macronutrient",
          geog_label  = "SA3",
          x_label     = "SA3",
          group_label = "Macronutrients",
          focus_var  = "Macronutrient",
          favor_left_threshold = 2,
          max_n = 6
        )
        
        hc <- df_plot %>%
          hchart(type = "bar", hcaes(x = SA3_name, y = val, group = Macronutrient)) %>%
          hc_xAxis(title = list(text = "SA3")) %>%
          hc_yAxis(title = list(text = paste0(Um_SA3()$unit))) %>%
          # Dynamic title: e.g., "Fat by SA3" or "Protein + 3 more by SA3"
          hc_title(text = paste0(tl$left, " by ", tl$right)) %>%
          hc_add_theme(hc_theme_economist()) %>%
          hc_colors(abscol) %>%
          hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(" ", Um_SA3()$unit)) %>%
          hc_plotOptions(column = list(stacking = if (identical(input$Macrostack, "Stacked")) "normal" else NULL)) %>%
          hc_plotOptions(bar    = list(stacking = if (identical(input$Macrostack, "Stacked")) "normal" else NULL)) %>%
          apply_font_styles(input$showDataLabels)
      }
      
      
      if (identical(input$choosetable, 'ADG') && is_SA3) {
        df_plot <- tab()
        
        # Dynamic title via the shared helper
        # x_var: SA3_name (x-axis), group_var: ADG_group (series)
        tl <- build_title_labels(
          df_plot     = df_plot,
          x_var       = "SA3_name",
          group_var   = "ADG_group",
          geog_label  = "SA3",
          x_label     = "SA3",
          group_label = "ADG groups",
          focus_var  = "ADG_group",
          favor_left_threshold = 2,
          max_n = 6
        )
        
        hc <- df_plot %>%
          hchart(type = "bar", hcaes(x = SA3_name, y = val, group = ADG_group)) %>%
          hc_xAxis(title = list(text = "SA3")) %>%
          hc_yAxis(title = list(text = input$adg_unit)) %>%
          # Dynamic title: e.g., "Vegetables by SA3" or "Grains + 4 more by SA3"
          hc_title(text = paste0(tl$left, " by ", tl$right)) %>%
          hc_add_theme(hc_theme_economist()) %>%
          hc_colors(abscol) %>%
          hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(" ", input$adg_unit)) %>%
          hc_plotOptions(column = list(stacking = "normal")) %>%
          hc_plotOptions(bar    = list(stacking = "normal")) %>%
          apply_font_styles(input$showDataLabels)
      }


      # ---------------- Exclusive geogs (SA4) ----------------
      
      if (identical(input$choosetable, 'AUSNUT') && is_SA4) {
        # Use the current plot data
        df_plot <- tab()
        
        # Dynamic title via the shared helper
        # x_var: SA4_name (x-axis), group_var: AUSNUT_descr (series)
        # geog_label "SA4" gives "... by SA4" as requested
        tl <- build_title_labels(
          df_plot     = df_plot,
          x_var       = "SA4_name",
          group_var   = "AUSNUT_descr",
          geog_label  = "SA4",
          x_label     = "SA4",
          group_label = "AUSNUT food groups",
          focus_var  = "AUSNUT_descr",
          favor_left_threshold = 2,
          max_n = 6
        )
        
        hc <- df_plot %>%
          hchart(type = "bar", hcaes(x = SA4_name, y = val, group = AUSNUT_descr)) %>%
          hc_xAxis(title = list(text = "Selected SA4s")) %>%  # keep your existing axis label
          hc_yAxis(title = list(text = paste0(input$A_Nutrient, " per 10k kJ"))) %>%
          # Dynamic title: e.g., "Soft drinks by SA4" or "Fruit + Vegetables + 3 more by SA4"
          hc_title(text = paste0(tl$left, " by ", tl$right)) %>%
          hc_add_theme(hc_theme_economist()) %>%
          hc_colors(abscol) %>%
          hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(" ", U()$unit)) %>%
          hc_plotOptions(column = list(stacking = "normal")) %>%
          apply_font_styles(input$showDataLabels)
      }
      
      
      if (identical(input$choosetable, 'Nutrients') && is_SA4) {
        df_plot <- tab()
        
        # Dynamic title via the shared helper
        # x_var: SA4_name (x-axis), group_var: Nutrient (series)
        tl <- build_title_labels(
          df_plot     = df_plot,
          x_var       = "SA4_name",
          group_var   = "Nutrient",
          geog_label  = "SA4",
          x_label     = "SA4",
          group_label = "Nutrients",
          focus_var  = "Nutrient",
          favor_left_threshold = 2,
          max_n = 6
        )
        
        hc <- df_plot %>%
          hchart(type = "bar", hcaes(x = SA4_name, y = val, group = Nutrient)) %>%
          hc_xAxis(title = list(text = "SA4")) %>%
          hc_yAxis(title = list(text = paste0(Un()$unit))) %>%
          # Dynamic title: e.g., "Protein by SA4" or "Fibre + 2 more by SA4"
          hc_title(text = paste0(tl$left, " by ", tl$right)) %>%
          hc_add_theme(hc_theme_economist()) %>%
          hc_colors(abscol) %>%
          hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(" ", Un()$unit)) %>%
          apply_font_styles(input$showDataLabels)
      }
      
      
      if (identical(input$choosetable, 'Macro') && is_SA4) {
        df_plot <- tab()
        
        # Dynamic title via the shared helper
        # x_var: SA4_name (x-axis), group_var: Macronutrient (series)
        tl <- build_title_labels(
          df_plot     = df_plot,
          x_var       = "SA4_name",
          group_var   = "Macronutrient",
          geog_label  = "SA4",
          x_label     = "SA4",
          group_label = "Macronutrients",
          focus_var  = "Macronutrient",
          favor_left_threshold = 2,
          max_n = 6
        )
        
        hc <- df_plot %>%
          hchart(type = "bar", hcaes(x = SA4_name, y = val, group = Macronutrient)) %>%
          hc_xAxis(title = list(text = "SA4")) %>%
          hc_yAxis(title = list(text = paste0(Um()$unit))) %>%
          # Dynamic title: e.g., "Fat by SA4" or "Protein + 3 more by SA4"
          hc_title(text = paste0(tl$left, " by ", tl$right)) %>%
          hc_add_theme(hc_theme_economist()) %>%
          hc_colors(abscol) %>%
          hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(" ", Um()$unit)) %>%
          hc_plotOptions(column = list(stacking = if (identical(input$Macrostack, "Stacked")) "normal" else NULL)) %>%
          hc_plotOptions(bar    = list(stacking = if (identical(input$Macrostack, "Stacked")) "normal" else NULL)) %>%
          apply_font_styles(input$showDataLabels)
      }
      
      
      if (identical(input$choosetable, 'ADG') && is_SA4) {
        df_plot <- tab()
        
        # Dynamic title via the shared helper
        # x_var: SA4_name (x-axis), group_var: ADG_group (series)
        tl <- build_title_labels(
          df_plot     = df_plot,
          x_var       = "SA4_name",
          group_var   = "ADG_group",
          geog_label  = "SA4",
          x_label     = "SA4",
          group_label = "ADG groups",
          focus_var  = "ADG_group",
          favor_left_threshold = 2,
          max_n = 6
        )
        
        hc <- df_plot %>%
          hchart(type = "bar", hcaes(x = SA4_name, y = val, group = ADG_group)) %>%
          hc_xAxis(title = list(text = "SA4")) %>%
          hc_yAxis(title = list(text = input$adg_unit)) %>%
          # Dynamic title: e.g., "Vegetables by SA4" or "Grains + 4 more by SA4"
          hc_title(text = paste0(tl$left, " by ", tl$right)) %>%
          hc_add_theme(hc_theme_economist()) %>%
          hc_colors(abscol) %>%
          hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(" ", input$adg_unit)) %>%
          hc_plotOptions(column = list(stacking = "normal")) %>%
          hc_plotOptions(bar    = list(stacking    = "normal")) %>%
          apply_font_styles(input$showDataLabels)
      }
      
      hc
    }
  })
  
  # ---- Table for DT display ----
  output$table <- DT::renderDataTable({
    req(tab())
    tab()
  })
  
  #--- Range table ----
  output$range_table <- DT::renderDataTable({
    
    tbl <- switch(input$choosetable,
      "AUSNUT"    = Ausnut_rangesummary(),
      "Nutrients" = Nutrient_rangesummary(),
      "Macro"     = Macro_rangesummary(),
      "ADG"       = ADG_rangesummary(),
      NULL
    )
    req(tbl)
    
    food_name <- unique(tbl$Food)[1]
    
    # ---- HEADER (must match data column order) ----
    container <- htmltools::withTags(
      table(
        class = "display",
        thead(
          tr(
            th(food_name),
            th("Geog. group"),
            th(style="text-align:center;", "Difference"),  # ← moved
            th("Val"),
            th("% difference")
          )
        )
      )
    )
    
    # ---- DATA (same order as header) ----
    tbl_display <- tbl %>%
      dplyr::select(
        -Food,
        Label,
        `Geog. group`,
        Difference,   # ← moved
        Val,
        `% difference`
      ) %>%
      as.data.frame()
    
    DT::datatable(
      tbl_display,
      container = container,
      rownames = FALSE,
      class = "compact stripe",
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        searching = FALSE,
        info = FALSE,
        autoWidth = FALSE,
        columnDefs = list(
          list(width = "30%", targets = 0),
          list(width = "17.5%", targets = 1:4)
        )
      )
    ) %>%
      DT::formatStyle(1, `text-align` = "left") %>%     # Label
      DT::formatStyle(2, `text-align` = "center") %>%   # Geog
      DT::formatStyle(3, `text-align` = "center") %>%   # Difference
      DT::formatStyle(4, `text-align` = "right") %>%    # Val
      DT::formatStyle(5, `text-align` = "right")        # %
  })  
  
  # ---- Download ----
  output$downloadTb <- downloadHandler(
    filename = function() {
      paste0("ACSF small area, 2023-24, ", input$choosegeog,
             " by ", input$choosetable, ", selected groups.xlsx")
    },
    content = function(file) { write_xlsx(tab(), path = file) }
  )                                                                                            
  
  
  # ---------------------------
  #        MAPPING 
  # ---------------------------
  
  australia_bounds <- list(
    lng1 = 100.0, lat1 = -55.0,  # SW (extra ocean buffer)
    lng2 = 170.0, lat2 = -2.0    # NE
  )
  
  output$map_group_ui <- renderUI({
    mode <- chosen_geog()
    req(mode %in% c("SA3", "SA4"))
    
    is_sa3 <- identical(mode, "SA3")
    
    df <- switch(
      input$choosetable,
      Nutrients = if (is_sa3) NutrientSA3_filtered() else NutrientSA4_filtered(),
      AUSNUT    = if (is_sa3) AusnutSA3_filtered()   else AusnutSA4_filtered(),
      Macro     = if (is_sa3) MacroSA3_filtered()    else MacroSA4_filtered(),
      NULL
    )
    req(df)
    
    if (input$choosetable == "Nutrients") {
      units <- unique(df$Unit)
      nuts  <- sort(unique(df$Nutrient))
      
      if (length(units) > 1) {
        selectInput(
          "map_group_choice",
          "Select a nutrient to map (units differ):",
          choices = nuts,
          selected = nuts[1]
        )
      } else if (length(nuts) > 1) {
        checkboxInput("map_sum_groups", "Sum selected nutrients on map", TRUE)
      }
      
    } else if (input$choosetable == "AUSNUT") {
      if (length(unique(df$Unit)) == 1 && length(unique(df$AUSNUT_descr)) > 1) {
        checkboxInput("map_sum_groups", "Sum selected food groups on map", TRUE)
      }
      
    } else if (input$choosetable == "Macro") {
      if (length(unique(df$Macronutrient)) > 1) {
        checkboxInput("map_sum_groups", "Sum selected macronutrients on map", TRUE)
      }
    }
  })
  
  
  map_slice <- reactive({
    mode <- chosen_geog()
    req(mode %in% c("SA3", "SA4"))
    
    is_sa3 <- identical(mode, "SA3")
    
    df <- switch(
      input$choosetable,
      AUSNUT    = if (is_sa3) AusnutSA3_filtered()   else AusnutSA4_filtered(),
      Nutrients = if (is_sa3) NutrientSA3_filtered() else NutrientSA4_filtered(),
      Macro     = if (is_sa3) MacroSA3_filtered()    else MacroSA4_filtered(),
      ADG       = if (is_sa3) ADGSA3_filtered()      else ADGSA4_filtered(),
      NULL
    )
    req(df)
    
    df <- df %>%
      dplyr::mutate(
        val = vapply(val, function(x) sum(as.numeric(x), na.rm = TRUE), numeric(1))
      )
    
    validate(
      need(is.numeric(df$val), paste("map_slice(): val is", class(df$val)))
    )
    
    code_col <- if (is_sa3) "SA3_code" else "SA4_code"
    name_col <- if (is_sa3) "SA3_name" else "SA4_name"
    
    # ---- table-specific aggregation ----
    out <- if (input$choosetable == "Nutrients") {
      
      units <- unique(df$Unit)
      
      if (length(units) > 1) {
        req(input$map_group_choice)
        df <- df %>% filter(Nutrient == input$map_group_choice)
        label <- paste0(input$map_group_choice, " (", units[1], ")")
        
      } else if (isTruthy(input$map_sum_groups)) {
        label <- paste0("Nutrients (", units[1], ")")
        
      } else {
        pick  <- sort(unique(df$Nutrient))[1]
        df    <- df %>% filter(Nutrient == pick)
        label <- paste0(pick, " (", units[1], ")")
      }
      
      df %>%
        group_by(.data[[code_col]], .data[[name_col]], Unit) %>%
        summarise(val = sum(val, na.rm = TRUE), .groups = "drop") %>%
        mutate(label = label)
      
    } else if (input$choosetable == "AUSNUT") {
      
      food <- sort(unique(df$AUSNUT_descr))[1]  # enforce max of ONE food group for maps 
      
      df %>%
        filter(AUSNUT_descr == food) %>%
        group_by(.data[[code_col]], .data[[name_col]], Unit) %>%
        summarise(val = sum(val, na.rm = TRUE), .groups = "drop") %>%
        mutate(label = paste0(food, " (", Unit, ")"))
      
      
    } else if (input$choosetable == "Macro") {
      
      macro <- sort(unique(df$Macronutrient))[1]
      
      df %>%
        filter(Macronutrient == macro) %>%
        group_by(.data[[code_col]], .data[[name_col]], Unit) %>%
        summarise(val = sum(val, na.rm = TRUE), .groups = "drop") %>%
        mutate(label = paste0(macro, " (", Unit, ")"),
               Unit = "%")
      
      
    } else if (input$choosetable == "ADG") {
      
      df %>%
        group_by(.data[[code_col]], .data[[name_col]], Unit) %>%
        summarise(val = sum(val, na.rm = TRUE), .groups = "drop") %>%
        mutate(label = paste0(unique(df$ADG_group),
                              " (", unique(df$Unit), ")"
        ))
    }
    
    # ---- standardise output ----
    out %>%
      rename(code = !!code_col, name = !!name_col)
  })
  
  
  # Initial map (base layer)
  output$sa3_map <- renderLeaflet({
    req(identical(chosen_geog(), "SA3"))
    
    leaflet(
      options = leafletOptions(preferCanvas = TRUE,
                               scrollWheelZoom = TRUE,
                               smoothWheelZoom = TRUE,
                               wheelPxPerZoomLevel = 400,
                               zoomSnap = 0.35, zoomDelta = 0.45, 
                               maxBounds = list(
                                 c(australia_bounds$lat1, australia_bounds$lng1),
                                 c(australia_bounds$lat2, australia_bounds$lng2)
                               ),
                               maxBoundsViscosity = 0.6,  # soft edge, not a hard wall
                               minZoom = 3,
                               maxZoom = 12 )) |>
      
      addProviderTiles(providers$CartoDB.Positron) |>
      fitBounds(
        lng1 = australia_bounds$lng1,
        lat1 = australia_bounds$lat1,
        lng2 = australia_bounds$lng2,
        lat2 = australia_bounds$lat2
      ) |>
      addPolygons(
        data = SA3_sp_simpl_med,
        group = "sa3",
        layerId = ~SA3_code,
        weight = 0.5,
        color = "#666",
        fillColor = "#98C2EC",  #"#BEBADA",   # "#cccccc",
        fillOpacity = 0.75,   
        options = pathOptions(smoothFactor = 0.5)
      )
  })                                                                              
  
  # Initial map (SA4 base layer)
  output$sa4_map <- renderLeaflet({
    req(identical(chosen_geog(), "SA4"))
    
    leaflet(
      options = leafletOptions(preferCanvas = TRUE,
                               scrollWheelZoom = TRUE,
                               smoothWheelZoom = TRUE,
                               wheelPxPerZoomLevel = 400,
                               zoomSnap = 0.35, zoomDelta = 0.45, 
                               maxBounds = list(
                                 c(australia_bounds$lat1, australia_bounds$lng1),
                                 c(australia_bounds$lat2, australia_bounds$lng2)
                               ),
                               maxBoundsViscosity = 0.6,  # soft edge, not a hard wall
                               minZoom = 3,
                               maxZoom = 12 )) |>
      
      addProviderTiles(providers$CartoDB.Positron) |>
      fitBounds(
        lng1 = australia_bounds$lng1,
        lat1 = australia_bounds$lat1,
        lng2 = australia_bounds$lng2,
        lat2 = australia_bounds$lat2
      ) |>
      addPolygons(
        data = SA4_sp_simpl_med,
        group = "sa4",
        layerId = ~SA4_code,
        weight = 0.5,
        color = "#666",
        fillColor = "#BEBADA",
        fillOpacity = 0.75,   
        options = pathOptions(smoothFactor = 0.5)
      )
  })         
  
  map_slice_event <- eventReactive(input$update_map, {
    map_slice()
  }, ignoreInit = FALSE)
  
  
    observeEvent(map_slice_event(), {
    
    lyr <- map_slice_event()
    req(NROW(lyr) > 0)
    
    # absolute kill switch: atomic numeric
    lyr$val <- as.numeric(unlist(lyr$val))
    
    mode <- geog_mode()
    req(mode %in% c("SA3", "SA4"))
    
    if (identical(mode, "SA3")) {
      
      sp     <- SA3_sp_simpl_med
      grp    <- "sa3"
      map_id <- "sa3_map"
      
      lyr2 <- lyr %>% dplyr::rename(SA3_code = code)
      map_unit <- unique(na.omit(lyr$Unit))[1]
      
      sp <- attach_layer_to_sp(sp, lyr2)
      
      code_col <- "SA3_code"
      name_col <- "SA3_name"
      
    } else {
      
      sp     <- SA4_sp_simpl_med
      grp    <- "sa4"
      map_id <- "sa4_map"
      
      lyr2 <- lyr %>% dplyr::rename(SA4_code = code)
      map_unit <- unique(na.omit(lyr$Unit))[1]
      
      sp <- attach_layer_to_sp_sa4(sp, lyr2)
      
      code_col <- "SA4_code"
      name_col <- "SA4_name"
    }
    
    vals <- sp@data$val
    finite_vals <- vals[is.finite(vals)]
    dom <- range(finite_vals, na.rm = TRUE)
    
    leaf <- leafletProxy(map_id) %>%
      clearGroup(grp) %>%
      clearControls()
    
    if (length(finite_vals) == 0 || all(is.na(finite_vals))) {
      
      leaf %>%
        addPolygons(
          data = sp,
          group = grp,
          layerId = ~get(code_col),
          weight = 0.5,
          color = "#666",
          fillColor = "#BEBADA", 
          fillOpacity = 0.75
        ) %>%
        addControl("No data available for current selection", position = "bottomleft")
      
    } else {
      
      # Build a continuous palette clamped to the exact domain
      pal <- colorNumeric(
        palette  = "viridis",
        domain   = dom,          # clamp palette to data min/max
        na.color = "#D9D9D9"
      )
      
      # Choose your own legend tick positions within the domain
      # (adjust the count to taste)
      ticks <- seq(dom[1], dom[2], length.out = 6)
      
      tooltip <- sprintf(
        "<strong>%s</strong><br/>%s<br/><b>Value:</b> %s %s",
        sp@data[[name_col]],
        sp@data$label,
        formatC(sp@data$val, format = "fg", digits = 4),
        map_unit
      ) %>% lapply(htmltools::HTML)
      
      leaf %>%
        addPolygons(
          data = sp,
          group = grp,
          layerId = ~get(code_col),
          weight = 0.5,
          color = "#666",
          fillColor = ~pal(val),
          fillOpacity = 0.75,
          highlightOptions = highlightOptions(
            color = "#EBF5FF",
            weight = 2,
            bringToFront = TRUE
          ),
          label = tooltip
        ) %>%
        addLegend(
          position = "bottomright",
          pal      = pal,
          values   = ticks,                 # <- critical: pass controlled ticks
          title    = unique(sp@data$label),
          opacity  = 0.65,
          labFormat = labelFormat(
            transform = function(x) x,      # identity transform
            digits    = 0
          )
        )
    }
  })
  
  
}

shinyApp(ui, server)

