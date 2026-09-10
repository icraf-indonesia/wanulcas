####### UI REVISION START #######
## last edited: 27 August 2026 ##


### HELPERS ####################################################

# section header — hasna edit
section_header <- function(icon_name, title, subtitle = NULL, color = NULL) {
  style <- if (!is.null(color)) paste0("border-left: 4px solid ", color, "; padding-left: 12px;") else "padding-left: 12px;"
  div(
    class = "section-header",
    style = style,
    div(
      style = "display:flex; align-items:center; gap:10px;",
      icon(icon_name, style = paste0("font-size:1.4em; color:", if (!is.null(color)) color else "#8B3E04", ";")),
      div(
        tags$h5(title, style = "margin:0; font-weight:700;"),
        if (!is.null(subtitle)) tags$small(subtitle, class = "text-muted") else NULL
      )
    )
  )
}


### INPUT GUI ##################################################

# ── CROP MANAGEMENT: crop selection in sidebar, combined planting table in main ──
crop_params_ui <- function() {
  card_body(
    class = "bordercard", height = "100%",
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        width = 340,
        section_header("seedling", "Crop Species Selection",
                       "Select up to 5 crop types from the library", "#007D92"),
        br(),
        div(
          style = "background:#f0f9fb; border-radius:8px; padding:14px; margin-bottom:16px;",
          uiOutput("input_crop_select")
        )
      ),
      # main: single combined planting table (Year + Day of planting per zone)
      div(
        section_header("calendar-days", "Crop Planting Schedule",
                       "Year and Day-of-year of planting for each zone. Paste multiple rows straight from Excel.",
                       "#007D92"),
        br(),
        div(class = "big-table", table_edit_ui("crop_planting_tbl", is_upload_button = F)),
        tags$p(class = "table-paste-hint", icon("clipboard"),
               " Tip: select a block in Excel, copy, click the first cell here and paste \u2014 whole rows fill in at once.")
      )
    )
  )
}

# ── TREE MANAGEMENT: tree selection in sidebar, combined planting table in main ──
tree_params_ui <- function() {
  card_body(
    class = "bordercard", height = "100%",
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        width = 340,
        section_header("tree", "Tree Species Selection",
                       "Select up to 3 tree species to simulate", "#8B3E04"),
        br(),
        div(
          style = "background:#fdf5ef; border-radius:8px; padding:14px;",
          uiOutput("input_tree_select")
        )
      ),
      # main: single combined planting table (Year + Day of planting per species)
      div(
        section_header("calendar-days", "Tree Planting Schedule",
                       "Year and Day-of-year of planting for each selected species. Paste multiple rows straight from Excel.",
                       "#8B3E04"),
        br(),
        div(class = "big-table", table_edit_ui("tree_planting_tbl", is_upload_button = F)),
        tags$p(class = "table-paste-hint", icon("clipboard"),
               " Tip: select a block in Excel, copy, click the first cell here and paste \u2014 whole rows fill in at once."),
        hr(),
        div(style = "text-align:center; margin-top:16px;",
          tags$p(class = "text-muted", "Looking for pruning settings?"),
          actionButton("goto_pruning", tagList(icon("scissors"), " Go to Pruning Event"),
                       class = "btn", style = "background:#61701f; color:#fff; padding:10px 22px; font-weight:600; border:none; border-radius:8px;"))
      )
    )
  )
}


# ── PROFITABILITY: merged Social/Private price table ──
profitability_ui <- function(id) {
  # Non-price variables still rendered by the normal config content,
  # but the price variables are shown in one merged Social/Private table.
  div(
    section_header("coins", "Prices & Economic Parameters",
                   "Enter Social and Private prices. Values sync with the Excel upload and the engine.",
                   "#FA842B"),
    br(),
    div(class = "alert alert-info", style = "font-size:0.85em;",
        icon("circle-info"),
        " These prices come from the ", tags$b("P_ParamAll"),
        " column of your Excel file. Editing here overrides the uploaded values."),
    div(class = "big-table", table_edit_ui("price_merged_tbl", is_upload_button = F)),
    tags$p(class = "table-paste-hint", icon("clipboard"),
           " Rows = price items, Columns = Social & Private. Paste from Excel is supported."),
    br(),
    # remaining (non-price) profitability variables via normal renderer
    tags$h6(icon("percent"), " Other Economic Parameters", style = "font-weight:700; margin-top:10px;"),
    get_input_content_nonprice(id)
  )
}

# get_input_content but excluding the merged price variables (they're in the table above)
get_input_content_nonprice <- function(id) {
  price_vars <- c("P_PriceFert", "P_CPestContPrice", "P_FenceMatCost",
                  "P_UnitLabCost", "P_CostExtOrg")
  idf <- input_vars_conf_df[input_vars_conf_df$id == id &
                              !(input_vars_conf_df$var %in% price_vars), ]
  if (nrow(idf) == 0) return(NULL)
  # temporarily render using the same machinery by faking a filtered frame
  # simplest: reuse get_input_content but the price vars will just also show;
  # instead we build content from the filtered idf directly.
  g_id <- sort(unique(idf$group_id))
  page_content <- lapply(g_id, function(x) {
    sc <- get_input_subcontent_filtered(id, x, price_vars)
    var_args <- if (!is.null(sc$var)) sc$var else list()
    table_args <- if (!is.null(sc$table)) sc$table else list()
    card_body(
      padding = 10, class = "bordercard",
      do.call(flowLayout, c(list(cellArgs = list(style = "width:auto; margin:0px;")), var_args)),
      do.call(flowLayout, c(list(cellArgs = list(style = "width:auto; margin:0px;")), table_args))
    )
  })
  do.call(tagList, page_content)
}

# get_input_subcontent variant that drops specific variables
get_input_subcontent_filtered <- function(id, group_id, drop_vars) {
  idf <- input_vars_conf_df[input_vars_conf_df$id == id &
                              input_vars_conf_df$group_id == group_id &
                              !(input_vars_conf_df$var %in% drop_vars), ]
  if (nrow(idf) == 0) return(list(var = NULL, table = NULL))
  idf <- idf[order(as.numeric(idf$order)), ]
  v_content <- NULL
  v <- idf[idf$type == "vars", "var"]
  if (length(v) > 0) {
    par_df <- inputvars_df[inputvars_df$var %in% v, ]
    par_df <- par_df[order(as.numeric(par_df$order)), ]
    if (nrow(par_df) > 0) {
      v_content <- numeric_input_ui(par_df$ui_id[1], par_df, tooltip_class = "custom-tooltip")
    }
  }
  a_content <- NULL
  adf <- idf[idf$type == "arrays", ]
  if (nrow(adf) > 0) {
    a <- unique(adf$subtype)
    a_id <- paste("input_array", a, id, group_id, sep = "_")
    a_content <- lapply(a_id, function(x) {
      card(full_screen = TRUE, card_body(padding = 10, style = "overflow-x:auto;",
        table_edit_ui(x, is_upload_button = F, vspace = "4px")))
    })
  }
  g_content <- NULL
  gdf <- idf[idf$type == "graphs", ]
  if (nrow(gdf) > 0) {
    g_content <- apply(gdf, 1, function(x) get_input_graph(x[["var_label"]], x[["var_desc"]], x[["var"]]))
    names(g_content) <- NULL
  }
  list(var = v_content, table = c(a_content, g_content))
}


# ── AGROFORESTRY DESIGN: hasna-edit layout (existing tables, no engine change) ──

# ── PEDOTRANSFER calculator panel (Soil Water page) ──
pedotransfer_ui <- function() {
  div(
    style = "margin-top:12px;",
    card(
      class = "bordercard",
      card_header(icon("calculator"), " Pedotransfer Calculator (4 soil layers)",
                  tags$small(" \u2014 compute soil hydraulic properties from texture",
                             class = "text-muted")),
      card_body(
        padding = 14,
        div(class = "alert alert-warning", style = "font-size:0.85em; margin-bottom:12px;",
            icon("triangle-exclamation"), tags$b(" Optional:"),
            " Skip this page if you already set your soil hydraulic data in the Excel file. ",
            "Use it only to compute values from soil texture, or to override the uploaded data."),
        layout_column_wrap(
          width = 1/2, gap = "16px",
          div(
            tags$h6(icon("flask-vial"), " Soil Texture Inputs (per layer)", style = "font-weight:700;"),
            layout_column_wrap(width = 1/2,
              selectInput("ptf_method", "Method",
                          choices = c("1 - Wosten (temperate)" = 1,
                                      "2 - Tomasella-Hodnett (tropical)" = 2),
                          selected = 2),
              numericInput("ptf_medsand", "Median sand size (\u00B5m, Wosten)", 290, min = 0, step = 1)
            ),
            tags$label("Soil properties per layer (paste from Excel):"),
            div(class = "big-table", table_edit_ui("ptf_input_tbl", is_upload_button = F)),
            tags$small(class = "table-paste-hint", icon("clipboard"),
              " Rows = Layers 1\u20134. Columns: Clay %, Silt %, OrgC %, BD, CEC, pH, K(FC), Ksat own. ",
              "Copy an 8-column block from Excel and paste."),
            tags$small(class = "text-muted", style = "font-size:0.8em; display:block; margin-top:6px;",
              "K (FC) = critical K used to define field capacity (cm d\u207B\u00B9). ",
              "CEC & pH are required for Tomasella-Hodnett (used for all layers). ",
              "Median sand size is used only by Wosten. ",
              "The 'Ksat own' column is only used when you answer 'No' below."),
            hr(),
            layout_column_wrap(width = 1/2,
              selectInput("ptf_top", "Topsoil layer",
                          choices = c("Layer 1 only" = 1, "None" = 0), selected = 1),
              selectInput("ptf_use_ptf_ksat", "Use the PTF estimate of Ksat?",
                          choices = c("Yes - use the calculated Ksat" = 1,
                                      "No - use my own Ksat value" = 0),
                          selected = 1)
            ),
            actionButton("ptf_apply", icon("calculator"), " Compute & Apply (all layers)",
                          class = "btn-success", width = "100%")
          ),
          div(
            tags$h6(icon("table"), " Computed van Genuchten Parameters (per layer)",
                    style = "font-weight:700;"),
            tableOutput("ptf_results"),
            hr(),
            tags$h6(icon("chart-line"), " Water Retention Curves", style = "font-weight:700;"),
            plotOutput("ptf_curve_plot", height = "260px"),
            tags$small(class = "text-muted", icon("circle-info"),
                       " Each layer's texture produces its own hydraulic parameters, ",
                       "mirroring the Soil Hydraulic table in the Excel model.")
          )
        )
      )
    )
  )
}

# ── PHOSPHORUS calculator panel ──
phosphorus_ui <- function() {
  soil_choices <- stats::setNames(1:13, c(
    "1 - Your 1st (custom)","2 - Your 2nd (custom)",
    "3 - Your 3rd (custom)","4 - Your 4th (custom)",
    "5 - Light Clay","6 - Basin Clay","7 - Light Sand","8 - Sitiung1",
    "9 - R.Bujang U","10 - Baganding","11 - LampungBMSF",
    "12 - Gajrug","13 - Sepungur"))
  div(
    style = "margin-top:12px;",
    card(
      class = "bordercard",
      card_header(icon("atom"), " Phosphorus Sorption Calculator",
                  tags$small(" \u2014 compute N_PStParam from P-Bray and soil type",
                             class = "text-muted")),
      card_body(
        padding = 14,
        div(class = "alert alert-warning", style = "font-size:0.85em; margin-bottom:12px;",
            icon("triangle-exclamation"),
            tags$b(" Optional:"),
            " Skip this page if you already set your phosphorus data in the Excel file. ",
            "Bulk density is pre-filled from your uploaded soil data until you change it here."),
        layout_column_wrap(
          width = 1/2, gap = "16px",
          div(
            tags$h6(icon("vial"), " P-Bray Inputs (mg/kg)", style = "font-weight:700;"),
            selectInput("phos_method", "P analysis method",
                        choices = c("P-Bray" = 1, "P-water" = 2),
                        selected = 1, width = "100%"),
            tags$label("P-Bray per Layer \u00D7 Zone (paste from Excel):"),
            div(class = "big-table", table_edit_ui("pbray_table", is_upload_button = F)),
            tags$small(class = "table-paste-hint", icon("clipboard"),
                       " Rows = Layers 1\u20134, Columns = Zones 1\u20134. Copy a 4\u00D74 block from Excel and paste."),
            hr(),
            tags$h6(icon("layer-group"), " Soil Type & Bulk Density per Layer",
                    style = "font-weight:700;"),
            layout_column_wrap(
              width = 1/2,
              div(selectInput("phos_soil_L1", "Layer 1 soil type", soil_choices, selected = 12),
                  numericInput("phos_bd_L1", "BD Layer 1 (g/cm\u00B3)", 1.37, min=0.5, max=2.5, step=0.01)),
              div(selectInput("phos_soil_L2", "Layer 2 soil type", soil_choices, selected = 12),
                  numericInput("phos_bd_L2", "BD Layer 2 (g/cm\u00B3)", 1.38, min=0.5, max=2.5, step=0.01))
            ),
            layout_column_wrap(
              width = 1/2,
              div(selectInput("phos_soil_L3", "Layer 3 soil type", soil_choices, selected = 12),
                  numericInput("phos_bd_L3", "BD Layer 3 (g/cm\u00B3)", 1.35, min=0.5, max=2.5, step=0.01)),
              div(selectInput("phos_soil_L4", "Layer 4 soil type", soil_choices, selected = 12),
                  numericInput("phos_bd_L4", "BD Layer 4 (g/cm\u00B3)", 1.35, min=0.5, max=2.5, step=0.01))
            ),
            conditionalPanel(
              "input.phos_soil_L1 <= 4 || input.phos_soil_L2 <= 4 || input.phos_soil_L3 <= 4 || input.phos_soil_L4 <= 4",
              hr(),
              tags$h6(icon("edit"), " Custom Soil Coefficients",
                      style = "font-weight:700; color:#dc3545;"),
              tags$small(class = "text-muted",
                         "Enter your own coefficients for soil types 'Your 1st' through 'Your 4th':"),
              layout_column_wrap(
                width = 1/4,
                numericInput("phos_custom_sm1", "SorbMax1", 1.859, min=0, step=0.01),
                numericInput("phos_custom_sm2", "SorbMax2", 0, min=0, step=0.01),
                numericInput("phos_custom_sa1", "SorbAff1", 2010, min=0, step=1),
                numericInput("phos_custom_sa2", "SorbAff2", 0, min=0, step=0.1)
              )
            ),
            hr(),
            actionButton("phos_apply", icon("atom"), " Compute & Apply",
                          class = "btn-success", width = "100%")
          ),
          div(
            tags$h6(icon("table"), " Computed P Parameters", style = "font-weight:700;"),
            tableOutput("phos_results"),
            hr(),
            tags$h6(icon("chart-line"), " Langmuir Sorption Isotherm (Layer 1)",
                    style = "font-weight:700;"),
            plotOutput("phos_curve_plot", height = "260px"),
            tags$small(class = "text-muted",
                       icon("circle-info"),
                       " Clicking 'Compute & Apply' updates N_PStParam, PStMin, and PStMax.",
                       tags$br(),
                       tags$a(href = "https://www.isric.org/explore/soilgrids", target = "_blank",
                              icon("external-link-alt"), " World Soil Database (SoilGrids)"))
          )
        )
      )
    )
  )
}

af_design_ui <- function() {
  af_vars_df <- inputvars_df[inputvars_df$var %in% c("AF_Circ", "AF_ZoneTot"), ]
  af_vars_df <- af_vars_df[order(as.numeric(af_vars_df$order)), ]
  card_body(
    class = "bordercard", height = "100%",
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        width = 330,
        section_header("map", "System & Zones", NULL, "#8B3E04"),
        br(),
        div(
          style = "background:#fdf5ef; border-radius:8px; padding:12px; margin-bottom:14px;",
          numeric_input_ui(af_vars_df$ui_id[1], af_vars_df, tooltip_class = "custom-tooltip")
        ),
        hr(),
        section_header("ruler-horizontal", "Zone Width (m)", NULL, "#FA842B"),
        br(),
        card(full_screen = TRUE, card_body(padding = 8, style = "overflow-x:auto;",
          table_edit_ui("input_array_zone_9_0", is_upload_button = F, vspace = "4px"))),
        hr(),
        section_header("layer-group", "Soil Layer Thickness (m)", NULL, "#61701f"),
        br(),
        card(full_screen = TRUE, card_body(padding = 8, style = "overflow-x:auto;",
          table_edit_ui("input_array_layer_9_0", is_upload_button = F, vspace = "4px")))
      ),
      # main: tree placement across / within zones + density
      div(
        div(
          style = "text-align:center; margin-bottom:16px;",
          tags$img(src = "WaNuLCAS_layer.png", alt = "WaNuLCAS zones and layers",
                   style = "max-width:100%; max-height:360px; border-radius:8px; box-shadow:0 2px 8px rgba(0,0,0,0.15);"),
          tags$p(class = "text-muted", style = "font-size:0.82em; margin-top:6px;",
                 "WaNuLCAS represents the field as a grid of ", tags$b("zones"),
                 " (columns, across the plot) and ", tags$b("soil layers"),
                 " (rows, with depth), plus ", tags$b("canopy layers"), " above ground.")
        ),
        section_header("tree", "Tree Placement",
                       "Position across zones, position within zone, and density (trees ha\u207B\u00B9)",
                       "#8B3E04"),
        br(),
        div(
          class = "alert alert-secondary", style = "font-size:0.84em;",
          icon("circle-info"),
          " Position across zones tells which zone each tree occupies; position within zone runs 0 (left) to 1 (right)."
        ),
        card(
          full_screen = TRUE,
          card_header(icon("arrows-left-right"), " Tree Position & Density"),
          card_body(padding = 10, style = "overflow-x:auto;",
                    table_edit_ui("input_array_tree_9_0", is_upload_button = F, vspace = "4px"))
        )
      )
    )
  )
}


get_input_graph <- function(title, desc, v) {
  title_tt <- div(title, style = "width:180px;")
  if (desc != "") {
    title_tt <- title_tt |> bslib::tooltip(desc, options = list(customClass = "custom-tooltip"))
  }
  
  # Safe lapply to avoid zero-length errors
  graph_items <- lapply(graph_subvars[[v]], function(x) {
    table_edit_ui(x, is_upload_button = F, vspace = "0px")
  })
  
  div(
    class = "whitecard",
    navset_card_underline(
      id = paste("input_graph_card", v, sep = "-"),
      full_screen = TRUE,
      height = 300,
      
      title = title_tt,
      nav_panel("Plot", card_body(padding = 5, plotlyOutput(
        paste("input_graph_plot", v, sep = "-")
      ))),
      nav_panel("Data", do.call(
        layout_column_wrap, c(list(width = "200px", fill = FALSE), graph_items)
      ))
    )
  )
}


get_input_subcontent <- function(id, group_id) {
  idf <- input_vars_conf_df[input_vars_conf_df$id == id &
                              input_vars_conf_df$group_id == group_id, ]
  if (nrow(idf) == 0)
    return(NULL)
  idf <- idf[order(as.numeric(idf$order)), ]
  
  # variable input
  v_content <- NULL
  v <- idf[idf$type == "vars", "var"]
  if (length(v) > 0) {
    par_df <- inputvars_df[inputvars_df$var %in% v, ]
    par_df <- par_df[order(as.numeric(par_df$order)), ]
    if (nrow(par_df) > 0) {
      n_ui <- numeric_input_ui(par_df$ui_id[1], par_df, tooltip_class = "custom-tooltip")
      v_content <- n_ui
    }
  }
  
  # array input — full-width card
  a_content <- NULL
  adf <- idf[idf$type == "arrays", ]
  if (nrow(adf) > 0) {
    a <- unique(adf$subtype)
    a_id <- paste("input_array", a, id, group_id, sep = "_")
    a_content <- lapply(a_id, function(x) {
      card(
        full_screen = TRUE,
        card_body(
          padding = 10,
          style = "overflow-x:auto;",
          table_edit_ui(x, is_upload_button = F, vspace = "4px")
        )
      )
    })
  }
  
  # graph input
  g_content <- NULL
  gdf <- idf[idf$type == "graphs", ]
  if (nrow(gdf) > 0) {
    g_content <- apply(gdf, 1, function(x) {
      g_content <- get_input_graph(x[["var_label"]], x[["var_desc"]], x[["var"]])
    })
    names(g_content) <- NULL
  }
  
  return(list(var = v_content, table = c(a_content, g_content)))
}



get_input_content <- function(id) {
  idf <- input_vars_conf_df[input_vars_conf_df$id == id, ]
  if (nrow(idf) == 0)
    return(NULL)
  # by group
  g_id <- sort(unique(idf$group_id))
  page_content <- lapply(g_id, function(x) {
    sc <- get_input_subcontent(id, x)
    
    var_args <- if (!is.null(sc$var))
      sc$var
    else
      list()
    table_args <- if (!is.null(sc$table))
      sc$table
    else
      list()
    
    content <- card_body(
      padding = 10,
      class = "bordercard",
      do.call(flowLayout, c(list(
        cellArgs = list(style = "width:auto; margin:0px;")
      ), var_args)),
      do.call(flowLayout, c(list(
        cellArgs = list(style = "width:auto; margin:0px;")
      ), table_args))
    )
    g_df <- input_group_df[input_group_df$group_id == x, ]
    if (nrow(g_df) > 0) {
      return (card(card_header(g_df$title), markdown(g_df$desc), content))
    }
    card(content)
  })
  
  if (length(page_content) == 1)
    return(page_content)
  
  card_body(
    class = "bordercard",
    height = "100%",
    do.call(flowLayout, c(list(
      cellArgs = list(style = "width:auto; margin:0px;")
    ), page_content))
  )
}


### INPUT TAB BUILDERS (module-aware) ##########################

# Sub-tab / sub-sub-tab builder. Title-matched special UI survives id changes.
input_subtab <- function(st) {
  row.names(st) <- NULL
  apply(st, 1, function(x) {
    id     <- as.numeric(x["id"])
    title  <- x["title"]
    is_core <- identical(unname(x["module"]), "Core")

    # Tree Management = hasna-edit sidebar selector + combined planting table
    if (is_core && title == "Tree Management") {
      return(nav_panel(title, tree_params_ui()))
    }
    # Tree Library = reactable species library (full-width)
    if (is_core && title == "Tree Library") {
      return(nav_panel(title, card_body(
        class = "bordercard", height = "100%",
        section_header("book-open", "Tree Species Library",
                       "Browse the full parameter database. Add or remove species.", "#FA842B"),
        br(),
        div(
          style = "display:flex; gap:8px; margin-bottom:12px;",
          actionButton("add_tree_button", "Add New Tree",
                       icon = icon("plus"), class = "btn-sm btn-success"),
          actionButton("remove_tree_button", "Remove Selected",
                       icon = icon("trash-can"), class = "btn-sm btn-danger")
        ),
        reactableOutput("input_tree_lib")
      )))
    }
    # Crop Management = hasna-edit sidebar selector + config content
    if (is_core && title == "Crop Management") {
      return(nav_panel(title, crop_params_ui()))
    }
    # Crop Library = reactable crop library (full-width)
    if (is_core && title == "Crop Library") {
      return(nav_panel(title, card_body(
        class = "bordercard", height = "100%",
        section_header("book-open", "Crop Species Library",
                       "Browse the full parameter database. Add or remove crop types.", "#61701f"),
        br(),
        div(
          style = "display:flex; gap:8px; margin-bottom:12px;",
          actionButton("add_crop_button", "Add New Crop",
                       icon = icon("plus"), class = "btn-sm btn-success"),
          actionButton("remove_crop_button", "Remove Selected",
                       icon = icon("trash-can"), class = "btn-sm btn-danger")
        ),
        reactableOutput("input_crop_lib")
      )))
    }
    # Agroforestry Design = hasna-edit layout (reuses existing table widgets)
    if (is_core && title == "Agroforestry Design") {
      return(nav_panel(title, af_design_ui()))
    }
    # Soil Water = split into Pedotransfer calculator + Water Retention Data subtabs
    if (is_core && title == "Soil Water") {
      content <- get_input_content(id)
      return(nav_panel(title, card_body(
        class = "subpanel", padding = 0,
        navset_card_pill(
          nav_panel(
            tagList(icon("calculator"), " Pedotransfer"),
            pedotransfer_ui()
          ),
          nav_panel(
            tagList(icon("chart-area"), " Water Retention Data"),
            card_body(
              padding = 10,
              tags$p(class = "text-muted", style = "font-size:0.88em;",
                     icon("circle-info"),
                     " These tables and graphs are the values the engine actually uses. ",
                     "They come from the xlsm upload or from the Pedotransfer calculator."),
              if (is.list(content) && !inherits(content, "shiny.tag"))
                do.call(tagList, content)
              else
                content
            )
          )
        )
      )))
    }
    # Soil Nutrient = Nitrogen subtab (from config) + Phosphorus calculator
    if (is_core && title == "Soil Nutrient") {
      sst <- input_gui_tabs_df[input_gui_tabs_df$parent_id == id, ]
      sst_ui <- if (nrow(sst) > 0) input_subtab(sst) else list()
      # add Phosphorus as a new subtab alongside Nitrogen
      phos_panel <- nav_panel(
        tagList(icon("atom"), " Phosphorus"),
        phosphorus_ui()
      )
      all_panels <- c(sst_ui, list(phos_panel))
      return(nav_panel(title, card_body(
        class = "subpanel", padding = 0,
        do.call(navset_card_pill, all_panels)
      )))
    }
    # Rainfall = guiding question sidebar that navigates the Rain Type subtabs
    if (is_core && title == "Rainfall") {
      rain_subtabs <- input_gui_tabs_df[input_gui_tabs_df$parent_id == id, ]
      rain_subtabs <- rain_subtabs[order(as.numeric(rain_subtabs$id)), ]
      # build the type panels (each Rain Type subtab content) as an UNNAMED list
      type_panels <- lapply(seq_len(nrow(rain_subtabs)), function(i) {
        rtid <- as.numeric(rain_subtabs$id[i])
        nav_panel(rain_subtabs$title[i], value = rain_subtabs$title[i],
                  card_body(padding = 8, get_input_content(rtid)))
      })
      names(type_panels) <- NULL
      return(nav_panel(title, card_body(
        class = "bordercard", height = "100%",
        layout_sidebar(
          fillable = TRUE,
          sidebar = sidebar(
            width = 300, open = TRUE, class = "sim-sidebar",
            tags$h6(icon("circle-question"), " What kind of rainfall data do you have?",
                    style = "font-weight:700; color:#4a90d9; margin:0 0 12px;"),
            radioButtons(
              "rain_guide",
              label = NULL,
              choiceNames = list(
                tagList(tags$b("a. Daily measured data"), tags$br(),
                        tags$small(class = "text-muted", "day-by-day rainfall records")),
                tagList(tags$b("b. Monthly average"), tags$br(),
                        tags$small(class = "text-muted", "average rainfall per month")),
                tagList(tags$b("c. Annual average only"), tags$br(),
                        tags$small(class = "text-muted", "one yearly total \u2014 randomly generated")),
                tagList(tags$b("d. Multi-year monthly dataset"), tags$br(),
                        tags$small(class = "text-muted", "several years \u2014 randomly generated"))
              ),
              choiceValues = c("Rain Type 1", "Rain Type 2", "Rain Type 3", "Rain Type 4"),
              selected = "Rain Type 1"
            ),
            hr(),
            div(class = "alert alert-info", style = "font-size:0.82em;",
                icon("circle-info"),
                " Types c and d generate synthetic daily rainfall from the statistics you provide.")
          ),
          # main: the selected Rain Type content, plus the type selector variable
          div(
            do.call(navset_hidden, c(list(id = "rain_type_panel"), type_panels))
          )
        )
      )))
    }
    # Profitability = merged Social/Private price table + other economic params
    if (title == "Profitability") {
      return(nav_panel(title, card_body(padding = 10, profitability_ui(id))))
    }
    # Soil Evaporation = guiding question sidebar (like Rainfall)
    if (is_core && title == "Soil Evaporation") {
      evap_subtabs <- input_gui_tabs_df[input_gui_tabs_df$parent_id == id, ]
      evap_subtabs <- evap_subtabs[order(as.numeric(gsub("\\D", "", evap_subtabs$title))), ]
      evap_panels <- lapply(seq_len(nrow(evap_subtabs)), function(i) {
        etid <- as.numeric(evap_subtabs$id[i])
        nav_panel(evap_subtabs$title[i], value = evap_subtabs$title[i],
                  card_body(padding = 8, get_input_content(etid)))
      })
      names(evap_panels) <- NULL
      return(nav_panel(title, card_body(
        class = "bordercard", height = "100%",
        layout_sidebar(
          fillable = TRUE,
          sidebar = sidebar(
            width = 300, open = TRUE, class = "sim-sidebar",
            tags$h6(icon("circle-question"), " What kind of soil evaporation data do you have?",
                    style = "font-weight:700; color:#4a90d9; margin:0 0 12px;"),
            radioButtons(
              "evap_guide", label = NULL,
              choiceNames = list(
                tagList(tags$b("a. Daily"), tags$br(),
                        tags$small(class = "text-muted", "day-by-day evaporation data")),
                tagList(tags$b("b. Constant temperature"), tags$br(),
                        tags$small(class = "text-muted", "single constant potential evaporation")),
                tagList(tags$b("c. Monthly average"), tags$br(),
                        tags$small(class = "text-muted", "average evaporation per month"))
              ),
              choiceValues = c("Evap Type 0", "Evap Type 1", "Evap Type 2"),
              selected = "Evap Type 0"
            )
          ),
          div(
            do.call(navset_hidden, c(list(id = "evap_type_panel"), evap_panels))
          )
        )
      )))
    }
    # Soil Temperature = guiding question sidebar (like Rainfall / Soil Evaporation)
    if (is_core && title == "Soil Temperature") {
      stemp_subtabs <- input_gui_tabs_df[input_gui_tabs_df$parent_id == id, ]
      stemp_subtabs <- stemp_subtabs[order(as.numeric(gsub("\\D", "", stemp_subtabs$title))), ]
      stemp_panels <- lapply(seq_len(nrow(stemp_subtabs)), function(i) {
        stid <- as.numeric(stemp_subtabs$id[i])
        ttl  <- stemp_subtabs$title[i]
        desc <- switch(ttl,
          "Soil Temp Type 1" = tags$div(class = "alert alert-info", style = "font-size:0.85em;",
            icon("thermometer-half"), tags$b(" Constant soil temperature"),
            tags$br(), "Enter a single constant value (\u00B0C) that will be used for every day of the simulation."),
          "Soil Temp Type 2" = tags$div(class = "alert alert-info", style = "font-size:0.85em;",
            icon("calendar-alt"), tags$b(" Monthly average soil temperature"),
            tags$br(), "Enter average soil temperature (\u00B0C) for each month (12 data points)."),
          "Soil Temp Type 0" = tags$div(class = "alert alert-info", style = "font-size:0.85em;",
            icon("chart-line"), tags$b(" Daily soil temperature data"),
            tags$br(), "Enter or paste daily soil temperature values (\u00B0C) for each day of the year (up to 365 points)."),
          NULL
        )
        nav_panel(ttl, value = ttl,
                  card_body(padding = 8, desc, get_input_content(stid)))
      })
      names(stemp_panels) <- NULL
      return(nav_panel(title, card_body(
        class = "bordercard", height = "100%",
        layout_sidebar(
          fillable = TRUE,
          sidebar = sidebar(
            width = 300, open = TRUE, class = "sim-sidebar",
            tags$h6(icon("circle-question"), " What kind of soil temperature data do you have?",
                    style = "font-weight:700; color:#4a90d9; margin:0 0 12px;"),
            radioButtons(
              "stemp_guide", label = NULL,
              choiceNames = list(
                tagList(tags$b("a. Constant temperature"), tags$br(),
                        tags$small(class = "text-muted", "single constant soil temperature")),
                tagList(tags$b("b. Monthly average"), tags$br(),
                        tags$small(class = "text-muted", "average soil temperature per month")),
                tagList(tags$b("c. Daily"), tags$br(),
                        tags$small(class = "text-muted", "day-by-day soil temperature data"))
              ),
              choiceValues = c("Soil Temp Type 1", "Soil Temp Type 2", "Soil Temp Type 0"),
              selected = "Soil Temp Type 1"
            )
          ),
          div(
            do.call(navset_hidden, c(list(id = "stemp_type_panel"), stemp_panels))
          )
        )
      )))
    }
    # Oil Palm Library = flat reactable (no sub-sub-tabs)
    if (is_core && title == "Oil Palm Library") {
      return(nav_panel(title, card_body(
        class = "bordercard", height = "100%",
        section_header("leaf", "Oil Palm Species Library",
                       "Browse and edit oil palm parameters", "#007D92"),
        br(),
        get_input_content(id)
      )))
    }

    sst <- input_gui_tabs_df[input_gui_tabs_df$parent_id == id, ]
    if (nrow(sst) > 0) {
      sst_ui  <- input_subtab(sst)
      content <- get_input_content(id)
      if (!is.null(content)) {
        sst_ui <- c(list(nav_panel("Variables", content)), sst_ui)
      }
      nav_panel(title, card_body(
        class = "subpanel", padding = 0,
        do.call(navset_card_pill, sst_ui)
      ))
    } else {
      content <- get_input_content(id)
      desc <- card_body(padding = 10, fillable = F, fill = F, x["desc"])
      nav_panel(title, desc, content)
    }
  })
}

# Friendly icon for a main tab (helps first-time users navigate)
tab_icon <- function(title) {
  m <- list("Tree" = "tree", "Crop" = "seedling",
            "Agroforestry System" = "map-location-dot",
            "Climate" = "cloud-sun-rain", "Soil" = "mound",
            "Economy" = "coins", "Management" = "clipboard-list")
  ic <- m[[title]]
  if (is.null(ic)) ic <- "folder-open"
  icon(ic)
}

# Main-tab builder, filtered by module column ("Core" / "Additional").
input_tab <- function(module) {
  tab_df <- input_gui_tabs_df[input_gui_tabs_df$parent_id == 0 &
                                input_gui_tabs_df$module == module, ]
  row.names(tab_df) <- NULL
  apply(tab_df, 1, function(x) {
    id    <- as.numeric(x["id"])
    ttl   <- unname(x["title"])
    label <- tagList(tab_icon(ttl), " ", ttl)   # icon + text
    st <- input_gui_tabs_df[input_gui_tabs_df$parent_id == id, ]
    if (nrow(st) > 0) {
      st_ui   <- input_subtab(st)
      content <- get_input_content(id)
      if (!is.null(content)) {
        st_ui <- c(list(nav_panel("Variables", content)), st_ui)
      }
      # predictable navset id per main tab, e.g. "subnav_Management"
      navset_id <- paste0("subnav_", gsub("[^A-Za-z0-9]", "", ttl))
      nav_panel(label, value = ttl, card_body(
        class = "subpanel", padding = 0,
        do.call(navset_card_underline, c(list(id = navset_id), st_ui))
      ))
    } else {
      # main tab with direct vars but no sub-tabs
      content <- get_input_content(id)
      desc <- card_body(padding = 10, fillable = F, fill = F, x["desc"])
      nav_panel(label, value = ttl, desc, content)
    }
  })
}

### HOME PAGE HELPERS ##########################################

home_module_card <- function(icon_name, title, desc, color, button_id, button_label = "Configure \u2192") {
  div(
    class = "home-module-card",
    style = paste0("border-top:4px solid ", color, ";"),
    div(class = "home-module-icon",
        style = paste0("background:", color, "22; color:", color, ";"),
        icon(icon_name, style = "font-size:1.6em;")),
    tags$h6(title, style = "font-weight:700; margin:8px 0 4px;"),
    tags$p(desc, class = "text-muted",
           style = "font-size:0.8em; margin-bottom:10px; min-height:34px;"),
    actionButton(button_id, button_label, class = "btn btn-sm",
                 style = paste0("background-color:", color, "; color:white; border:none; width:100%;"))
  )
}

home_nav_btn <- function(icon_name, label, id, bg, col) {
  actionButton(id, tagList(icon(icon_name), " ", label), class = "home-big-btn",
    style = paste0("background:", bg, "; color:", col, ";"))
}

home_panel <- nav_panel(
  title = "", icon = icon("house"),
  reactable.extras::reactable_extras_dependency(),
  div(class = "home",
    div(style = "text-align:right;",
      p("WaNuLCAS", span("5.0", style = "color:#EADEBD;"),
        style = "font-size:4em; font-family:'Arial black'; margin:0;"),
      p(
        span("Wa", style = "color:#8ECAE6; font-family:'Arial black';", .noWS = c("before","after")), "ter, ",
        span("Nu", style = "color:#E4A4A0; font-family:'Arial black';", .noWS = c("before","after")), "trient and ",
        span("L",  style = "color:#FFD15C; font-family:'Arial black';", .noWS = c("before","after")), "ight ",
        span("C",  style = "color:#FFD15C; font-family:'Arial black';", .noWS = c("before","after")), "apture in ",
        span("A",  style = "color:#ADC178; font-family:'Arial black';", .noWS = c("before","after")), "groforestry ",
        span("S",  style = "color:#ADC178; font-family:'Arial black';", .noWS = c("before","after")), "ystem",
        style = "font-size:1.5em; margin:4px 0 10px;"),
      div(style = "max-width:560px; margin-left:auto; margin-right:0;",
        tags$p(style = "font-size:1.02em; color:#EADEBDee; line-height:1.6; margin-bottom:18px;",
          "A process-based model for simulating water, nutrient and light dynamics in agroforestry systems.")),
      div(style = "display:flex; gap:12px; flex-wrap:wrap; justify-content:flex-end;",
        home_nav_btn("play-circle", "Run & Output",   "nav_to_sim",      "#FA842Bcc", "#fff"),
        home_nav_btn("sliders",     "Input Section",   "nav_to_inputs",   "#8B3E04",   "#fff"),
        home_nav_btn("circle-info", "About the Model", "nav_to_about",    "#607d8b",   "#fff"),
        home_nav_btn("book-open",   "Tutorial",        "nav_to_tutorial", "#4a90d9",   "#fff"),
        tags$a(href = "https://github.com/icraf-indonesia/wanulcas", target = "_blank",
               class = "btn home-big-btn", tagList(icon("github"), " GitHub"),
               style = "background:#24292e; color:#fff;"))
    ),
    # Module cards: hidden until "Input Section" is clicked
    conditionalPanel(
      condition = "output.show_home_modules == true",
      div(id = "home_modules",
        div(style = "margin-top:22px;",
          div(span(class = "home-core-badge", icon("star"), " CORE MODULES"),
              span(" \u2014 required for the model to run", style = "font-size:0.85em; color:#EADEBD;")),
          div(class = "home-grid",
            home_module_card("tree", "Tree Parameters", "Species, growth traits, planting schedule, oil palm", "#8B3E04", "nav_to_tree"),
            home_module_card("seedling", "Crop Parameters", "Crop types, agronomic parameters, calendar", "#007D92", "nav_to_crop"),
            home_module_card("map", "Agroforestry System", "Zone layout, tree spacing, soil layers", "#FA842B", "nav_to_af"),
            home_module_card("cloud-sun-rain", "Climate", "Rainfall, temperature, evapotranspiration", "#4a90d9", "nav_to_weather"),
            home_module_card("flask", "Soil", "Soil water (pedotransfer) & nutrients", "#61701f", "nav_to_soilnutrient"))
        ),
        div(style = "margin-top:18px;",
          div(span(class = "home-add-badge", icon("plus-circle"), " ADDITIONAL MODULES"),
              span(" \u2014 optional, for extended analysis", style = "font-size:0.85em; color:#EADEBD;")),
          div(class = "home-grid",
            home_module_card("clipboard-list", "Management", "Weeding, pruning, latex, grazing, slash & burn schedules", "#61701f", "nav_to_management"),
            home_module_card("chart-line", "Economy", "Profitability, labour, input costs", "#FA842B", "nav_to_economy"),
            home_module_card("tree", "Tree (Advanced)", "Additional tree parameters beyond the core set", "#8B3E04", "nav_to_addtree"),
            home_module_card("flask", "Soil (Advanced)", "SOM, roots, GHG, erosion, phosphorus dynamics", "#607d8b", "nav_to_addsoil"),
            home_module_card("fire", "Slash & Burn", "Burning & slashing schedule and impacts", "#dc3545", "nav_to_slashburn"))
        )
      )
    ),
    div(style = "position:fixed; right:50px; bottom:8px; text-align:right; font-size:0.8em; opacity:0.85; line-height:1.5;",
      p(HTML("&copy; World Agroforestry (ICRAF) - 2026"), style = "margin:0; font-weight:600;"),
      p(style = "margin:2px 0 0; max-width:640px;",
        tags$b("Authors: "),
        "Meine van Noordwijk, Betha Lusiana, Ni\u2019matul Khasanah, Rachmat Mulia, Hasna Afifah, and Degi Harja Asmara"),
      p(style = "margin:2px 0 0;",
        tags$a(href = "mailto:N.Khasanah@landscapealliance.org,H.Afifah@landscapealliance.org",
               style = "color:#8ECAE6; font-weight:600;",
               icon("envelope"), " Contact"))
    )
  )
)


### SIMULATION SCENARIO CONTROLS ###############################
# Sliders for 8 scenario switches/multipliers (config id = -1).
# Labels only — no var names shown.

sim_slider <- function(id, label, min, max, value, step = 1) {
  div(
    class = "sim-slider-row",
    div(class = "sim-slider-lab", label),
    div(class = "sim-slider-inp",
        sliderInput(id, NULL, min = min, max = max, value = value,
                    step = step, width = "100%", ticks = FALSE))
  )
}

sim_controls_ui <- function() {
  tagList(
    div(class = "sim-slider-legend",
        icon("sliders"), tags$b(" Scenario Controls"),
        tags$small(" \u2014 switch model processes on/off (1 = on, 0 = off)",
                   class = "text-muted")),
    sim_slider("sim_AF_AnyTrees_is",       "\U0001F333 Include Trees?",              0, 1, 1),
    sim_slider("sim_AF_Crop_is",           "\U0001F33E Include Crops?",              0, 1, 1),
    sim_slider("sim_AF_RunWatLim_is",      "\U0001F4A7 Water Limitation?",           0, 1, 1),
    sim_slider("sim_AF_RunNutLim_is_N",    "\U0001F9EA Nitrogen (N) Limitation?",    0, 1, 1),
    sim_slider("sim_AF_RunNutLim_is_P",    "\U0001F9EA Phosphorus (P) Limitation?",  0, 1, 1),
    sim_slider("sim_AF_DynPestImpacts_is", "\U0001F41B Include Pests?",              0, 1, 0),
    sim_slider("sim_W_WaterLog_is",        "\U0001F30A Water Logging?",              0, 1, 0),
    sim_slider("sim_W_Hyd_is",             "\U0001F4A6 Hydraulic Redistribution?",   0, 1, 0),
    sim_slider("sim_RAIN_Multiplier",      "\U0001F327\UFE0F Rain Multiplier",       0, 4, 1, step = 0.1),
    sim_slider("sim_CA_DOYStart",          "\U0001F4C5 Start Day (DOY)",             1, 365, 14)
  )
}


### UI #########################################################

ui <-
  page_navbar(
    id = "main_page",
    theme = bs_theme(
      version = 5,
      primary = theme_color$primary,
      secondary = theme_color$secondary,
      dark = theme_color$dark,
      success = theme_color$success,
      info = theme_color$info,
      warning = theme_color$warning,
      danger = theme_color$danger,
      font_scale = 0.8,
      "navbar-light-color" = theme_color$light1,
      "navbar-light-active-color" = "white",
      "navbar-light-hover-color" = theme_color$secondary
    ),
    navbar_options = navbar_options(bg = theme_color$primary, theme = "light"),
    header =
      tags$head(
        tags$style(
          tags$link(rel = "shortcut icon", href = "favicon.ico"),
          HTML("
            .custom-tooltip { --bs-tooltip-bg:#8B3E04; --bs-tooltip-border-radius:8px; --bs-tooltip-opacity:1; --bs-tooltip-max-width:300px; }
            .card-header { background-color:#F5F0E0; font-weight:600; }
            .subpanel .card-header { background-color:white; border-width:0px; }
            .subpanel .card { border-width:0px; }
            .bordercard .card { border-width:1px; }
            .bordercard .card-header { border-width:1px; background-color:#F5F0E0; }
            .whitecard .card-header { border-width:0px; background-color:#FFFFFF; }
            .section-header { margin-bottom:4px; }
            .table-paste-hint { font-size:0.84em; color:#888; margin-top:10px; }
            /* Simulation sliders: label on the left, slider on the right (no overlap) */
            .sim-sidebar { background:#faf6ef; }
            .sim-slider-legend { font-size:0.95em; margin-bottom:10px; padding-bottom:8px; border-bottom:2px solid #e5d9c3; }
            .sim-slider-row { display:flex; align-items:center; gap:8px; padding:7px 2px; border-bottom:1px solid #efe7d7; }
            .sim-slider-lab { flex:0 0 44%; font-size:0.86em; font-weight:600; line-height:1.15; }
            .sim-slider-inp { flex:1 1 56%; padding-top:14px; }
            .sim-slider-inp .irs-grid { display:none !important; }
            .sim-slider-inp .irs-min, .sim-slider-inp .irs-max { display:none !important; }
            .sim-slider-inp .irs { height:24px !important; }
            .sim-slider-inp .irs-line, .sim-slider-inp .irs-bar { top:14px !important; }
            .sim-slider-inp .irs-handle { top:6px !important; }
            .sim-slider-inp .irs-single, .sim-slider-inp .irs-from, .sim-slider-inp .irs-to { top:-8px !important; }
            .sim-slider-inp .form-group, .sim-slider-inp .shiny-input-container { margin-bottom:0 !important; }
            /* Bigger, friendlier data tables (jspreadsheet / excelR) */
            .big-table .jexcel > tbody > tr > td { height:34px !important; font-size:0.95em !important; padding:6px 10px !important; }
            .big-table .jexcel > thead > tr > td { height:38px !important; font-size:0.92em !important; font-weight:600 !important; background:#f5f0e0 !important; }
            .jexcel > tbody > tr > td { padding:5px 8px; }
            /* Per-zone coloring for crop planting table headers (3 cols per zone) */
            #crop_planting_tbl .jexcel > thead > tr > td:nth-child(2),
            #crop_planting_tbl .jexcel > thead > tr > td:nth-child(3),
            #crop_planting_tbl .jexcel > thead > tr > td:nth-child(4)  { background:#d7ecd9 !important; color:#1b5e20 !important; }
            #crop_planting_tbl .jexcel > thead > tr > td:nth-child(5),
            #crop_planting_tbl .jexcel > thead > tr > td:nth-child(6),
            #crop_planting_tbl .jexcel > thead > tr > td:nth-child(7)  { background:#fde8d0 !important; color:#8B3E04 !important; }
            #crop_planting_tbl .jexcel > thead > tr > td:nth-child(8),
            #crop_planting_tbl .jexcel > thead > tr > td:nth-child(9),
            #crop_planting_tbl .jexcel > thead > tr > td:nth-child(10) { background:#d4e4f7 !important; color:#1a4d80 !important; }
            #crop_planting_tbl .jexcel > thead > tr > td:nth-child(11),
            #crop_planting_tbl .jexcel > thead > tr > td:nth-child(12),
            #crop_planting_tbl .jexcel > thead > tr > td:nth-child(13) { background:#f7d4e4 !important; color:#80104d !important; }
            /* Give all array/data tables room so they aren't cramped */
            .jexcel_container { width:100% !important; }
            .jexcel > tbody > tr > td { min-width:70px; height:30px; }
            .jexcel > thead > tr > td { min-width:70px; }
            .ReactTable .rt-td, .rt-td { font-size:0.92em; padding:8px 10px !important; }
            .home {
              background-color:#8B3E04;
              background-image:url('images/wanulcas_diagram.png');
              background-repeat:no-repeat; background-position:left center; background-size:auto 100%;
              height:100%; padding:50px 60px 50px 50px; color:#fff; text-shadow:2px 2px 6px rgba(0,0,0,0.5);
              overflow-y:auto;
            }
            .home-grid { display:grid; grid-template-columns:repeat(auto-fill, minmax(190px, 1fr)); gap:14px; margin-top:14px; }
            .home-big-btn { border:none !important; border-radius:10px !important; padding:14px 26px !important; font-size:1.05em !important; font-weight:700 !important; box-shadow:0 3px 10px rgba(0,0,0,0.25); }
            .home-big-btn:hover { transform:translateY(-2px); box-shadow:0 5px 16px rgba(0,0,0,0.3); }
            .home-module-card { background:rgba(255,255,255,0.96); border-radius:12px; padding:16px; text-align:center; color:#222; text-shadow:none; box-shadow:0 2px 12px rgba(0,0,0,0.15); transition:transform .15s ease, box-shadow .15s ease; cursor:pointer; }
            .home-module-card:hover { transform:translateY(-3px); box-shadow:0 6px 20px rgba(0,0,0,0.2); }
            .home-module-icon { width:52px; height:52px; border-radius:50%; display:flex; align-items:center; justify-content:center; margin:0 auto 8px; }
            .home-core-badge { display:inline-block; background:#8B3E04; color:white; font-size:0.7em; font-weight:700; padding:2px 8px; border-radius:20px; margin-bottom:8px; text-shadow:none; }
            .home-add-badge { display:inline-block; background:#61701f; color:white; font-size:0.7em; font-weight:700; padding:2px 8px; border-radius:20px; margin-bottom:8px; text-shadow:none; }
            .jexcel > tbody > tr > td.readonly { color:#cc3d00; font-weight:bold; }
            .reactable-text-input { max-width:80px; }
            .compact_button { width:auto; height:36px; padding:5px 20px; }
            .selectize-dropdown { z-index:999999 !important; }
")
        ),
        tags$script(src = "jspreadsheet.js"),
        tags$link(rel = "stylesheet", href = "jspreadsheet.css", type = "text/css"),
        tags$script(src = "jsuites.js"),
        tags$link(rel = "stylesheet", href = "jsuites.css", type = "text/css"),
        tags$link(rel = "stylesheet", href = "table.css", type = "text/css")
      ),
    window_title = "WaNuLCAS 5.0",
    title =
      tags$b(
        tags$img(height = 22, src = "images/wanulcas_logo.svg", style = "margin-right:5px;"),
        "WaNuLCAS",
        span("5.0", style = "color:#FA842B;")
      ),
    padding = 0,

    home_panel,

    nav_panel(
      title = "Core Parameters",
      icon = icon("star"),
      do.call(navset_card_tab, c(list(id = "core_panel"), input_tab("Core")))
    ),

    nav_panel(
      title = "Additional Parameters",
      value = "Additional Parameters",
      icon = icon("plus"),
      do.call(navset_card_tab, c(list(id = "add_panel"), input_tab("Additional")))
    ),

    nav_panel(
      title = "Simulation",
      icon = icon("gears"),
      card_body(
        padding = 0,
        layout_sidebar(
          fillable = TRUE,
          sidebar = sidebar(
            width = 360, class = "sim-sidebar", open = "open",
            sim_controls_ui()
          ),
        div(
          style = "margin:16px 0 0 20px",
          flowLayout(
            cellArgs = list(style = "width:auto; margin:0px; height:30px;"),
            div("Simulation Time (days):", style = "padding:5px 0;font-weight:bold"),
            numericInput("n_iteration", NULL, value = 50, width = "150px"),
            input_task_button(
              "sim_run_button",
              "Run Simulation",
              icon = icon("play"),
              style = compact_button_style
            ),
            conditionalPanel(
              condition = "output.is_sim_output",
              actionButton(
                "reset_button",
                "Reset output log variables",
                icon = icon("arrows-rotate"),
                style = compact_button_style
              ),
              downloadButton("download_output", "Download output data", style = compact_button_style)
            )
          )
        ),
        card_body(
          class = "bordercard",
          padding = 0,
          height = "100%",
          fillable = F,
          conditionalPanel(condition = "output.is_sim_output", navset_card_tab(
            nav_panel(
              title = "Time series output",
              card_body(
                class = "whitecard",
                padding = 0,
                uiOutput("sim_output_ui")
              )
            ),
            nav_panel(
              title = "Final value output",
              card_body(
                class = "whitecard",
                padding = 0,
                uiOutput("sim_output_final_ui")
              )
            )
          )),
          conditionalPanel(
            condition = "!output.is_sim_output",
            card_body(
              padding = 10,
              height = "100%",
              div("Please select the output variables below"),
              layout_column_wrap(
                card(
                  card_header(
                    "Time series output variables",
                    div(
                      actionButton(
                        "clear_selected_output_vars",
                        "Clear selections",
                        icon = icon("square"),
                        class = "btn-sm"
                      ),
                      actionButton(
                        "reset_default_output_vars",
                        "Reset to default",
                        icon = icon("arrows-rotate"),
                        class = "btn-sm"
                      )
                    ),
                    class = "d-flex justify-content-between"
                  ),
                  layout_column_wrap(
                    style = css(grid_template_columns = "2fr 1fr"),
                    width = NULL,
                    reactableOutput("output_var_selector"),
                    card(
                      class = "whitecard",
                      card_header("Selected variables:", uiOutput("selected_vars_info")),
                      uiOutput("output_var_selected")
                    )
                  ),
                ),
                card(
                  card_header(
                    "Final value output variables",
                    div(
                      actionButton(
                        "clear_selected_output_final_vars",
                        "Clear selections",
                        icon = icon("square"),
                        class = "btn-sm"
                      ),
                      actionButton(
                        "reset_default_output_final_vars",
                        "Reset to default",
                        icon = icon("arrows-rotate"),
                        class = "btn-sm"
                      )
                    ),
                    class = "d-flex justify-content-between"
                  ),
                  layout_column_wrap(
                    style = css(grid_template_columns = "2fr 1fr"),
                    width = NULL,
                    reactableOutput("output_final_var_selector"),
                    card(
                      class = "whitecard",
                      card_header("Selected variables:", uiOutput("selected_final_vars_info")),
                      uiOutput("output_final_var_selected")
                    )
                  ),
                )
                
              )
            )
          )
        )
        )
      )
    ),

    nav_panel(
      title = "",
      value = "help",
      icon = bs_icon("question-circle", size = "1.3em"),
      navset_card_tab(
        id = "info_panel",
        nav_panel(
          title = "About",
          icon = icon("circle-info"),
          card_body(includeMarkdown("docs/about.md"))
        ),
        nav_panel(
          title = "Background",
          icon = icon("book"),
          card_body(includeMarkdown("docs/background.md"))
        ),
        nav_panel(
          title = "Overview",
          icon = icon("book"),
          card_body(includeMarkdown("docs/overview.md"))
        ),
        nav_panel(
          title = "Tutorial",
          icon = icon("book"),
          card_body(includeMarkdown("docs/manual.md"))
        ),
        nav_panel(
          title = "Technical Notes",
          icon = icon("screwdriver-wrench"),
          card_body(includeMarkdown("docs/w_notes.md"))
        )
      )
    ),

    nav_spacer(),
    nav_menu(
      title = "Options",
      icon = icon("ellipsis-vertical"),
      align = "right",
      nav_item(
        style = "margin: 0 20px",
        fileInput(
          "upload_parameter",
          span(icon("upload"), "Upload input parameter file"),
          accept = c("application/yaml", ".yaml", ".yml"),
          width = "300px"
        )
      ),
      nav_item(style = "border-top: 2px dashed lightgray; margin:10px 20px"),
      nav_item(
        style = "margin: 0 20px",
        fileInput(
          "upload_xls_parameter",
          span(
            icon("file-excel"),
            "Import and apply MS-Excel parameter file from earlier version of WaNuLCAS"
          ),
          accept = c("application/vnd.ms-excel", ".xlsx", ".xls", ".xlsm"),
          width = "300px"
        )
      ),
      nav_item(style = "border-top: 2px dashed lightgray; margin:10px 20px"),
      nav_item(span(
        icon("download"),
        downloadLink("download_parameter", "Download and save parameters"),
        style = "margin:0 20px"
      )),
      nav_item(span(
        icon("file-excel"),
        downloadLink("download_xlsm_template", "Download xlsm template"),
        style = "margin:0 20px"
      )),
      nav_item(span(
        icon("file-pdf"),
        downloadLink("download_params_pdf", "Download input parameter description (PDF)"),
        style = "margin:0 20px"
      )),
      nav_item(span(
        icon("file-pdf"),
        downloadLink("download_output_params_pdf", "Download output parameter description (PDF)"),
        style = "margin:0 20px"
      ))
    )
  )
