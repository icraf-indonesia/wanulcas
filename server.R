# Rahmat
# last revision: 2 September 2026 -- Hasna



server <- function(input, output, session) {
  options(shiny.maxRequestSize = 1000 * 1024^2)
  
  options(
    reactable.theme = reactableTheme(
      style = list(fontFamily = "Arial, Helvetica, sans-serif", fontSize = "1em"),
      stripedColor = "#FBFAF4",
      highlightColor = "#F5F0E0"
    )
  )
  
  data_dir <- paste0(tempdir(), "/data_temp")
  
  #not working for vector of number
  f_number <- function(v, ...) {
    format(v, big.mark = ",", scientific = F, ...)
  }
  
  f_percent <- function(v) {
    sprintf("%0.1f%%", v * 100)
  }
  
  ### reactiveValues #############################################
  
  rv_var <- do.call(reactiveValues, wanulcas_params_def$vars)
  rv_arr <- do.call(reactiveValues, arr_inp)
  rv_graph <- do.call(reactiveValues, graph_inp)
  
  rv <- reactiveValues(
    wanulcas_cfg = list(),
    sim_output = NULL,
    output_timeseries_vars = default_output_timeseries_vars,
    output_final_vars = default_output_final_vars,
    output_graph_cfg = wanulcas_params_def$output$timeseries_layout
  )
  
  ### conditional variables #####################################
  
  conditional_id <-
    c("is_sim_output")
  conditional_v <-
    c("sim_output")
  
  mapply(function(id, val) {
    output[[id]] <- reactive({
      type <- suffix(val)
      if (is.null(rv[[val]])) {
        return(FALSE)
      }
      if (type == "df") {
        if (nrow(rv[[val]]) == 0)
          return(F)
      }
      TRUE
    })
    outputOptions(output, id, suspendWhenHidden = FALSE)
  }, conditional_id, conditional_v)
  
  ### INPUT PARAMETES AND DATA
  
  ### vars input UI ######################
  inputvars_ui_id <- unique(inputvars_df$ui_id)
  
  rv_var_edit <- reactiveValues()
  numeric_var <- reactive({
    numeric_var <- lapply(inputvars_ui_id, function(x) {
      var_ids <- inputvars_df[inputvars_df$ui_id == x, "var"]
      rv_var_edit[[x]] <- numeric_input_server(x, var_ids)
    })
  })
  
  observe(numeric_var())
  
  lapply(inputvars_ui_id, function(x) {
    observe({
      inp <- rv_var_edit[[x]]()
      lapply(names(inp), function(x) {
        rv_var[[x]] <- inp[[x]]
      })
    })
  })
  
  ### array input UI ######################
  
  rv_arr_edit <- reactiveValues()

  # Zonelayer fragment IDs eligible for Layer×Zone matrix display.
  # Strictly limited to the plain 16-row zonelayer_df (not tree/nut/pcomp variants).
  .zl_ids <- arr_ids_df$ui_id[arr_ids_df$arr == "zonelayer_df"]

  # Display-only decimal rounding (revision #9): 2 dp everywhere, 4 dp on the
  # Nitrogen tab (id 26). This affects ONLY what the user sees in the table;
  # the values stored in rv_arr / sent to the engine keep full precision.
  .nitrogen_ids <- arr_ids_df$ui_id[arr_ids_df$id == 26]
  round_for_display <- function(df, ui_id) {
    if (is.null(df) || !is.data.frame(df)) return(df)
    dp <- if (ui_id %in% .nitrogen_ids) 4L else 2L
    for (cn in names(df)) {
      if (is.numeric(df[[cn]])) df[[cn]] <- round(df[[cn]], dp)
    }
    df
  }

  # Oil Palm Library fragments (tab 4) to show TRANSPOSED: properties as rows,
  # tree_id (species) as columns. Covers tree / treefruit / fruit subtypes.
  .op_ids <- arr_ids_df$ui_id[arr_ids_df$id == 4 &
    arr_ids_df$subtype %in% c("tree", "treefruit", "fruit")]

  react_arr <- reactive({
    react_arr <- lapply(names(arr_inp), function(x) {
      nkeys <- length(arr_conf[[x]]$keys)
      nvar <- length(arr_conf[[x]]$title_desc) - nkeys

      # Decide whether this fragment can be shown as a 4×4 matrix
      base_df <- isolate(rv_arr[[x]])
      can_reshape <- x %in% .zl_ids && nvar > 0 &&
        !is.null(base_df) && nrow(base_df) == 16L
      can_transpose <- x %in% .op_ids && nvar > 0 && !is.null(base_df)

      if (can_transpose) {
        key_names <- arr_conf[[x]]$keys
        has_tree <- "tree_id" %in% key_names
        subkeys  <- setdiff(key_names, "tree_id")   # e.g. Fruitbunch (may be empty)
        tr_reactive <- reactive({
          df <- rv_arr[[x]]
          tryCatch({
            if (is.null(df) || nrow(df) == 0) stop("empty")
            var_names <- setdiff(names(df), key_names)
            if (length(subkeys) == 0) {
              # ---- single-key (tree_id only): properties = rows, Tree = columns ----
              tree_ids <- df[["tree_id"]]
              out <- data.frame(Property = var_names, stringsAsFactors = FALSE)
              for (i in seq_len(nrow(df))) {
                out[[paste0("Tree ", tree_ids[i])]] <- as.numeric(df[i, var_names])
              }
            } else {
              # ---- multi-key (tree_id + subkey): rows = Property x subkey,
              #      columns = Tree n. (matches the reference "ripe" tables) ----
              sub_col <- subkeys[1]
              sub_levels <- unique(df[[sub_col]])
              tree_levels <- if (has_tree) sort(unique(df[["tree_id"]])) else 1
              out <- data.frame(
                Property = rep(var_names, each = length(sub_levels)),
                stringsAsFactors = FALSE)
              out[[sub_col]] <- rep(sub_levels, times = length(var_names))
              for (tid in tree_levels) {
                colname <- paste0("Tree ", tid)
                vals <- numeric(nrow(out))
                for (r in seq_len(nrow(out))) {
                  v <- out$Property[r]; sk <- out[[sub_col]][r]
                  sel <- df[[sub_col]] == sk & (if (has_tree) df[["tree_id"]] == tid else TRUE)
                  vals[r] <- if (any(sel)) as.numeric(df[[v]][which(sel)[1]]) else NA
                }
                out[[colname]] <- vals
              }
            }
            round_for_display(out, x)
          }, error = function(e) df)
        })
        # column count varies; disable Property (+ subkey) columns
        nlead <- 1L + length(subkeys)
        ntree_cols <- if ("tree_id" %in% key_names)
          length(unique(isolate(rv_arr[[x]])[["tree_id"]])) else 1L
        rv_arr_edit[[x]] <- table_edit_server(
          x, tr_reactive,
          col_disable = c(rep(TRUE, nlead), rep(FALSE, ntree_cols)),
          col_type    = c(rep(NA, nlead), rep("numeric", ntree_cols))
        )
      } else if (can_reshape) {
        zl_reactive <- reactive({
          df <- rv_arr[[x]]
          out <- tryCatch({
            if (is.null(df) || nrow(df) != 16L) stop("not 16 rows")
            key_names <- arr_conf[[x]]$keys           # char vector of key names
            var_names <- setdiff(names(df), key_names)
            res <- data.frame(Layer = 1:4)
            for (v in var_names) {
              vals <- as.numeric(df[[v]])
              if (length(vals) == 16L) {
                mat <- matrix(vals, nrow = 4, ncol = 4, byrow = TRUE)
                for (z in 1:4) res[[paste0(v, "_Z", z)]] <- mat[, z]
              }
            }
            round_for_display(res, x)
          }, error = function(e) df)          # fall back to raw on any error
          out
        })
        rv_arr_edit[[x]] <- table_edit_server(
          x, zl_reactive,
          col_disable = c(TRUE, rep(FALSE, nvar * 4L)),
          col_type    = c(NA, rep("numeric", nvar * 4L))
        )
      } else {
        rv_arr_edit[[x]] <- table_edit_server(
          x,
          reactive(round_for_display(rv_arr[[x]], x)),
          col_title = arr_conf[[x]]$title_desc,
          col_disable = c(rep(T, nkeys), rep(F, nvar)),
          col_type = c(rep(NA, nkeys), rep("numeric", nvar))
        )
      }
    })
  })
  
  observe(react_arr())
  
  lapply(names(arr_inp), function(x) {
    observe({
      df <- rv_arr_edit[[x]]()
      if (!is.null(df)) {
        # If this is a reshaped zonelayer matrix, flatten back before storing
        is_zl_matrix <- x %in% .zl_ids &&
          any(grepl("_Z[1-4]$", names(df))) && nrow(df) == 4L
        # If this is a transposed Oil Palm table, un-transpose before storing
        is_op_transpose <- x %in% .op_ids && "Property" %in% names(df)
        if (is_zl_matrix) {
          tryCatch({
            orig <- isolate(rv_arr[[x]])
            if (!is.null(orig) && nrow(orig) == 16L) {
              key_names <- arr_conf[[x]]$keys
              var_names <- setdiff(names(orig), key_names)
              for (v in var_names) {
                zcols <- paste0(v, "_Z", 1:4)
                if (all(zcols %in% names(df))) {
                  mat <- as.matrix(df[, zcols, drop = FALSE])
                  orig[[v]] <- as.vector(t(mat))   # zone varies fastest
                }
              }
              rv_arr[[x]] <- orig
            }
          }, error = function(e) NULL)
        } else if (is_op_transpose) {
          tryCatch({
            orig <- isolate(rv_arr[[x]])
            if (!is.null(orig)) {
              key_names <- arr_conf[[x]]$keys
              var_names <- setdiff(names(orig), key_names)
              subkeys <- setdiff(key_names, "tree_id")
              has_tree <- "tree_id" %in% key_names
              dp <- if (x %in% .nitrogen_ids) 4L else 2L
              if (length(subkeys) == 0) {
                # single-key: columns "Tree n" = rows of orig
                tree_ids <- orig[["tree_id"]]
                for (vi in seq_along(var_names)) {
                  v <- var_names[vi]; prow <- which(df$Property == v)
                  if (length(prow) == 1) {
                    for (i in seq_len(nrow(orig))) {
                      colname <- paste0("Tree ", tree_ids[i])
                      if (colname %in% names(df)) {
                        newv <- as.numeric(df[[colname]][prow])
                        if (is.na(newv) || round(as.numeric(orig[[v]][i]), dp) != newv) {
                          orig[[v]][i] <- newv
                        }
                      }
                    }
                  }
                }
              } else {
                # multi-key: match on Property + subkey + Tree column
                sub_col <- subkeys[1]
                for (r in seq_len(nrow(df))) {
                  v  <- df$Property[r]; sk <- df[[sub_col]][r]
                  if (!(v %in% var_names)) next
                  for (colname in grep("^Tree ", names(df), value = TRUE)) {
                    tid <- as.numeric(sub("^Tree ", "", colname))
                    sel <- orig[[sub_col]] == sk &
                           (if (has_tree) orig[["tree_id"]] == tid else TRUE)
                    if (any(sel)) {
                      newv <- as.numeric(df[[colname]][r])
                      idx <- which(sel)[1]
                      if (is.na(newv) || round(as.numeric(orig[[v]][idx]), dp) != newv) {
                        orig[[v]][idx] <- newv
                      }
                    }
                  }
                }
              }
              rv_arr[[x]] <- orig
            }
          }, error = function(e) NULL)
        } else {
          # Preserve full precision on cells the user didn't actually edit:
          # the display is rounded, so only overwrite a value when it differs
          # from the rounded stored value (i.e. a genuine edit).
          orig <- isolate(rv_arr[[x]])
          if (is.null(orig) || !identical(dim(orig), dim(df)) ||
              !identical(names(orig), names(df))) {
            rv_arr[[x]] <- df
          } else {
            dp <- if (x %in% .nitrogen_ids) 4L else 2L
            merged <- orig
            for (cn in names(df)) {
              if (is.numeric(orig[[cn]]) && is.numeric(df[[cn]])) {
                changed <- round(as.numeric(orig[[cn]]), dp) != as.numeric(df[[cn]])
                changed[is.na(changed)] <- TRUE
                merged[[cn]][changed] <- as.numeric(df[[cn]])[changed]
              } else {
                merged[[cn]] <- df[[cn]]
              }
            }
            rv_arr[[x]] <- merged
          }
        }
      }
    })
  })
  
  ### graph input UI ######################
  
  rv_graph_edit <- reactiveValues()
  react_graph <- reactive({
    react_graph <- lapply(names(graph_inp), function(x) {
      rv_graph_edit[[x]] <- table_edit_server(
        x,
        reactive(rv_graph[[x]]),
        allowRowModif = T,
        nrow = NA,
        col_type = rep("numeric", length(rv_graph[[x]]))
      )
    })
  })
  
  observe(react_graph())
  
  lapply(names(graph_inp), function(x) {
    observe({
      df <- rv_graph_edit[[x]]()
      if (!is.null(df)) {
        rv_graph[[x]] <- df
      }
    })
  })
  
  ### Combined planting-schedule tables (visual only) #############
  # ONE editable table per page, backed by the excelR widget so Excel
  # copy/paste of multiple rows works. Reads/writes the SAME rv_graph
  # values the engine already uses; engine/param flow unchanged.
  planting_table_setup <- function(id, year_var, doy_var, item_label,
                                   x_label = "Planting Event") {
    yk <- graph_subvars[[year_var]]                     # simfix
    dk <- graph_subvars[[doy_var]]                      # simfix
    if (is.null(yk) || is.null(dk) || length(yk) == 0) return(invisible())
    n <- length(yk)
    subvars <- as.vector(rbind(yk, dk))                 # Year1, DoY1, Year2, DoY2, ...
    labels  <- as.vector(rbind(paste0("PlantY ",   item_label, seq_len(n)),
                               paste0("PlantDoY ", item_label, seq_len(n))))

    combined <- reactive({
      base <- rv_graph[[subvars[1]]]
      if (is.null(base) || nrow(base) == 0) return(NULL)
      df <- data.frame(base[[1]])
      names(df)[1] <- x_label
      for (i in seq_along(subvars)) {
        g <- rv_graph[[subvars[i]]]
        df[[labels[i]]] <- if (!is.null(g)) g[[2]] else NA
      }
      df
    })

    edited <- table_edit_server(
      id, combined,
      allowRowModif = F,
      col_type    = rep("numeric", length(subvars) + 1L),
      col_disable = c(TRUE, rep(FALSE, length(subvars))),   # x column read-only
      col_width   = c(130, rep(95, length(subvars)))
    )

    observeEvent(edited(), {
      df <- edited()
      if (is.null(df)) return()
      for (i in seq_along(subvars)) {
        col <- df[[labels[i]]]
        if (is.null(col)) next
        newy <- suppressWarnings(as.numeric(col))
        g <- rv_graph[[subvars[i]]]
        if (is.null(g)) next
        # only write when the column actually changed (prevents feedback loops)
        if (length(newy) == nrow(g) && !isTRUE(all.equal(as.numeric(g[[2]]), newy))) {
          g[[2]] <- newy
          rv_graph[[subvars[i]]] <- g
        }
      }
    })
  }

  planting_table_setup("tree_planting_tbl", "T_PlantY", "T_PlantDoY", "Sp")

  # Crop planting table: merge PlantY, PlantDoY AND CQ CropType per zone,
  # grouped and colored by zone (revision #7).
  crop_planting_setup <- function(id) {
    yk <- graph_subvars[["CA_PlantYear"]]
    dk <- graph_subvars[["CA_PlantDoY"]]
    ck <- graph_subvars[["CQ_CropType"]]
    if (is.null(yk) || is.null(dk) || length(yk) == 0) return(invisible())
    n <- length(yk)
    # per zone: Year, DoY, CropType
    subvars <- character(0); labels <- character(0); zone_of <- integer(0)
    for (z in seq_len(n)) {
      trip <- c(yk[z], dk[z], if (!is.null(ck) && length(ck) >= z) ck[z] else NA)
      lab  <- c(paste0("PlantY Z", z), paste0("PlantDoY Z", z), paste0("CQ CropType Z", z))
      subvars <- c(subvars, trip)
      labels  <- c(labels, lab)
      zone_of <- c(zone_of, rep(z, 3))
    }
    keep <- !is.na(subvars)
    subvars <- subvars[keep]; labels <- labels[keep]; zone_of <- zone_of[keep]

    combined <- reactive({
      base <- rv_graph[[subvars[1]]]
      if (is.null(base) || nrow(base) == 0) return(NULL)
      df <- data.frame(base[[1]]); names(df)[1] <- "Planting Event"
      for (i in seq_along(subvars)) {
        g <- rv_graph[[subvars[i]]]
        df[[labels[i]]] <- if (!is.null(g)) g[[2]] else NA
      }
      df
    })

    edited <- table_edit_server(
      id, combined,
      allowRowModif = F,
      col_type    = rep("numeric", length(subvars) + 1L),
      col_disable = c(TRUE, rep(FALSE, length(subvars))),
      col_width   = c(130, rep(100, length(subvars)))
    )

    observeEvent(edited(), {
      df <- edited()
      if (is.null(df)) return()
      for (i in seq_along(subvars)) {
        col <- df[[labels[i]]]
        if (is.null(col)) next
        newy <- suppressWarnings(as.numeric(col))
        g <- rv_graph[[subvars[i]]]
        if (is.null(g)) next
        if (length(newy) == nrow(g) && !isTRUE(all.equal(as.numeric(g[[2]]), newy))) {
          g[[2]] <- newy
          rv_graph[[subvars[i]]] <- g
        }
      }
    })
  }
  crop_planting_setup("crop_planting_tbl")

  generate_graph_plot <- function(var) {
    g_ids <- graph_subvars[[var]]
    if (is.null(g_ids))
      return(NULL)
    
    desc <- input_vars_conf_df[input_vars_conf_df$var == var, "var_desc"]
    fig <- plot_ly()
    for (g_id in g_ids) {
      df <- rv_graph[[g_id]]
      
      fig <- fig |> add_trace(
        x = df[[1]],
        y = df[[2]],
        type = "scatter",
        mode = "lines+markers",
        name = names(df)[2]
      )
    }
    fig <- fig |> plotly::layout(
      legend = list(orientation = 'h'),
      showlegend = T,
      title = desc,
      yaxis = list(title = var),
      xaxis = list(title = wanulcas_params_def$graphs[[var]]$x_var),
      hoverlabel = list(namelength = -1)
    )
    
    fig <- fig |> plotly::layout(
      xaxis = list(showgrid = F),
      yaxis = list(title = ""),
      margin = list(l = 0),
      showlegend = F,
      title = ""
    )
    fig <- fig |> plotly::config(displayModeBar = FALSE)
    # }
    return(fig)
  }
  
  lapply(graph_vars, function(x) {
    gp_id <- paste("input_graph_plot", x, sep = "-")
    output[[gp_id]] <- renderPlotly(generate_graph_plot(x))
  })
  
  with_tooltip <- function(tooltip_col) {
    JS(
      sprintf(
        'function(cellInfo) {
    const style = "cursor: help"
    const title = cellInfo.row["%s"]
    return `<span style="${style}" title="${title}">${cellInfo.value}</span>`
   }',
        tooltip_col
      )
    )
  }
  
  #### crop species library ###############
  
  # crop_ui_id <- "input_array_crop_7_0"   # simfix (was: hardcoded old tab id)
  # tree_ui_id <- "input_array_tree_8_0"   # simfix (was: hardcoded old tab id)
  # Derive the array-widget id from wherever the species-selector variable now
  # lives, so the tree/crop selectors survive any config restructure.
  .sel_ui_id <- function(sel_var, subtype) {                          # simfix
    r <- input_vars_conf_df[input_vars_conf_df$var == sel_var, ]      # simfix
    paste("input_array", subtype, r$id[1], r$group_id[1], sep = "_")  # simfix
  }                                                                   # simfix
  crop_ui_id <- .sel_ui_id("CQ_Species",     "crop")                  # simfix
  tree_ui_id <- .sel_ui_id("T_Species",      "tree")                  # simfix
  nut_ui_id  <- .sel_ui_id("AF_RunNutLim_is", "nut")                  # simfix

  # read.csv mangles species headers with spaces/hyphens into dots.         # simfix
  # The engine matches the RAW name, so keep a lookup back to it.            # simfix
  .species_raw_map <- function(csv) {                                       # simfix
    raw <- tryCatch(names(read.csv(csv, check.names = FALSE, nrows = 1)),
                    error = function(e) character(0))
    stats::setNames(raw, make.names(raw))
  }
  tree_name_map <- .species_raw_map("config/tree_species.csv")              # simfix
  crop_name_map <- .species_raw_map("config/crop_species.csv")              # simfix
  .to_raw <- function(x, map) {                                             # simfix
    x <- as.character(x)
    ifelse(x %in% names(map), unname(map[x]), x)
  }
  
  get_crop_list <- function() {
    c(user_crop(), crop_species_col)
  }
  
  crop_select_ids <- c("input_crop_1",
                       "input_crop_2",
                       "input_crop_3",
                       "input_crop_4",
                       "input_crop_5")
  
  output$input_crop_select <- renderUI({
    crop_list <- get_crop_list()
    cr_select <- rv_arr[[crop_ui_id]]$CQ_Species
    
    # Safe lapply to avoid zero-length errors
    crop_ui <- lapply(c(1:5), function(i) {
      selectInput(crop_select_ids[i],
                  paste0("Crop ", i, ":"),
                  crop_list,
                  selected = cr_select[i])
    })
    do.call(flowLayout, c(list(
      cellArgs = list(style = "width:200px; margin:0px;")
    ), crop_ui))
  })
  
  lapply(c(1:5), function(i) {
    observeEvent(input[[crop_select_ids[i]]], {
      rv_arr[[crop_ui_id]]$CQ_Species[i] <- input[[crop_select_ids[i]]]
    })
  })
  
  output$input_crop_lib <- renderReactable({
    edit_crop <- user_crop()
    edit_col <- NULL
    if (length(edit_crop) > 0) {
      edit_col <- lapply(edit_crop, function(x) {
        colDef(
          cell = text_extra("crop_edit", class = "reactable-text-input"),
          headerStyle = list(
            background = theme_color$primary,
            color = "#FFF"
          )
        )
      })
      names(edit_col) <- edit_crop
    }
    hpar <- list(color = theme_color$primary)
    reactable(
      crop_species_df[c(crop_key_col, edit_crop , crop_species_col)],
      highlight = T,
      compact = T,
      striped = T,
      pagination = F,
      groupBy = "group",
      columns = c(
        list(
          group = colDef(
            name = "Categories",
            width = 240,
            style = list(color = theme_color$primary),
            headerStyle = hpar
          ),
          var_desc = colDef(show = F),
          var_label = colDef(
            name = "Parameters",
            width = 240,
            style = list(color = theme_color$primary),
            headerStyle = hpar,
            html = TRUE,
            cell = with_tooltip("var_desc")
          ),
          sub_var = colDef(
            name = "Att",
            style = list(color = theme_color$primary, width = 30),
            headerStyle = hpar
          )
        ),
        edit_col
      )
    )
  })
  
  observeEvent(input$crop_edit, {
    i <- input$crop_edit
    crop_species_df[i$row, i$column] <<- as.numeric(i$value)
  })
  
  observeEvent(input$add_crop_button, {
    show_input_dialog(
      "Add New Crop Type",
      "Select the base crop parameter from the available library and define the crop name",
      "confirm_add_crop",
      input_var = "input_crop_name",
      input_label = "New crop name:",
      custom_input = selectInput(
        "input_crop_def",
        "Base parameters:",
        crop_species_col
      )
    )
  })
  
  user_crop <- reactiveVal()
  
  observeEvent(input$confirm_add_crop, {
    removeModal()
    cn <- input$input_crop_name
    if (cn == "")
      return()
    crop_species_df[[cn]] <<- crop_species_df[[input$input_crop_def]]
    user_crop(c(user_crop(), cn))
  })
  
  observeEvent(input$remove_crop_button, {
    if (length(user_crop()) == 0)
      return()
    show_input_dialog(
      "Remove Crop",
      "",
      "confirm_remove_crop",
      custom_input = selectInput("removed_crop", "Select the crop to removed:", user_crop())
    )
  })
  
  observeEvent(input$confirm_remove_crop, {
    removeModal()
    rc <- input$removed_crop
    if (rc == "")
      return()
    crop_species_df[[rc]] <<- NULL
    uc <- user_crop()
    uc <-  uc[uc != rc]
    user_crop(uc)
  })
  
  observe({
    req(input$crop_edit_text)
    values <- input$crop_edit_text
  })
  
  #### tree species library ###############
  
  get_tree_list <- function() {
    c(user_tree(), tree_species_col)
  }
  
  tree_select_ids <- c("input_tree_1", "input_tree_2", "input_tree_3")
  
  output$input_tree_select <- renderUI({
    tree_list <- get_tree_list()
    tr_select <- rv_arr[[tree_ui_id]]$T_Species
    # Safe lapply to avoid zero-length errors
    tree_ui <- lapply(c(1:3), function(i) {
      selectInput(tree_select_ids[i],
                  paste0("Tree ", i, ":"),
                  tree_list,
                  selected = tr_select[i])
    })
    do.call(flowLayout, c(list(
      cellArgs = list(style = "width:300px; margin:0px;")
    ), tree_ui))
  })
  
  lapply(c(1:3), function(i) {
    observeEvent(input[[tree_select_ids[i]]], {
      rv_arr[[tree_ui_id]]$T_Species[i] <- input[[tree_select_ids[i]]]
    })
  })
  
  output$input_tree_lib <- renderReactable({
    edit_tree <- user_tree()
    
    edit_col <- NULL
    if (length(edit_tree) > 0) {
      edit_col <- lapply(edit_tree, function(x) {
        colDef(
          cell = text_extra("tree_edit", class = "reactable-text-input"),
          headerStyle = list(
            background = theme_color$primary,
            color = "#FFF"
          )
        )
      })
      names(edit_col) <- edit_tree
    }
    hpar <- list(color = theme_color$primary)
    reactable(
      tree_species_df[c(tree_key_col, edit_tree , tree_species_col)],
      highlight = T,
      compact = T,
      striped = T,
      pagination = F,
      groupBy = "group",
      columns = c(
        list(
          group = colDef(
            name = "Categories",
            width = 300,
            style = list(color = theme_color$primary),
            headerStyle = hpar
          ),
          var_desc = colDef(show = F),
          var_label = colDef(
            name = "Parameters",
            width = 300,
            style = list(color = theme_color$primary),
            headerStyle = hpar,
            html = TRUE,
            cell = with_tooltip("var_desc")
          ),
          sub_var = colDef(
            name = "Att",
            style = list(color = theme_color$primary, width = 30),
            headerStyle = hpar
          )
        ),
        edit_col
      )
    )
  })
  
  observeEvent(input$tree_edit, {
    i <- input$tree_edit
    tree_species_df[i$row, i$column] <<- as.numeric(i$value)
  })
  
  observeEvent(input$add_tree_button, {
    show_input_dialog(
      "Add New tree Type",
      "Select the base tree parameter from the available library and define the tree name",
      "confirm_add_tree",
      input_var = "input_tree_name",
      input_label = "New tree name:",
      custom_input = selectInput(
        "input_tree_def",
        "Base parameters:",
        tree_species_col
      )
    )
  })
  
  user_tree <- reactiveVal()
  
  observeEvent(input$confirm_add_tree, {
    removeModal()
    cn <- input$input_tree_name
    if (cn == "")
      return()
    tree_species_df[[cn]] <<- tree_species_df[[input$input_tree_def]]
    user_tree(c(user_tree(), cn))
  })
  
  observeEvent(input$remove_tree_button, {
    if (length(user_tree()) == 0)
      return()
    show_input_dialog(
      "Remove tree",
      "",
      "confirm_remove_tree",
      custom_input = selectInput("removed_tree", "Select the tree to removed:", user_tree())
    )
  })
  
  observeEvent(input$confirm_remove_tree, {
    removeModal()
    rc <- input$removed_tree
    if (rc == "")
      return()
    tree_species_df[[rc]] <<- NULL
    uc <- user_tree()
    uc <-  uc[uc != rc]
    user_tree(uc)
  })

  ### Run Simulation #############
  validate_crop <- function(input_crop) {
    ifelse(is.null(input_crop),
           c(user_crop(), crop_species_col)[1],
           input_crop)
  }
  
  validate_tree <- function(input_tree) {
    ifelse(is.null(input_tree),
           c(user_tree(), tree_species_col)[1],
           input_tree)
  }
  
  species_var_keys <- c("var", "sub_var")
  
  apply_species_params <- function(params) {
    # Resolve each selected species to a REAL column of the library, tolerating
    # the read.csv name mangling (spaces/hyphens become dots) and
    # falling back to a valid species rather than crashing on an unknown one.
    .valid_species <- function(sel, df) {                                  # simfix
      cols <- colnames(df)
      real <- setdiff(cols, c("var", "sub_var", "order"))
      if (is.null(sel) || length(sel) == 0)
        return(real[seq_len(min(3L, length(real)))])
      vapply(sel, function(s) {
        s <- as.character(s)
        if (!is.na(s) && s %in% cols) return(s)
        s2 <- make.names(s)
        if (s2 %in% cols) return(s2)
        real[1]                       # last-resort: first available species
      }, character(1), USE.NAMES = FALSE)
    }
    croplist <- .valid_species(rv_arr[[crop_ui_id]]$CQ_Species, crop_species_df)  # simfix
    treelist <- .valid_species(rv_arr[[tree_ui_id]]$T_Species,  tree_species_df)  # simfix
    croplist_df <- crop_species_df[c(species_var_keys, croplist)]
    treelist_df <- tree_species_df[c(species_var_keys, treelist)]
    params <- params |> apply_croplist_params(croplist_df) |> apply_treelist_params(treelist_df)
    # Hand the engine the RAW species names so name-based                  # simfix
    # oil-palm detection works; the UI keeps the read.csv (dotted) form.        # simfix
    if (!is.null(params$arrays$tree_df$vars$T_Species))                         # simfix
      params$arrays$tree_df$vars$T_Species  <- .to_raw(treelist, tree_name_map) # simfix
    if (!is.null(params$arrays$crop_df$vars$CQ_Species))                        # simfix
      params$arrays$crop_df$vars$CQ_Species <- .to_raw(croplist, crop_name_map) # simfix
    return(params)
  }
  
  get_input_parameters <- function() {
    params <- list()
    # vars
    # params$vars <- reactiveValuesToList(rv_var)                       # crashguard (was)
    params$vars <- sanitize_scalar_vars(reactiveValuesToList(rv_var),   # crashguard
                                        wanulcas_params_def$vars)       # crashguard
    # arrays
    v_arr <- reactiveValuesToList(rv_arr)
    params$arrays <- sapply(names(wanulcas_params_def$arrays), function(x) {
      key_cols <- names(wanulcas_def_arr[[x]])
      arrs <- v_arr[arr_ids_df[arr_ids_df$arr == x, "ui_id"]]
      names(arrs) <- NULL
      arr_df <- do.call(cbind, arrs)
      narr <- names(arr_df)
      narr <- narr[!narr %in% key_cols]
      list(keys = as.list(wanulcas_def_arr[[x]]),
           vars = as.list(arr_df[, narr, drop = FALSE]))
    }, simplify = F)
    # graphs
    v_graph <- reactiveValuesToList(rv_graph)
    params$graphs <- wanulcas_params_def$graphs
    xy <- lapply(names(wanulcas_params_def$graphs), function(x) {
      v <- v_graph[graph_subvars[[x]]]
      vg <- lapply(v, function(x) {
        list(x_val = x[[1]], y_val = x[[2]])
      })
      names(vg) <- names(params$graphs[[x]]$xy_data)
      vg
    })
    params$graphs <- mapply(function(a, b) {
      a$xy_data <- b
      a
    }, params$graphs, xy, SIMPLIFY = F)
    
    params <- apply_species_params(params)
    
    # species library
    edit_crop <- isolate(user_crop())
    if(length(edit_crop) > 0) {
      df <- crop_species_df[c(species_var_keys, edit_crop)]
      params[["crop_library"]] <- df
    }
    edit_tree <- isolate(user_tree())
    if(length(edit_tree) > 0) {
      df <- tree_species_df[c(species_var_keys, edit_tree)]
      params[["tree_library"]] <- df
    }
    return(params)
  }
  
  # sim_output <- reactiveVal()
  local_task <- reactiveVal()
  
  task <- ExtendedTask$new(
    function(n, pars, outvars, progress)
      mirai(
        safe_run_wanulcas(n, pars, outvars, progress),      # crashguard
        run_wanulcas = run_wanulcas,
        safe_run_wanulcas = safe_run_wanulcas,               # crashguard
        n = n,
        pars = pars,
        outvars = outvars,
        progress = progress
      )
  ) |> bind_task_button("sim_run_button")
  
  if (is_run_online) {
    observeEvent(input$sim_run_button, {
      if (!is_simulation_ready())
        return()
      
      n_iteration <- input$n_iteration
      pars <- isolate(get_input_parameters())
      progress <- AsyncProgress$new(
        session,
        min = 1,
        max = n_iteration,
        message = "Processing the server",
        detail = "Please wait while preparing the server session.."
      )
      on.exit(progress$close())
      progress_trigger <- function(i, n) {
        progress$set(i, "Running simulation", paste("Day", i, "of", n))
      }
      print("Starting simulation: online")
      task$invoke(
        n_iteration,
        pars,
        output_timeseries_vars = rv$output_timeseries_vars,
        output_final_vars = rv$output_final_vars,
        progress = progress_trigger
      )
    })
  } else {
    local_task <- eventReactive(input$sim_run_button, ignoreNULL = T, {
      if (!is_simulation_ready())
        return()
      n_iteration <- input$n_iteration
      pars <- isolate(get_input_parameters())
      
      progress <- Progress$new(session, min = 1, max = n_iteration)
      on.exit(progress$close())
      progress_trigger <- function(i, n) {
        progress$set(i, "Running simulation", paste("Day", i, "of", n))
      }
      print("Starting simulation: local")

      # ---- Comprehensive parameter logging ----                         # logging
      cat(strrep("=", 60), "\n")
      cat("[Simulation] Days:", n_iteration, "\n")
      # Scenario switches
      cat("[Simulation] Scenario switches:\n")
      for (sv in c("AF_AnyTrees_is","AF_Crop_is","AF_RunWatLim_is",
                    "AF_DynPestImpacts_is","W_WaterLog_is","RAIN_Multiplier","CA_DOYStart")) {
        cat("  ", sv, "=", pars$vars[[sv]], "\n")
      }
      # Tree species + oil palm flags
      tsp <- pars$arrays$tree_df$vars$T_Species
      cat("[Simulation] Tree species:", paste(tsp, collapse = ", "), "\n")
      cat("[Simulation] is_oilpalm:",
          paste(grepl("Oil ?palm|Elais guneensis", tsp), collapse = ", "), "\n")
      # Crop species
      csp <- pars$arrays$crop_df$vars$CQ_Species
      cat("[Simulation] Crop species:", paste(csp, collapse = ", "), "\n")
      # AF system
      cat("[Simulation] AF_ZoneTot:", pars$vars$AF_ZoneTot,
          " AF_Circ:", pars$vars$AF_Circ, "\n")
      cat("[Simulation] Zone widths:", paste(pars$arrays$zone_df$vars$AF_ZoneWidth, collapse=", "), "\n")
      cat("[Simulation] Layer depths:", paste(pars$arrays$layer_df$vars$AF_DepthLay, collapse=", "), "\n")
      # Soil water
      cat("[Simulation] W_BDLayer:", paste(pars$arrays$layer_df$vars$W_BDLayer, collapse=", "), "\n")
      # Nutrient N/P
      nlim <- pars$arrays$nut_df$vars$AF_RunNutLim_is
      cat("[Simulation] AF_RunNutLim_is (N,P):", paste(nlim, collapse=", "), "\n")
      npst <- pars$arrays$zonelayer_df$vars$N_PStParam
      if (!is.null(npst)) {
        cat("[Simulation] N_PStParam (16 values, Layer x Zone):\n")
        for (lay in 1:4) {
          idx <- ((lay-1)*4+1):(lay*4)
          cat(sprintf("   Layer %d: Z1=%.6f Z2=%.6f Z3=%.6f Z4=%.6f\n",
                      lay, npst[idx[1]], npst[idx[2]], npst[idx[3]], npst[idx[4]]))
        }
      }
      pmin <- pars$arrays$layer_df$vars$PStMin
      pmax <- pars$arrays$layer_df$vars$PStMax
      if (!is.null(pmin)) cat("[Simulation] PStMin:", paste(round(pmin, 8), collapse=", "), "\n")
      if (!is.null(pmax)) cat("[Simulation] PStMax:", paste(round(pmax, 4), collapse=", "), "\n")
      # Soil water parameters
      cat("[Simulation] W_BDLayer:", paste(pars$arrays$layer_df$vars$W_BDLayer, collapse=", "), "\n")
      # Debug: check the raw fragment for W_BDLayer
      v_arr_snap <- isolate(reactiveValuesToList(rv_arr))
      bd_frag <- v_arr_snap[["input_array_layer_27_0"]]
      if (!is.null(bd_frag) && "W_BDLayer" %in% names(bd_frag)) {
        cat("[Simulation] W_BDLayer in fragment 27:", paste(bd_frag$W_BDLayer, collapse=", "), "\n")
      }
      # VG / pedotransfer parameters per layer
      ld <- pars$arrays$layer_df$vars
      cat("[Simulation] --- Soil Hydraulic (pedotransfer) per layer ---\n")
      cat("[Simulation] W_ThetaSat:", paste(round(ld$W_ThetaSat, 6), collapse=", "), "\n")
      cat("[Simulation] W_Alpha:", paste(round(ld$W_Alpha, 6), collapse=", "), "\n")
      cat("[Simulation] W_n:", paste(round(ld$W_n, 6), collapse=", "), "\n")
      if (!is.null(ld$W_ThetaInacc)) cat("[Simulation] W_ThetaInacc:", paste(round(ld$W_ThetaInacc, 6), collapse=", "), "\n")
      if (!is.null(ld$W_FieldCapKcrit)) cat("[Simulation] W_FieldCapKcrit:", paste(round(ld$W_FieldCapKcrit, 6), collapse=", "), "\n")
      if (!is.null(ld$ClayLayer)) cat("[Simulation] ClayLayer:", paste(ld$ClayLayer, collapse=", "), "\n")
      if (!is.null(ld$SiltLayer)) cat("[Simulation] SiltLayer:", paste(ld$SiltLayer, collapse=", "), "\n")
      if (!is.null(ld$S_KsatInitV)) cat("[Simulation] S_KsatInitV:", paste(round(ld$S_KsatInitV, 4), collapse=", "), "\n")
      if (!is.null(ld$S_KsatDefV)) cat("[Simulation] S_KsatDefV:", paste(round(ld$S_KsatDefV, 4), collapse=", "), "\n")
      cat("[Simulation] W_ThetaInit:", paste(round(head(pars$arrays$zonelayer_df$vars$W_ThetaInit, 4), 4), collapse=", "), "(first 4)\n")
      cat("[Simulation] --- Water / hydraulic scalars ---\n")
      cat("[Simulation] W_PMax:", pars$vars$W_PMax, "\n")
      cat("[Simulation] W_SeepScalar:", pars$vars$W_SeepScalar, "\n")
      cat("[Simulation] W_Hyd_is (Hydraulic Redistribution):", pars$vars$W_Hyd_is, "\n")
      if (!is.null(pars$vars$W_HydEqFraction)) cat("[Simulation] W_HydEqFraction:", pars$vars$W_HydEqFraction, "\n")
      if (!is.null(pars$vars$W_WaterLog_is)) cat("[Simulation] W_WaterLog_is:", pars$vars$W_WaterLog_is, "\n")
      cat("[Simulation] CW_Alpha (crop water):", pars$vars$CW_Alpha, "\n")
      if (!is.null(pars$vars$TW_Alpha)) cat("[Simulation] TW_Alpha (tree water):", pars$vars$TW_Alpha, "\n")
      # Soil water retention graph summary per layer
      if (!is.null(pars$graphs$W_PhiTheta)) {
        for (L in 1:length(pars$graphs$W_PhiTheta$xy_data)) {
          xyL <- pars$graphs$W_PhiTheta$xy_data[[L]]
          cat(sprintf("[Simulation] W_PhiTheta L%d: %d pts, y(phi) range %.4f - %.4f\n",
              L, length(xyL$x_val), min(unlist(xyL$y_val)), max(unlist(xyL$y_val))))
        }
      }
      # Output var counts
      cat("[Simulation] Output timeseries vars:", length(rv$output_timeseries_vars), "\n")
      cat("[Simulation] Output final vars:", length(rv$output_final_vars), "\n")
      cat(strrep("=", 60), "\n")

      safe_run_wanulcas(                                                # crashguard
        n_iteration,
        pars,
        output_timeseries_vars = rv$output_timeseries_vars,
        output_final_vars = rv$output_final_vars,
        progress = progress_trigger
      )
    })
  }
  
  is_simulation_ready <- function() {
    if (length(rv$output_timeseries_vars) == 0) {
      show_alert(
        "Output variables was not selected",
        "Please select the output variable on the table below by checking the correspondent box."
      )
      return(F)
    }
    return(T)
  }
  
  ### Output ######################
  
  # observe(rv$sim_output <- local_task())                   # crashguard (was)
  observe({                                                  # crashguard
    res <- local_task()                                    # crashguard
    if (is_sim_error(res)) {                                 # crashguard
      show_alert(                                              # crashguard
        "Simulation could not complete",                       # crashguard
        paste0("The model stopped with this message:\n\n",    # crashguard
               res$.error,                                     # crashguard
               "\n\nYour inputs are unchanged \u2014 adjust ", # crashguard
               "the parameters and run again."),               # crashguard
        type = "error")                                        # crashguard
    } else if (!is.null(res)) {                              # crashguard
      rv$sim_output <- res                                   # crashguard
    }                                                        # crashguard
  })                                                         # crashguard

  # observe(rv$sim_output <- task$result())                  # crashguard (was)
  observe({                                                  # crashguard
    res <- task$result()                                    # crashguard
    if (is_sim_error(res)) {                                 # crashguard
      show_alert(                                              # crashguard
        "Simulation could not complete",                       # crashguard
        paste0("The model stopped with this message:\n\n",    # crashguard
               res$.error,                                     # crashguard
               "\n\nYour inputs are unchanged \u2014 adjust ", # crashguard
               "the parameters and run again."),               # crashguard
        type = "error")                                        # crashguard
    } else if (!is.null(res)) {                              # crashguard
      rv$sim_output <- res                                   # crashguard
    }                                                        # crashguard
  })                                                         # crashguard
  
  #### Output vars selection ###################
  
  output$output_var_selector <- renderReactable({
    selected <- which(output_vars_disp_df$var %in% rv$output_timeseries_vars,
                      arr.ind = TRUE)
    reactable(
      output_vars_disp_df,
      selection = "multiple",
      onClick = "select",
      defaultSelected = selected,
      highlight = T,
      compact = T,
      striped = T,
      filterable = T,
      showPageSizeOptions = T,
      pageSizeOptions = c(10, 20, 40, 100),
      defaultPageSize = 20,
      paginateSubRows = T,
      columns = list(
        var = colDef(name = "Variable"),
        arr = colDef(name = "Array Dimension")
      )
    )
  })
  
  output$output_final_var_selector <- renderReactable({
    selected <- which(output_vars_disp_df$var %in% rv$output_final_vars,
                      arr.ind = TRUE)
    reactable(
      output_vars_disp_df,
      selection = "multiple",
      onClick = "select",
      defaultSelected = selected,
      highlight = T,
      compact = T,
      striped = T,
      filterable = T,
      showPageSizeOptions = T,
      pageSizeOptions = c(10, 20, 40, 100),
      defaultPageSize = 20,
      paginateSubRows = T,
      columns = list(
        var = colDef(name = "Variable"),
        arr = colDef(name = "Array Dimension")
      )
    )
  })
  
  
  output$output_var_selected <- renderUI({
    i <- getReactableState("output_var_selector", "selected")
    df <- output_vars_disp_df[i, "var"]
    output$selected_vars_info <- renderUI(tags$strong(length(df)))
    tags$ul(lapply(df, tags$li))
  })
  
  observe({
    i <- getReactableState("output_var_selector", "selected")
    if (!is.null(i)) {
      rv$output_timeseries_vars <- output_vars_disp_df[i, "var"]
    }
  })
  
  observeEvent(input$clear_selected_output_vars,
               rv$output_timeseries_vars <- c())
  
  observeEvent(input$reset_default_output_vars, {
    selected <- which(output_vars_disp_df$var %in% default_output_timeseries_vars,
                      arr.ind = TRUE)
    updateReactable("output_var_selector", selected = selected)
  })
  
  output$output_final_var_selected <- renderUI({
    i <- getReactableState("output_final_var_selector", "selected")
    df <- output_vars_disp_df[i, "var"]
    output$selected_final_vars_info <- renderUI(tags$strong(length(df)))
    tags$ul(lapply(df, tags$li))
  })
  
  observe({
    i <- getReactableState("output_final_var_selector", "selected")
    if (!is.null(i)) {
      rv$output_final_vars <- output_vars_disp_df[i, "var"]
    }
  })
  
  observeEvent(input$clear_selected_output_final_vars,
               rv$output_final_vars <- c())
  
  observeEvent(input$reset_default_output_final_vars, {
    selected <- which(output_vars_disp_df$var %in% default_output_final_vars,
                      arr.ind = TRUE)
    updateReactable("output_final_var_selector", selected = selected)
  })
  
  observeEvent(input$reset_button, {
    show_input_dialog(
      "Reset Output",
      "The current output will be removed. Continue resetting the output?",
      "confirm_reset_button",
      "Yes"
    )
  })
  
  observeEvent(input$confirm_reset_button, {
    removeModal()
    rv$sim_output <- NULL
  })

  #### dynamic output graph  #########################

  card_id_counter <- 0
  get_next_card_id <- function() {
    card_id_counter <<- card_id_counter + 1
    return(paste0("outgraph", card_id_counter))
  }
  
  page_id_counter <- 0
  get_next_page_id <- function() {
    page_id_counter <<- page_id_counter + 1
    return(paste0("page", page_id_counter))
  }

  reset_output_config <- function() {
    card_id_counter <<- 0
    page_id_counter <<- 0
    rv$output_graph_cfg <- list()
  }
  
  generate_dim_keys <- function(sp, arr, df) {
    if (is.null(arr) || is.na(arr))
      return()
    # filter dataframe with the selected dimension keys
    if (arr == "single_df") {
      key_df <- data.frame(single = 0)
    } else {
      key_df <- wanulcas_def_arr[[arr]]
      if (!is.null(sp)) {
        k_df <- as.data.frame(t(sapply(sp, function(a) {
          unlist(strsplit(a, " "))
        })))
        k <- unique(k_df[[1]])
        f_df <- df
        f_key_df <- key_df
        for (x in k) {
          f_df <- f_df[f_df[[x]] %in% k_df[k_df[[1]] == x, 2], ]
          f_key_df <- f_key_df[f_key_df[[x]] %in% k_df[k_df[[1]] == x, 2], ]
        }
        df <- f_df
        if (class(f_key_df) == "data.frame") {
          key_df <- f_key_df
        } else {
          key_df <- data.frame(f_key_df)
          colnames(key_df) <- k
        }
      }
    }
    return(key_df)
  }
  
  generate_output_graph <- function(df, key_df, vars) {
    if (is.null(key_df))
      return()
    kn <- names(key_df)
    subfont <- list(size = 14)

    # ---- single_df (no dimensions) ----
    if (kn[1] == "single") {
      fig <- plot_ly(type = "scatter", mode = "lines")
      for (v in vars) {
        fig <- fig |> add_trace(
          x = df[["time"]], y = df[[v]], name = v,
          color = I(chart_color[match(v, vars)])
        )
      }
      fig <- fig |> plotly::layout(
        xaxis = list(title = "Days"),
        hoverlabel = list(namelength = -1)
      )
      return(fig)
    }

    # ---- Has dimension keys: combine zone into one facet with colored lines ----
    # Facet by non-zone keys (e.g. layer, SlNut); within each facet,
    # draw a separate colored line per zone (or per zone combo).
    has_zone <- "zone" %in% kn
    if (has_zone) {
      facet_keys <- setdiff(kn, "zone")
      zone_vals  <- sort(unique(key_df[["zone"]]))
    } else {
      facet_keys <- kn
      zone_vals  <- NULL
    }

    # Build facet combos from non-zone keys
    if (length(facet_keys) == 0) {
      # Only zone key -> one facet, all zones as colored lines
      facet_combos <- data.frame(.dummy = 1)
    } else {
      facet_combos <- unique(key_df[, facet_keys, drop = FALSE])
      facet_combos <- facet_combos[do.call(order, facet_combos), , drop = FALSE]
    }
    nfacets <- nrow(facet_combos)

    # zone palette (4 colors, reusable)
    zone_colors <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a",
                     "#66a61e", "#e6ab02", "#a6761d", "#666666")

    figs <- lapply(seq_len(nfacets), function(fi) {
      # filter data to this facet's non-zone key values
      df2 <- df
      facet_label <- ""
      if (length(facet_keys) > 0 && !".dummy" %in% names(facet_combos)) {
        for (fk in facet_keys) {
          df2 <- df2[df2[[fk]] == facet_combos[[fk]][fi], ]
        }
        facet_label <- paste(
          paste0("<i>", facet_keys, ":</i> <b>",
                 sapply(facet_keys, function(fk) facet_combos[[fk]][fi]), "</b>"),
          collapse = "; "
        )
      }

      fig <- plot_ly(type = "scatter", mode = "lines")
      if (has_zone && length(zone_vals) > 1) {
        # plot each zone as a separate colored line within this facet
        for (v in vars) {
          for (zi in seq_along(zone_vals)) {
            zv <- zone_vals[zi]
            df3 <- df2[df2[["zone"]] == zv, ]
            trace_name <- if (length(vars) == 1) {
              paste0("Z", zv)
            } else {
              paste0(v, " Z", zv)
            }
            fig <- fig |> add_trace(
              x = df3[["time"]], y = df3[[v]],
              name = trace_name,
              legendgroup = trace_name,
              color = I(zone_colors[((zi - 1) %% length(zone_colors)) + 1]),
              showlegend = (fi == 1)
            )
          }
        }
      } else {
        # no zone or single zone: plot vars with standard colors
        for (v in vars) {
          fig <- fig |> add_trace(
            x = df2[["time"]], y = df2[[v]], name = v,
            legendgroup = v,
            color = I(chart_color[match(v, vars)]),
            showlegend = (fi == 1)
          )
        }
      }

      fig <- fig |> plotly::layout(
        annotations = list(list(
          y = 1, yref = "paper", yanchor = "bottom",
          text = facet_label, showarrow = FALSE, font = subfont
        )),
        xaxis = list(title = "Days"),
        yaxis = list(title = ""),
        hoverlabel = list(namelength = -1)
      )
      fig
    })

    if (nfacets == 1) {
      figs[[1]]
    } else {
      subplot(figs, shareX = TRUE, shareY = TRUE,
              titleX = TRUE, titleY = TRUE, nrows = nfacets)
    }
  }
  
  ### output time series UI ##############
  
  output$sim_output_ui <- renderUI({
    out <- rv$sim_output
    if (is.null(out))
      return()
    formatted_output_data(format_output_data(out$timeseries_vars))
    output_cfg <- isolate(rv$output_graph_cfg)
    #reset the output cfg
    reset_output_config()
    
    page_panels <- NULL
    if (length(output_cfg) > 0) {
      n <- length(output_cfg)
      page_ids <- replicate(n, get_next_page_id())
      page_panels <- lapply(1:n, function(i) {
        create_output_page_panel(page_ids[i], output_cfg[[i]]$title, output_cfg[[i]]$content)
      })
      names(page_panels) <- NULL
    }
    
    btn <- actionButton(
      "add_output_page_button",
      "Add New Page",
      icon = icon("plus"),
      style = compact_button_style
    )
    card_body(do.call(navset_card_pill, c(
      list(id = "output_timeseries", nav_item(btn), nav_spacer()),
      page_panels
    )))
  })
  
  observeEvent(input$add_output_page_button, {
    show_input_dialog(
      "Add New Output Page",
      confirm_id = "confirm_add_page",
      input_var = "input_page_title",
      input_label = "Page title:"
    )
  })
  
  user_crop <- reactiveVal()
  
  observeEvent(input$confirm_add_page, {
    removeModal()
    p_id <- get_next_page_id()
    add_output_page(p_id, input$input_page_title)
  })
  
  get_output_page_ui <- function(page_id, title, content_list = NULL) {
    add_card_button_id <- paste0("add_dynamic_card_button_", page_id)
    observeEvent(input[[add_card_button_id]], add_output_card(page_id, title))
    
    remove_page_button_id <- paste0("remove_page_button_", page_id)
    observeEvent(input[[remove_page_button_id]], nav_remove("output_timeseries", page_id))
    
    nav_panel(value = page_id, title = title, div(
      do.call(layout_column_wrap, c(
        list(
          id = paste0("dynamic_card_container_", page_id),
          width = "400px",
          style = "margin:10px"
        ),
        content_list
      )),
      div(
        style = "width:100%",
        actionButton(
          style = "float:right; margin-right:50px",
          add_card_button_id,
          "Add Output Graph",
          icon = icon("plus")
        ),
        actionButton(
          style = "float:right; margin-right:20px",
          remove_page_button_id,
          "Remove This Page",
          icon = icon("trash-can")
        )
      )
    ))
  }
  
  add_output_page <- function(page_id, title) {
    set_output_config_page(page_id, title)
    nav_insert("output_timeseries",
               get_output_page_ui(page_id, title),
               select = TRUE)
  }
  
  set_output_config_page <- function(page_id, title) {
    rv$output_graph_cfg[[page_id]][["title"]] <- title
  }
  
  set_output_config_content <- function(page_id,
                                        page_title,
                                        card_id,
                                        vars = NULL,
                                        filter = NULL) {
    if (!is.null(page_title)) {
      rv$output_graph_cfg[[page_id]][["title"]] <- page_title
    }
    rv$output_graph_cfg[[page_id]][["content"]][[card_id]][["vars"]] <- vars
    rv$output_graph_cfg[[page_id]][["content"]][[card_id]][["filter"]] <- filter
  }
  
  create_output_page_panel <- function(page_id, title, content_cfg = NULL) {
    content_list <- list()
    if (!is.null(content_cfg)) {
      n <- length(content_cfg)
      card_ids <- replicate(n, get_next_card_id())
      data <- isolate(formatted_output_data())
      content_list <- lapply(1:n, function(i) {
        id <- card_ids[i]
        vars <- content_cfg[[i]]$vars
        filter <- content_cfg[[i]]$filter
        card_graph_ui(id, data, vars, filter)
      })
      names(content_list) <- NULL
      
      lapply(1:n, function(i) {
        id <- card_ids[i]
        card_graph_server(
          id,
          data,
          content_cfg = content_cfg[[i]],
          page_id = page_id,
          page_title = title,
          update_card = set_output_config_content
        )
      })
    }
    
    content_list <- c(content_list, uiOutput(paste0("graph_add_", page_id)))
    get_output_page_ui(page_id, title, content_list)
    
  }
  
  add_output_card <- function(page_id, title) {
    id <- get_next_card_id()
    data <- isolate(formatted_output_data())
    ui <- card_graph_ui(id, data)
    insertUI(
      selector = paste0("#dynamic_card_container_", page_id),
      where = "beforeEnd",
      ui = ui
    )
    card_graph_server(
      id,
      data,
      page_id = page_id,
      page_title = title,
      update_card = set_output_config_content
    )
    return(id)
  }
  
  ### output final vars UI ###############
  
  fin_id_prefix <- "output_final_var_"
  
  output$sim_output_final_ui <- renderUI({
    out <- rv$sim_output
    if (is.null(out))
      return()
    out_list <- out$final_vars
    # Safe lapply to avoid zero-length errors
    out_ui <- lapply(names(out_list), function(x) {
      card(
        full_screen = T,
        card_header(
          class = "d-flex justify-content-between",
          suffix_remove(x),
          table_download_link(
            paste0(fin_id_prefix, x),
            paste0(suffix_remove(x), "_final_vars.csv")
          )
        ),
        reactableOutput(paste0(fin_id_prefix, x))
      )
    })
    card_body(do.call(flowLayout, c(list(
      cellArgs = list(style = "width:auto; margin:0px; max-width:600px")
    ), out_ui)))
  })
  
  observe({
    out <- rv$sim_output
    if (is.null(out))
      return()
    out_list <- out$final_vars
    lapply(names(out_list), function(x) {
      output[[paste0(fin_id_prefix, x)]] <- renderReactable(reactable(
        out_list[[x]],
        defaultColDef = colDef(width = 120, cell = numeric_cell_coldef)
      ))
    })
  })

  output$download_output_cfg <- downloadHandler(
    filename = function() {
      paste("output_config.yaml")
    },
    content = function(fname) {
      write_yaml(rv$output_graph_cfg, fname)
    },
    contentType = "application/yaml"
  )
  
  ### UPLOAD ######################
  
  observeEvent(input$upload_parameter, {
    dpath <- input$upload_parameter$datapath
    set_parameters(read_params(dpath))
    show_alert(
      "Upload Successful!",
      "The paramaters file has been successfully uploaded!",
      "success"
    )
  })
  
  set_parameters <- function(params) {
    # vars
    lapply(inputvars_ui_id, function(x) {
      varnames <- inputvars_df[inputvars_df$ui_id == x, "var"]
      update_numeric_input_ui(x, params$vars[varnames])
    })
    
    # arrays
    arrays_df <- array_params_to_ui_inp(params$arrays)
    lapply(names(arrays_df), function(x) {
      rv_arr[[x]] <- arrays_df[[x]]
    })

    # Normalize uploaded species names to the UI (read.csv/dotted) form so     # simfix
    # the dropdowns display the correct selection.                             # simfix
    .norm_species <- function(ui_id, col) {                                    # simfix
      df <- rv_arr[[ui_id]]
      if (!is.null(df) && col %in% names(df)) {
        df[[col]] <- make.names(as.character(df[[col]]))
        rv_arr[[ui_id]] <- df
      }
    }
    .norm_species(tree_ui_id, "T_Species")                                     # simfix
    .norm_species(crop_ui_id, "CQ_Species")                                    # simfix

    # Simulation scenario sliders (added): reflect uploaded values         # simfix
    .sim_sl <- c(sim_AF_AnyTrees_is = "AF_AnyTrees_is",
                 sim_AF_Crop_is = "AF_Crop_is",
                 sim_AF_RunWatLim_is = "AF_RunWatLim_is",
                 sim_AF_DynPestImpacts_is = "AF_DynPestImpacts_is",
                 sim_W_WaterLog_is = "W_WaterLog_is",
                 sim_W_Hyd_is = "W_Hyd_is",
                 sim_RAIN_Multiplier = "RAIN_Multiplier",
                 sim_CA_DOYStart = "CA_DOYStart")
    lapply(names(.sim_sl), function(sid) {
      v <- .sim_sl[[sid]]
      if (!is.null(params$vars[[v]])) {
        rv_var[[v]] <- params$vars[[v]]
        updateSliderInput(session, sid, value = params$vars[[v]])
      }
    })
    nl <- rv_arr[[nut_ui_id]]$AF_RunNutLim_is
    if (!is.null(nl)) {
      updateSliderInput(session, "sim_AF_RunNutLim_is_N", value = nl[1])
      updateSliderInput(session, "sim_AF_RunNutLim_is_P", value = nl[2])
    }
    
    # graphs
    graph_df <- graph_params_to_ui_inp(params$graphs)
    lapply(names(graph_df), function(x) {
      rv_graph[[x]] <- graph_df[[x]]
    })
    
    if(!is.null(params$crop_library)) {
      df <- as.data.frame(do.call(cbind, params$crop_library))
      crop_names <- setdiff(names(df), species_var_keys)
      crop_species_df <<- cbind(crop_species_df, df[crop_names])
      user_crop(crop_names)
    }
    
    if(!is.null(params$tree_library)) {
      df <- as.data.frame(do.call(cbind, params$tree_library))
      tree_names <- setdiff(names(df), species_var_keys)
      tree_species_df <<- cbind(tree_species_df, df[tree_names])
      user_tree(tree_names)
    }
    
    crops <- rv_arr[[crop_ui_id]]$CQ_Species
    for (i in seq_along(crop_select_ids)) {                                    # simfix (was 1:5)
      updateSelectInput(session, crop_select_ids[i], selected = crops[i])
    }
    
    trees <- rv_arr[[tree_ui_id]]$T_Species
    for (i in seq_along(tree_select_ids)) {                                    # simfix (was 1:5 -> only 3 slots)
      updateSelectInput(session, tree_select_ids[i], selected = trees[i])
    }
    
    if (!is.null(params$output$timeseries_vars))
      rv$output_timeseries_vars <- params$output$timeseries_vars
    if (!is.null(params$output$timeseries_layout))
      rv$output_graph_cfg <- params$output$timeseries_layout
  }
  
  observeEvent(input$upload_xls_parameter, {
    dpath <- input$upload_xls_parameter$datapath
    params <- get_input_parameters()
    params <- apply_xls_params(params, dpath, xls_config_df)

    # P_CostExtOrg is present in the xlsm P_ParamAll column (ExtOrg1/2 P & S)
    # but apply_xls_params doesn't map it. Read it here, mirroring the engine's
    # own get_par_xls_list indexing (rows 1:length(PF_UnitAll)).
    tryCatch({
      wb_x <- openxlsx2::wb_load(dpath)
      px_df <- openxlsx2::wb_to_df(wb_x, "LinkToStella")
      pf <- as.list(px_df[1:length(PF_UnitAll), "P_ParamAll"])
      names(pf) <- PF_UnitAll
      # inpprice_df order: ExtOrgInputs=[1,2,1,2], PriceType=[1(private),1,2(social),2]
      # matching the P_PriceFert convention c(...P, ...P, ...S, ...S)
      params$arrays$inpprice_df$vars$P_CostExtOrg <- as.numeric(c(
        pf[["ExtOrg1P"]], pf[["ExtOrg2P"]],
        pf[["ExtOrg1S"]], pf[["ExtOrg2S"]]
      ))
      cat("[xlsm] P_CostExtOrg imported:",
          paste(params$arrays$inpprice_df$vars$P_CostExtOrg, collapse = ", "), "\n")
    }, error = function(e) {
      cat("[xlsm] Could not import P_CostExtOrg:", conditionMessage(e), "\n")
    })

    set_parameters(params)
    show_alert(
      "Upload Successful!",
      "The MS-Excel paramaters file has been successfully uploaded and applied!",
      "success"
    )
  })
  
  formatted_output_data <- reactiveVal()
  
  format_output_data <- function(data) {
    if (is.null(data)) {
      return()
    }
    var_data <- lapply(names(data), function(x) {
      df <- data[[x]]
      keys <- NULL
      keys_label <- NULL
      if (x != "single_df") {
        keys <- apply(wanulcas_def_arr[[x]], 2, unique, simplify = F)
        keys_label <- sapply(names(keys), function(a) {
          k <- paste(a, keys[[a]])
          kk <- as.list(k)
          names(kk) <- k
          kk
        }, simplify = F, USE.NAMES = T)
      }
      v <- setdiff(names(df), c("time", names(keys)))
      list(
        vars = v,
        keys = keys_label,
        arr = x,
        data = df
      )
    })
    names(var_data) <- names(data)
    
    v <- unlist(sapply(var_data, function(x)
      x[["vars"]]))
    a <- unlist(sapply(var_data, function(x)
      rep(x[["arr"]], length(x[["vars"]]))))
    var_df <- data.frame(vars = v, arr = a)
    # a variable may exist on multiple array, it should be selected to the shortest array dimensions
    # TODO: should prevented on the wanulcas output loop
    var_df <- aggregate(
      var_df[2],
      var_df[-2],
      FUN = function(x) {
        d <- unique(x)
        d[which.min(nchar(d))]
      }
    )
    return(list(var_df = var_df, arr_data = var_data))
  }
  
  observeEvent(input$upload_output_data_button, {
    print(paste("Extracting the files:", input$upload_parameter$name))
    dpath <- input$upload_output_data_button$datapath
    data <- upload_output_data(dpath)
    fdata <- format_output_data(data)
    formatted_output_data(fdata)
  })
  
  show_alert_file_error <- function(file_error) {
    show_alert("File Error!",
               paste("File error! Or it is not a", file_error, "file!"),
               type = "error")
  }
  
  data_dir <- paste0(tempdir(), "/data_temp")
  
  upload_output_data <- function(dpath) {
    file_list <- NULL
    try(file_list <- utils::unzip(dpath, list = TRUE), silent = T)
    if (is.null(file_list)) {
      show_alert_file_error("compressed (zip)")
      return()
    }
    utils::unzip(dpath, exdir = data_dir, junkpaths = T)
    d <- list()
    for (f in file_list$Name) {
      arr <- paste0(prefix(f, "."), "_df")
      fpath <- paste0(data_dir, "/", f)
      df <- read.csv(fpath)
      d[[arr]] <- df
    }
    print("Output data uploaded!")
    showNotification("Output data uploaded!", type = "message")
    return(d)
  }
  
  
  ### DOWNLOAD ######################
  
  output$download_parameter <- downloadHandler(
    filename = function() {
      paste("wanulcas_params.yaml")
    },
    content = function(fname) {
      pars <- isolate(get_input_parameters())
      pars$output <- list(
        timeseries_vars = rv$output_timeseries_vars,
        timeseries_layout = rv$output_graph_cfg
      )
      write_params(pars, fname)
    }
  )

  # Downloadable blank/reference Excel parameter template
  output$download_xlsm_template <- downloadHandler(
    filename = function() "Wanulcas_template.xlsm",
    content = function(fname) {
      file.copy("www/Wanulcas_template.xlsm", fname, overwrite = TRUE)
    },
    contentType = "application/vnd.ms-excel.sheet.macroEnabled.12"
  )

  output$download_params_pdf <- downloadHandler(
    filename = function() "WaNuLCAS_input_parameters_description.pdf",
    content = function(fname) {
      file.copy("www/WaNuLCAS_parameters_description.pdf", fname, overwrite = TRUE)
    },
    contentType = "application/pdf"
  )

  output$download_output_params_pdf <- downloadHandler(
    filename = function() "WaNuLCAS_output_parameters_description.pdf",
    content = function(fname) {
      file.copy("www/WaNuLCAS_output_parameters_description.pdf", fname, overwrite = TRUE)
    },
    contentType = "application/pdf"
  )
  
  output$download_output <- downloadHandler(
    filename = function() {
      paste("wanulcas_output.zip")
    },
    content = function(fname) {
      sim_output <- rv$sim_output
      if (is.null(sim_output))
        return()
      
      lv <- sim_output$log_vars
      log_arr <- paste0("log_", names(lv))
      names(lv) <- log_arr
      
      fv <- sim_output$final_vars
      fin_arr <- paste0("final_", names(fv))
      names(fv) <- fin_arr
      
      io_df <- data.frame(var = c(log_arr, fin_arr))
      io_df$file <- sapply(io_df$var, function(x)
        paste(head(unlist(
          strsplit(x, "_")
        ), -1), collapse = "_"))
      
      setwd(tempdir())
      fs <- save_variables(io_df, c(lv, fv))
      z <- zip::zip(zipfile = fname, files = fs)
      return(z)
    },
    contentType = "application/zip"
  )
  
  
  ### GUI #######################
  
  
  card_graph_ui <- function(id,
                            data,
                            def_vars = NULL,
                            def_filter = NULL) {
    var_df <- data$var_df
    arr_data <- data$arr_data
    def_v_choices <- var_df$vars
    def_arr <- NULL
    def_dim_choices <- NULL
    if (!is.null(def_vars)) {
      def_arr <- var_df[var_df$vars == def_vars[1], "arr"]
      if (length(def_arr) > 0) {
        def_v_choices <- var_df[var_df$arr == def_arr, "vars"]
        def_dim_choices <- arr_data[[def_arr]]$keys
      }
    }
    
    ns <- NS(id)
    button_id <- ns("remove_card_button")
    var_select_id <- ns("var_select")
    dim_id <- ns("dim_compare")
    var_ext_select_id <- ns("var_ext_select")
    sp_select_id <- ns("sp_select")
    graph_id <- ns("graph_id")
    card_title_id <- ns("card_title")
    table_id <- ns("table")
    
    div(
      id = ns("card"),
      navset_card_underline(
        full_screen = T,
        title = textOutput(card_title_id),
        
        sidebar = sidebar(
          open = is.null(def_vars),
          selectizeInput(
            inputId = var_select_id,
            label = "Variables:",
            choices = def_v_choices,
            selected = def_vars,
            multiple = TRUE,
            options = list(dropdownParent = 'body')
          ),
          conditionalPanel(
            condition = "input['var_select'].length > 0",
            ns = ns,
            selectizeInput(
              inputId = sp_select_id,
              label = "Filter:",
              choices = def_dim_choices,
              selected = def_filter,
              multiple = TRUE,
              options = list(dropdownParent = 'body')
            )
          )
        ),
        nav_spacer(),
        nav_panel(
          "Graph",
          icon = icon("chart-line"),
          card_body(padding = 0, plotlyOutput(graph_id, height = "300px"))
        ),
        nav_panel(
          "Data",
          icon = icon("table"),
          card_body(
            padding = 0,
            download_link(table_id),
            reactableOutput(table_id)
          )
        ),
        nav_item(actionLink(button_id, "", icon = icon("trash-can")))
      )
    )
  }
  
  card_graph_server <- function(id,
                                data,
                                content_cfg = NULL,
                                page_id,
                                page_title,
                                update_card) {
    moduleServer(id, function(input, output, session) {
      variable_df <- reactiveVal(data$var_df)
      array_data <- reactiveVal(data$arr_data)
      
      selected_df <- reactiveVal()
      key_df <- reactiveVal()
      
      page_id <- page_id
      page_title <- page_title
      button_id <- "remove_card_button"
      var_select_id <- "var_select"
      dim_id <- "dim_compare"
      sp_select_id <- "sp_select"
      graph_id <- "graph_id"
      card_title_id <- "card_title"
      table_id <- "table"
      
      observeEvent(input[[button_id]], {
        ns <- NS(id)
        removeUI(selector = paste0("#", ns("card")))
        update_card(page_id, page_title, id, NULL, NULL)
      })
      
      # variable selection
      observeEvent(input[[var_select_id]], {
        vs <- input[[var_select_id]]
        sp <- input[[sp_select_id]]
        var_df <- variable_df()
        arr_data <- array_data()
        output[[card_title_id]] <- renderText(paste(vs, collapse = ", "))
        if (is.null(vs)) {
          if (is.null(content_cfg)) {
            updateSelectizeInput(session, var_select_id, choices = var_df$vars)
            updateSelectizeInput(session,
                                 sp_select_id,
                                 choices = character(0),
                                 selected = character(0))
          } else {
            updateSelectizeInput(
              session,
              var_select_id,
              choices = var_df$vars,
              selected = content_cfg$vars
            )
            content_cfg <<- NULL
          }
          output[[dim_id]] <- NULL
        } else if (length(vs) == 1) {
          arr <- var_df[var_df$vars == vs, "arr"]
          vfilt <- var_df[var_df$arr == arr, "vars"]
          dim_choices <- arr_data[[arr]]$keys
          # sp <- input[[sp_select_id]]
          updateSelectizeInput(session,
                               var_select_id,
                               choices = vfilt,
                               selected = vs)
          updateSelectizeInput(session,
                               sp_select_id,
                               choices = dim_choices,
                               selected = sp)
        }
        update_card(page_id, page_title, id, vs, sp)
      }, ignoreNULL = FALSE)
      
      # sub plot or array dimension selection
      observeEvent(input[[sp_select_id]], {
        sp <- input[[sp_select_id]]
        vs <- input[[var_select_id]]
        var_df <- variable_df()
        arr <- var_df[var_df$vars == vs[1], "arr"]
        vfilt <- var_df[var_df$arr == arr, "vars"]
        if (is.null(sp)) {
          output[[dim_id]] <- NULL
        } else if (length(sp) >= 1) {
          updateSelectizeInput(session,
                               var_select_id,
                               choices = vfilt,
                               selected = vs)
          
        }
        update_card(page_id, page_title, id, vs, sp)
      }, ignoreNULL = FALSE)
      
      output[[graph_id]] <- renderPlotly({
        vs <- input[[var_select_id]]
        if (is.null(vs)) {
          validate(need(F, text_output_empty))
        } else {
          generate_output_graph(selected_df(), key_df(), vs)
        }
      })
      
      output[[table_id]] <- renderReactable({
        vs <- input[[var_select_id]]
        if (is.null(vs)) {
          validate(need(F, text_output_empty))
        } else {
          reactable(
            selected_df(),
            highlight = T,
            compact = T,
            showPageSizeOptions = T,
            defaultColDef = colDef(cell = numeric_cell_coldef)
          )
        }
      })
 
      observe({
        vs <- input[[var_select_id]]
        sp <- input[[sp_select_id]]
        var_df <- variable_df()
        arr_data <- array_data()
        v_df <- var_df[var_df$vars %in% vs, ]
        arr <- unique(v_df$arr)[1]
        df <- arr_data[[arr]]$data[c("time", names(arr_data[[arr]]$keys), vs)]
        keys <- generate_dim_keys(sp, arr, df)
        f_df <- df
        if (!is.null(keys) && ncol(keys) > 0) {
          for (n in names(keys)) {
            if (n == "single")
              next
            f_df <- f_df[f_df[[n]] %in% unique(keys[[n]]), ]
          }
        }
        key_df(keys)
        selected_df(f_df)
      })
      
    })
  }
  

  ### Simulation scenario slider wiring (added) #################
  sim_scalar_sliders <- c(                                            # simfix
    sim_AF_AnyTrees_is       = "AF_AnyTrees_is",
    sim_AF_Crop_is           = "AF_Crop_is",
    sim_AF_RunWatLim_is      = "AF_RunWatLim_is",
    sim_AF_DynPestImpacts_is = "AF_DynPestImpacts_is",
    sim_W_WaterLog_is        = "W_WaterLog_is",
    sim_W_Hyd_is             = "W_Hyd_is",
    sim_RAIN_Multiplier      = "RAIN_Multiplier",
    sim_CA_DOYStart          = "CA_DOYStart"
  )
  lapply(names(sim_scalar_sliders), function(sid) {                   # simfix
    v <- sim_scalar_sliders[[sid]]
    observeEvent(input[[sid]], { rv_var[[v]] <- input[[sid]] }, ignoreInit = TRUE)
  })
  # AF_RunNutLim_is is a column of the nut array (rows: N = 1, P = 2)
  observeEvent(input$sim_AF_RunNutLim_is_N, {                         # simfix
    df <- rv_arr[[nut_ui_id]]
    if (!is.null(df) && "AF_RunNutLim_is" %in% names(df)) {
      df$AF_RunNutLim_is[1] <- input$sim_AF_RunNutLim_is_N
      rv_arr[[nut_ui_id]] <- df
    }
  }, ignoreInit = TRUE)
  observeEvent(input$sim_AF_RunNutLim_is_P, {                         # simfix
    df <- rv_arr[[nut_ui_id]]
    if (!is.null(df) && "AF_RunNutLim_is" %in% names(df)) {
      df$AF_RunNutLim_is[2] <- input$sim_AF_RunNutLim_is_P
      rv_arr[[nut_ui_id]] <- df
    }
  }, ignoreInit = TRUE)

  ### Pedotransfer calculator (Phase 5) ###########################
  # Computes van Genuchten parameters from soil texture and writes
  # the results into rv_graph (water retention curves) and rv_arr
  # (bulk density). Only fires on "Apply" button — if user never
  # clicks, the values from default_params / xlsm stay untouched.
  ptf_result <- reactiveVal(NULL)        # list of vg per layer
  ptf_ksat_used <- reactiveVal(NULL)     # the Ksat actually applied per layer

  # Paste-capable soil properties table (4 layers x 8 properties)
  ptf_input_data <- reactiveVal(data.frame(
    Layer  = 1:4,
    Clay   = c(23.12, 23.12, 25.00, 27.00),
    Silt   = c(63.41, 63.41, 60.00, 58.00),
    OrgC   = c(1.25, 1.00, 0.80, 0.60),
    BD     = c(1.37, 1.38, 1.35, 1.35),
    CEC    = c(10.2, 10.2, 9.5, 9.0),
    pH     = c(4.55, 4.55, 4.60, 4.65),
    K_FC   = c(0.1, 0.1, 0.1, 0.1),
    Ksat_own = c(18.326, 18.326, 18.326, 18.326),
    check.names = FALSE
  ))
  ptf_input_edited <- table_edit_server(
    "ptf_input_tbl", ptf_input_data,
    allowRowModif = FALSE,
    col_type    = c("numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"),
    col_disable = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    col_width   = c(56, 66, 66, 66, 66, 60, 56, 60, 80)
  )

  # Auto-fill the BD column from the engine's W_BDLayer (e.g. after xlsm upload)
  # until the user edits the table themselves.
  ptf_touched <- reactiveVal(FALSE)
  observeEvent(ptf_input_edited(), { ptf_touched(TRUE) }, ignoreInit = TRUE)
  observe({
    if (isTRUE(ptf_touched())) return()
    bdv <- NULL
    for (lk in arr_ids_df$ui_id[arr_ids_df$arr == "layer_df"]) {
      ldf <- rv_arr[[lk]]
      if (!is.null(ldf) && "W_BDLayer" %in% names(ldf) && nrow(ldf) == 4) {
        bdv <- as.numeric(ldf$W_BDLayer); break
      }
    }
    if (!is.null(bdv) && length(bdv) >= 4) {
      cur <- ptf_input_data()
      cur$BD <- bdv[1:4]
      ptf_input_data(cur)
    }
  })

  observeEvent(input$ptf_apply, {
    method <- as.integer(input$ptf_method)
    top_layer <- as.integer(input$ptf_top)
    use_ptf_ksat <- as.integer(input$ptf_use_ptf_ksat)
    med_sand <- input$ptf_medsand %||% 290

    # Read the soil properties table
    tb <- ptf_input_edited(); if (is.null(tb)) tb <- ptf_input_data()
    getcol <- function(nm) as.numeric(tb[[nm]])
    clay <- getcol("Clay"); silt <- getcol("Silt"); orgC <- getcol("OrgC")
    bd_vals <- getcol("BD"); cec <- getcol("CEC"); ph <- getcol("pH")
    kfc <- getcol("K_FC"); ksat_own <- getcol("Ksat_own")

    vg_list <- lapply(1:4, function(L) {
      pedotransfer_vg(
        clay = clay[L], silt = silt[L], orgC = orgC[L], bd = bd_vals[L],
        top  = if (top_layer == 1 && L == 1) 1 else 0,
        cec  = cec[L] %||% 10, ph = ph[L] %||% 5.5, method = method
      )
    })
    ptf_result(vg_list)

    theta_sat <- sapply(vg_list, function(v) v$theta_sat)
    alpha     <- sapply(vg_list, function(v) v$alpha)
    n_vals    <- sapply(vg_list, function(v) v$n)

    # Calculated Ksat = the PTF estimate (xlsm B47 -> B57/C57).
    ptf_ksat_est <- sapply(vg_list, function(v) v$ksat)
    # Ksat actually used: Yes -> calculated estimate; No -> the user's own column.
    ksat_vals <- if (use_ptf_ksat == 1) ptf_ksat_est else ksat_own
    ptf_ksat_used(ksat_vals)

    # Field capacity per layer (based on critical K)
    fc_vals <- sapply(1:4, function(L)
      vg_field_capacity_kcrit(vg_list[[L]], k_crit = kfc[L] %||% 0.1, med_sand = med_sand))

    cat("[Pedotransfer] Method:", method, "| use PTF Ksat:", use_ptf_ksat, "\n")
    cat("[Pedotransfer] theta_sat:", paste(round(theta_sat,4), collapse=", "), "\n")
    cat("[Pedotransfer] alpha:", paste(round(alpha,4), collapse=", "), "\n")
    cat("[Pedotransfer] n:", paste(round(n_vals,4), collapse=", "), "\n")
    cat("[Pedotransfer] Ksat calculated:", paste(round(ptf_ksat_est,3), collapse=", "), "\n")
    cat("[Pedotransfer] Ksat applied:", paste(round(ksat_vals,3), collapse=", "), "\n")
    cat("[Pedotransfer] FieldCap (K-crit):", paste(round(fc_vals,4), collapse=", "), "\n")
    cat("[Pedotransfer] BD:", paste(bd_vals, collapse=", "), "\n")

    # Write per-layer VG params + BD + Ksat + field capacity to layer fragments
    for (lk in arr_ids_df$ui_id[arr_ids_df$arr == "layer_df"]) {
      ldf <- rv_arr[[lk]]
      if (is.null(ldf) || nrow(ldf) != 4) next
      if ("W_BDLayer"       %in% names(ldf)) ldf$W_BDLayer       <- bd_vals
      if ("W_ThetaSat"      %in% names(ldf)) ldf$W_ThetaSat      <- theta_sat
      if ("W_Alpha"         %in% names(ldf)) ldf$W_Alpha         <- alpha
      if ("W_n"             %in% names(ldf)) ldf$W_n             <- n_vals
      if ("S_KsatInitV"     %in% names(ldf)) ldf$S_KsatInitV     <- ksat_vals
      if ("W_FieldCapKcrit" %in% names(ldf)) ldf$W_FieldCapKcrit <- fc_vals
      rv_arr[[lk]] <- ldf
    }
    cat("[Pedotransfer] Wrote VG params, BD, Ksat, FieldCap to layer fragments\n")

    show_alert(
      "Pedotransfer Applied (4 layers)",
      paste0("Per-layer van Genuchten parameters computed (method ", method, ").\n",
             "Ksat applied: ", paste(round(ksat_vals,2), collapse=", "), "\n",
             "FieldCap: ", paste(round(fc_vals,3), collapse=", ")),
      type = "success"
    )
  })

  output$ptf_results <- renderTable({
    vg_list <- ptf_result()
    if (is.null(vg_list)) return(data.frame(Info = "Click 'Compute & Apply' to calculate"))
    tb <- ptf_input_edited(); if (is.null(tb)) tb <- ptf_input_data()
    kfc <- as.numeric(tb[["K_FC"]])
    fc_vals <- sapply(1:4, function(L)
      vg_field_capacity_kcrit(vg_list[[L]], k_crit = kfc[L] %||% 0.1))
    # Show the Ksat ACTUALLY applied (calculated if Yes, own if No)
    ksat_show <- ptf_ksat_used()
    if (is.null(ksat_show)) ksat_show <- sapply(vg_list, function(v) v$ksat)
    data.frame(
      Layer       = paste("Layer", 1:4),
      Theta_sat   = round(sapply(vg_list, function(v) v$theta_sat), 4),
      Ksat_used   = round(ksat_show, 3),
      Alpha       = round(sapply(vg_list, function(v) v$alpha), 4),
      n           = round(sapply(vg_list, function(v) v$n), 4),
      Theta_res   = round(sapply(vg_list, function(v) v$theta_res), 4),
      FieldCap    = round(fc_vals, 4),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE)

  output$ptf_curve_plot <- renderPlot({
    vg_list <- ptf_result()
    if (is.null(vg_list)) {
      plot.new(); text(0.5, 0.5, "Press 'Compute & Apply' to see the curves", cex = 1.2, col = "gray")
      return()
    }
    cols <- c("#8B3E04", "#FA842B", "#61701f", "#007D92")
    par(mar = c(4, 4, 2, 1))
    plot(NA, xlim = c(0, 4.2), ylim = c(0, max(sapply(vg_list, function(v) v$theta_sat))*1.05),
         xlab = "pF (log10 |h| in cm)", ylab = "theta (m3/m3)",
         main = "Water Retention Curves (per layer)", cex.main = 1)
    for (L in 1:4) {
      rc <- vg_retention_curve(vg_list[[L]])
      lines(rc$pF, rc$theta, lwd = 2.5, col = cols[L])
    }
    legend("topright", paste("Layer", 1:4), col = cols, lwd = 2.5, cex = 0.85, bg = "white")
    grid(col = "gray90")
  })

  ### Phosphorus calculator (Phase 5) ############################
  phos_result <- reactiveVal(NULL)

  # Auto-fill the phosphorus BD inputs from the engine's W_BDLayer
  # (e.g. after xlsm upload) until the user edits them manually.
  phos_bd_touched <- reactiveVal(FALSE)
  observeEvent(list(input$phos_bd_L1, input$phos_bd_L2,
                    input$phos_bd_L3, input$phos_bd_L4), {
    phos_bd_touched(TRUE)
  }, ignoreInit = TRUE)

  observe({
    if (isTRUE(phos_bd_touched())) return()
    bd_df <- rv_arr[["input_array_layer_27_0"]]
    if (!is.null(bd_df) && "W_BDLayer" %in% names(bd_df)) {
      bdv <- as.numeric(bd_df$W_BDLayer)
      if (length(bdv) >= 4) {
        updateNumericInput(session, "phos_bd_L1", value = bdv[1])
        updateNumericInput(session, "phos_bd_L2", value = bdv[2])
        updateNumericInput(session, "phos_bd_L3", value = bdv[3])
        updateNumericInput(session, "phos_bd_L4", value = bdv[4])
      }
    }
  })

  # P-Bray paste-capable table (4 layers x 4 zones)
  pbray_data <- reactiveVal(data.frame(
    Layer = 1:4, Zone_1 = 5.38, Zone_2 = 5.38, Zone_3 = 5.38, Zone_4 = 5.38
  ))
  pbray_edited <- table_edit_server(
    "pbray_table", pbray_data,
    allowRowModif = FALSE,
    col_type    = c("numeric", "numeric", "numeric", "numeric", "numeric"),
    col_disable = c(TRUE, FALSE, FALSE, FALSE, FALSE),
    col_width   = c(70, 90, 90, 90, 90)
  )

  observeEvent(input$phos_apply, {
    method <- as.integer(input$phos_method)
    # Read P-Bray from the editable table
    pb <- pbray_edited()
    if (is.null(pb)) pb <- pbray_data()
    pbray_mat <- as.matrix(pb[, -1, drop = FALSE])  # drop Layer col -> 4x4
    storage.mode(pbray_mat) <- "numeric"
    bd <- c(input$phos_bd_L1, input$phos_bd_L2,
            input$phos_bd_L3, input$phos_bd_L4)
    soil_ids <- as.integer(c(input$phos_soil_L1, input$phos_soil_L2,
                              input$phos_soil_L3, input$phos_soil_L4))
    # Override custom coefficients for soil types 1-4
    for (sid in unique(soil_ids[soil_ids <= 4])) {
      phos_soil_types$SorbMax1[sid] <<- input$phos_custom_sm1 %||% phos_soil_types$SorbMax1[sid]
      phos_soil_types$SorbMax2[sid] <<- input$phos_custom_sm2 %||% phos_soil_types$SorbMax2[sid]
      phos_soil_types$SorbAff1[sid] <<- input$phos_custom_sa1 %||% phos_soil_types$SorbAff1[sid]
      phos_soil_types$SorbAff2[sid] <<- input$phos_custom_sa2 %||% phos_soil_types$SorbAff2[sid]
    }
    res <- build_n_pstparam(pbray_mat, soil_ids, bd, method)
    phos_result(res)
    cat("[Phosphorus] P-Bray matrix:\n"); print(pbray_mat)
    cat("[Phosphorus] Soil types:", soil_ids, "| BD:", bd, "\n")
    cat("[Phosphorus] N_PStInit (L1):", res$N_PStInit[1,], "\n")
    cat("[Phosphorus] PStSMin:", res$PStSMin, "\n")
    cat("[Phosphorus] PStSMax:", res$PStSMax, "\n")
    zl_key <- "input_array_zonelayer_999_0"
    zl_df <- rv_arr[[zl_key]]
    if (!is.null(zl_df) && "N_PStParam" %in% names(zl_df)) {
      zl_df$N_PStParam <- as.vector(t(res$N_PStInit))
      rv_arr[[zl_key]] <- zl_df
      cat("[Phosphorus] Wrote N_PStParam\n")
    } else { cat("[Phosphorus] WARNING: N_PStParam key not found\n") }
    layer_key <- "input_array_layer_81_0"
    lay_df <- rv_arr[[layer_key]]
    if (!is.null(lay_df)) {
      if ("PStMin" %in% names(lay_df)) lay_df$PStMin <- res$PStSMin
      if ("PStMax" %in% names(lay_df)) lay_df$PStMax <- res$PStSMax
      rv_arr[[layer_key]] <- lay_df
      cat("[Phosphorus] Wrote PStMin/PStMax\n")
    }
    show_alert("Phosphorus Applied",
      sprintf("N_PStInit(L1,Z1)=%.6f  PStMin(L1)=%.8f  PStMax(L1)=%.4f",
              res$N_PStInit[1,1], res$PStSMin[1], res$PStSMax[1]),
      type = "success")

    # Also write W_BDLayer to the engine (same BD values used for P calc)   # phase5
    # so whichever calculator the user touches last sets the engine BD.
    all_layer_keys <- arr_ids_df$ui_id[arr_ids_df$arr == "layer_df"]
    for (lk in all_layer_keys) {
      ldf <- rv_arr[[lk]]
      if (!is.null(ldf) && "W_BDLayer" %in% names(ldf)) {
        ldf$W_BDLayer <- bd
        rv_arr[[lk]] <- ldf
        cat("[Phosphorus] Also wrote W_BDLayer =", paste(bd, collapse=","), "to", lk, "\n")
      }
    }
  })

  output$phos_results <- renderTable({
    res <- phos_result()
    if (is.null(res)) return(data.frame(Parameter = "Click 'Compute & Apply' to calculate",
                                         Value = ""))
    data.frame(
      Parameter = c(paste0("N_PStInit L", 1:4, " Z1"),
                    paste0("PStSMin L", 1:4),
                    paste0("PStSMax L", 1:4)),
      Value = c(res$N_PStInit[, 1], res$PStSMin, res$PStSMax)
    )
  }, striped = TRUE, hover = TRUE, digits = 8)

  output$phos_curve_plot <- renderPlot({
    res <- phos_result()
    if (is.null(res)) {
      plot.new(); text(0.5, 0.5, "Press 'Compute & Apply' to see the curve", cex = 1.2, col = "gray")
      return()
    }
    ka <- res$Ka[[1]]
    par(mar = c(4, 5, 2, 1))
    plot(ka$conc * 1000, ka$pmobile[-1], type = "l", lwd = 2.5, col = "#007D92",
         xlab = "P concentration (\u00D710\u207B\u00B3 mg/cm\u00B3)",
         ylab = "P-mobile (mg/cm\u00B3)",
         main = "Langmuir Sorption Isotherm (Layer 1)", cex.main = 1,
         log = "x")
    grid(col = "gray90")
  })

  ### Merged Social/Private price table (Profitability) ###########
  # Presents P_PriceFert, P_CostExtOrg, P_CPestContPrice, P_FenceMatCost,
  # P_UnitLabCost in ONE table with Social & Private columns. Reads/writes
  # the same rv_arr fragments the engine uses.
  # Row layout (each maps to a specific array cell):
  #   1 Fertilizer N   -> nutprice P_PriceFert[N]
  #   2 Fertilizer P   -> nutprice P_PriceFert[P]
  #   3 ExtOrg 1       -> inpprice P_CostExtOrg[ExtOrg1]
  #   4 ExtOrg 2       -> inpprice P_CostExtOrg[ExtOrg2]
  #   5 Herbicide      -> price P_CPestContPrice
  #   6 Fence material -> price P_FenceMatCost
  #   7 Labour cost    -> price P_UnitLabCost
  price_row_labels <- c("Fertilizer N (kg-1)", "Fertilizer P (kg-1)",
                        "External Organic 1 (kg-1)", "External Organic 2 (kg-1)",
                        "Herbicide (ha-1)", "Fence material (ha-1)",
                        "Labour cost (person-day-1)")

  # Read a value from the relevant fragment, given var + private/social index
  # PriceType order in arrays: Private = 1, Social = 2
  price_merged_data <- reactive({
    nut <- rv_arr[["input_array_nutprice_38_0"]]     # P_PriceFert: [N-Priv,P-Priv,N-Soc,P-Soc]
    inp <- rv_arr[["input_array_inpprice_38_0"]]     # P_CostExtOrg: [E1-Priv,E2-Priv,E1-Soc,E2-Soc]
    prc <- rv_arr[["input_array_price_38_0"]]        # price_df: [Priv, Soc]
    g <- function(df, col, i) if (!is.null(df) && col %in% names(df)) as.numeric(df[[col]])[i] else NA
    data.frame(
      Item = price_row_labels,
      Private = c(g(nut,"P_PriceFert",1), g(nut,"P_PriceFert",2),
                  g(inp,"P_CostExtOrg",1), g(inp,"P_CostExtOrg",2),
                  g(prc,"P_CPestContPrice",1), g(prc,"P_FenceMatCost",1),
                  g(prc,"P_UnitLabCost",1)),
      Social  = c(g(nut,"P_PriceFert",3), g(nut,"P_PriceFert",4),
                  g(inp,"P_CostExtOrg",3), g(inp,"P_CostExtOrg",4),
                  g(prc,"P_CPestContPrice",2), g(prc,"P_FenceMatCost",2),
                  g(prc,"P_UnitLabCost",2)),
      check.names = FALSE
    )
  })

  price_merged_edited <- table_edit_server(
    "price_merged_tbl", price_merged_data,
    allowRowModif = FALSE,
    col_type    = c("text", "numeric", "numeric"),
    col_disable = c(TRUE, FALSE, FALSE),
    col_width   = c(200, 110, 110)
  )

  observeEvent(price_merged_edited(), {
    df <- price_merged_edited()
    if (is.null(df) || nrow(df) != 7) return()
    priv <- as.numeric(df$Private); soc <- as.numeric(df$Social)
    if (any(is.na(c(priv, soc)))) return()

    # nutprice_df: P_PriceFert = [N-Priv, P-Priv, N-Soc, P-Soc]
    nut <- rv_arr[["input_array_nutprice_38_0"]]
    if (!is.null(nut) && "P_PriceFert" %in% names(nut)) {
      nut$P_PriceFert <- c(priv[1], priv[2], soc[1], soc[2])
      rv_arr[["input_array_nutprice_38_0"]] <- nut
    }
    # inpprice_df: P_CostExtOrg = [E1-Priv, E2-Priv, E1-Soc, E2-Soc]
    inp <- rv_arr[["input_array_inpprice_38_0"]]
    if (!is.null(inp) && "P_CostExtOrg" %in% names(inp)) {
      inp$P_CostExtOrg <- c(priv[3], priv[4], soc[3], soc[4])
      rv_arr[["input_array_inpprice_38_0"]] <- inp
    }
    # price_df: each var = [Priv, Soc]
    prc <- rv_arr[["input_array_price_38_0"]]
    if (!is.null(prc)) {
      if ("P_CPestContPrice" %in% names(prc)) prc$P_CPestContPrice <- c(priv[5], soc[5])
      if ("P_FenceMatCost" %in% names(prc))   prc$P_FenceMatCost   <- c(priv[6], soc[6])
      if ("P_UnitLabCost" %in% names(prc))    prc$P_UnitLabCost    <- c(priv[7], soc[7])
      rv_arr[["input_array_price_38_0"]] <- prc
    }
    cat("[Prices] Merged table applied. Private:", paste(priv, collapse=","),
        "| Social:", paste(soc, collapse=","), "\n")
  })

  ### Tree page -> Pruning Event shortcut ##########################
  observeEvent(input$goto_pruning, {
    nav_select("main_page", "Additional Parameters")
    nav_select("add_panel", "Management")
    nav_select("subnav_Management", "Prunning Event")
  })

  ### Soil Evaporation guiding question -> switch panel ############
  observeEvent(input$evap_guide, {
    nav_select("evap_type_panel", input$evap_guide)
    # TEMP_PotEvapConst_is: 1 = constant potential evaporation, 0 = daily or monthly
    val <- if (input$evap_guide == "Evap Type 1") 1 else 0
    rv_var[["TEMP_PotEvapConst_is"]] <- val
    desc <- switch(input$evap_guide,
                   "Evap Type 0" = "Daily evaporation data",
                   "Evap Type 1" = "Constant potential evaporation",
                   "Evap Type 2" = "Monthly average evaporation",
                   input$evap_guide)
    cat("[Soil Evaporation] Selected:", desc, "-> TEMP_PotEvapConst_is =", val, "\n")
  }, ignoreInit = TRUE)

  ### Soil Temperature guiding question -> switch panel + set TEMP_AType ####
  observeEvent(input$stemp_guide, {
    nav_select("stemp_type_panel", input$stemp_guide)
    # Engine: TEMP_AType=1 -> constant, =3 -> daily, else -> monthly
    atype <- switch(input$stemp_guide,
                    "Soil Temp Type 1" = 1,
                    "Soil Temp Type 2" = 2,
                    3)
    rv_var[["TEMP_AType"]] <- atype
    desc <- switch(input$stemp_guide,
                   "Soil Temp Type 1" = "Constant soil temperature",
                   "Soil Temp Type 2" = "Monthly average soil temperature",
                   "Soil Temp Type 0" = "Daily soil temperature data",
                   input$stemp_guide)
    cat("[Soil Temperature] Selected:", desc, "-> TEMP_AType =", atype, "\n")
  }, ignoreInit = TRUE)

  ### Rainfall guiding question -> switch panel + set RAIN_AType ####
  observeEvent(input$rain_guide, {
    nav_select("rain_type_panel", input$rain_guide)
    # RAIN_AType: 1=daily data, 2=random from monthly, 3=random from annual, 4=monthly tabulated
    atype <- switch(input$rain_guide,
                    "Rain Type 1" = 1,
                    "Rain Type 2" = 2,
                    "Rain Type 3" = 3,
                    "Rain Type 4" = 4, 1)
    rv_var[["RAIN_AType"]] <- atype
    desc <- switch(input$rain_guide,
                   "Rain Type 1" = "Daily measured rainfall data (RAIN_data)",
                   "Rain Type 2" = "Random generator from monthly data (rainfall simulator)",
                   "Rain Type 3" = "Random generator from annual average only",
                   "Rain Type 4" = "Monthly average tabulated data (RAIN_MonthTot)",
                   input$rain_guide)
    cat("[Rainfall] Selected:", desc, "-> RAIN_AType =", atype, "\n")
  }, ignoreInit = TRUE)

  ### Home page navigation (deep-linked to subtabs) ################
  observeEvent(input$nav_to_tree, {                                          # homenav
    nav_select("main_page", "Core Parameters")
    nav_select("core_panel", "Tree")
  })
  observeEvent(input$nav_to_crop, {                                          # homenav
    nav_select("main_page", "Core Parameters")
    nav_select("core_panel", "Crop")
  })
  observeEvent(input$nav_to_af, {                                            # homenav
    nav_select("main_page", "Core Parameters")
    nav_select("core_panel", "Agroforestry System")
  })
  observeEvent(input$nav_to_weather, {                                       # homenav
    nav_select("main_page", "Core Parameters")
    nav_select("core_panel", "Climate")
  })
  observeEvent(input$nav_to_soilnutrient, {                                  # homenav
    nav_select("main_page", "Core Parameters")
    nav_select("core_panel", "Soil")
  })
  # "Input Section" reveals the module cards on the homepage itself
  home_modules_shown <- reactiveVal(FALSE)
  observeEvent(input$nav_to_inputs, {                                        # homenav
    home_modules_shown(TRUE)
  })
  output$show_home_modules <- reactive({ home_modules_shown() })
  outputOptions(output, "show_home_modules", suspendWhenHidden = FALSE)
  observeEvent(input$nav_to_management, {                                    # homenav
    nav_select("main_page", "Additional Parameters")
    nav_select("add_panel", "Management")
  })
  observeEvent(input$nav_to_economy, {                                       # homenav
    nav_select("main_page", "Additional Parameters")
    nav_select("add_panel", "Economy")
  })
  observeEvent(input$nav_to_addtree, {                                       # homenav
    nav_select("main_page", "Additional Parameters")
    nav_select("add_panel", "Tree")
  })
  observeEvent(input$nav_to_addsoil, {                                       # homenav
    nav_select("main_page", "Additional Parameters")
    nav_select("add_panel", "Soil")
  })
  observeEvent(input$nav_to_slashburn, {                                     # homenav
    nav_select("main_page", "Additional Parameters")
    nav_select("add_panel", "Management")
  })
  observeEvent(input$nav_to_sim, {                                           # homenav
    nav_select("main_page", "Simulation")
  })
  observeEvent(input$nav_to_about, {                                         # homenav
    nav_select("main_page", "help")
    nav_select("info_panel", "About")
  })
  observeEvent(input$nav_to_tutorial, {                                      # homenav
    nav_select("main_page", "help")
    nav_select("info_panel", "Tutorial")
  })

}
