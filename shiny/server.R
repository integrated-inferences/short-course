library(shiny)
library(CausalQueries)
library(DT)
library(dplyr)
if (!requireNamespace("rintrojs", quietly = TRUE)) {
  install.packages("rintrojs")
}
library(rintrojs)

cq_call <- function(name, ...) {
  fn <- getFromNamespace(name, "CausalQueries")
  fn(...)
}

build_event_label <- function(nodes, values) {
  parts <- mapply(function(node, value) {
    if (is.na(value)) {
      return(NULL)
    }
    paste0(node, value)
  }, nodes, values, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  label <- paste(Filter(Negate(is.null), parts), collapse = "")
  if (label == "") {
    return(NA_character_)
  }
  label
}

by_hand <- function(model, data, query) {
  nodes <- model$nodes
  values <- as.numeric(data[1, nodes, drop = TRUE])
  event_label <- build_event_label(nodes, values)
  if (is.na(event_label)) {
    stop("Please provide at least one observed value (not all NA).")
  }

  mapping <- cq_call("get_data_families", model, mapping_only = TRUE) |>
    data.frame()
  if (!(event_label %in% rownames(mapping))) {
    stop("Provided data pattern does not match any known data family.")
  }

  amb <- grab(model, what = "ambiguities_matrix")

  query_types <- get_query_types(model, query)$types
  if (!is.logical(query_types)) {
    stop("Please enter a logical case-level query (e.g. 'Y[X=1] > Y[X=0]').")
  }

  result <- amb |>
    data.frame() |>
    dplyr::mutate(
      type = rownames(amb),
      in_query = query_types,
      priors = cq_call("get_type_prob", model)
    )

  consistent_types <- colnames(mapping)[mapping[event_label, , drop = TRUE] == 1]
  if (length(consistent_types) == 0) {
    stop("No complete data types are consistent with the provided data.")
  }
  result$in_data <- rowSums(result[, consistent_types, drop = FALSE]) > 0

  denominator <- sum(result$priors[result$in_data])
  if (is.na(denominator) || denominator <= 0) {
    stop("No causal types are consistent with the data. Check the data or relax restrictions.")
  }
  numerator <- sum(result$priors[result$in_data & result$in_query])
  posterior <- numerator / denominator

  list(
    result = result,
    denominator = denominator,
    numerator = numerator,
    posterior = posterior,
    data_type_name = event_label
  )
}

server <- function(input, output, session) {
  model_reactive <- reactiveVal(NULL)
  base_model_reactive <- reactiveVal(NULL)
  restrictions_reactive <- reactiveVal(list(keep = list(), drop = list()))
  nl_restrictions_reactive <- reactiveVal(list())
  parameters_reactive <- reactiveVal(list())
  updated_model_reactive <- reactiveVal(NULL)
  compact_data_reactive <- reactiveVal(NULL)
  query_rows <- reactiveVal(c(1))

  notify_error <- function(message) {
    showNotification(message, type = "error")
  }
  notify_warn <- function(message) {
    showNotification(message, type = "warning")
  }
  notify_ok <- function(message) {
    showNotification(message, type = "message")
  }

  tour_enabled <- reactiveVal(FALSE)

  disable_guidance <- function() {
    tour_enabled(FALSE)
    tour_state$model <- TRUE
    tour_state$restrictions <- TRUE
    tour_state$parameters <- TRUE
    tour_state$update <- TRUE
    tour_state$query <- TRUE
  }

  start_guided_tour <- function() {
    tour_enabled(TRUE)
    tour_state$model <- FALSE
    tour_state$restrictions <- FALSE
    tour_state$parameters <- FALSE
    tour_state$update <- FALSE
    tour_state$query <- FALSE
    introjs(
      session,
      options = list(
        steps = c(
          tour_steps_model,
          tour_steps_restrictions,
          tour_steps_parameters,
          tour_steps_update,
          tour_steps_query
        ),
        showProgress = TRUE,
        scrollToElement = TRUE
      )
    )
  }

  session$onFlushed(function() {
    showModal(modalDialog(
      title = "Welcome",
      tags$div(
        style = "text-align: center; margin-bottom: 10px;",
        tags$div(
          style = "display: inline-block; background: #ffffff; padding: 6px; border-radius: 8px;",
          tags$img(src = "causalqueries-hex.png", alt = "CausalQueries logo", height = "120px")
        )
      ),
      p("This app helps you make, refine, update, and query causal models."),
      p("Choose how you'd like to begin:"),
      footer = tagList(
        actionButton("welcome_go", "Go to app", class = "btn-primary"),
        actionButton("welcome_guide", "Guided walk-through", class = "btn-success")
      ),
      easyClose = FALSE
    ))
  }, once = TRUE)

  observeEvent(input$welcome_go, {
    removeModal()
    disable_guidance()
  })

  observeEvent(input$welcome_guide, {
    removeModal()
    showModal(modalDialog(
      title = "Guided walk-through",
      tags$ol(
        tags$li("Make a model (required)."),
        tags$li("Optionally refine with restrictions and parameters."),
        tags$li("Update with data if you want posterior beliefs."),
        tags$li("Pose queries and plot results."),
        tags$li("Copy replication code if needed.")
      ),
      footer = tagList(
        actionButton("welcome_start", "Start", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$welcome_start, {
    removeModal()
    updateTabsetPanel(session, "main_tabs", selected = "Make Model")
    start_guided_tour()
  })

  observeEvent(input$guide_me, {
    updateTabsetPanel(session, "main_tabs", selected = "Make Model")
    start_guided_tour()
  })

  tour_steps_model <- list(
    list(
      element = "#model_panel",
      intro = "Do this first: enter your model statement.",
      position = "bottom"
    )
  )

  tour_steps_restrictions <- list(
    list(
      element = "#restrictions_panel",
      intro = "If you want, you can now restrict your model. Click Next to see parameters.",
      position = "bottom"
    ),
    list(
      element = "#parameters_panel",
      intro = "Optionally, set parameters here. When ready, go to Update Model (or skip straight to Query).",
      position = "bottom"
    )
  )

  tour_steps_parameters <- list(
    list(
      element = "#parameters_panel",
      intro = "Optionally, set parameters here. When ready, go to Update Model (or skip straight to Query).",
      position = "bottom"
    )
  )

  tour_steps_update <- list(
    list(
      element = "#update_panel",
      intro = "When ready to update, set options and click Update Model.",
      position = "bottom"
    )
  )

  tour_steps_query <- list(
    list(
      element = "#query_panel",
      intro = "Enter queries and (optionally) givens, then click Compute Queries.",
      position = "bottom"
    )
  )

  tour_state <- reactiveValues(
    model = FALSE,
    restrictions = FALSE,
    parameters = FALSE,
    update = FALSE,
    query = FALSE
  )

  run_tour <- function(flag, steps) {
    if (!isolate(tour_enabled())) {
      return()
    }
    if (isolate(tour_state[[flag]])) {
      return()
    }
    introjs(session, options = list(steps = steps, showProgress = TRUE, scrollToElement = TRUE))
    tour_state[[flag]] <- TRUE
  }

  format_given_label <- function(given) {
    if (is.null(given) || is.na(given) || given == "") {
      return("")
    }
    parts <- strsplit(given, "\\.")[[1]]
    if (length(parts) < 2) {
      return(given)
    }
    pairs <- split(parts, ceiling(seq_along(parts) / 2))
    labels <- vapply(pairs, function(pair) {
      if (length(pair) < 2) {
        return(pair[1])
      }
      paste0(pair[1], "=", pair[2])
    }, character(1))
    paste(labels, collapse = ", ")
  }

  format_param_set_label <- function(param_set) {
    parts <- strsplit(param_set, "\\.")[[1]]
    node <- parts[1]
    if (length(parts) == 1) {
      return(node)
    }
    givens <- parts[-1]
    pairs <- split(givens, ceiling(seq_along(givens) / 2))
    labels <- vapply(pairs, function(pair) {
      if (length(pair) < 2) {
        return(pair[1])
      }
      paste0(pair[1], "=", pair[2])
    }, character(1))
    paste0(node, " given ", paste(labels, collapse = ", "))
  }

  format_param_label <- function(nodal_type, given, prefix = "Prob type") {
    given_label <- format_given_label(given)
    if (given_label == "") {
      return(paste(prefix, nodal_type))
    }
    paste(prefix, nodal_type, "when", given_label)
  }

  format_char_vector <- function(values) {
    if (length(values) == 1) {
      return(paste0("'", values, "'"))
    }
    paste0("c(", paste0("'", values, "'", collapse = ", "), ")")
  }

  format_num_vector <- function(values) {
    if (length(values) == 1) {
      return(format(values, scientific = FALSE))
    }
    paste0("c(", paste(format(values, scientific = FALSE), collapse = ", "), ")")
  }

  type_help_ui <- function(node, parents) {
    parent_count <- length(parents)
    if (parent_count == 0) {
      return(tagList(
        tags$p(paste0(node, " has no parents so its two types are simply:")),
        tags$ul(
          tags$li(paste0("0 if ", node, " is 0 (absent any intervention)")),
          tags$li(paste0("1 if ", node, " is 1 (absent any intervention)"))
        )
      ))
    }

    if (parent_count == 1) {
      parent <- parents[1]
      return(tagList(
        tags$p(paste0(node, " has one parent so its four types are:")),
        tags$ul(
          tags$li(paste0("00: ", node, " = 0 regardless of value of ", parent)),
          tags$li(paste0("01: ", node, " = 0 if ", parent, " = 0 and 1 if ", parent, " = 1")),
          tags$li(paste0("10: ", node, " = 1 if ", parent, " = 0 and 0 if ", parent, " = 1")),
          tags$li(paste0("11: ", node, " = 1 regardless of the value of ", parent))
        )
      ))
    }

    value_count <- paste0("2<sup>2<sup>", parent_count, "</sup></sup> = ", 2^(2^parent_count))
    parent_values <- paste0("2<sup>", parent_count, "</sup> = ", 2^parent_count)
    tagList(
      tags$p(HTML(paste0(
        node, " has ", parent_count,
        " parents and so ", value_count, " possible values. ",
        "The guide below shows how to interpret the values ", node,
        " takes for each of the possible ", parent_values, " values of the parents."
      )))
    )
  }

  format_labels_list <- function(labels_list) {
    parts <- vapply(names(labels_list), function(node) {
      types <- labels_list[[node]]
      paste0(node, " = ", format_char_vector(types))
    }, character(1))
    paste0("list(", paste(parts, collapse = ", "), ")")
  }

  apply_type_restrictions <- function(model, restrictions) {
    if (length(restrictions) == 0) {
      return(model)
    }
    keep_list <- restrictions$keep
    drop_list <- restrictions$drop
    if (length(keep_list) > 0) {
      model <- set_restrictions(
        model,
        labels = keep_list,
        keep = TRUE
      )
    }
    if (length(drop_list) > 0) {
      model <- set_restrictions(
        model,
        labels = drop_list,
        keep = FALSE
      )
    }
    model
  }

  apply_nl_restrictions <- function(model, nl_restrictions) {
    if (length(nl_restrictions) == 0) {
      return(model)
    }
    for (node_name in names(nl_restrictions)) {
      entry <- nl_restrictions[[node_name]]
      drop_types <- entry$drop_types
      keep_types <- entry$keep_types

      if (!is.null(drop_types) && length(drop_types) > 0) {
        model <- set_restrictions(
          model,
          labels = setNames(list(drop_types), node_name),
          keep = FALSE
        )
      }
      if (!is.null(keep_types) && length(keep_types) > 0) {
        keep_effective <- if (!is.null(drop_types)) setdiff(keep_types, drop_types) else keep_types
        if (length(keep_effective) > 0) {
          model <- set_restrictions(
            model,
            labels = setNames(list(keep_effective), node_name),
            keep = TRUE
          )
        }
      }
    }
    model
  }

  apply_parameters <- function(model, parameters) {
    if (length(parameters) == 0) {
      return(model)
    }
    for (node_name in names(parameters)) {
      node_pars <- parameters[[node_name]]
      if (length(node_pars) > 0) {
        model <- set_parameters(
          model,
          param_names = names(node_pars),
          parameters = as.numeric(node_pars)
        )
      }
    }
    model
  }

  default_query_text <- function(model) {
    if (is.null(model) || is.null(model$nodes) || length(model$nodes) == 0) {
      return("Y[X=1] > Y[X=0]")
    }
    nodes <- model$nodes
    x1 <- nodes[1]
    xn <- nodes[length(nodes)]
    paste0(xn, "[", x1, "=1] > ", xn, "[", x1, "=0]")
  }

  rebuild_model <- function() {
    base_model <- base_model_reactive()
    if (is.null(base_model)) {
      return(NULL)
    }
    model <- base_model
    model <- apply_type_restrictions(model, restrictions_reactive())
    model <- apply_nl_restrictions(model, nl_restrictions_reactive())
    model <- apply_parameters(model, parameters_reactive())
    model
  }

  rebuild_or_notify <- function() {
    model_rebuilt <- rebuild_model()
    if (is.null(model_rebuilt)) {
      notify_error("Base model not found. Please recreate the model.")
      return(NULL)
    }
    model_reactive(model_rebuilt)
    model_rebuilt
  }

  get_parents_for_node <- function(model, node) {
    dag <- tryCatch(grab(model, what = "dag"), error = function(e) NULL)
    if (is.null(dag) && !is.null(model$dag)) {
      dag <- model$dag
    }
    if (is.null(dag) && !is.null(model$statement)) {
      dag <- tryCatch(cq_call("make_dag", model$statement), error = function(e) NULL)
    }
    dag <- tryCatch(data.frame(dag), error = function(e) NULL)
    if (is.null(dag) || !all(c("v", "w") %in% names(dag))) {
      return(character(0))
    }
    if ("e" %in% names(dag)) {
      dag <- dag[dag$e == "->", , drop = FALSE]
    }
    unique(as.character(dag$v[dag$w == node]))
  }

  format_event_label <- function(types_df, event, nodes) {
    row <- types_df[types_df$event == event, nodes, drop = FALSE]
    if (nrow(row) == 0) {
      return(event)
    }
    parts <- mapply(function(node, value) {
      if (is.na(value)) {
        return(NULL)
      }
      paste0(node, "=", value)
    }, nodes, as.list(row[1, ]), SIMPLIFY = FALSE, USE.NAMES = FALSE)
    paste(Filter(Negate(is.null), parts), collapse = ", ")
  }

  data_types_for_model <- function(model) {
    all_types <- cq_call("get_all_data_types", model)
    types_df <- data.frame(all_types, stringsAsFactors = FALSE)
    if (!("event" %in% names(types_df))) {
      types_df$event <- rownames(types_df)
    }
    nodes <- model$nodes
    types_df$strategy <- apply(types_df[, nodes, drop = FALSE], 1, function(values) {
      observed <- nodes[!is.na(values)]
      paste(observed, collapse = "")
    })
    types_df
  }

  compact_data_from_inputs <- function(types_df, full_strategy, partial_strategies) {
    full_rows <- types_df[types_df$strategy == full_strategy, , drop = FALSE]
    full_events <- full_rows$event
    full_counts <- sapply(full_events, function(event) {
      input_id <- paste0("full_count_", event)
      value <- input[[input_id]]
      if (is.null(value) || is.na(value)) 0 else as.numeric(value)
    })

    compact_df <- data.frame(
      event = full_events,
      strategy = full_rows$strategy,
      count = as.numeric(full_counts),
      stringsAsFactors = FALSE
    )

    if (length(partial_strategies) == 0) {
      return(compact_df)
    }

    partial_rows <- lapply(partial_strategies, function(strategy_value) {
      strategy_rows <- types_df[types_df$strategy == strategy_value, , drop = FALSE]
      if (nrow(strategy_rows) == 0) {
        return(NULL)
      }
      event_counts <- sapply(strategy_rows$event, function(event) {
        input_id <- paste0("partial_count_", strategy_value, "_", event)
        value <- input[[input_id]]
        if (is.null(value) || is.na(value)) 0 else as.numeric(value)
      })
      data.frame(
        event = strategy_rows$event,
        strategy = strategy_value,
        count = as.numeric(event_counts),
        stringsAsFactors = FALSE
      )
    })

    partial_rows <- Filter(Negate(is.null), partial_rows)
    if (length(partial_rows) == 0) {
      return(compact_df)
    }

    partial_df <- do.call(rbind, partial_rows)
    combined <- rbind(compact_df, partial_df)
    combined <- combined[!is.na(combined$event) & combined$event != "", , drop = FALSE]
    combined <- combined[combined$event %in% types_df$event, , drop = FALSE]
    combined <- combined |>
      dplyr::group_by(event, strategy) |>
      dplyr::summarize(count = sum(count), .groups = "drop")
    attr(combined, "non_integer") <- any(abs(combined$count - round(combined$count)) > 1e-6)
    combined$count <- as.integer(round(combined$count))
    combined
  }

  build_nl_choices <- function(node, parents) {
    choices <- c(
      setNames(paste("always_0", node, sep = "|"),
               paste(node, "= 0 regardless of parents")),
      setNames(paste("always_1", node, sep = "|"),
               paste(node, "= 1 regardless of parents"))
    )

    for (p in parents) {
      choices <- c(
        choices,
        setNames(paste("increasing", p, node, sep = "|"),
                 paste(node, "increasing in", p)),
        setNames(paste("non_decreasing", p, node, sep = "|"),
                 paste(node, "non-decreasing in", p)),
        setNames(paste("decreasing", p, node, sep = "|"),
                 paste(node, "decreasing in", p)),
        setNames(paste("non_increasing", p, node, sep = "|"),
                 paste(node, "non-increasing in", p))
      )
    }

    if (length(parents) >= 2) {
      pairs <- combn(parents, 2, simplify = FALSE)
      for (pair in pairs) {
        p1 <- pair[1]
        p2 <- pair[2]
        choices <- c(
          choices,
          setNames(paste("interacts", p1, p2, node, sep = "|"),
                   paste(node, "has interaction between", p1, "and", p2)),
          setNames(paste("complements", p1, p2, node, sep = "|"),
                   paste(p1, "and", p2, "are complements for", node)),
          setNames(paste("substitutes", p1, p2, node, sep = "|"),
                   paste(p1, "and", p2, "are substitutes for", node))
        )
      }
    }

    choices
  }

  nl_types_from_selection <- function(base_model, node, selected) {
    parents <- get_parents_for_node(base_model, node)
    type_len <- 2 ^ length(parents)
    always_0_type <- paste(rep("0", type_len), collapse = "")
    always_1_type <- paste(rep("1", type_len), collapse = "")

    types_list <- list()
    for (sel in selected) {
      parts <- strsplit(sel, "\\|")[[1]]
      fn_name <- parts[1]
      if (fn_name %in% c("always_0", "always_1")) {
        types_list[[length(types_list) + 1]] <- if (fn_name == "always_0") always_0_type else always_1_type
      } else {
        fn <- get(fn_name, mode = "function")
        args <- as.list(parts[2:length(parts)])
        statement <- do.call(fn, args)
        types_map <- get_query_types(base_model, statement, map = "nodal_type")$types
        types_list[[length(types_list) + 1]] <- names(types_map)[types_map]
      }
    }

    unique(unlist(types_list))
  }

  output$model_plot <- renderPlot({
    model <- model_reactive()
    if (is.null(model)) {
      return(NULL)
    }
    plot(model)
  })

  output$parameters_table <- renderTable({
    model <- model_reactive()
    if (is.null(model)) {
      return(NULL)
    }
    params <- model$parameters_df
    if (is.null(params)) {
      return(NULL)
    }
    params
  })

  observeEvent(input$create_model, {
    tryCatch({
      dag <- cq_call("make_dag", input$model_string) |>
        data.frame()

      if (nrow(dag) == 0) {
        max_parents <- 0
      } else {
        max_parents <- dag |>
          dplyr::group_by(w) |>
          dplyr::summarize(sizes = dplyr::n()) |>
          dplyr::pull(sizes) |>
          max()
      }

      if (max_parents > 3) {
        notify_error("Please provide a model in which each child node has at most 3 parents")
        model_reactive(NULL)
        base_model_reactive(NULL)
        restrictions_reactive(list(keep = list(), drop = list()))
        parameters_reactive(list())
        updated_model_reactive(NULL)
        compact_data_reactive(NULL)
        query_rows(c(1))
        updateTextInput(session, "query_text_1", value = "")
        updateTextInput(session, "given_text_1", value = "")
        updateCheckboxGroupInput(session, "query_use", selected = c("priors"))
        updateCheckboxGroupInput(session, "partial_strategies", selected = character(0))
        return(NULL)
      }

      model <- make_model(input$model_string)
      base_model_reactive(model)
      restrictions_reactive(list(keep = list(), drop = list()))
      nl_restrictions_reactive(list())
      parameters_reactive(list())
      updated_model_reactive(NULL)
      compact_data_reactive(NULL)
      model_reactive(model)
      query_rows(c(1))
      updateTextInput(session, "query_text_1", value = default_query_text(model))
      updateTextInput(session, "given_text_1", value = "")
      updateCheckboxGroupInput(session, "query_use", selected = c("priors"))
      updateCheckboxGroupInput(session, "partial_strategies", selected = character(0))

      nodes <- model$nodes
      if (length(nodes) >= 1) {
        x1 <- nodes[1]
        xn <- nodes[length(nodes)]
        default_query <- paste0(xn, "[", x1, "=1] == ", xn, "[", x1, "=0]")
        updateTextInput(session, "query", value = default_query)
      }
      notify_ok("Model created successfully!")
      run_tour("restrictions", tour_steps_restrictions)
    }, error = function(e) {
      notify_error(paste("Error creating model:", e$message))
      model_reactive(NULL)
      base_model_reactive(NULL)
      restrictions_reactive(list(keep = list(), drop = list()))
      nl_restrictions_reactive(list())
      parameters_reactive(list())
      updated_model_reactive(NULL)
      compact_data_reactive(NULL)
      query_rows(c(1))
      updateTextInput(session, "query_text_1", value = default_query_text(NULL))
      updateTextInput(session, "given_text_1", value = "")
      updateCheckboxGroupInput(session, "query_use", selected = c("priors"))
      updateCheckboxGroupInput(session, "partial_strategies", selected = character(0))
    })
  })

  output$restrictions_ui <- renderUI({
    model <- model_reactive()
    if (is.null(model)) {
      return(p("Please create a model first"))
    }

    tagList(
      radioButtons(
        "restriction_mode",
        label = "Restriction input:",
        choices = c("Nodal types" = "types", "Natural language" = "nl"),
        selected = "types",
        inline = TRUE
      ),
      uiOutput("restriction_mode_ui")
    )
  })

  output$restriction_mode_ui <- renderUI({
    model <- model_reactive()
    if (is.null(model)) {
      return(NULL)
    }

    if (input$restriction_mode == "nl") {
      tagList(
        selectInput(
          "restrict_node_nl",
          label = "Select node:",
          choices = c("", model$nodes),
          selected = ""
        ),
        uiOutput("nl_options_ui"),
        radioButtons(
          "nl_restriction_action",
          label = "Action:",
          choices = list("Keep selected restrictions" = "keep",
                         "Drop selected restrictions" = "drop"),
          selected = "keep",
          inline = TRUE
        ),
        div(
          style = "display: flex; gap: 8px; margin-top: 6px;",
          actionButton("apply_nl_restriction", "Apply Restriction", class = "btn-warning"),
          actionButton("clear_restrictions", "Clear Restrictions", class = "btn-danger"),
          actionButton("restrictions_done", "Done", class = "btn-default")
        )
      )
    } else {
      tagList(
        selectInput(
          "restrict_node",
          label = "Select node:",
          choices = c("", model$nodes),
          selected = ""
        ),
        uiOutput("node_types_ui"),
        div(
          style = "display: flex; gap: 8px; margin-top: 6px;",
          actionButton("apply_restriction", "Apply Restriction", class = "btn-warning"),
          actionButton("clear_restrictions", "Clear Restrictions", class = "btn-danger"),
          actionButton("restrictions_done", "Done", class = "btn-default")
        )
      )
    }
  })

  output$node_types_ui <- renderUI({
    model <- model_reactive()
    node <- input$restrict_node

    if (is.null(model) || node == "" || is.null(node)) {
      return(NULL)
    }

    types <- model$nodal_types[[node]]
    if (is.null(types) || length(types) == 0) {
      return(p("No types available for this node"))
    }

    tagList(
      tags$details(
        tags$summary(
          tags$span(
            class = "glyphicon glyphicon-info-sign",
            style = "color: #1f8b4c; font-size: 15px; margin-right: 6px;"
          ),
          "Interpretation of nodal types"
        ),
        uiOutput("restriction_type_help_text"),
        tableOutput("restriction_type_help_table")
      ),
      radioButtons("restriction_action",
                   label = "Action:",
                   choices = list("Keep selected types" = "keep",
                                  "Drop selected types" = "drop"),
                   selected = "keep",
                   inline = TRUE),
      checkboxGroupInput("selected_types",
                         label = paste("Types for", node, ":"),
                         choices = types,
                         selected = NULL)
    )
  })

  output$nl_options_ui <- renderUI({
    model <- model_reactive()
    node <- input$restrict_node_nl

    if (is.null(model) || node == "" || is.null(node)) {
      return(NULL)
    }

    parents <- get_parents_for_node(model, node)
    choices <- build_nl_choices(node, parents)
    label_text <- if (length(parents) == 0) {
      paste("Restrictions for", node, ":")
    } else {
      paste("Restrictions for", node, "(based on parents:", paste(parents, collapse = ", "), "):")
    }

    tagList(
      checkboxGroupInput(
        "nl_selected_restrictions",
        label = label_text,
        choices = choices,
        selected = NULL
      ),
      textOutput("nl_types_preview")
    )
  })

  output$nl_types_preview <- renderText({
    base_model <- base_model_reactive()
    node <- input$restrict_node_nl
    selected <- input$nl_selected_restrictions
    action <- input$nl_restriction_action
    if (is.null(base_model) || node == "" || length(selected) == 0) {
      return("Types kept: (none selected)")
    }

    types_to_use <- nl_types_from_selection(base_model, node, selected)
    if (length(types_to_use) == 0) {
      return("Types kept: none (no types match selections)")
    }

    existing <- nl_restrictions_reactive()[[node]]
    drop_types <- if (!is.null(existing)) existing$drop_types else character(0)

    if (action == "drop") {
      return(paste0("Types to drop: ", paste(types_to_use, collapse = ", ")))
    }

    if (length(drop_types) > 0) {
      types_to_use <- setdiff(types_to_use, drop_types)
    }
    if (length(types_to_use) == 0) {
      return("Types kept: none (all selected types are already dropped)")
    }

    paste0("Types kept: ", paste(types_to_use, collapse = ", "))
  })

  observeEvent(input$apply_restriction, {
    model <- model_reactive()
    node <- input$restrict_node
    action <- input$restriction_action
    selected_types <- input$selected_types

    if (is.null(model) || node == "" || length(selected_types) == 0) {
      notify_warn("Please select a node and at least one type")
      return()
    }

    tryCatch({
      current_restrictions <- restrictions_reactive()
      if (action == "keep") {
        current_restrictions$keep[[node]] <- selected_types
      } else {
        current_restrictions$drop[[node]] <- selected_types
      }
      restrictions_reactive(current_restrictions)
      parameters_reactive(list())
      updated_model_reactive(NULL)
      compact_data_reactive(NULL)

      if (is.null(rebuild_or_notify())) {
        return()
      }
      notify_ok("Restriction applied successfully!")

      updateSelectInput(session, "restrict_node", selected = "")
      updateCheckboxGroupInput(session, "selected_types", selected = NULL)
      if (tour_enabled()) {
        run_tour("parameters", tour_steps_parameters)
      }
    }, error = function(e) {
      notify_error(paste("Error applying restriction:", e$message))
    })
  })

  observeEvent(input$apply_nl_restriction, {
    model <- model_reactive()
    base_model <- base_model_reactive()
    node <- input$restrict_node_nl
    action <- input$nl_restriction_action
    selected <- input$nl_selected_restrictions

    if (is.null(model) || is.null(base_model) || node == "" || length(selected) == 0) {
      notify_warn("Please select a node and at least one restriction")
      return()
    }

    tryCatch({
      types_to_use <- nl_types_from_selection(base_model, node, selected)
      if (length(types_to_use) == 0) {
        notify_warn("Selected restrictions do not map to any nodal types.")
        return()
      }

      current_nl <- nl_restrictions_reactive()
      existing <- current_nl[[node]]
      if (is.null(existing)) {
        existing <- list(keep_types = character(0), drop_types = character(0))
      }

      if (action == "keep") {
        if (length(existing$keep_types) > 0) {
          existing$keep_types <- intersect(existing$keep_types, types_to_use)
        } else {
          existing$keep_types <- types_to_use
        }
        if (length(existing$drop_types) > 0) {
          existing$keep_types <- setdiff(existing$keep_types, existing$drop_types)
        }
        if (length(existing$keep_types) == 0) {
          notify_warn("No nodal types remain after applying this restriction.")
        }
      } else {
        existing$drop_types <- unique(c(existing$drop_types, types_to_use))
      }

      current_nl[[node]] <- existing
      nl_restrictions_reactive(current_nl)
      parameters_reactive(list())
      updated_model_reactive(NULL)
      compact_data_reactive(NULL)

      if (is.null(rebuild_or_notify())) {
        return()
      }
      notify_ok("Restriction applied successfully!")

      updateSelectInput(session, "restrict_node_nl", selected = "")
      updateCheckboxGroupInput(session, "nl_selected_restrictions", selected = NULL)
      if (tour_enabled()) {
        run_tour("parameters", tour_steps_parameters)
      }
    }, error = function(e) {
      notify_error(paste("Error applying restriction:", e$message))
    })
  })

  observeEvent(input$clear_restrictions, {
    if (is.null(base_model_reactive())) {
      return()
    }
    restrictions_reactive(list(keep = list(), drop = list()))
    nl_restrictions_reactive(list())
    parameters_reactive(list())
    updated_model_reactive(NULL)
    compact_data_reactive(NULL)
    if (!is.null(rebuild_or_notify())) {
      notify_ok("All restrictions cleared!")
    }
  })

  observeEvent(input$restrictions_done, {
    disable_guidance()
  })

  output$current_restrictions <- renderText({
    restrictions <- restrictions_reactive()
    nl_restrictions <- nl_restrictions_reactive()
    if (length(restrictions$keep) == 0 && length(restrictions$drop) == 0 && length(nl_restrictions) == 0) {
      return("No restrictions applied")
    }

    restriction_text <- "Current restrictions:\n"
    if (length(restrictions$keep) > 0) {
      keep_parts <- vapply(names(restrictions$keep), function(node) {
        types <- paste(restrictions$keep[[node]], collapse = ", ")
        paste0(node, "=[", types, "]")
      }, character(1))
      restriction_text <- paste0(restriction_text, "Keep: ", paste(keep_parts, collapse = "; "), "\n")
    }
    if (length(restrictions$drop) > 0) {
      drop_parts <- vapply(names(restrictions$drop), function(node) {
        types <- paste(restrictions$drop[[node]], collapse = ", ")
        paste0(node, "=[", types, "]")
      }, character(1))
      restriction_text <- paste0(restriction_text, "Drop: ", paste(drop_parts, collapse = "; "), "\n")
    }

    if (length(nl_restrictions) > 0) {
      restriction_text <- paste0(restriction_text, "Natural language:\n")
      for (node in names(nl_restrictions)) {
        keep_types <- nl_restrictions[[node]]$keep_types
        drop_types <- nl_restrictions[[node]]$drop_types
        if (!is.null(keep_types) && length(keep_types) > 0) {
          if (!is.null(drop_types) && length(drop_types) > 0) {
            keep_types <- setdiff(keep_types, drop_types)
          }
          restriction_text <- paste0(
            restriction_text,
            node, ": Keep [", paste(keep_types, collapse = ", "), "]\n"
          )
        }
        if (!is.null(drop_types) && length(drop_types) > 0) {
          restriction_text <- paste0(
            restriction_text,
            node, ": Drop [", paste(drop_types, collapse = ", "), "]\n"
          )
        }
      }
    }

    restriction_text
  })

  output$parameters_ui <- renderUI({
    model <- model_reactive()
    if (is.null(model)) {
      return(p("Please create a model first"))
    }

    tagList(
      selectInput(
        "param_node",
        label = "Select node to set parameters:",
        choices = c("", model$nodes),
        selected = ""
      ),
      uiOutput("node_param_ui"),
      actionButton("apply_parameters", "Apply Parameters", class = "btn-warning"),
      actionButton("clear_parameters", "Clear Parameters", class = "btn-danger")
    )
  })

  output$node_param_ui <- renderUI({
    model <- model_reactive()
    node <- input$param_node

    if (is.null(model) || node == "" || is.null(node)) {
      return(NULL)
    }

    params_df <- model$parameters_df
    params_df <- params_df[params_df$node == node, , drop = FALSE]
    if (nrow(params_df) == 0) {
      return(p("No parameters available for this node"))
    }

    param_sets <- unique(params_df$param_set)
    inputs <- lapply(param_sets, function(param_set) {
      set_rows <- params_df[params_df$param_set == param_set, , drop = FALSE]
      entries <- lapply(seq_len(nrow(set_rows)), function(i) {
        label <- format_param_label(set_rows$nodal_type[i], set_rows$given[i])
        div(
          style = "display: flex; align-items: center; gap: 8px; margin-bottom: 6px;",
          tags$span(style = "min-width: 200px;", label),
          numericInput(
            inputId = paste0("param_name_", set_rows$param_names[i]),
            label = NULL,
            value = NA,
            min = 0,
            max = 1,
            step = 0.01,
            width = "100px"
          )
        )
      })
      tagList(
        h5(format_param_set_label(param_set)),
        do.call(tagList, entries)
      )
    })

    tagList(
      tags$details(
        tags$summary(
          tags$span(
            class = "glyphicon glyphicon-info-sign",
            style = "color: #1f8b4c; font-size: 15px; margin-right: 6px;"
          ),
          "Interpretation of nodal types"
        ),
        uiOutput("param_type_help_text"),
        tableOutput("param_type_help_table")
      ),
      do.call(tagList, inputs)
    )
  })

  observeEvent(input$apply_parameters, {
    model <- model_reactive()
    node <- input$param_node

    if (is.null(model) || node == "" || is.null(node)) {
      notify_warn("Please select a node")
      return()
    }

    params_df <- model$parameters_df
    params_df <- params_df[params_df$node == node, , drop = FALSE]
    if (nrow(params_df) == 0) {
      notify_warn("No parameters available for this node")
      return()
    }

    param_names <- params_df$param_names
    vals <- sapply(param_names, function(pname) {
      input_id <- paste0("param_name_", pname)
      v <- input[[input_id]]
      if (is.null(v) || is.na(v)) NA_real_ else as.numeric(v)
    })

    valid_idx <- which(!is.na(vals))
    if (length(valid_idx) == 0) {
      notify_warn("Please enter at least one parameter value")
      return()
    }

    node_pars <- vals[valid_idx]
    names(node_pars) <- param_names[valid_idx]

    current_parameters <- parameters_reactive()
    current_parameters[[node]] <- node_pars
    parameters_reactive(current_parameters)

    if (is.null(rebuild_or_notify())) {
      return()
    }
    notify_ok("Parameters applied successfully!")
    if (tour_enabled()) {
      updateTabsetPanel(session, "main_tabs", selected = "Update Model")
      run_tour("update", tour_steps_update)
    }
  })

  observeEvent(input$clear_parameters, {
    if (is.null(base_model_reactive())) {
      return()
    }
    parameters_reactive(list())
    if (!is.null(rebuild_or_notify())) {
      notify_ok("All parameters cleared!")
    }
  })

  output$current_parameters <- renderText({
    params <- parameters_reactive()
    if (length(params) == 0) {
      return("No parameters set")
    }

    txt <- "Current parameters (by param name):\n"
    for (node in names(params)) {
      node_pars <- params[[node]]
      line <- paste0(
        node, ": ",
        paste0(names(node_pars), " = ", round(node_pars, 3), collapse = ", ")
      )
      txt <- paste0(txt, line, "\n")
    }
    txt
  })

  output$restriction_type_help_text <- renderUI({
    model <- model_reactive()
    node <- input$restrict_node
    if (is.null(model) || node == "" || is.null(node)) {
      return(NULL)
    }
    parents <- get_parents_for_node(model, node)
    type_help_ui(node, parents)
  })

  output$restriction_type_help_table <- renderTable({
    model <- model_reactive()
    node <- input$restrict_node
    if (is.null(model) || node == "" || is.null(node)) {
      return(NULL)
    }
    parents <- get_parents_for_node(model, node)
    if (length(parents) < 2) {
      return(NULL)
    }
    interp <- tryCatch(interpret_type(model, nodes = node)[[node]], error = function(e) NULL)
    if (is.null(interp)) {
      return(NULL)
    }
    interp
  }, rownames = FALSE)

  output$param_type_help_text <- renderUI({
    model <- model_reactive()
    node <- input$param_node
    if (is.null(model) || node == "" || is.null(node)) {
      return(NULL)
    }
    parents <- get_parents_for_node(model, node)
    type_help_ui(node, parents)
  })

  output$param_type_help_table <- renderTable({
    model <- model_reactive()
    node <- input$param_node
    if (is.null(model) || node == "" || is.null(node)) {
      return(NULL)
    }
    parents <- get_parents_for_node(model, node)
    if (length(parents) < 2) {
      return(NULL)
    }
    interp <- tryCatch(interpret_type(model, nodes = node)[[node]], error = function(e) NULL)
    if (is.null(interp)) {
      return(NULL)
    }
    interp
  }, rownames = FALSE)

  output$data_inputs <- renderUI({
    model <- model_reactive()
    if (is.null(model)) {
      return(p("Please create a model first"))
    }

    nodes <- model$nodes
    inputs <- lapply(nodes, function(node) {
      radioButtons(
        inputId = paste0("data_", node),
        label = node,
        choices = list("0" = "0", "1" = "1", "NA" = "NA"),
        selected = "0",
        inline = TRUE
      )
    })

    do.call(tagList, inputs)
  })

  update_types <- reactive({
    model <- model_reactive()
    if (is.null(model)) {
      return(NULL)
    }
    data_types_for_model(model)
  })

  current_compact_data <- reactive({
    model <- model_reactive()
    types_df <- update_types()
    if (is.null(model) || is.null(types_df)) {
      return(NULL)
    }
    full_strategy <- paste(model$nodes, collapse = "")
    compact_data <- compact_data_from_inputs(
      types_df,
      full_strategy,
      input$partial_strategies
    )
    if (is.null(compact_data)) {
      return(NULL)
    }
    compact_data[compact_data$count > 0, , drop = FALSE]
  })

  show_preview <- reactiveVal(TRUE)

  observeEvent(input$update_model, {
    show_preview(FALSE)
  })

  observeEvent(reactiveValuesToList(input), {
    show_preview(TRUE)
  }, ignoreInit = TRUE)

  observeEvent(update_types(), {
    model <- model_reactive()
    types_df <- update_types()
    if (is.null(model) || is.null(types_df)) {
      return()
    }
    full_strategy <- paste(model$nodes, collapse = "")
    strategies <- sort(unique(types_df$strategy))
    strategies <- strategies[strategies != full_strategy]
    strategies <- strategies[strategies != ""]
    if (length(strategies) == 0) {
      updateCheckboxGroupInput(session, "partial_strategies", choices = character(0), selected = character(0))
      return()
    }
    strategy_labels <- vapply(strategies, function(strategy) {
      paste0("data on ", paste(strsplit(strategy, "")[[1]], collapse = " and "), " only")
    }, character(1))
    updateCheckboxGroupInput(
      session,
      "partial_strategies",
      choices = setNames(strategies, strategy_labels),
      selected = intersect(input$partial_strategies, strategies)
    )
  }, ignoreInit = TRUE)

  output$full_data_inputs <- renderUI({
    model <- model_reactive()
    types_df <- update_types()
    if (is.null(model) || is.null(types_df)) {
      return(p("Please create a model first"))
    }

    full_strategy <- paste(model$nodes, collapse = "")
    full_rows <- types_df[types_df$strategy == full_strategy, , drop = FALSE]
    if (nrow(full_rows) == 0) {
      return(p("No complete data types available for this model"))
    }

    inputs <- lapply(full_rows$event, function(event) {
      label <- format_event_label(types_df, event, model$nodes)
      div(
        style = "display: flex; align-items: center; gap: 8px; margin-bottom: 6px;",
        tags$span(style = "min-width: 140px;", label),
        numericInput(
          inputId = paste0("full_count_", event),
          label = NULL,
          value = 0,
          min = 0,
          step = 1,
          width = "100px"
        )
      )
    })

    do.call(tagList, inputs)
  })

  output$partial_strategy_inputs <- renderUI({
    model <- model_reactive()
    types_df <- update_types()
    if (is.null(model) || is.null(types_df)) {
      return(NULL)
    }

    full_strategy <- paste(model$nodes, collapse = "")
    strategies <- sort(unique(types_df$strategy))
    strategies <- strategies[strategies != full_strategy]
    strategies <- strategies[strategies != ""]
    if (length(strategies) == 0) {
      return(p("No partial strategies available for this model"))
    }

    strategy_labels <- vapply(strategies, function(strategy) {
      paste0("data on ", paste(strsplit(strategy, "")[[1]], collapse = " and "), " only")
    }, character(1))

    selected_strategies <- input$partial_strategies
    if (is.null(selected_strategies) || length(selected_strategies) == 0) {
      return(p("Select a strategy to enter partial data"))
    }

    inputs <- lapply(selected_strategies, function(strategy_value) {
      strategy_rows <- types_df[types_df$strategy == strategy_value, , drop = FALSE]
      if (nrow(strategy_rows) == 0) {
        return(NULL)
      }

      event_labels <- setNames(
        vapply(strategy_rows$event, function(event) {
          format_event_label(types_df, event, model$nodes)
        }, character(1)),
        strategy_rows$event
      )

      entry_rows <- lapply(strategy_rows$event, function(event) {
        label <- event_labels[[event]]
        div(
          style = "display: flex; align-items: center; gap: 8px; margin-bottom: 6px;",
          tags$span(style = "min-width: 140px;", label),
          numericInput(
            inputId = paste0("partial_count_", strategy_value, "_", event),
            label = NULL,
            value = 0,
            min = 0,
            step = 1,
            width = "100px"
          )
        )
      })

      tagList(
        h5(paste0("Strategy: ", strategy_labels[[which(strategies == strategy_value)[1]]])),
        do.call(tagList, entry_rows)
      )
    })

    do.call(tagList, Filter(Negate(is.null), inputs))
  })

  observeEvent(input$update_model, {
    model <- model_reactive()
    if (is.null(model)) {
      notify_warn("Please create a model first")
      return()
    }

    types_df <- update_types()
    if (is.null(types_df)) {
      notify_error("Unable to determine data types for this model")
      return()
    }

    showNotification("Updating model... this may take a moment.", type = "message", duration = 10)
    full_strategy <- paste(model$nodes, collapse = "")
    compact_data <- compact_data_from_inputs(
      types_df,
      full_strategy,
      input$partial_strategies
    )
    if (!is.null(attr(compact_data, "non_integer")) && attr(compact_data, "non_integer")) {
      notify_warn("Some counts were not integers and were rounded to the nearest integer.")
    }
    compact_data <- compact_data[compact_data$count > 0, , drop = FALSE]
    compact_data$count <- as.integer(compact_data$count)
    compact_data_reactive(compact_data)
    if (nrow(compact_data) == 0) {
      notify_warn("No valid data rows provided for updating.")
      return()
    }

    tryCatch({
      options(mc.cores = parallel::detectCores())
      rstan::rstan_options(auto_write = TRUE)
      updated <- update_model(
        model,
        data = compact_data,
        refresh = input$update_refresh,
        iter = input$update_iter
      )
      updated_model_reactive(updated)
      notify_ok("Model updated successfully!")
      if (tour_enabled()) {
        updateTabsetPanel(session, "main_tabs", selected = "Query model")
        run_tour("query", tour_steps_query)
      }
    }, error = function(e) {
      notify_error(paste("Error updating model:", e$message))
    })
  })

  observeEvent(input$clear_data, {
    model <- model_reactive()
    types_df <- update_types()
    if (is.null(model) || is.null(types_df)) {
      return()
    }

    full_strategy <- paste(model$nodes, collapse = "")
    full_rows <- types_df[types_df$strategy == full_strategy, , drop = FALSE]
    if (nrow(full_rows) > 0) {
      for (event in full_rows$event) {
        updateNumericInput(session, paste0("full_count_", event), value = 0)
      }
    }

    strategies <- sort(unique(types_df$strategy))
    strategies <- strategies[strategies != full_strategy]
    strategies <- strategies[strategies != ""]
    if (length(strategies) > 0) {
      for (strategy_value in strategies) {
        strategy_rows <- types_df[types_df$strategy == strategy_value, , drop = FALSE]
        if (nrow(strategy_rows) == 0) {
          next
        }
        for (event in strategy_rows$event) {
          updateNumericInput(session, paste0("partial_count_", strategy_value, "_", event), value = 0)
        }
      }
    }

    updateCheckboxGroupInput(session, "partial_strategies", selected = character(0))
    compact_data_reactive(NULL)
    show_preview(TRUE)
    notify_ok("All data inputs cleared.")
  })

  results <- eventReactive(input$calculate, {
    model <- model_reactive()
    if (is.null(model)) {
      return(list(error = "Please create a model first"))
    }

    nodes <- model$nodes
    data_list <- lapply(nodes, function(node) {
      input_id <- paste0("data_", node)
      value <- input[[input_id]]
      if (is.null(value)) {
        return(0)
      }
      if (value == "NA") {
        return(NA_real_)
      }
      as.numeric(value)
    })
    names(data_list) <- nodes
    data <- data.frame(data_list)

    query <- input$query
    if (query == "") {
      return(list(error = "Please enter a query"))
    }

    tryCatch({
      result <- by_hand(model, data, query)
      result$error <- NULL
      result
    }, error = function(e) {
      list(error = e$message)
    })
  })

  output$error_message <- renderUI({
    res <- results()
    if (!is.null(res$error)) {
      return(HTML(paste0("<div style='color: red;'><strong>Error:</strong> ", res$error, "</div>")))
    }
    return(NULL)
  })

  output$summary <- renderUI({
    res <- results()
    if (!is.null(res$error)) {
      return(NULL)
    }

    HTML(paste0(
      "<ul style='font-size: 14px; line-height: 1.8;'>",
      "<li><strong>Denominator</strong> (probability of all types consistent with data): ",
      "<span style='color: #0066cc; font-weight: bold;'>", format(res$denominator, digits = 6), "</span></li>",
      "<li><strong>Numerator</strong> (probability of all types consistent with data and query): ",
      "<span style='color: #0066cc; font-weight: bold;'>", format(res$numerator, digits = 6), "</span></li>",
      "<li><strong>Posterior (Numerator / Denominator) </strong>: ",
      "<span style='color: #cc0000; font-weight: bold; font-size: 16px;'>", format(res$posterior, digits = 6), "</span></li>",
      "</ul>"
    ))
  })

  output$results_table <- renderDT({
    res <- results()
    if (!is.null(res$error) || is.null(res$result)) {
      return(NULL)
    }

    table_data <- res$result |>
      dplyr::mutate(
        in_data = ifelse(in_data, "Yes", "No"),
        in_query = ifelse(in_query, "Yes", "No"),
        priors = round(priors, 3)
      ) |>
      dplyr::mutate(
        sort_group = dplyr::case_when(
          in_data == "Yes" & in_query == "Yes" ~ 1,
          in_data == "Yes" & in_query == "No" ~ 2,
          in_data == "No" & in_query == "Yes" ~ 3,
          TRUE ~ 4
        )
      ) |>
      dplyr::arrange(sort_group) |>
      dplyr::select(
        type,
        in_data,
        in_query,
        priors
      )

    datatable(
      table_data,
      options = list(
        scrollX = TRUE,
        pageLength = 20,
        dom = "tip"
      ),
      rownames = FALSE
    ) |>
      formatStyle(
        "in_data",
        backgroundColor = styleEqual("Yes", "lightpink")
      ) |>
      formatStyle(
        "in_query",
        backgroundColor = styleEqual("Yes", "lightgreen")
      )
  })

  output$update_summary <- renderPrint({
    updated_model <- updated_model_reactive()
    if (is.null(updated_model)) {
      return("No updated model yet")
    }
    inspect(updated_model, "stan_summary")
  })

  output$compact_data_preview <- renderTable({
    if (!show_preview()) {
      return(NULL)
    }
    compact_data <- current_compact_data()
    if (is.null(compact_data) || nrow(compact_data) == 0) {
      return(data.frame(Note = "No data entered yet", stringsAsFactors = FALSE))
    }
    compact_data
  })

  observeEvent(input$add_query_row, {
    ids <- query_rows()
    query_rows(c(ids, max(ids) + 1))
  })

  observeEvent(input$clear_query_rows, {
    query_rows(c(1))
    updateTextInput(session, "query_text_1", value = default_query_text(model_reactive()))
  })

  observeEvent(input$main_tabs, {
    if (is.null(input$main_tabs)) {
      return()
    }
    if (input$main_tabs == "Update Model") {
      run_tour("update", tour_steps_update)
    }
    if (input$main_tabs == "Query model") {
      run_tour("query", tour_steps_query)
    }
  }, ignoreInit = TRUE)

  output$query_inputs <- renderUI({
    ids <- query_rows()
    model <- model_reactive()
    default_query <- default_query_text(model)
    inputs <- lapply(ids, function(id) {
      textInput(
        inputId = paste0("query_text_", id),
        label = if (id == 1) "Queries" else NULL,
        value = if (id == 1) default_query else "",
        placeholder = "e.g. Y[X=1] > Y[X=0]"
      )
    })
    do.call(tagList, inputs)
  })

  given_rows <- reactiveVal(c(1))

  observeEvent(input$add_given_row, {
    ids <- given_rows()
    given_rows(c(ids, max(ids) + 1))
  })

  observeEvent(input$clear_given_rows, {
    given_rows(c(1))
    updateTextInput(session, "given_text_1", value = "")
  })

  output$given_inputs <- renderUI({
    ids <- given_rows()
    inputs <- lapply(ids, function(id) {
      textInput(
        inputId = paste0("given_text_", id),
        label = if (id == 1) "Given (optional)" else NULL,
        value = "",
        placeholder = "e.g. M==1"
      )
    })
    do.call(tagList, inputs)
  })

  query_results <- eventReactive(input$compute_queries, {
    base_model <- if (!is.null(updated_model_reactive())) updated_model_reactive() else model_reactive()
    if (is.null(base_model)) {
      notify_warn("Please create a model first")
      return(NULL)
    }

    ids <- query_rows()
    if (length(ids) == 0) {
      queries <- character(0)
    } else {
      queries <- vapply(ids, function(id) {
        value <- input[[paste0("query_text_", id)]]
        if (is.null(value)) "" else value
      }, character(1))
    }
    queries <- queries[queries != ""]
    if (length(queries) == 0) {
      notify_warn("Please enter at least one query")
      return(NULL)
    }

    use_choices <- input$query_use
    using <- if (is.null(use_choices)) character(0) else use_choices

    given_ids <- given_rows()
    given_entries <- vapply(given_ids, function(id) {
      value <- input[[paste0("given_text_", id)]]
      if (is.null(value)) "" else value
    }, character(1))
    given_entries <- given_entries[given_entries != ""]
    given_text <- if (length(given_entries) == 0) NULL else paste(given_entries, collapse = " & ")

    query_model(
      base_model,
      query = queries,
      given = given_text,
      using = using,
      expand_grid = TRUE
    )
  })

  make_query_plot <- function() {
    qm <- query_results()
    if (is.null(qm)) {
      return(NULL)
    }
    plot(qm)
  }

  output$query_plot <- renderPlot({
    make_query_plot()
  })

  query_table_data <- reactive({
    qm <- query_results()
    if (is.null(qm)) {
      return(NULL)
    }
    table_data <- tryCatch(
      data.frame(qm, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
    if (is.null(table_data) || nrow(table_data) == 0) {
      return(NULL)
    }

    if ("label" %in% names(table_data)) {
      table_data$label <- NULL
    }
    if ("case_level" %in% names(table_data)) {
      table_data$case_level <- NULL
    }
    if ("given" %in% names(table_data)) {
      table_data$given <- ifelse(is.na(table_data$given) | table_data$given == "", "-", table_data$given)
    }

    table_data
  })

  output$query_table <- renderTable({
    query_table_data()
  })

  output$download_query_table <- downloadHandler(
    filename = function() {
      "query_results.csv"
    },
    content = function(file) {
      table_data <- query_table_data()
      if (is.null(table_data)) {
        write.csv(data.frame(), file, row.names = FALSE)
        return()
      }
      write.csv(table_data, file, row.names = FALSE)
    }
  )

  observeEvent(input$copy_replication, {
    session$sendCustomMessage("copy_replication", list())
  })

  output$download_query_plot <- downloadHandler(
    filename = function() {
      "query_plot.png"
    },
    content = function(file) {
      plot_obj <- make_query_plot()
      if (is.null(plot_obj)) {
        png(filename = file, width = 1000, height = 700, res = 144)
        plot.new()
        text(0.5, 0.5, "No plot available", cex = 1.2)
        dev.off()
        return()
      }
      png(filename = file, width = 1000, height = 700, res = 144)
      make_query_plot()
      dev.off()
    }
  )

  observeEvent(input$clipboard_status, {
    status <- input$clipboard_status
    if (is.null(status$msg)) {
      return()
    }
    if (isTRUE(status$ok)) {
      notify_ok(status$msg)
    } else {
      notify_warn(status$msg)
    }
  })

  output$replication_code <- renderUI({
    model <- model_reactive()
    if (is.null(model)) {
      return(tags$pre(tags$code("Create a model to see replication code.")))
    }

    statement <- model$statement
    if (is.null(statement) || statement == "") {
      statement <- input$model_string
    }
    if (is.null(statement) || statement == "") {
      return(tags$pre(tags$code("Create a model to see replication code.")))
    }
    lines <- c(paste0("model <- make_model(\"", statement, "\")"))

    restrictions <- restrictions_reactive()
    nl_restrictions <- nl_restrictions_reactive()
    merged_keep <- restrictions$keep
    merged_drop <- restrictions$drop
    if (length(nl_restrictions) > 0) {
      for (node in names(nl_restrictions)) {
        keep_types <- nl_restrictions[[node]]$keep_types
        drop_types <- nl_restrictions[[node]]$drop_types
        if (!is.null(keep_types) && length(keep_types) > 0) {
          merged_keep[[node]] <- unique(c(merged_keep[[node]], keep_types))
        }
        if (!is.null(drop_types) && length(drop_types) > 0) {
          merged_drop[[node]] <- unique(c(merged_drop[[node]], drop_types))
        }
      }
    }

    if (length(merged_keep) > 0) {
      lines <- c(lines,
                 paste0("model <- model |> set_restrictions(labels = ",
                        format_labels_list(merged_keep),
                        ", keep = TRUE)"))
    }
    if (length(merged_drop) > 0) {
      lines <- c(lines,
                 paste0("model <- model |> set_restrictions(labels = ",
                        format_labels_list(merged_drop),
                        ", keep = FALSE)"))
    }

    params <- parameters_reactive()
    if (length(params) > 0) {
      param_names <- unlist(lapply(params, names), use.names = FALSE)
      param_values <- unlist(params, use.names = FALSE)
      if (length(param_names) > 0) {
        lines <- c(lines,
                   paste0("model <- model |> set_parameters(param_names = ",
                          format_char_vector(param_names),
                          ", parameters = ",
                          format_num_vector(param_values),
                          ")"))
      }
    }

    compact_data <- compact_data_reactive()
    if (!is.null(compact_data)) {
      compact_data <- compact_data[compact_data$count > 0, , drop = FALSE]
      if (nrow(compact_data) > 0) {
        data_line <- paste0(
          "data <- data.frame(event = ",
          format_char_vector(compact_data$event),
          ", strategy = ",
          format_char_vector(compact_data$strategy),
          ", count = ",
          format_num_vector(compact_data$count),
          ")"
        )
        lines <- c(lines, data_line, "model <- update_model(model, data)")
      }
    }

    ids <- query_rows()
    queries <- vapply(ids, function(id) {
      value <- input[[paste0("query_text_", id)]]
      if (is.null(value)) "" else value
    }, character(1))
    queries <- queries[queries != ""]
    if (length(queries) > 0) {
      query_line <- paste0("queries <- query_model(model, query = ", format_char_vector(queries))
      given_ids <- given_rows()
      given_entries <- vapply(given_ids, function(id) {
        value <- input[[paste0("given_text_", id)]]
        if (is.null(value)) "" else value
      }, character(1))
      given_entries <- given_entries[given_entries != ""]
      given_text <- if (length(given_entries) == 0) NULL else paste(given_entries, collapse = " & ")
      if (!is.null(given_text) && given_text != "") {
        query_line <- paste0(query_line, ", given = ", format_char_vector(given_text))
      }
      using <- input$query_use
      if (!is.null(using) && length(using) > 0) {
        query_line <- paste0(query_line, ", using = ", format_char_vector(using))
      }
      query_line <- paste0(query_line, ", expand_grid = TRUE)")
      lines <- c(lines, query_line, "queries |> plot()")
    }

    code_text <- paste(lines, collapse = "\n")
    tagList(
      tags$pre(
        tags$code(
          class = "language-r",
          htmltools::HTML(code_text)
        )
      ),
      tags$script("if (window.hljs) { hljs.highlightAll(); }")
    )
  })
}

