library(shiny)
library(CausalQueries)
library(DT)
library(dplyr)

cq_call <- function(name, ...) {
  fn <- getFromNamespace(name, "CausalQueries")
  fn(...)
}

by_hand <- function(model, data, query) {
  data_type <- cq_call("data_type_names", model, data)
  amb <- grab(model, what = "ambiguities_matrix")

  result <- amb |>
    data.frame() |>
    dplyr::mutate(
      type = rownames(amb),
      in_query = get_query_types(model, query)$types,
      priors = cq_call("get_type_prob", model),
      in_data = .data[[data_type]] == 1
    )

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
    data_type_name = data_type
  )
}

server <- function(input, output, session) {
  model_reactive <- reactiveVal(NULL)
  base_model_reactive <- reactiveVal(NULL)
  restrictions_reactive <- reactiveVal(list())
  nl_restrictions_reactive <- reactiveVal(list())
  parameters_reactive <- reactiveVal(list())

  notify_error <- function(message) {
    showNotification(message, type = "error")
  }
  notify_warn <- function(message) {
    showNotification(message, type = "warning")
  }
  notify_ok <- function(message) {
    showNotification(message, type = "message")
  }

  apply_type_restrictions <- function(model, restrictions) {
    if (length(restrictions) == 0) {
      return(model)
    }
    for (node_name in names(restrictions)) {
      entry <- restrictions[[node_name]]
      model <- set_restrictions(
        model,
        labels = setNames(list(entry$types), node_name),
        keep = entry$keep
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
          parameters = as.numeric(node_pars),
          node = node_name,
          nodal_type = names(node_pars)
        )
      }
    }
    model
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
    unique(as.character(dag$v[dag$w == node]))
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
        restrictions_reactive(list())
        parameters_reactive(list())
        return(NULL)
      }

      model <- make_model(input$model_string)
      base_model_reactive(model)
      restrictions_reactive(list())
      nl_restrictions_reactive(list())
      parameters_reactive(list())
      model_reactive(model)

      nodes <- model$nodes
      if (length(nodes) >= 1) {
        x1 <- nodes[1]
        xn <- nodes[length(nodes)]
        default_query <- paste0(xn, "[", x1, "=1] == ", xn, "[", x1, "=0]")
        updateTextInput(session, "query", value = default_query)
      }
      notify_ok("Model created successfully!")
    }, error = function(e) {
      notify_error(paste("Error creating model:", e$message))
      model_reactive(NULL)
      base_model_reactive(NULL)
      restrictions_reactive(list())
      nl_restrictions_reactive(list())
      parameters_reactive(list())
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
          actionButton("clear_restrictions", "Clear Restrictions", class = "btn-danger")
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
          actionButton("clear_restrictions", "Clear Restrictions", class = "btn-danger")
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
      current_restrictions[[node]] <- list(
        types = selected_types,
        keep = (action == "keep")
      )
      restrictions_reactive(current_restrictions)

      if (is.null(rebuild_or_notify())) {
        return()
      }
      notify_ok("Restriction applied successfully!")

      updateSelectInput(session, "restrict_node", selected = "")
      updateCheckboxGroupInput(session, "selected_types", selected = NULL)
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

      if (is.null(rebuild_or_notify())) {
        return()
      }
      notify_ok("Restriction applied successfully!")

      updateSelectInput(session, "restrict_node_nl", selected = "")
      updateCheckboxGroupInput(session, "nl_selected_restrictions", selected = NULL)
    }, error = function(e) {
      notify_error(paste("Error applying restriction:", e$message))
    })
  })

  observeEvent(input$clear_restrictions, {
    if (is.null(base_model_reactive())) {
      return()
    }
    restrictions_reactive(list())
    nl_restrictions_reactive(list())
    if (!is.null(rebuild_or_notify())) {
      notify_ok("All restrictions cleared!")
    }
  })

  output$current_restrictions <- renderText({
    restrictions <- restrictions_reactive()
    nl_restrictions <- nl_restrictions_reactive()
    if (length(restrictions) == 0 && length(nl_restrictions) == 0) {
      return("No restrictions applied")
    }

    restriction_text <- "Current restrictions:\n"
    if (length(restrictions) > 0) {
      restriction_text <- paste0(restriction_text, "Nodal types:\n")
      for (node in names(restrictions)) {
        action <- if (restrictions[[node]]$keep) "Keep" else "Drop"
        types <- paste(restrictions[[node]]$types, collapse = ", ")
        restriction_text <- paste0(restriction_text,
                                   node, ": ", action, " [", types, "]\n")
      }
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

    types <- model$nodal_types[[node]]
    if (is.null(types) || length(types) == 0) {
      return(p("No types available for this node"))
    }

    inputs <- lapply(types, function(t) {
      numericInput(
        inputId = paste0("param_", node, "_", t),
        label = paste0("Probability for type ", t),
        value = NA,
        min = 0,
        max = 1,
        step = 0.01
      )
    })

    do.call(tagList, inputs)
  })

  observeEvent(input$apply_parameters, {
    model <- model_reactive()
    node <- input$param_node

    if (is.null(model) || node == "" || is.null(node)) {
      notify_warn("Please select a node")
      return()
    }

    types <- model$nodal_types[[node]]
    if (is.null(types) || length(types) == 0) {
      notify_warn("No types available for this node")
      return()
    }

    vals <- sapply(types, function(t) {
      input_id <- paste0("param_", node, "_", t)
      v <- input[[input_id]]
      if (is.null(v) || is.na(v)) NA_real_ else as.numeric(v)
    })

    valid_idx <- which(!is.na(vals))
    if (length(valid_idx) == 0) {
      notify_warn("Please enter at least one parameter value")
      return()
    }

    node_pars <- vals[valid_idx]
    names(node_pars) <- types[valid_idx]

    current_parameters <- parameters_reactive()
    current_parameters[[node]] <- node_pars
    parameters_reactive(current_parameters)

    if (is.null(rebuild_or_notify())) {
      return()
    }
    notify_ok("Parameters applied successfully!")
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

    txt <- "Current parameters (per node and nodal type):\n"
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
        choices = list("0" = 0, "1" = 1),
        selected = 0,
        inline = TRUE
      )
    })

    do.call(tagList, inputs)
  })

  results <- eventReactive(input$calculate, {
    model <- model_reactive()
    if (is.null(model)) {
      return(list(error = "Please create a model first"))
    }

    nodes <- model$nodes
    data_list <- lapply(nodes, function(node) {
      input_id <- paste0("data_", node)
      if (is.null(input[[input_id]])) {
        return(0)
      }
      as.numeric(input[[input_id]])
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
      list(error = paste("Error:", e$message))
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
}

