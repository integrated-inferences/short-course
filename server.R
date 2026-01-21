library(shiny)
library(CausalQueries)
library(DT)
library(dplyr)

# Helper function based on solutions.qmd
by_hand <- function(model, data, query){

  data_type <- CausalQueries:::data_type_names(model, data)
  amb <- grab(model, what = "ambiguities_matrix")

  result <- amb |>
    data.frame() |>
    dplyr::mutate(
      type    = rownames(amb),
      in_query = get_query_types(model, query)$types,
      priors   = CausalQueries:::get_type_prob(model)
    ) |>
    dplyr::filter(.data[[data_type]] == 1) |>
    dplyr::mutate(rescaled_priors = priors / sum(priors))

  # Calculate denominator (sum of priors for types consistent with data)
  denominator <- sum(result$priors)

  # Calculate numerator (sum of priors for types consistent with data AND query)
  numerator <- sum(result$priors[result$in_query])

  # Calculate posterior
  posterior <- numerator / denominator

  return(list(
    result = result,
    denominator = denominator,
    numerator = numerator,
    posterior = posterior,
    data_type_name = data_type
  ))
}

# Server
server <- function(input, output, session) {

  # Reactive value to store the model
  model_reactive <- reactiveVal(NULL)

  # Reactive value to store the base model (before restrictions)
  base_model_reactive <- reactiveVal(NULL)

  # Reactive value to store restrictions
  restrictions_reactive <- reactiveVal(list())

  # Reactive value to store parameters
  parameters_reactive <- reactiveVal(list())

  # Helper to rebuild model from base model, restrictions, and parameters
  rebuild_model <- function() {
    base_model <- base_model_reactive()
    if (is.null(base_model)) {
      return(NULL)
    }

    # Start from base model
    model_rebuilt <- base_model

    # Apply all restrictions
    current_restrictions <- restrictions_reactive()
    if (length(current_restrictions) > 0) {
      restrictions_list <- lapply(current_restrictions, function(x) x$types)
      keep_values <- sapply(current_restrictions, function(x) x$keep)

      for (i in seq_along(restrictions_list)) {
        node_name <- names(restrictions_list)[i]
        types_to_use <- restrictions_list[[i]]
        keep_val <- keep_values[i]

        model_rebuilt <- set_restrictions(
          model_rebuilt,
          labels = setNames(list(types_to_use), node_name),
          keep = keep_val
        )
      }
    }

    # Apply all parameters
    current_parameters <- parameters_reactive()
    if (length(current_parameters) > 0) {
      for (node_name in names(current_parameters)) {
        node_pars <- current_parameters[[node_name]]
        if (length(node_pars) > 0) {
          model_rebuilt <- set_parameters(
            model_rebuilt,
            parameters = as.numeric(node_pars),
            node = node_name,
            nodal_type = names(node_pars)
          )
        }
      }
    }

    model_rebuilt
  }

  # Plot the current model on the Make Model tab
  output$model_plot <- renderPlot({
    model <- model_reactive()
    if (is.null(model)) {
      return(NULL)
    }
    plot(model)
  })

  # Create model when button is clicked
  observeEvent(input$create_model, {
    tryCatch({
      # Check maximum number of parents per child node before building the model
      dag <- CausalQueries:::make_dag(input$model_string) |>
        data.frame()

      max_parents <- dag |>
        dplyr::group_by(w) |>
        dplyr::summarize(sizes = dplyr::n()) |>
        dplyr::pull(sizes) |>
        max()

      if (max_parents > 3) {
        showNotification(
          "Please provide a model in which each child node has at most 3 parents",
          type = "error"
        )
        model_reactive(NULL)
        base_model_reactive(NULL)
        restrictions_reactive(list())
        parameters_reactive(list())
        return(NULL)
      }

      model <- make_model(input$model_string)
      base_model_reactive(model)  # Store base model
      restrictions_reactive(list())  # Reset restrictions
      parameters_reactive(list())    # Reset parameters
      model_reactive(model)
      showNotification("Model created successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error creating model:", e$message), type = "error")
      model_reactive(NULL)
      base_model_reactive(NULL)
      restrictions_reactive(list())
      parameters_reactive(list())
    })
  })

  # UI for restrictions
  output$restrictions_ui <- renderUI({
    model <- model_reactive()
    if (is.null(model)) {
      return(p("Please create a model first"))
    }

    tagList(
      selectInput("restrict_node",
                  label = "Select node to restrict:",
                  choices = c("", model$nodes),
                  selected = ""),
      uiOutput("node_types_ui"),
      actionButton("apply_restriction", "Apply Restriction", class = "btn-warning"),
      actionButton("clear_restrictions", "Clear All Restrictions", class = "btn-danger")
    )
  })

  # Display types for selected node
  output$node_types_ui <- renderUI({
    model <- model_reactive()
    node <- input$restrict_node

    if (is.null(model) || node == "" || is.null(node)) {
      return(NULL)
    }

    # Get types for this node - these should always exist
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

  # Apply restriction
  observeEvent(input$apply_restriction, {
    model <- model_reactive()
    node <- input$restrict_node
    action <- input$restriction_action
    selected_types <- input$selected_types

    if (is.null(model) || node == "" || length(selected_types) == 0) {
      showNotification("Please select a node and at least one type", type = "warning")
      return()
    }

    tryCatch({
      # Get current restrictions
      current_restrictions <- restrictions_reactive()

      # Add / update restriction for this node
      if (action == "keep") {
        current_restrictions[[node]] <- list(types = selected_types, keep = TRUE)
      } else {
        current_restrictions[[node]] <- list(types = selected_types, keep = FALSE)
      }

      restrictions_reactive(current_restrictions)

      # Rebuild model from base model, restrictions, and parameters
      model_rebuilt <- rebuild_model()
      if (is.null(model_rebuilt)) {
        showNotification("Base model not found. Please recreate the model.", type = "error")
        return()
      }

      model_reactive(model_rebuilt)
      showNotification("Restriction applied successfully!", type = "message")

      # Reset inputs
      updateSelectInput(session, "restrict_node", selected = "")
      updateCheckboxGroupInput(session, "selected_types", selected = NULL)

    }, error = function(e) {
      showNotification(paste("Error applying restriction:", e$message), type = "error")
    })
  })

  # Clear all restrictions
  observeEvent(input$clear_restrictions, {
    base_model <- base_model_reactive()
    if (is.null(base_model)) {
      return()
    }

    # Clear restrictions and rebuild model (with any parameters)
    restrictions_reactive(list())
    model_clean <- rebuild_model()
    if (!is.null(model_clean)) {
      model_reactive(model_clean)
    }
    showNotification("All restrictions cleared!", type = "message")
  })

  # Display current restrictions
  output$current_restrictions <- renderText({
    restrictions <- restrictions_reactive()
    if (length(restrictions) == 0) {
      return("No restrictions applied")
    }

    restriction_text <- "Current restrictions:\n"
    for (node in names(restrictions)) {
      action <- if (restrictions[[node]]$keep) "Keep" else "Drop"
      types <- paste(restrictions[[node]]$types, collapse = ", ")
      restriction_text <- paste0(restriction_text,
                                 node, ": ", action, " [", types, "]\n")
    }
    return(restriction_text)
  })

  # UI for parameters
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
      actionButton("clear_parameters", "Clear All Parameters", class = "btn-danger")
    )
  })

  # Per-node parameter inputs
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
        label   = paste0("Probability for type ", t),
        value   = NA,
        min     = 0,
        max     = 1,
        step    = 0.01
      )
    })

    do.call(tagList, inputs)
  })

  # Apply parameters
  observeEvent(input$apply_parameters, {
    model <- model_reactive()
    node  <- input$param_node

    if (is.null(model) || node == "" || is.null(node)) {
      showNotification("Please select a node", type = "warning")
      return()
    }

    types <- model$nodal_types[[node]]
    if (is.null(types) || length(types) == 0) {
      showNotification("No types available for this node", type = "warning")
      return()
    }

    # Collect parameter values for this node
    vals <- sapply(types, function(t) {
      input_id <- paste0("param_", node, "_", t)
      v <- input[[input_id]]
      if (is.null(v) || is.na(v)) NA_real_ else as.numeric(v)
    })

    # Keep only non-NA entries
    valid_idx <- which(!is.na(vals))
    if (length(valid_idx) == 0) {
      showNotification("Please enter at least one parameter value", type = "warning")
      return()
    }

    node_pars <- vals[valid_idx]
    names(node_pars) <- types[valid_idx]

    # Update stored parameters
    current_parameters <- parameters_reactive()
    current_parameters[[node]] <- node_pars
    parameters_reactive(current_parameters)

    # Rebuild model and update reactive model
    model_rebuilt <- rebuild_model()
    if (is.null(model_rebuilt)) {
      showNotification("Base model not found. Please recreate the model.", type = "error")
      return()
    }
    model_reactive(model_rebuilt)
    showNotification("Parameters applied successfully!", type = "message")
  })

  # Clear all parameters
  observeEvent(input$clear_parameters, {
    base_model <- base_model_reactive()
    if (is.null(base_model)) {
      return()
    }

    parameters_reactive(list())
    model_clean <- rebuild_model()
    if (!is.null(model_clean)) {
      model_reactive(model_clean)
    }
    showNotification("All parameters cleared!", type = "message")
  })

  # Display current parameters
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

  # Dynamic data inputs based on model nodes
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

  # Calculate results
  results <- eventReactive(input$calculate, {
    model <- model_reactive()
    if (is.null(model)) {
      return(list(error = "Please create a model first"))
    }

    # Get data from inputs
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

    # Get query
    query <- input$query
    if (query == "") {
      return(list(error = "Please enter a query"))
    }

    # Calculate
    tryCatch({
      result <- by_hand(model, data, query)
      result$error <- NULL
      return(result)
    }, error = function(e) {
      return(list(error = paste("Error:", e$message)))
    })
  })

  # Display error message
  output$error_message <- renderUI({
    res <- results()
    if (!is.null(res$error)) {
      return(HTML(paste0("<div style='color: red;'><strong>Error:</strong> ", res$error, "</div>")))
    }
    return(NULL)
  })

  # Display summary
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

   # Display results table
   output$results_table <- renderDT({
     res <- results()
     if (!is.null(res$error) || is.null(res$result)) {
       return(NULL)
     }

     data_type <- res$data_type_name

     # Format the table nicely
     table_data <- res$result |>
       dplyr::mutate(
         in_query = ifelse(in_query, "Yes", "No"),
         priors = round(priors, 3),
         rescaled_priors = round(rescaled_priors, 3)
       ) |>
       # Put causal type first, drop the data-type column (it is 1 everywhere)
       dplyr::select(
         type,
         in_query,
         priors,
         rescaled_priors,
         dplyr::everything(),
         -dplyr::all_of(data_type)
       )

     datatable(
       table_data,
       options = list(
         scrollX = TRUE,
         pageLength = 20,
         dom = "tip"  # table, info, pagination; no search box
       ),
       rownames = FALSE
     ) |>
       formatStyle(
         "in_query",
         backgroundColor = styleEqual("Yes", "lightgreen")
       )
   })
}

