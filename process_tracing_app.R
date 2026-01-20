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

# UI
ui <- fluidPage(
  titlePanel("Process Tracing by Hand"),

  tabsetPanel(
    id = "main_tabs",

    # Tab 1: Make Model
    tabPanel(
      "Make Model",
      fluidRow(
        column(
          width = 4,
          h4("1. Input Model"),
          textInput(
            "model_string",
            label = "Model (e.g., 'X -> Y' or 'S -> C -> Y <- R <- X; X -> C -> R')",
            value = "X -> M -> Y",
            placeholder = "Enter model specification"
          ),
          actionButton("create_model", "Create Model", class = "btn-primary"),

          hr(),

          h4("2. Set Restrictions (Optional)"),
          uiOutput("restrictions_ui"),
          verbatimTextOutput("current_restrictions"),

          hr(),

          h4("3. Set Parameters (Optional)"),
          uiOutput("parameters_ui"),
          verbatimTextOutput("current_parameters")
        ),
        column(
          width = 8,
          h4("Current model"),
          plotOutput("model_plot", height = "400px")
        )
      )
    ),

    # Tab 2: Data & Query
    tabPanel(
      "Data & Query",
      fluidRow(
        column(
          width = 4,
          h4("4. Input Data"),
          uiOutput("data_inputs"),

          hr(),

          h4("5. Input Query"),
          textInput(
            "query",
            label = "Query (e.g., 'Y[X=1] == Y[X=0]' or 'Y[S=1] < Y[S=0]')",
            value = "Y[X=1] == Y[X=0]",
            placeholder = "Enter causal query"
          ),

          hr(),

          actionButton("calculate", "Calculate", class = "btn-success")
        ),
        column(
          width = 8,
          h3("Results"),
          htmlOutput("error_message"),
          h4("Summary"),
          htmlOutput("summary"),
          h4("Detailed Results Table"),
          DTOutput("results_table")
        )
      )
    ),

    # Tab 3: Helper / Walkthrough
    tabPanel(
      "About",
      h3("Process Tracing by Hand: A Walkthrough"),
      HTML("
            <div style='max-width: 900px; line-height: 1.8; font-size: 14px;'>

            <h4>What is this tool?</h4>
            <p>This tool helps you perform <strong>process tracing</strong> using causal models. Process tracing involves:</p>
            <ul>
              <li>Starting with a causal model (DAG) representing your theory</li>
              <li>Observing data on a single case (values of nodes in your model)</li>
              <li>Asking a causal question (query) about that case</li>
              <li>Calculating the probability that your query is true, given the observed data</li>
            </ul>

            <h4>The Logic Behind It</h4>
            <p>The calculation works by:</p>
            <ol>
              <li><strong>Identifying causal types</strong>: All possible ways the world could work (combinations of nodal types)</li>
              <li><strong>Filtering to types consistent with data</strong>: Which types could have generated the observed data?</li>
              <li><strong>Calculating the denominator</strong>: Total probability of all types consistent with data</li>
              <li><strong>Calculating the numerator</strong>: Probability of types consistent with data <em>and</em> the query</li>
              <li><strong>Computing the posterior</strong>: Numerator / Denominator = probability query is true given the data</li>
            </ol>

            <hr>

            <h4>Example Walkthrough: SXCRY Model</h4>
            <p>Let's work through an example step by step using the model from the solutions document.</p>

            <h5>Step 1: Define the Model</h5>
            <p>Enter this model specification:</p>
            <pre style='background-color: #f5f5f5; padding: 10px; border-radius: 5px;'><code>S -> C -> Y <- R <- X; X -> C -> R</code></pre>
            <p>This creates a model with 5 nodes: S, C, Y, R, and X, with the following structure:</p>
            <ul>
              <li>S → C → Y</li>
              <li>R ← X (R is caused by X)</li>
              <li>Y ← R (Y is caused by R)</li>
              <li>X → C → R (X causes C, which causes R)</li>
            </ul>
            <p>Click <strong>\"Create Model\"</strong> to build the model.</p>

            <h5>Step 2: Set Restrictions (Optional)</h5>
            <p>You can restrict which nodal types are allowed. For this example, we'll restrict:</p>
            <ul>
              <li><strong>C</strong>: Keep types \"1110\" and \"1111\"</li>
              <li><strong>R</strong>: Keep types \"0001\" and \"0000\"</li>
              <li><strong>Y</strong>: Keep type \"0001\"</li>
            </ul>
            <p>To do this:</p>
            <ol>
              <li>Select \"C\" from the node dropdown</li>
              <li>Choose \"Keep selected types\"</li>
              <li>Check \"1110\" and \"1111\"</li>
              <li>Click \"Apply Restriction\"</li>
              <li>Repeat for R and Y nodes</li>
            </ol>

            <h5>Step 3: Input Data for One Case</h5>
            <p>For this example, let's say we observe:</p>
            <ul>
              <li>S = 1</li>
              <li>X = 1</li>
              <li>C = 0</li>
              <li>R = 0</li>
              <li>Y = 0</li>
            </ul>
            <p>Set these values using the radio buttons that appear after creating the model.</p>

            <h5>Step 4: Define a Query</h5>
            <p>Enter a causal query. For example:</p>
            <pre style='background-color: #f5f5f5; padding: 10px; border-radius: 5px;'><code>Y[S=1] < Y[S=0]</code></pre>
            <p>This asks: \"Does S=1 cause Y to be lower than it would be if S=0?\"</p>
            <p>Other example queries you could try:</p>
            <ul>
              <li><code>Y[S=1] > Y[S=0]</code> - Does S=1 cause Y to be higher?</li>
              <li><code>Y[X=1] == Y[X=0]</code> - Does X have no effect on Y?</li>
            </ul>

            <h5>Step 5: Calculate and Interpret Results</h5>
            <p>Click <strong>\"Calculate\"</strong> to see the results:</p>
            <ul>
              <li><strong>Denominator</strong>: The total probability of all causal types that are consistent with the observed data (S=1, X=1, C=0, R=0, Y=0). This represents all the ways the world could work that would produce this data pattern.</li>
              <li><strong>Numerator</strong>: The probability of causal types that are consistent with the data <em>and</em> satisfy the query. This represents the ways the world could work that produce the data <em>and</em> make the query true.</li>
              <li><strong>Posterior</strong>: Numerator / Denominator. This is the probability that your query is true, given the observed data. A value close to 1 means the query is very likely true; close to 0 means it's very unlikely.</li>
            </ul>

            <h5>Understanding the Detailed Table</h5>
            <p>The detailed results table shows:</p>
            <ul>
              <li>All causal types consistent with the observed data</li>
              <li>Whether each type satisfies the query (highlighted in green if Yes)</li>
              <li>The prior probability of each type</li>
              <li>The rescaled prior (normalized so they sum to 1, conditional on the data)</li>
            </ul>
            <p>The posterior is simply the sum of rescaled priors for all types where \"in_query\" is Yes.</p>

            <hr>

            <h4>Key Insights</h4>
            <ul>
              <li>Process tracing is <strong>theory-dependent</strong>: The same data can support different conclusions depending on your model and priors</li>
              <li>The posterior depends on both the <strong>data</strong> and your <strong>prior beliefs</strong> about how the world works</li>
              <li>Restrictions allow you to incorporate theoretical knowledge about which causal mechanisms are possible</li>
              <li>The calculation is transparent: you can see exactly which types contribute to the numerator and denominator</li>
            </ul>

            </div>
          ")
        )
      )
    )


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

# Run the application
shinyApp(ui = ui, server = server)
