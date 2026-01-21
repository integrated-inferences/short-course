library(shiny)
library(CausalQueries)
library(DT)
library(dplyr)

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

