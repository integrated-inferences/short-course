library(shiny)
library(CausalQueries)
library(DT)
library(dplyr)
if (!requireNamespace("rintrojs", quietly = TRUE)) {
  install.packages("rintrojs")
}
library(rintrojs)

source("tips.R")

panelBox <- function(title, ..., level = 4) {
  heading <- if (level == 3) h3(title) else h4(title)
  div(
    class = "panel-box",
    heading,
    ...
  )
}

tip_icon <- function(id) {
  tip_text <- tips[[id]]
  if (is.null(tip_text)) {
    return(NULL)
  }
  tags$span(
    title = tip_text,
    class = "glyphicon glyphicon-question-sign",
    style = "color: #1f8b4c; font-size: 15px; margin-left: 6px; cursor: help;"
  )
}

tip_label <- function(text, id) {
  tagList(text, tip_icon(id))
}

about_tab <- function() {
  tabPanel(
    "About",
    fluidRow(
      column(
        width = 6,
        panelBox(
          "About the App",
          HTML("
            <p>This \"shiny\" app lets you explore the <a href='https://integrated-inferences.github.io/CausalQueries/' target='_blank'><code>CausalQueries</code></a> package. The <a href='https://cran.r-project.org/web/packages/CausalQueries/index.html' target='_blank'><code>CausalQueries</code></a> R package, maintained by <a href='https://github.com/till-tietz' target='_blank'>Till Tietz</a>, lets you declare binary causal models, update beliefs about causal types given data and calculate arbitrary estimands. Model definition is implemented via a dagitty style syntax. Updating is implemented in <a href='https://github.com/stan-dev/rstan/wiki/Rstan-Getting-Started' target='_blank'>Stan</a>.</p>
          ")
        ),
        panelBox(
          "Authors",
          HTML("
            <p><a href='https://macartan.github.io/' target='_blank'>Macartan Humphreys</a> and <a href='https://politics.ubc.ca/profile/alan-jacobs/' target='_blank'>Alan Jacobs</a> are the authors of <em>Integrated Inferences</em>.</p>
          ")
        )
      ),
      column(
        width = 6,
        panelBox(
          "Background",
          HTML("
            <p>For more background see <a href='https://integrated-inferences.github.io/book/' target='_blank'><em>Integrated Inferences</em></a>, which provides an introduction to fundamental principles of causal inference and Bayesian updating and shows how these tools can be used to implement and justify inferences using within-case (process tracing) evidence, correlational patterns across many cases, or a mix of the two.</p>
          ")
        ),
        panelBox(
          "Resources",
          HTML("
            <p>Learn more about <a href='https://integrated-inferences.github.io/CausalQueries/' target='_blank'><code>CausalQueries</code></a> and related resources at <a href='https://integrated-inferences.github.io/' target='_blank'>Integrated Inferences</a>.</p>
          ")
        ),
        div(
          style = "margin-top: 10px; text-align: center;",
          div(
            style = "display: inline-block; background: #ffffff; padding: 6px; border-radius: 8px;",
            tags$img(
              src = "causalqueries-hex.png",
              alt = "CausalQueries logo",
              height = "140px",
              style = "display: block;"
            )
          )
        )
      )
    )
  )
}

ui <- fluidPage(
  introjsUI(),
  tags$head(
    tags$style(HTML("
      .panel-box {
        background: #f7f9fc;
        border: 1px solid #e3e8f0;
        border-radius: 10px;
        padding: 14px 16px;
        margin-bottom: 14px;
        box-shadow: 0 1px 2px rgba(16, 24, 40, 0.04);
      }
      .panel-box h4 {
        margin-top: 0;
      }
      .panel-box h3 {
        margin-top: 0;
      }
      pre code.hljs {
        border-radius: 8px;
        padding: 12px;
        font-size: 12.5px;
      }
    "))
    ,
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/github.min.css"
    ),
    tags$link(
      rel = "icon",
      type = "image/png",
      href = "causalqueries-hex.png"
    ),
    tags$script(
      src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js"
    ),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('copy_replication', function(message) {
        var codeNode = document.querySelector('#replication_code_container code');
        var text = codeNode ? codeNode.textContent : '';
        if (!text) {
          Shiny.setInputValue('clipboard_status', { ok: false, msg: 'No replication code to copy.' }, { priority: 'event' });
          return;
        }
        if (!navigator.clipboard || !navigator.clipboard.writeText) {
          Shiny.setInputValue('clipboard_status', { ok: false, msg: 'Clipboard access is not available in this browser.' }, { priority: 'event' });
          return;
        }
        navigator.clipboard.writeText(text).then(function() {
          Shiny.setInputValue('clipboard_status', { ok: true, msg: 'Replication code copied to clipboard.' }, { priority: 'event' });
        }).catch(function() {
          Shiny.setInputValue('clipboard_status', { ok: false, msg: 'Could not copy to clipboard.' }, { priority: 'event' });
        });
      });

    "))
  ),
  titlePanel(HTML("<code>CausalQueries</code>: Make, update, and query causal models")),
  div(
    style = "display: flex; align-items: center; gap: 12px; margin: 6px 0 14px 0;",
    tags$span(
      style = "font-size: 13.5px;",
      "Start by making a model, refine it if needed, update with data if needed, then pose queries."
    ),
    actionButton("guide_me", "Guide me", class = "btn-info btn-sm")
  ),

  tabsetPanel(
    id = "main_tabs",
    tabPanel(
      "Make Model",
      fluidRow(
        column(
          width = 4,
          panelBox(
            tip_label("1. Input Model", "model"),
            id = "model_panel",
            textInput(
              "model_string",
              label = tip_label("Model (e.g. 'X->Y' or 'X -> M -> Y <-> X')", "causal_statement"),
              value = "X -> M -> Y <- X",
              placeholder = "Enter model specification"
            ),
            actionButton("create_model", "Create Model", class = "btn-primary")
          ),
          panelBox(
            tip_label("2. Set Restrictions (Optional)", "restrictions"),
            id = "restrictions_panel",
            uiOutput("restrictions_ui"),
            verbatimTextOutput("current_restrictions")
          ),
          panelBox(
            tip_label("3. Set Parameters (Optional)", "parameters"),
            id = "parameters_panel",
            uiOutput("parameters_ui"),
            verbatimTextOutput("current_parameters")
          )
        ),
        column(
          width = 8,
          panelBox(
            "Current model",
            plotOutput("model_plot", height = "400px")
          ),
          panelBox(
            "Model parameters",
            tableOutput("parameters_table")
          )
        )
      )
    ),
    tabPanel(
      "Update Model",
      fluidRow(
        column(
          width = 5,
          panelBox(
            tip_label("1. Complete Data Types", "complete_data_type"),
            p("Enter counts for fully observed data types (default 0)."),
            uiOutput("full_data_inputs")
          ),
          panelBox(
            tip_label("2. Partial Data Types", "partial_data_type"),
            p("Select a strategy and enter counts for all implied data types."),
            checkboxGroupInput(
              "partial_strategies",
              label = "Strategies",
              choices = character(0)
            ),
            uiOutput("partial_strategy_inputs")
          )
        ),
        column(
          width = 7,
          panelBox(
            tip_label("3. Update Options", "update_model"),
            id = "update_panel",
            div(
              style = "display: flex; align-items: flex-end; gap: 12px; flex-wrap: wrap;",
              numericInput(
                "update_refresh",
                label = "Refresh",
                value = 0,
                min = 0,
                step = 1,
                width = "120px"
              ),
              numericInput(
                "update_iter",
                label = "Iterations",
                value = 1000,
                min = 1,
                step = 100,
                width = "140px"
              ),
              actionButton("update_model", "Update Model", class = "btn-primary"),
              actionButton("clear_data", "Clear Data", class = "btn-danger")
            )
          ),
          panelBox(
            "Current data",
            tableOutput("compact_data_preview")
          ),
          panelBox(
            "Stan Summary",
            verbatimTextOutput("update_summary")
          )
        )
      )
    ),
    tabPanel(
      "Query model",
      fluidRow(
        column(
          width = 4,
          panelBox(
            tip_label("Input Queries", "query"),
            id = "query_panel",
            div(
              style = "display: flex; gap: 8px; margin-bottom: 8px;",
              actionButton("add_query_row", "Add query", class = "btn-warning"),
              actionButton("clear_query_rows", "Clear queries", class = "btn-danger")
            ),
            uiOutput("query_inputs"),
            div(
              style = "display: flex; gap: 8px; margin: 6px 0 8px 0;",
              actionButton("add_given_row", "Add given", class = "btn-warning"),
              actionButton("clear_given_rows", "Clear givens", class = "btn-danger")
            ),
            uiOutput("given_inputs")
          ),
          panelBox(
            "Options",
            checkboxGroupInput(
              "query_use",
              label = tip_label("Use", "using"),
              choices = c("priors", "posteriors", "parameters"),
              selected = c("priors")
            ),
            actionButton("compute_queries", "Compute Queries", class = "btn-primary")
          )
        ),
        column(
          width = 8,
          panelBox(
            "Query Plot",
            plotOutput("query_plot", height = "400px"),
            div(
              style = "margin-top: 8px;",
              downloadButton("download_query_plot", "Download PNG", class = "btn-default btn-sm")
            )
          ),
          panelBox(
            "Query Results",
            div(
              style = "margin-bottom: 8px;",
              downloadButton("download_query_table", "Download CSV", class = "btn-default btn-sm")
            ),
            tableOutput("query_table")
          )
        )
      )
    ),
    tabPanel(
      "Replication Code",
      fluidRow(
        column(
          width = 12,
          panelBox(
            "Minimal Replication Code",
            div(
              style = "margin-bottom: 8px;",
              actionButton("copy_replication", "Copy to clipboard", class = "btn-default btn-sm")
            ),
            div(id = "replication_code_container", uiOutput("replication_code")),
            tags$script("if (window.hljs) { hljs.highlightAll(); }")
          )
        )
      )
    ),
    tabPanel(
      "Intuition",
      div(
        style = "margin-bottom: 10px; font-size: 14px;",
        "This tab gives intuition for how a case-level inference is made (Bayesian process tracing). ",
        "On the left you input the data you see along with your query; on the right we then show the set of ",
        "'causal types' that are (a) consistent with the data and (b) consistent with the data and the query. ",
        "The final inference is the probability of the latter divided by the probability of the former."
      ),
      fluidRow(
        column(
          width = 4,
          panelBox(
            "Case level data",
            uiOutput("data_inputs")
          ),
          panelBox(
            "Case level query",
            textInput(
              "query",
              label = "Query (e.g., 'Y[X=1] == Y[X=0]' or 'Y[S=1] < Y[S=0]')",
              value = "",
              placeholder = "Enter causal query"
            ),
            actionButton("calculate", "Calculate", class = "btn-success")
          )
        ),
        column(
          width = 8,
          panelBox(
            "Results",
            htmlOutput("error_message"),
            h4("Summary"),
            htmlOutput("summary"),
            level = 3
          ),
          panelBox(
            "Detailed Results Table",
            DTOutput("results_table")
          )
        )
      )
    ),
    about_tab()
  ),
  tags$hr(),
  div(
    style = "margin: 8px 0 18px 0; font-size: 13px;",
    HTML(paste0(
      "For more about <code>CausalQueries</code> and the Integrated Inferences framework see ",
      "<a href='https://integrated-inferences.github.io/' target='_blank'>resources</a>."
    ))
  )
)

