tips <- list(
  model = "A causal model specifies the nodes (binary variables) and their causal links, e.g. X -> M -> Y.",
  causal_statement = "Provide causal structure. Two headed arrows imply unobservable confounding.",
  restrictions = "Restrictions rule out nodal types. Examples: set_restrictions(labels = list(M = c('10','11'))) or set_restrictions(statement = 'Y[X=1] > Y[X=0]').",
  parameters = "Parameters are probabilities over nodal types (causal responses) at each node. They describe a specific possible world.",
  complete_data_type = "Complete data types record values for all nodes (e.g. X=0, M=1, Y=1).",
  partial_data_type = "Partial data types record values for only some nodes, possibly different nodes for different units.",
  update_model = "Updating uses Bayesian inference via a Stan model to combine priors with data and produce posteriors.",
  query = "A query uses bracket syntax for interventions. Examples: Y[X=1] - Y[X=0], Y[X=1] > Y[X=0], Y[X=1]==1.",
  given = "Conditions restrict attention to cases matching observational statements, e.g. X==1 & Y==1.",
  using = "Choose what to use when answering queries. Parameters use the default values (fastest), priors use prior uncertainty, posteriors use updated uncertainty."
)

