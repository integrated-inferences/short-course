tips <- list(
  model = "A causal model specifies the nodes (binary variables) and their causal links, e.g. X -> M -> Y.",
  causal_statement = "Provide the causal structure. Two headed arrows imply unobservable confounding.",
  restrictions = "Restrictions rule out specified types (causal responses). For instance you can impose monotonicity of Y in X by eliminating Y.10",
  parameters = "Parameters are probabilities over types (causal responses) at each node. They describe a specific possible world.",
  complete_data_type = "Complete data types record values for all nodes (e.g. X=0, M=1, Y=1).",
  partial_data_type = "Partial data types record values for only some nodes, possibly different nodes for different units.",
  update_model = "Updating uses Bayesian inference via a Stan model to combine priors with data and produce posteriors.",
  query = "A query uses bracket syntax for interventions. Examples: Y[X=1] - Y[X=0], Y[X=1] > Y[X=0], Y[X=1]==1.",
  given = "Restrict attention to cases matching specified observed or counterfactual conditions, e.g. X==1 & Y==1.",
  using = "Specify whether answers should be returned using specified parameters (fastest), priors, or posterior beliefs about parameters."
)

