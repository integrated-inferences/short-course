---
title: "solution"
format: html
---





::: {.cell}

```{.r .cell-code}
model <- 
  
  make_model("S -> C -> Y <- R <- X; X -> C -> R") |>
  
  set_restrictions(labels = list(
    C = c("1110", "1111"), 
    R = c("0001", "0000"), 
    Y = c("0001")),
    keep = TRUE)


model <- 
  
  make_model("S -> C -> Y <- R <- X; X -> C -> R") |>
  
  set_restrictions(labels = list(C = c("1110", "1111")), keep = TRUE) |>
  set_restrictions(labels = list(R = c("0001", "0000")), keep = TRUE) |>
  set_restrictions(labels = list(Y = c("0001")), keep = TRUE) 
```
:::




# Causal types consistent with a query


::: {.cell}

```{.r .cell-code}
get_query_types(model, "Y[S=1] < Y[S=0]")
```

::: {.cell-output .cell-output-stdout}

```

Causal types satisfying query's condition(s)  

 query =  Y[S=1]<Y[S=0] 

S0.X1.C1110.R0001.Y0001  S1.X1.C1110.R0001.Y0001


 Number of causal types that meet condition(s) =  2
 Total number of causal types in model =  16
```


:::
:::


# Mapping from causal types to consistent data types


::: {.cell}

```{.r .cell-code}
inspect(model, what = "ambiguities_matrix") 
```

::: {.cell-output .cell-output-stdout}

```

ambiguities_matrix (Ambiguities matrix)
Mapping from causal types into data types:

snippet (use grab() to access full 16 x 7 object): 

                    S1X1C0R0Y0 S0X0C1R0Y0 S1X0C1R0Y0 S0X1C1R0Y0 S1X1C1R0Y0
S0X0C1110R0000Y0001          0          1          0          0          0
S1X0C1110R0000Y0001          0          0          1          0          0
S0X1C1110R0000Y0001          0          0          0          1          0
S1X1C1110R0000Y0001          1          0          0          0          0
S0X0C1111R0000Y0001          0          1          0          0          0
S1X0C1111R0000Y0001          0          0          1          0          0
S0X1C1111R0000Y0001          0          0          0          1          0
S1X1C1111R0000Y0001          0          0          0          0          1
S0X0C1110R0001Y0001          0          1          0          0          0
S1X0C1110R0001Y0001          0          0          1          0          0
                    S0X1C1R1Y1 S1X1C1R1Y1
S0X0C1110R0000Y0001          0          0
S1X0C1110R0000Y0001          0          0
S0X1C1110R0000Y0001          0          0
S1X1C1110R0000Y0001          0          0
S0X0C1111R0000Y0001          0          0
S1X0C1111R0000Y0001          0          0
S0X1C1111R0000Y0001          0          0
S1X1C1111R0000Y0001          0          0
S0X0C1110R0001Y0001          0          0
S1X0C1110R0001Y0001          0          0
```


:::
:::


# Prior probabilities of each causal type


::: {.cell}

```{.r .cell-code}
CausalQueries:::get_type_prob(model)
```

::: {.cell-output .cell-output-stdout}

```
 [1] 0.0625 0.0625 0.0625 0.0625 0.0625 0.0625 0.0625 0.0625 0.0625 0.0625
[11] 0.0625 0.0625 0.0625 0.0625 0.0625 0.0625
```


:::
:::


# Put it all together


::: {.cell}

```{.r .cell-code}
worksheet <-
  inspect(model, what = "ambiguities_matrix") |>
  data.frame() |>
  mutate(
    in_query = get_query_types(model, "Y[S=1] < Y[S=0]")$types,
    priors  = CausalQueries:::get_type_prob(model))
```

::: {.cell-output .cell-output-stdout}

```

ambiguities_matrix (Ambiguities matrix)
Mapping from causal types into data types:

snippet (use grab() to access full 16 x 7 object): 

                    S1X1C0R0Y0 S0X0C1R0Y0 S1X0C1R0Y0 S0X1C1R0Y0 S1X1C1R0Y0
S0X0C1110R0000Y0001          0          1          0          0          0
S1X0C1110R0000Y0001          0          0          1          0          0
S0X1C1110R0000Y0001          0          0          0          1          0
S1X1C1110R0000Y0001          1          0          0          0          0
S0X0C1111R0000Y0001          0          1          0          0          0
S1X0C1111R0000Y0001          0          0          1          0          0
S0X1C1111R0000Y0001          0          0          0          1          0
S1X1C1111R0000Y0001          0          0          0          0          1
S0X0C1110R0001Y0001          0          1          0          0          0
S1X0C1110R0001Y0001          0          0          1          0          0
                    S0X1C1R1Y1 S1X1C1R1Y1
S0X0C1110R0000Y0001          0          0
S1X0C1110R0000Y0001          0          0
S0X1C1110R0000Y0001          0          0
S1X1C1110R0000Y0001          0          0
S0X0C1111R0000Y0001          0          0
S1X0C1111R0000Y0001          0          0
S0X1C1111R0000Y0001          0          0
S1X1C1111R0000Y0001          0          0
S0X0C1110R0001Y0001          0          0
S1X0C1110R0001Y0001          0          0
```


:::

```{.r .cell-code}
worksheet |>
  kable()
```

::: {.cell-output-display}


|                    | S1X1C0R0Y0| S0X0C1R0Y0| S1X0C1R0Y0| S0X1C1R0Y0| S1X1C1R0Y0| S0X1C1R1Y1| S1X1C1R1Y1|in_query | priors|
|:-------------------|----------:|----------:|----------:|----------:|----------:|----------:|----------:|:--------|------:|
|S0X0C1110R0000Y0001 |          0|          1|          0|          0|          0|          0|          0|FALSE    | 0.0625|
|S1X0C1110R0000Y0001 |          0|          0|          1|          0|          0|          0|          0|FALSE    | 0.0625|
|S0X1C1110R0000Y0001 |          0|          0|          0|          1|          0|          0|          0|FALSE    | 0.0625|
|S1X1C1110R0000Y0001 |          1|          0|          0|          0|          0|          0|          0|FALSE    | 0.0625|
|S0X0C1111R0000Y0001 |          0|          1|          0|          0|          0|          0|          0|FALSE    | 0.0625|
|S1X0C1111R0000Y0001 |          0|          0|          1|          0|          0|          0|          0|FALSE    | 0.0625|
|S0X1C1111R0000Y0001 |          0|          0|          0|          1|          0|          0|          0|FALSE    | 0.0625|
|S1X1C1111R0000Y0001 |          0|          0|          0|          0|          1|          0|          0|FALSE    | 0.0625|
|S0X0C1110R0001Y0001 |          0|          1|          0|          0|          0|          0|          0|FALSE    | 0.0625|
|S1X0C1110R0001Y0001 |          0|          0|          1|          0|          0|          0|          0|FALSE    | 0.0625|
|S0X1C1110R0001Y0001 |          0|          0|          0|          0|          0|          1|          0|TRUE     | 0.0625|
|S1X1C1110R0001Y0001 |          1|          0|          0|          0|          0|          0|          0|TRUE     | 0.0625|
|S0X0C1111R0001Y0001 |          0|          1|          0|          0|          0|          0|          0|FALSE    | 0.0625|
|S1X0C1111R0001Y0001 |          0|          0|          1|          0|          0|          0|          0|FALSE    | 0.0625|
|S0X1C1111R0001Y0001 |          0|          0|          0|          0|          0|          1|          0|FALSE    | 0.0625|
|S1X1C1111R0001Y0001 |          0|          0|          0|          0|          0|          0|          1|FALSE    | 0.0625|


:::
:::



# Helper


::: {.cell}

```{.r .cell-code}
by_hand <- function(model, data, query){
  
  data_type <- CausalQueries:::data_type_names(model, data)

  grab(model, what = "ambiguities_matrix") |>
    data.frame() |>
    select(all_of(data_type)) |>
    mutate(
      in_query = get_query_types(model, query)$types,
      priors   = CausalQueries:::get_type_prob(model)
    ) |>
    filter(.data[[data_type]] == 1) |>
    mutate(rescaled_priors = priors / sum(priors)) |>
    {\(df) {
      posterior <- sum(df$rescaled_priors[df$in_query])
      cat(
        "Posterior is the sum of rescaled priors for\n",
        "all possible types in query: ",
        posterior,
        "\n",
        sep = ""
      )
      df
    }}()
}
```
:::


# Using the function


::: {.cell}

```{.r .cell-code}
model <- make_model("S -> C -> Y <- R <- X; X -> C -> R") |>
  set_restrictions(labels = list( C = c("1110", "1111"), 
                                  R = c("0001", "0000"), 
                                  Y = c("0001")), 
                   keep = TRUE) 
data <- data.frame(S= 1, X=1, C= 0, R =0, Y = 0) 

query <- "Y[S=1] < Y[S=0]" 

by_hand(model, data, query)
```

::: {.cell-output .cell-output-stdout}

```
Posterior is the sum of rescaled priors for
all possible types in query: 0.5
```


:::

::: {.cell-output .cell-output-stdout}

```
                    S1X1C0R0Y0 in_query priors rescaled_priors
S1X1C1110R0000Y0001          1    FALSE 0.0625             0.5
S1X1C1110R0001Y0001          1     TRUE 0.0625             0.5
```


:::

```{.r .cell-code}
by_hand(model, data, "Y[S=1] > Y[S=0]")
```

::: {.cell-output .cell-output-stdout}

```
Posterior is the sum of rescaled priors for
all possible types in query: 0
```


:::

::: {.cell-output .cell-output-stdout}

```
                    S1X1C0R0Y0 in_query priors rescaled_priors
S1X1C1110R0000Y0001          1    FALSE 0.0625             0.5
S1X1C1110R0001Y0001          1    FALSE 0.0625             0.5
```


:::

```{.r .cell-code}
by_hand(model, data, "Y[X=1] == Y[X=0]")
```

::: {.cell-output .cell-output-stdout}

```
Posterior is the sum of rescaled priors for
all possible types in query: 1
```


:::

::: {.cell-output .cell-output-stdout}

```
                    S1X1C0R0Y0 in_query priors rescaled_priors
S1X1C1110R0000Y0001          1     TRUE 0.0625             0.5
S1X1C1110R0001Y0001          1     TRUE 0.0625             0.5
```


:::
:::



