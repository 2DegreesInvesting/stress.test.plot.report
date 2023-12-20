## Plots good practices

Plots in this repository should follow as much as possible the following guidelines. General idea is to make the plots easier to understand when reading the code, easily reusable for different studies with different inputs, and quickly integrable in RShiny.

1.  Code structure

-   A plot pipeline is composed by a data preprocessing function and a plot function. 1 data preproc function can feed multiple plots.
-   The data in input of a plot should only contain columns that will be used by the plot. no extra, and no processing outside of the data preproc function. Ideally no data processing (other than filtering eventually) happens in a plot function.
-   All variables should be given an explicit name (no : data, x, param1, group_sum, ...)
-   Variables syntax should follow those rules:
    -   all lowercase
    -   words sepearated by "\_", no usage of"."
    -   ideally no numbers. If a variable has to be iterated (plot_1, plot_2, ...), then this part of the code should probable be inside a for loop.

2.  Data preprocessing

-   The input data should be analysis_data. If a column is computed without aggregation and is used more than once in data preproc functions, it should be included to analysis_data.
-   Usually, the data is aggregated in some way, and the columns used for aggregation are re-used later in the plots (e.g. for facetting, coloring, iterating plots over run_ids..) . One or multiple columns can be used for this grouping, and they should be provided as input parameters. When this happens, make use of the `group_by_at(vector_of_string_column_names)` syntax (in general, use the dplyr::\*\_at functions that can read string inputs to use variable inputs).
-   Columns from analysis that are used/transformed in the function should be explicitely identified, either in the function name (if the variable cannot vary) or as a parameter (e.g. `value_to_transform="exposure_at_default`)\
-   All rows in the output dataframe should be used in the plot it feeds to.

#### Basic function example :

``` r
prepare_plot_data <- function(analysis_data, group_variable_vec, variable_to_transform){
  plot_data <- analysis_data |>
  dplyr::select_at(c(group_variable_vec,  variable_to_transform)) |>
  dplyr::group_by_at(group_variable_vec) |>
  dplyr::summarise(
    value_to_plot=sum(!!rlang::sym(variable_to_transform))
  )
  
  return(plot_data)
}
```

3.  Plot function

-   Columns used for plotting should be indicated in the input parameters of the function, or be present in the function name if this plot is variable specific. In order to plot using a variable input, this syntax should be used :
-   No data preprocessing should happen in the plotting function except filterings
-   If there is a filtering done inside the plotting function, it should be to display different plots. i.e. count vs percentage pie chart
-   The input data variable should be named the same as the output of the plot's associated data preprocessing function. i.e. the plot input data shouldn't just be named "data"
-   Usually, all variations that would be done on a plot will depend on the input column names. To create ggplots using variable input, those syntax are recommended :
-   aesthetics : `aes(x = !!rlang::sym(x_var), y = !!rlang::sym(y_var))`
-   facetting : `facet_wrap(as.formula(paste("~", facet_var)))`
-   All plots should be presented with examples in a .Rmd vignette