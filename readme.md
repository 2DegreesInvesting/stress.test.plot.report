## Plots good practices

1.  Code structure

-   A plot pipeline is composed by a data preprocessing function and a plot function. 1 data preproc function can feed multiple plots.
-   The data in input of a plot should only contain columns that will be used by the plot. no extra, and no processing outside of the data preproc function. Ideally no data processing (other than filtering eventually) happens in a plot function.
-  All variables should be given an explicit name (no : data, x, param1, group_sum, ...)

2.  Data preprocessing

-   The input data should be analysis_data. If a column is computed without aggregation and is used more than once in data preproc functions, it should be included to analysis_data.
-   Usually, the data is aggregated in some way, and the columns used for aggregation are re-used later in the plots (e.g. for facetting, coloring, iterating plots over run_ids..) . One or multiple columns can be used for this grouping, and they should be provided as input parameters. To cover those cases quickly, it is recommended to group using `group_by(accross(all_of(vector_of_column_name)))` syntax.
-   Columns from analysis that are used/transformed in the function should be explicitely identified, either in the function name (if the variable cannot vary) or as a parameter (e.g. `value_to_transform="exposure_at_default`)  
-   Limit the use of tidyr::pivot. All rows in the output dataframe should be used in a plot.

#### Basic function example : 
``` r
prepare_plot_data <- function(analysis_data, group_variable_vec, variable_to_transform){
  plot_data <- analysis_data |>
  # group by all columns
  dplyr::group_by(
    dplyr::accross(
      dplyr::all_of(group_variable_vec)
    )
  ) |>
  dplyr::summarise(
    value_to_plot=sum(!!rlang::sym(value_to_plot_char))
  )
  
  return(plot_data)
}
```

3.  Plot function

-   Columns used for plotting should be indicated in the input parameters of the function, or be present in the function name if this plot is variable specific.
-   NO data preprocessing should happen in the plotting function
-   If there is a filtering done inside the plotting function, it is to display different plots on different filterings at once. i.e. count vs percentage pie chart

