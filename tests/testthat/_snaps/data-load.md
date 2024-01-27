# analysis_data loads_properly on ald_sector

    Code
      analysis_data
    Output
      # A tibble: 35 x 34
         ald_sector portfolio_id  term asset_type   exposure_value_usd
         <chr>      <chr>        <dbl> <chr>                     <dbl>
       1 Coal       portfolio_A      1 fixed_income         1590393892
       2 Coal       portfolio_A      1 fixed_income         1590393892
       3 Coal       portfolio_A      1 fixed_income         1590393892
       4 Coal       portfolio_A      1 fixed_income         1590393892
       5 Coal       portfolio_A      1 fixed_income         1590393892
       6 Coal       portfolio_A      1 fixed_income         1590393892
       7 Coal       portfolio_A      1 fixed_income         1590393892
       8 Oil&Gas    portfolio_A      1 fixed_income         1067757612
       9 Oil&Gas    portfolio_A      1 fixed_income         1067757612
      10 Oil&Gas    portfolio_A      1 fixed_income         1067757612
      # i 25 more rows
      # i 29 more variables: loss_given_default <dbl>, pd_portfolio <dbl>,
      #   run_id <chr>, roll_up_type <chr>, scenario_geography <chr>,
      #   baseline_scenario <chr>, shock_scenario <chr>, risk_free_rate <dbl>,
      #   discount_rate <dbl>, div_netprofit_prop_coef <dbl>,
      #   carbon_price_model <chr>, market_passthrough <dbl>,
      #   financial_stimulus <dbl>, start_year <dbl>, growth_rate <dbl>, ...

# analysis_data loads_properly on ald_sector and business_unit

    Code
      analysis_data
    Output
      # A tibble: 112 x 35
         ald_sector ald_business_unit portfolio_id  term asset_type exposure_value_usd
         <chr>      <chr>             <chr>        <dbl> <chr>                   <dbl>
       1 Coal       Coal              portfolio_A      1 fixed_inc~         1590393892
       2 Coal       Coal              portfolio_A      1 fixed_inc~         1590393892
       3 Coal       Coal              portfolio_A      1 fixed_inc~         1590393892
       4 Coal       Coal              portfolio_A      1 fixed_inc~         1590393892
       5 Coal       Coal              portfolio_A      1 fixed_inc~         1590393892
       6 Coal       Coal              portfolio_A      1 fixed_inc~         1590393892
       7 Coal       Coal              portfolio_A      1 fixed_inc~         1590393892
       8 Oil&Gas    Gas               portfolio_A      1 fixed_inc~          484029118
       9 Oil&Gas    Gas               portfolio_A      1 fixed_inc~          484029118
      10 Oil&Gas    Gas               portfolio_A      1 fixed_inc~          484029118
      # i 102 more rows
      # i 29 more variables: loss_given_default <dbl>, pd_portfolio <dbl>,
      #   run_id <chr>, roll_up_type <chr>, scenario_geography <chr>,
      #   baseline_scenario <chr>, shock_scenario <chr>, risk_free_rate <dbl>,
      #   discount_rate <dbl>, div_netprofit_prop_coef <dbl>,
      #   carbon_price_model <chr>, market_passthrough <dbl>,
      #   financial_stimulus <dbl>, start_year <dbl>, growth_rate <dbl>, ...

