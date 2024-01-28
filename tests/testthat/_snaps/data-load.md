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

# main_data_load_trajectories_data_from_file loads_properly on ald_sector and business_unit

    Code
      analysis_data
    Output
      # A tibble: 1,746 x 7
         ald_sector ald_business_unit run_id               year production_baseline_~1
         <chr>      <chr>             <chr>               <dbl>                  <dbl>
       1 Coal       Coal              27187540-f356-48b9~  2022                  0.537
       2 Coal       Coal              27187540-f356-48b9~  2023                  0.845
       3 Coal       Coal              27187540-f356-48b9~  2024                  1    
       4 Coal       Coal              27187540-f356-48b9~  2025                  0.897
       5 Coal       Coal              27187540-f356-48b9~  2026                  0.878
       6 Coal       Coal              27187540-f356-48b9~  2027                  0.878
       7 Coal       Coal              27187540-f356-48b9~  2028                  0.865
       8 Coal       Coal              27187540-f356-48b9~  2029                  0.853
       9 Coal       Coal              27187540-f356-48b9~  2030                  0.841
      10 Coal       Coal              27187540-f356-48b9~  2031                  0.824
      # i 1,736 more rows
      # i abbreviated name: 1: production_baseline_scenario
      # i 2 more variables: production_target_scenario <dbl>,
      #   production_shock_scenario <dbl>

