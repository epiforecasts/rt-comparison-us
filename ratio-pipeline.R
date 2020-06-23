# Update Rts, get ratios, and plot - regional + national
# 
# Run nowcasts, 
# including:
# Cases: update-cases-nowcast.R
# Deaths: update-nowcast.R
# 
# Then:

# 1 Update/reshape Rt estimates
source(here::here("update-reshape-rts.R"))
# 2 Update ratios
source(here::here("update-ratios.R"))
# 3 Update counts
source(here::here("update-counts.R"))
# 4 Plot and save plots: linerange & ratio, national & regional
source(here::here("update-plots.R"))
