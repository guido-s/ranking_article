library(tidyverse)
library(mtrank)

### Functions to get PReTA and adjusted according to P-score

source("./Empirical_study/helpers/alternative_nma.R")
source("./Empirical_study/helpers/nma.krahn.output.R")
source("./Empirical_study/helpers/Pscores_function.R")
source("./Empirical_study/helpers/league_table.R")
source("./Empirical_study/helpers/league_table_var.R")
source("./Empirical_study/helpers/intersect2.R")
source("./Empirical_study/helpers/Prepare_Multi.R")
source("./Empirical_study/helpers/Prepare_Single.R")
source("./Empirical_study/helpers/Prepare_function.R")
source("./Empirical_study/helpers/pscrs.R")
source("./Empirical_study/helpers/pscore_graph.R")
source("./Empirical_study/helpers/is_star.R")

# A list with networks from nmadb
nmadb <- readRDS("./Empirical_study/NMAs_nmadb/nmadb.rds")

# A list with NMAs in terms of Risk Ratio obtained from nmadb's networks
mod_RR <- readRDS("./Empirical_study/NMAs_nmadb/mod_RR.rds")

# A function to calculate rankings across methods 
source("./Empirical_study/helpers/all_rankings.R")

# A function to calculate correlation coeffiecients across methods 
source("./Empirical_study/helpers/get_correlations.R")

