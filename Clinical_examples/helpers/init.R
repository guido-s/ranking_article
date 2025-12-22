### Load the necessary libraries, all of which are available on CRAN. 

library(mtrank)
library(netmeta)
library(tidyverse)
library(magrittr)

### Source code to get ranking based on the PReTA approach
### Code obtained from the GitHub page:
### https://github.com/esm-ispm-unibe-ch/alternativenma/tree/master/R

source("./Clinical_examples/helpers/nma.krahn.output.R")
source("./Clinical_examples/helpers/alternative_nma.R")
source("./Clinical_examples/helpers/netmetaranks.R")


### Source code to get ranking based on P-scores adjusted for the SWD
### Code obtained from the GitHub page:
### https://github.com/DimitrisMavridis/RankingNMA/blob/master/extendedP-scores

source("./Clinical_examples/helpers/Pscores_function.R")
source("./Clinical_examples/helpers/league_table.R")
source("./Clinical_examples/helpers/league_table_var.R")
source("./Clinical_examples/helpers/intersect2.R")
source("./Clinical_examples/helpers/Prepare_Multi.R")
source("./Clinical_examples/helpers/Prepare_Single.R")
source("./Clinical_examples/helpers/Prepare_function.R")
source("./Clinical_examples/helpers/pscrs.R")
source("./Clinical_examples/helpers/pscore_graph.R")

