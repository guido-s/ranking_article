### Load R packages and auxiliary R functions

source("./Clinical_examples/helpers/init.R")


### Get the 'antidepressants' dataset from the R package mtrank

data("antidepressants")


### Rank treatments based on P-scores

pw1 <- pairwise(
  studlab = studyid, treat = drug_name,
  event =  responders, n = ntotal,
  data = antidepressants,
  sm = "OR")

# Fit NMA model
nma1 <- netmeta(pw1, small.values = "undesirable",
  reference.group = "Trazodone", common = FALSE)

# Figure 3A: visualize NMA estimates
forest(nma1,
  overall.hetstat = TRUE, addrows = 0, calcwidth.hetstat = TRUE,
  print.tau = TRUE, digits.tau = 2,
  print.I2 = FALSE, print.pval.Q = FALSE,
  #
  drop.reference.group = TRUE,
  #
  label.left = "Favors trazodone",
  label.right = "Favors other treatments",
  #
  header.line = TRUE, spacing = 1.5,
  #
  file = "Figure3A.pdf", width = 6)

# Ranking based on P-scores
nr1 <- netrank(nma1)
print(nr1, digits = 2)


### Rank treatments based on the proposed approach

# Define a TCC using 1.20 as the SWD
ranks1 <- tcc(nma1, swd = 1.20)

# Visualize the TCC in terms of the basic parameters ("Treatments vs Trazodone")
forest(ranks1,
  label.right = "Favors other treatments",
  label.left = "Favors Trazodone")

# Fit the model and get the ability estimates
ability1 <- mtrank(ranks1)

# Ability estimates and probability that each treatment is ranked first
ability1

# Figure 3B: visualize the ability estimates
forest(ability1, spacing = 1.5,
  file = "Figure3B.pdf", width = 6)

# Figure 4: line graph for sensitivity analysis
sensitivity1 <- linegraph(ability1,
  swd = seq(1.10, 1.50, by = 0.10),
  swd.ref = 1.20, k = 6)
#
sensitivity1

# Figure 4 with same colors as in main manuscript
# install.packages("ggsci")
# library(ggsci)
# sensitivity1 + scale_color_bmj() + ylab(expression("Normalized ability (" * hat(pi)[X] * ")"))


### Rank treatments based on the PReTA approach

anma1 <- alternativenma(nma1, small.values = "bad") 

preta1 <- data.frame(
  treatment = row.names(anma1$averages),
  PReTA = round(anma1$averages$Pscoreaverage, 2))

preta1 %>% arrange(desc(PReTA))


### Rank treatments based on P-scores adjusted for the SWD

pscore1_swd <- data.frame(
  Pscore_swd = round(p_scores(list(nma1), log(1.20), NULL, "B"), 2))
pscore1_swd %>% arrange(desc(Pscore_swd))


### Rank treatments based on the frequentist pBV approach

set.seed(1904)
ranko1 <- rankogram(nma1, nsim = 50000)
pBV1 <- data.frame(pBV = round(ranko1$ranking.matrix.random[, 1], 2))
pBV1 %>% arrange(desc(pBV))


### Table 2

ORs1 <- data.frame(OR = round(exp(nma1$TE.random[, "Trazodone"]), 2)) %>%
  rownames_to_column(var = "treatment")
#
pscore1 <- data.frame(Pscore = round(nr1$ranking.random, 2)) %>%
  rownames_to_column(var = "treatment")
#
pscore1_swd %<>% rownames_to_column(var = "treatment")
#
pBV1 %<>% rownames_to_column(var = "treatment")
#
pi1 <- ability1$probabilities %>%
  rename(pi_X = probability) %>%
  mutate(pi_X = round(pi_X, 2))
#
ORs1 %>%
  left_join(pscore1, by = "treatment") %>%
  left_join(pscore1_swd, by = "treatment") %>%
  left_join(preta1, by = "treatment") %>%
  left_join(pBV1, by = "treatment") %>%
  left_join(pi1, by = "treatment") %>%
  column_to_rownames(var = "treatment") %>%
  arrange(desc(Pscore))
