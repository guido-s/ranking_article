### Load R packages and auxiliary R functions

source("./Clinical_examples/helpers/init.R")


### Get the 'diabetes' dataset from the mtrank package

data("diabetes")


### Rank treatments based on P-scores

pw2 <- pairwise(
  studlab = study, treat = t,
  event =  r, n = n,
  data = diabetes,
  sm = "OR")

# Fit NMA model
nma2 <- netmeta(pw2, small.values = "desirable",
  reference.group = "Placebo", common = FALSE)

# Figure 5A: visualize NMA estimates
forest(nma2,
  overall.hetstat = TRUE, addrows = 0, calcwidth.hetstat = TRUE,
  print.tau = TRUE, digits.tau = 2,
  print.I2 = FALSE, print.pval.Q = FALSE,
  #
  drop.reference.group = TRUE,
  #
  label.left = "Favors other treatments",
  label.right = "Favors Placebo",
  #
  header.line = TRUE, spacing = 1.5,
  #
  file = "Figure5A.pdf", width = 6)

# Ranking based on P-scores
nr2 <- netrank(nma2)
print(nr2, digits = 2)


### Rank treatments based on the proposed approach

# Define a TCC using 1.20 as the SWD
ranks2 <- tcc(nma2, swd = 1.20)

# Visualize the TCC in terms of the basic parameters ("Treatments vs Placebo")
forest(ranks2,
  label.right="Favors Placebo",
  label.left = "Favors other treatments")

# Fit the model and get the ability estimates
ability2 <- mtrank(ranks2)

# Ability estimates and probability that each treatment is ranked first
ability2

# Figure 5B: visualize the ability estimates
forest(ability2, spacing = 1.5,
  file = "Figure5B.pdf", width = 6)

# Figure 6: line graph for sensitivity analysis
sensitivity2 <- linegraph(ability2,
  swd = seq(1.10, 1.50, by = 0.10),
  swd.ref = 1.20)
#
sensitivity2

# Figure 6 with same colors as in main manuscript
# install.packages("ggsci")
# library(ggsci)
# sensitivity2 + scale_color_bmj() + ylab(expression("Normalized ability (" * hat(pi)[X] * ")"))


### Rank treatments based on the PReTA approach

anma2 <- alternativenma(nma2, small.values = "good") 

preta2 <- data.frame(
  treatment = row.names(anma2$averages),
  PReTA = round(anma2$averages$Pscoreaverage, 2))

preta2 %>% arrange(desc(PReTA))


### Rank treatments based on P-scores adjusted for the SWD

pscore2_swd <- data.frame(
  Pscore_swd = round(p_scores(list(nma2), log(1.20), NULL, "H"), 2))
pscore2_swd %>% arrange(desc(Pscore_swd))


### Rank treatments based on the frequentist pBV approach

set.seed(1906)
ranko2 <- rankogram(nma2, nsim = 50000)
pBV2 <- data.frame(pBV = round(ranko2$ranking.matrix.random[, 1], 2))
pBV2 %>% arrange(desc(pBV))


### Table 2

ORs2 <- data.frame(OR = round(exp(nma2$TE.random[, "Placebo"]), 2)) %>%
  rownames_to_column(var = "treatment")
#
pscore2 <- data.frame(Pscore = round(nr2$ranking.random, 2)) %>%
  rownames_to_column(var = "treatment")
#
pscore2_swd %<>% rownames_to_column(var = "treatment")
#
pBV2 %<>% rownames_to_column(var = "treatment")
#
pi2 <- ability2$probabilities %>%
  rename(pi_X = probability) %>%
  mutate(pi_X = round(pi_X, 2))
#
ORs2 %>%
  left_join(pscore2, by = "treatment") %>%
  left_join(pscore2_swd, by = "treatment") %>%
  left_join(preta2, by = "treatment") %>%
  left_join(pBV2, by = "treatment") %>%
  left_join(pi2, by = "treatment") %>%
  column_to_rownames(var = "treatment") %>%
  arrange(desc(Pscore))
