# Script for computing verification results from manymome package

# manymome uses the same approach as the Process module
# so the results should be identical

# install.packages("manymome")

### Exammple Global Warming data set

# Read data set from Hayes (2022)
df <- read.csv(testthat::test_path("global_warming.csv"))

# Calculate interaction term
df$NegativeEmotion__Age <- df$NegativeEmotion * df$Age

# Syntax from Process module
syntax <- "
  GovernmentAction ~ c1*NegativeEmotion + c2*Age + c3*NegativeEmotion__Age

  # Mediation, indirect, and total effects
  NegativeEmotion__GovernmentAction.Age__16 := (c1 + c3*30)
  NegativeEmotion__GovernmentAction.Age__50 := (c1 + c3*51)
  NegativeEmotion__GovernmentAction.Age__84 := (c1 + c3*67)
  tot.NegativeEmotion__GovernmentAction.Age__16 := (c1 + c3*30)
  tot.NegativeEmotion__GovernmentAction.Age__50 := (c1 + c3*51)
  tot.NegativeEmotion__GovernmentAction.Age__84 := (c1 + c3*67)
"

# Fit lavaan model
fit <- lavaan::sem(syntax, data = df, missing = "fiml")

# Calculate conditional indirect effects with manymome
effects <- manymome::cond_indirect_effects(
  wlevels = "Age",
  x = "NegativeEmotion",
  y = "GovernmentAction",
  fit = fit,
  w_method = "percentile",
  standardized_x = TRUE,
  standardized_y = TRUE
)

# Save for testing
write.csv(effects, test_path("global_warming_cond_indirect_effects.csv"))

### Exammple Climate Change Disasters data set

df <- read.csv(testthat::test_path("climate_change_disasters.csv"))

df$Justification__Skepticism <- df$Justification * df$Skepticism
df$Framing1__Skepticism <- df$Framing * df$Skepticism

df$Framing1 <- df$Framing

syntax <- "
  DonationAttitude ~ c1*Framing1 + b1*Justification + c2*Skepticism + c3*Framing1__Skepticism
  Justification ~ a1*Framing1 + a2*Skepticism + a3*Framing1__Skepticism

  # Residual covariances
  DonationAttitude ~~ DonationAttitude

  # Mediation, indirect, and total effects
  Framing__DonationAttitude.Framing__1.Skepticism__16 := (c1 + c3*1.6)
  Framing__DonationAttitude.Framing__1.Skepticism__50 := (c1 + c3*2.8)
  Framing__DonationAttitude.Framing__1.Skepticism__84 := (c1 + c3*5.2)
  Framing__Justification__DonationAttitude.Framing__1.Skepticism__16 := (a1 + a3*1.6)*b1
  Framing__Justification__DonationAttitude.Framing__1.Skepticism__50 := (a1 + a3*2.8)*b1
  Framing__Justification__DonationAttitude.Framing__1.Skepticism__84 := (a1 + a3*5.2)*b1
  tot.Framing__DonationAttitude.Framing__1.Skepticism__16 := (c1 + c3*1.6) + (a1 + a3*1.6)*b1
  tot.Framing__DonationAttitude.Framing__1.Skepticism__50 := (c1 + c3*2.8) + (a1 + a3*2.8)*b1
  tot.Framing__DonationAttitude.Framing__1.Skepticism__84 := (c1 + c3*5.2) + (a1 + a3*5.2)*b1
  totInd.Framing__DonationAttitude.Framing__1.Skepticism__16 := (a1 + a3*1.6)*b1
  totInd.Framing__DonationAttitude.Framing__1.Skepticism__50 := (a1 + a3*2.8)*b1
  totInd.Framing__DonationAttitude.Framing__1.Skepticism__84 := (a1 + a3*5.2)*b1
"

fit <- lavaan::sem(syntax, data = df, missing = "fiml")

# Calculate conditional direct effects
effectsDirect <- manymome::cond_indirect_effects(
  wlevels = "Skepticism",
  x = "Framing1",
  y = "DonationAttitude",
  fit = fit,
  w_method = "percentile",
  standardized_x = FALSE, # don't standardize Framing because it's a factor
  standardized_y = TRUE
)

# Calculate conditional indirect effects
effectsIndirect <- manymome::cond_indirect_effects(
  wlevels = "Skepticism",
  x = "Framing1",
  y = "DonationAttitude",
  m = "Justification", # indirect effect through Justification
  fit = fit,
  w_method = "percentile",
  standardized_x = FALSE,
  standardized_y = TRUE
)

write.csv(
  rbind(
    effectsIndirect[c("[Skepticism]", "(Skepticism)", "std", "ind")],
    effectsDirect[c("[Skepticism]", "(Skepticism)", "std", "ind")]
  ), test_path("climate_change_disasters_cond_indirect_effects.csv")
)
