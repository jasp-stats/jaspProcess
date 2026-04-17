
test_that("Missing values work without independent covariances", {
  options <- getOptionsBayesian()
  options$covariates <- list("contGamma", "debMiss1", "debMiss30", "debMiss80")
  options$processModels <- list(getProcessModel(list(list(processDependent = "debMiss80",
                                                                      processIndependent = "debMiss1", processType = "mediators",
                                                                      processVariable = "contGamma"), list(processDependent = "debMiss80",
                                                                                                            processIndependent = "debMiss1", processType = "moderators",
                                                                                                            processVariable = "debMiss30"))))
  options$processModels[[1]]$independentCovariances <- FALSE
  options$processModels[[1]]$intercepts <- TRUE
  set.seed(1)
  results <- jaspTools::runAnalysis("BayesianProcess", "debug.csv", options)

  table <- results[["results"]][["modelSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Model 1", 69, 418.353015738978, 404.167649037733, 38.0831270292123,
                                      0.58, 404.037300573496, 38.1334494656413))

	table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_covariancesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
	                               list(186, 197.642203743007, 674.050518618612, "debMiss80", 382.682691746751,
	                                    348.457174361111, "<unicode>", 0.997101731601075, "debMiss80",
	                                    139.145252235788, 154, 258, 2.07339253231642, 4.35403142856012,
	                                    "contGamma", 2.86286626176398, 2.74084538621459, "<unicode>",
	                                    1.00417817276285, "contGamma", 0.577715915253529, 154))

	table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_mediationEffectsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
	                               list(85, -1.47574204027912, 0.519422797602804, 16, "debMiss1", "debMiss80",
	                                    "", -0.474216333010148, -0.496302283082364, "<unicode>", "",
	                                    1.00707816070103, 0.526136521172827, 147, 109, -0.894589308342602,
	                                    0.240169128868961, 50, "debMiss1", "debMiss80", "", -0.317980426522198,
	                                    -0.317842151522786, "<unicode>", "", 1.01434680306432, 0.298135162955701,
	                                    97, 203, -0.635898440289165, 0.232170722362577, 84, "debMiss1",
	                                    "debMiss80", "", -0.171317367874374, -0.173256137233884, "<unicode>",
	                                    "", 0.99989104066179, 0.2303238436986, 116, 167, -0.0581640136170782,
	                                    0.108158873786955, "", "debMiss1", "contGamma", "debMiss80",
	                                    0.00830514337934994, 0.00308370983773346, "<unicode>", "<unicode>",
	                                    1.00023883424941, 0.036523346064018, 60))

	table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_pathCoefficientsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
	                               list(20, -3.43295200972111, 18.0122101903546, "(Intercept)", 8.33341740149667,
	                                    9.56829958350078, "<unicode>", "Normal(0,10)", 1.05999347328935,
	                                    "debMiss80", 6.26236939679863, 83, 187, 1.57734227010841, 2.42879564018793,
	                                    "(Intercept)", 2.01020662709592, 2.03069956720223, "<unicode>",
	                                    "Normal(0,10)", 1.00141883085515, "contGamma", 0.228804794849895,
	                                    150, "", "", "", "(Intercept)", 3.04981150169565, 3.04981150169565,
	                                    "<unicode>", "", "", "debMiss1", 0, "", "", "", "", "(Intercept)",
	                                    7.77994174547826, 7.77994174547826, "<unicode>", "", "", "debMiss30",
	                                    0, "", "", "", "", "(Intercept)", 65.4570525044341, 65.4570525044341,
	                                    "<unicode>", "", "", "debMiss1:debMiss30", 0, "", 106, -0.90916859496365,
	                                    0.242053164108471, "debMiss1", -0.321216173146344, -0.323989120441333,
	                                    "<unicode>", "Normal(0,10)", 1.01742785017051, "debMiss80",
	                                    0.302010156363592, 97, 19, -9.77368986820213, 3.45206200801801,
	                                    "contGamma", -3.15188237966899, -3.54241555744695, "<unicode>",
	                                    "Normal(0,10)", 1.26514764905017, "debMiss80", 3.44910323884098,
	                                    50, 103, -0.432396369199807, 0.849585058998405, "debMiss30",
	                                    0.195291793058033, 0.221092065278765, "<unicode>", "Normal(0,10)",
	                                    0.999869765767626, "debMiss80", 0.32916579790112, 93, 89, -0.0267064396864598,
	                                    0.0469292285928962, "debMiss1:debMiss30", 0.0103378486394462,
	                                    0.0108707932551303, "<unicode>", "Normal(0,10)", 1.00110198506916,
	                                    "debMiss80", 0.0183903311308238, 88, 145, -0.0171500778847166,
	                                    0.011345167895876, "debMiss1", -0.00179577755050783, -0.00162384601917353,
	                                    "<unicode>", "Normal(0,10)", 1.00714175780694, "contGamma",
	                                    0.00776543996492007, 95))

	table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_totalEffectsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
	                               list(84, -1.55517789047617, 0.457838925829123, 16, "Total", "debMiss1",
	                                    "debMiss80", -0.465911189630798, -0.484707684531113, "<unicode>",
	                                    1.00780237787219, 0.529744634845427, 126, 107, -0.912043694039484,
	                                    0.198742785934624, 50, "Total", "debMiss1", "debMiss80", -0.309675283142848,
	                                    -0.301626865329773, "<unicode>", 1.02310364982784, 0.303592256268402,
	                                    109, 201, -0.66283907752209, 0.25503051706793, 84, "Total",
	                                    "debMiss1", "debMiss80", -0.163012224495024, -0.167926713797487,
	                                    "<unicode>", 1.00324419088241, 0.236302626940476, 117, 167,
	                                    -0.0581640136170782, 0.108158873786955, "", "Total indirect",
	                                    "debMiss1", "debMiss80", 0.00830514337934994, 0.00308370983773346,
	                                    "<unicode>", 1.00023883424941, 0.036523346064018, 60))
})

test_that("Incomplete Hayes configuration works", {
  options <- getOptionsBayesian()
  options$standardizedModelEstimates <- FALSE
  options$processModels <- list(getProcessModel(list()))
  options$processModels[[1]]$inputType <- "inputModelNumber"
  options$processModels[[1]]$modelNumber <- 5
  options$processModels[[1]]$localTests <- TRUE
  options$syntax <- TRUE
  set.seed(1)
  results <- jaspTools::runAnalysis("BayesianProcess", "debug.csv", options)

  plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_conceptPathPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "conceptual-path-plot-incomplete")

	plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_statPathPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "statistical-path-plot-incomplete")
})
