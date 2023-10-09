
context('Unit tests for classic process analysis')

test_that("Test that .procModelIsComplete works", {
  mockModelComplete <- list(
    inputType = "inputVariables",
    processRelationships = list(list(processDependent = "contNormal",
                                     processIndependent = "contGamma", processType = "mediators",
                                     processVariable = "contcor1"))
  )
  mockModelNotComplete <- mockModelComplete
  mockModelNotComplete[["processRelationships"]][[1]][["processVariable"]] <- ""

  mockModelEmpty <- mockModelComplete
  mockModelEmpty[["processRelationships"]] <- list()

  mockModelCompleteDirect <- list(
    inputType = "inputVariables",
    processRelationships = list(list(processDependent = "contNormal",
                                     processIndependent = "contGamma", processType = "directs",
                                     processVariable = ""))
  )

  expect_true(jaspProcess:::.procModelIsComplete(mockModelComplete))
  expect_true(jaspProcess:::.procModelIsComplete(mockModelCompleteDirect))
  expect_false(jaspProcess:::.procModelIsComplete(mockModelNotComplete))
  expect_false(jaspProcess:::.procModelIsComplete(mockModelEmpty))
})

test_that("Test that .procIsReady works", {
  optsEmptyDependent <- list(
    dependent = ""
  )

  optsEmptyCovariatesFactors <- list(
    dependent = "contNormal",
    covariates = list(),
    factors = list()
  )

  optsEmptyModels <- list(
    dependent = "contNormal",
    covariates = list("contGamma"),
    factors = list(),
    processModels = list()
  )

  optsReady <- list(
    dependent = "contNormal",
    covariates = list("contGamma"),
    factors = list(),
    processModels = list(list(
      inputType = "inputVariables",
      processRelationships = list(list(processDependent = "contNormal",
                                       processIndependent = "contGamma", processType = "mediators",
                                       processVariable = "contcor1"))
    ))
  )

  expect_false(jaspProcess:::.procIsReady(optsEmptyDependent))
  expect_false(jaspProcess:::.procIsReady(optsEmptyCovariatesFactors))
  expect_false(jaspProcess:::.procIsReady(optsEmptyModels))
  expect_true(jaspProcess:::.procIsReady(optsReady))
})

test_that("Test that .procProcessRelationshipsToGraph works - mediation", {
  processRelationships <- list(list(processDependent = "contNormal",
                                    processIndependent = "contGamma", processType = "mediators",
                                    processVariable = "contcor1"))

  graph <- jaspProcess:::.procProcessRelationshipsToGraph(processRelationships)

  expect_true(igraph::is_igraph(graph))
  expect_true(igraph::is_dag(graph))
  expect_equal(igraph::V(graph)$name, c("contGamma", "contNormal", "contcor1"))
  expect_equal(igraph::E(graph)$source, c("contGamma", "contGamma", "contcor1"))
  expect_equal(igraph::E(graph)$target, c("contNormal", "contcor1", "contNormal"))
})

test_that("Test that .procProcessRelationshipsToGraph works - moderation", {
  processRelationships <- list(list(processDependent = "contNormal",
                                    processIndependent = "contGamma", processType = "moderators",
                                    processVariable = "contcor1"))

  graph <- jaspProcess:::.procProcessRelationshipsToGraph(processRelationships)

  expect_true(igraph::is_igraph(graph))
  expect_true(igraph::is_dag(graph))
  expect_equal(igraph::V(graph)$name, c("contGamma", "contNormal", "contcor1", "contGamma:contcor1"))
  expect_equal(igraph::E(graph)$source, c("contGamma", "contcor1", "contGamma:contcor1"))
  expect_equal(igraph::E(graph)$target, c("contNormal", "contNormal", "contNormal"))
})

test_that("Test that .procProcessRelationshipsToGraph works - moderated moderation", {
  processRelationships <- list(list(processDependent = "contNormal",
                                    processIndependent = "contGamma", processType = "moderators",
                                    processVariable = "contcor1"),
                               list(processDependent = "contNormal",
                                    processIndependent = "contcor1", processType = "moderators",
                                    processVariable = "contcor2"))

  graph <- jaspProcess:::.procProcessRelationshipsToGraph(processRelationships)

  expect_true(igraph::is_igraph(graph))
  expect_true(igraph::is_dag(graph))
  expect_equal(igraph::V(graph)$name, c("contGamma", "contNormal", "contcor1", "contGamma:contcor1", "contcor2", "contGamma:contcor2", "contGamma__contcor1__contcor2", "contcor1:contcor2"))
  expect_equal(igraph::E(graph)$source, c("contGamma", "contcor1", "contGamma:contcor1", "contcor2", "contGamma:contcor2", "contGamma__contcor1__contcor2", "contcor1:contcor2"))
  expect_equal(igraph::E(graph)$target, c("contNormal", "contNormal", "contNormal", "contNormal", "contNormal", "contNormal", "contNormal"))
})

test_that("Test that .procProcessRelationshipsToGraph works - direct", {
  processRelationships <- list(list(processDependent = "contNormal",
                                    processIndependent = "contGamma", processType = "directs",
                                    processVariable = ""))

  graph <- jaspProcess:::.procProcessRelationshipsToGraph(processRelationships)

  expect_true(igraph::is_igraph(graph))
  expect_true(igraph::is_dag(graph))
  expect_equal(igraph::V(graph)$name, c("contGamma", "contNormal"))
  expect_equal(igraph::E(graph)$source, c("contGamma"))
  expect_equal(igraph::E(graph)$target, c("contNormal"))
})

test_that("Test that .procProcessRelationshipsToGraph works - confounder", {
  processRelationships <- list(list(processDependent = "contNormal",
                                    processIndependent = "contGamma", processType = "confounders",
                                    processVariable = "contcor1"))

  graph <- jaspProcess:::.procProcessRelationshipsToGraph(processRelationships)

  expect_true(igraph::is_igraph(graph))
  expect_true(igraph::is_dag(graph))
  expect_equal(igraph::V(graph)$name, c("contGamma", "contNormal", "contcor1"))
  expect_equal(igraph::E(graph)$source, c("contGamma", "contcor1", "contcor1"))
  expect_equal(igraph::E(graph)$target, c("contNormal", "contGamma", "contNormal"))
})

test_that("Test that .procGraphAddAttributes works", {
  edgeList <- matrix(c("contGamma", "contNormal",
                       "contGamma", "contcor1",
                       "contcor1", "contNormal",
                       "contcor2", "contNormal",
                       "contGamma:contcor2", "contNormal"
                       ), ncol = 2, byrow = TRUE)
  graph <- igraph::graph_from_edgelist(edgeList)
  igraph::E(graph)$source <- edgeList[,1]
  igraph::E(graph)$target <- edgeList[,2]
  graph <- jaspProcess:::.procGraphAddAttributes(graph)

  expect_equal(igraph::V(graph)$isInt, c(FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_equal(igraph::V(graph)$intVars, strsplit(unique(as.vector(t(edgeList))), ":|__"))
  expect_equal(igraph::V(graph)$intLength, c(1, 1, 1, 1, 2))
  expect_equal(igraph::V(graph)$isDep, c(FALSE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(igraph::V(graph)$isExo, c(TRUE, FALSE, FALSE, TRUE, TRUE))
  expect_equal(igraph::V(graph)$isMed, c(FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_equal(igraph::V(graph)$isPartOfInt, c(FALSE, FALSE, FALSE, TRUE, FALSE))
  expect_equal(igraph::V(graph)$isTreat, c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(igraph::E(graph)$isMod, c(TRUE, FALSE, FALSE, TRUE, FALSE))
  expect_equal(igraph::E(graph)$modVars, list("contcor2", NULL, NULL, "contGamma", NULL))
})
