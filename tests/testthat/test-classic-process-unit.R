# Unit tests for Classic Process analysis

# Helper functions --------------------------------------------------------

create_graph_from_edgeList <- function(edgeList) {
  graph <- igraph::graph_from_edgelist(edgeList)
  igraph::E(graph)$source <- edgeList[,1]
  igraph::E(graph)$target <- edgeList[,2]
  graph <- jaspProcess:::.procGraphAddAttributes(graph)
  return(graph)
}

createDummyGraphModelModeratedMediation <- function() {
  edgeList <- matrix(c("contGamma", "contNormal",
                       "contGamma", "contcor1",
                       "contcor1", "contNormal",
                       "contcor2", "contNormal",
                       "contGamma:contcor2", "contNormal"
  ), ncol = 2, byrow = TRUE)
  return(create_graph_from_edgeList(edgeList))
}

createDummyGraphModelModeratedModeration <- function() {
  edgeList <- matrix(c(
    "contGamma", "contNormal",
    "contcor1", "contNormal",
    "contcor2", "contNormal",
    "contGamma:contcor1", "contNormal",
    "contGamma:contcor2", "contNormal",
    "contcor1:contcor2", "contNormal",
    "contGamma__contcor1__contcor2", "contNormal"
  ), ncol = 2, byrow = TRUE)
  return(create_graph_from_edgeList(edgeList))
}

createDummyGraphModelDoubleModeratedModeration <- function() {
  edgeList <- matrix(c(
    "contGamma", "contNormal",
    "contcor1", "contNormal",
    "contcor2", "contNormal",
    "debCollin1", "contNormal",
    "contGamma:contcor1", "contNormal",
    "contGamma:contcor2", "contNormal",
    "contGamma:debCollin1", "contNormal",
    "contcor1:contcor2", "contNormal",
    "contcor1:debCollin1", "contNormal",
    "contGamma__contcor1__contcor2", "contNormal",
    "contGamma__contcor1__debCollin1", "contNormal"
  ), ncol = 2, byrow = TRUE)
  return(create_graph_from_edgeList(edgeList))
}

createDummyGraphModelTwoModeratedModerators <- function() {
  edgeList <- matrix(c(
    "contGamma", "contNormal",
    "contcor1", "contNormal",
    "contcor2", "contNormal",
    "debCollin1", "contNormal",
    "debCollin2", "contNormal",
    "contGamma:contcor1", "contNormal",
    "contGamma:contcor2", "contNormal",
    "contGamma:debCollin1", "contNormal",
    "contGamma:debCollin2", "contNormal",
    "contcor1:contcor2", "contNormal",
    "debCollin1:debCollin2", "contNormal",
    "contGamma__contcor1__contcor2", "contNormal",
    "contGamma__debCollin1__debCollin2", "contNormal"
  ), ncol = 2, byrow = TRUE)
  return(create_graph_from_edgeList(edgeList))
}

createDummyGraphModelTwoParallelMediators <- function() {
  edgeList <- matrix(c("contGamma", "contNormal",
                       "contGamma", "contcor1",
                       "contGamma", "contcor2",
                       "contcor1", "contNormal",
                       "contcor2", "contNormal"
  ), ncol = 2, byrow = TRUE)
  return(create_graph_from_edgeList(edgeList))
}

createDummyGraphModelTwoSerialMediators <- function(connected = FALSE, sparse = FALSE) {
  edgeList <- matrix(c("contGamma", "contNormal",
                       "contGamma", "contcor1",
                       "contcor1", "contcor2",
                       "contcor2", "contNormal"
  ), ncol = 2, byrow = TRUE)

  if (connected) {
    edgeList <- rbind(edgeList, c("contGamma", "contcor2"))
  }

  if (!sparse) {
    edgeList <- rbind(edgeList, c("contcor1", "contNormal"))
  }

  return(create_graph_from_edgeList(edgeList))
}

createDummyGraphModelThreeParallelMediators <- function() {
  edgeList <- matrix(c("contGamma", "contNormal",
                       "contGamma", "contcor1",
                       "contGamma", "contcor2",
                       "contGamma", "debCollin1",
                       "contcor1", "contNormal",
                       "contcor2", "contNormal",
                       "debCollin1", "contNormal"
  ), ncol = 2, byrow = TRUE)
  return(create_graph_from_edgeList(edgeList))
}

createDummyGraphModelThreeSerialMediators <- function() {
  edgeList <- matrix(c("contGamma", "contNormal",
                       "contGamma", "contcor1",
                       "contcor1", "contcor2",
                       "contcor2", "debCollin1",
                       "contcor1", "contNormal",
                       "contcor2", "contNormal",
                       "debCollin1", "contNormal"
  ), ncol = 2, byrow = TRUE)
  return(create_graph_from_edgeList(edgeList))
}

# Unit tests --------------------------------------------------------------

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
  expect_true(all(!igraph::V(graph)$isNestedInt))
  expect_equal(igraph::V(graph)$isHigherOrderInt, c(FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_equal(igraph::V(graph)$isDep, c(FALSE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(igraph::V(graph)$isExo, c(TRUE, FALSE, FALSE, TRUE, TRUE))
  expect_equal(igraph::V(graph)$isMed, c(FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_equal(igraph::V(graph)$isPartOfInt, c(FALSE, FALSE, FALSE, TRUE, FALSE))
  expect_equal(igraph::V(graph)$isTreat, c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(igraph::E(graph)$isMod, c(TRUE, FALSE, FALSE, TRUE, FALSE))
  expect_equal(igraph::E(graph)$modVars, c("contcor2", NA, NA, "contGamma", NA))
})

test_that("Test that .procGraphAddAttributes works - moderated moderation", {
  edgeList <- matrix(c(
    "contGamma", "contNormal",
    "contcor1", "contNormal",
    "contcor2", "contNormal",
    "contGamma:contcor1", "contNormal",
    "contGamma:contcor2", "contNormal",
    "contcor1:contcor2", "contNormal",
    "contGamma__contcor1__contcor2", "contNormal"
  ), ncol = 2, byrow = TRUE)
  graph <- igraph::graph_from_edgelist(edgeList)
  igraph::E(graph)$source <- edgeList[,1]
  igraph::E(graph)$target <- edgeList[,2]
  graph <- jaspProcess:::.procGraphAddAttributes(graph)
  expect_equal(igraph::V(graph)$isInt, c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE))
  expect_equal(igraph::V(graph)$intVars, strsplit(unique(as.vector(t(edgeList))), ":|__"))
  expect_equal(igraph::V(graph)$intLength, c(1, 1, 1, 1, 2, 2, 2, 3))
  expect_equal(igraph::V(graph)$isNestedInt, c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE))
  expect_equal(igraph::V(graph)$isHigherOrderInt, c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_equal(igraph::E(graph)$modVars, list(c("contcor1", "contcor2"), c("contGamma", "contcor2"), c("contGamma", "contcor1"), as.character(NA), as.character(NA), as.character(NA), as.character(NA)))
})

test_that("Test that .procEncodePath works", {
  pathDecodedW <- list(processDependent = "Y",
                      processIndependent = "X", processType = "moderators",
                      processVariable = "W")

  pathEncodedW <- list(processDependent = "JaspProcess_Dependent_Encoded",
                       processIndependent = "JaspProcess_Independent_Encoded", processType = "moderators",
                       processVariable = "JaspProcess_ModeratorW_Encoded")

  pathDecodedZ <- list(processDependent = "Y",
                       processIndependent = "X", processType = "moderators",
                       processVariable = "Z")

  pathEncodedZ <- list(processDependent = "JaspProcess_Dependent_Encoded",
                       processIndependent = "JaspProcess_Independent_Encoded", processType = "moderators",
                       processVariable = "JaspProcess_ModeratorZ_Encoded")

  pathDecodedM <- list(processDependent = "Y",
                       processIndependent = "X", processType = "mediators",
                       processVariable = "M")

  pathEncodedM <- list(processDependent = "JaspProcess_Dependent_Encoded",
                       processIndependent = "JaspProcess_Independent_Encoded", processType = "mediators",
                       processVariable = "JaspProcess_Mediator_Encoded")

  pathDecodedM1 <- list(processDependent = "Y",
                       processIndependent = "X", processType = "mediators",
                       processVariable = "M1")

  pathEncodedM1 <- list(processDependent = "JaspProcess_Dependent_Encoded",
                       processIndependent = "JaspProcess_Independent_Encoded", processType = "mediators",
                       processVariable = "JaspProcess_Mediator_Encoded1")

  pathDecodedConfounder <- list(processDependent = "Y",
                       processIndependent = "X", processType = "confounders",
                       processVariable = "M")

  pathEncodedConfounder <- list(processDependent = "JaspProcess_Dependent_Encoded",
                       processIndependent = "JaspProcess_Independent_Encoded", processType = "confounders",
                       processVariable = "JaspProcess_Mediator_Encoded")

  pathDecodedDirect <- list(processDependent = "Y",
                       processIndependent = "X", processType = "directs",
                       processVariable = "")

  pathEncodedDirect <-list(processDependent = "JaspProcess_Dependent_Encoded",
                           processIndependent = "JaspProcess_Independent_Encoded", processType = "directs",
                           processVariable = "")

  expect_equal(jaspProcess:::.procEncodePath(pathDecodedW), pathEncodedW)
  expect_equal(jaspProcess:::.procEncodePath(pathDecodedZ), pathEncodedZ)
  expect_equal(jaspProcess:::.procEncodePath(pathDecodedM), pathEncodedM)
  expect_equal(jaspProcess:::.procEncodePath(pathDecodedM1), pathEncodedM1)
  expect_equal(jaspProcess:::.procEncodePath(pathDecodedConfounder), pathEncodedConfounder)
  expect_equal(jaspProcess:::.procEncodePath(pathDecodedDirect), pathEncodedDirect)
})

test_that("Test that .procEncodeProcessRelationships works", {
  processRelationshipsDecoded <- list(list(processDependent = "Y",
                                           processIndependent = "X", processType = "mediators",
                                           processVariable = "M"),
                                      list(processDependent = "Y",
                                           processIndependent = "X", processType = "moderators",
                                           processVariable = "W"))

  processRelationshipsEncoded <- list(list(processDependent = "JaspProcess_Dependent_Encoded",
                                           processIndependent = "JaspProcess_Independent_Encoded", processType = "mediators",
                                           processVariable = "JaspProcess_Mediator_Encoded"),
                                      list(processDependent = "JaspProcess_Dependent_Encoded",
                                           processIndependent = "JaspProcess_Independent_Encoded", processType = "moderators",
                                           processVariable = "JaspProcess_ModeratorW_Encoded"))

  expect_equal(
    jaspProcess:::.procEncodeProcessRelationships(processRelationshipsDecoded),
    processRelationshipsEncoded
  )
})

test_that("Test that .procReplaceDummyVars works", {
  varsOneMed <- c("JaspProcess_Independent_Encoded", "JaspProcess_Dependent_Encoded",
            "JaspProcess_Mediator_Encoded",
            "JaspProcess_ModeratorW_Encoded", "JaspProcess_ModeratorZ_Encoded")

  varsTwoMed <- c("JaspProcess_Independent_Encoded", "JaspProcess_Dependent_Encoded",
                  "JaspProcess_Mediator_Encoded1", "JaspProcess_Mediator_Encoded2",
                  "JaspProcess_ModeratorW_Encoded", "JaspProcess_ModeratorZ_Encoded")

  modelOptions <-  list(
    modelNumberIndependent = "contGamma",
    modelNumberMediators = list("contcor1", "contcor2"),
    modelNumberModeratorW = "debCollin1",
    modelNumberModeratorZ = "debCollin2"
  )

  globalDependent <- "contNormal"

  expect_equal(
    jaspProcess:::.procReplaceDummyVars(varsOneMed, modelOptions, globalDependent),
    c("contGamma", "contNormal", "contcor1", "debCollin1", "debCollin2")
  )
  expect_equal(
    jaspProcess:::.procReplaceDummyVars(varsTwoMed, modelOptions, globalDependent),
    c("contGamma", "contNormal", "contcor1", "contcor2", "debCollin1", "debCollin2")
  )
})

test_that("Test that .procModelGraphInputModelNumber works", {
  edgeList <- matrix(c("JaspProcess_Independent_Encoded", "JaspProcess_Dependent_Encoded",
                       "JaspProcess_Independent_Encoded", "JaspProcess_Mediator_Encoded",
                       "JaspProcess_Mediator_Encoded", "JaspProcess_Dependent_Encoded",
                       "JaspProcess_ModeratorW_Encoded", "JaspProcess_Dependent_Encoded",
                       "JaspProcess_Independent_Encoded:JaspProcess_ModeratorW_Encoded", "JaspProcess_Dependent_Encoded"
  ), ncol = 2, byrow = TRUE)
  graph <- igraph::graph_from_edgelist(edgeList)
  igraph::E(graph)$source <- edgeList[,1]
  igraph::E(graph)$target <- edgeList[,2]

  modelOptions <-  list(
    modelNumberIndependent = "contGamma",
    modelNumberMediators = list("contcor1"),
    modelNumberModeratorW = "contcor2",
    modelNumberModeratorZ = ""
  )

  globalDependent <- "contNormal"
  graph <- jaspProcess:::.procGraphAddAttributes(graph)
  graph <- jaspProcess:::.procModelGraphInputModelNumber(graph, modelOptions, globalDependent)
  expect_equal(igraph::V(graph)$name, c("contGamma", "contNormal", "contcor1", "contcor2", "contGamma:contcor2"))
})

test_that("Test that .procGraphAddParNamesSingleModel works", {
  graph <- createDummyGraphModelModeratedMediation()
  graph <- jaspProcess:::.procGraphAddParNamesSingleModel(graph)
  expect_equal(igraph::E(graph)$parSymbol, c("c", "a", "b", "c", "c"))
  expect_equal(igraph::E(graph)$parIndex, c(1, 1, 1, 2, 3))
  expect_equal(igraph::E(graph)$parName, c("c1", "a1", "b1", "c2", "c3"))
})

test_that("Test that .procCombVars works", {
  graph <- igraph::make_empty_graph()

  vars <- c("contGamma", "contNormal")

  graph <- jaspProcess:::.procCombVars(graph, vars)

  expect_equal(igraph::V(graph)$name, vars)
  expect_equal(igraph::E(graph)$source, vars[c(1, 1, 2)])
  expect_equal(igraph::E(graph)$target, vars[c(2, 1, 2)])
})

test_that("Test that .procResCovGraphSingleModel works", {
  graph <- createDummyGraphModelModeratedMediation()
  opts <- list(independentCovariances = TRUE, mediatorCovariances = TRUE)

  resCovGraph <- jaspProcess:::.procResCovGraphSingleModel(graph, opts)

  expect_contains(igraph::V(resCovGraph)$name, igraph::V(graph)$name[-length(graph)])
  expect_contains(igraph::E(resCovGraph)$source, igraph::V(graph)$name[-length(graph)][c(1, 1, 2:4)])
  expect_contains(igraph::E(resCovGraph)$target, igraph::V(graph)$name[-length(graph)][c(2, 1, 2:4)])
})

test_that("Test that .procRegSyntax works", {
  graph <- createDummyGraphModelModeratedMediation()
  graph <- jaspProcess:::.procGraphAddParNamesSingleModel(graph)
  syntax <- jaspProcess:::.procRegSyntax(graph)
  expect_equal(syntax, "contNormal ~ c1*contGamma + b1*contcor1 + c2*contcor2 + c3*contGamma:contcor2\ncontcor1 ~ a1*contGamma")
})

test_that("Test that .procMedEffectsSyntaxModPars works - no contrasts", {
  graph <- createDummyGraphModelModeratedMediation()
  graph <- jaspProcess:::.procGraphAddParNamesSingleModel(graph)
  modProbes <- list(contcor2 = c("2.5%" = 0.1, "50%" = 0.5, "97.5%" = 0.9))

  pathEdge <- igraph::E(graph)["contGamma" %--% "contNormal"]
  sourceNode <- "contGamma"

  modPars <- jaspProcess:::.procMedEffectsSyntaxModPars(pathEdge, sourceNode, list(), graph, modProbes)
  expect_equal(modPars, c("c3*0.1", "c3*0.5", "c3*0.9"))
})

test_that("Test that .procMedEffectsSyntaxModPars works - with contrasts", {
  edgeList <- matrix(c("contGammaA", "contNormal",
                       "contGammaB", "contNormal",
                       "contGammaA", "contcor1",
                       "contGammaB", "contcor1",
                       "contcor1", "contNormal",
                       "contcor2", "contNormal",
                       "contGammaA:contcor2", "contNormal",
                       "contGammaB:contcor2", "contNormal"
  ), ncol = 2, byrow = TRUE)
  graph <- create_graph_from_edgeList(edgeList)
  graph <- jaspProcess:::.procGraphAddParNamesSingleModel(graph)
  modProbes <- list(contcor2 = c("2.5%" = 0.1, "50%" = 0.5, "97.5%" = 0.9))
  contrasts <- list(contGamma = matrix(c(0, 1, 0, 0, 0, 1), 3, 2, dimnames = list(NULL, c("A", "B"))))
  contrFacVars <- jaspProcess:::.procContrFacVars(contrasts)
  pathEdge <- igraph::E(graph)["contGammaA" %--% "contNormal"]
  sourceNode <- "contGammaA"

  modPars <- jaspProcess:::.procMedEffectsSyntaxModPars(pathEdge, sourceNode, contrFacVars, graph, modProbes)
  expect_equal(modPars, c("c4*0.1", "c4*0.5", "c4*0.9"))
})

test_that("Test that .procMedEffectsSyntaxGetLhs works - no contrasts", {
  graph <- createDummyGraphModelModeratedMediation()
  graph <- jaspProcess:::.procGraphAddParNamesSingleModel(graph)
  modProbes <- list(contcor2 = c("2.5%" = 0.1, "50%" = 0.5, "97.5%" = 0.9))
  contrasts <- list()
  path <- c(contGamma = 1, contNormal = 2)
  lhs <- jaspProcess:::.procMedEffectsSyntaxGetLhs(path, graph, modProbes, contrasts)
  expect_equal(lhs, c("contGamma__contNormal.contcor2__2.5", "contGamma__contNormal.contcor2__50", "contGamma__contNormal.contcor2__97.5"))
})

test_that("Test that .procMedEffectsSyntaxGetLhs works - with contrasts", {
  edgeList <- matrix(c("contGammaA", "contNormal",
                       "contGammaB", "contNormal",
                       "contGammaA", "contcor1",
                       "contGammaB", "contcor1",
                       "contcor1", "contNormal",
                       "contcor2", "contNormal",
                       "contGammaA:contcor2", "contNormal",
                       "contGammaB:contcor2", "contNormal"
  ), ncol = 2, byrow = TRUE)
  graph <- create_graph_from_edgeList(edgeList)
  graph <- jaspProcess:::.procGraphAddParNamesSingleModel(graph)
  modProbes <- list(contcor2 = c("2.5%" = 0.1, "50%" = 0.5, "97.5%" = 0.9))
  contrasts <- list(contGamma = matrix(c(0, 1, 0, 0, 0, 1), 3, 2, dimnames = list(NULL, c("A", "B"))))
  path <- c(contGammaA = 1, contNormal = 2)
  lhs <- jaspProcess:::.procMedEffectsSyntaxGetLhs(path, graph, modProbes, contrasts)
  expect_equal(lhs, c("contGamma__contNormal.contGamma__A.contcor2__2.5",
                      "contGamma__contNormal.contGamma__B.contcor2__2.5",
                      "contGamma__contNormal.contGamma__A.contcor2__50",
                      "contGamma__contNormal.contGamma__B.contcor2__50",
                      "contGamma__contNormal.contGamma__A.contcor2__97.5",
                      "contGamma__contNormal.contGamma__B.contcor2__97.5"))
})

test_that("Test that .procMedEffectsSyntaxGetRhs works - no contrasts", {
  graph <- createDummyGraphModelModeratedMediation()
  graph <- jaspProcess:::.procGraphAddParNamesSingleModel(graph)
  modProbes <- list(contcor2 = c("2.5%" = 0.1, "50%" = 0.5, "97.5%" = 0.9))
  contrasts <- list()
  path <- c(contGamma = 1, contNormal = 2)
  rhs <- jaspProcess:::.procMedEffectsSyntaxGetRhs(path, graph, modProbes, contrasts)
  expect_equal(rhs, c("(c1 + c3*0.1)", "(c1 + c3*0.5)", "(c1 + c3*0.9)"))
})

test_that("Test that .procMedEffectsSyntaxGetRhs works - with contrasts", {
  edgeList <- matrix(c("contGammaA", "contNormal",
                       "contGammaB", "contNormal",
                       "contGammaA", "contcor1",
                       "contGammaB", "contcor1",
                       "contcor1", "contNormal",
                       "contcor2", "contNormal",
                       "contGammaA:contcor2", "contNormal",
                       "contGammaB:contcor2", "contNormal"
  ), ncol = 2, byrow = TRUE)
  graph <- create_graph_from_edgeList(edgeList)
  graph <- jaspProcess:::.procGraphAddParNamesSingleModel(graph)
  modProbes <- list(contcor2 = c("2.5%" = 0.1, "50%" = 0.5, "97.5%" = 0.9))
  contrasts <- list(contGamma = matrix(c(0, 1, 0, 0, 0, 1), 3, 2, dimnames = list(NULL, c("A", "B"))))
  path <- c(contGammaA = 1, contNormal = 2)
  rhs <- jaspProcess:::.procMedEffectsSyntaxGetRhs(path, graph, modProbes, contrasts)
  expect_equal(rhs, c("(c1 + c4*0.1)", "(c2 + c5*0.1)", "(c1 + c4*0.5)",
                      "(c2 + c5*0.5)", "(c1 + c4*0.9)", "(c2 + c5*0.9)"))
})

test_that("Test that .procMedEffectsSyntax works", {
  graph <- createDummyGraphModelModeratedMediation()
  graph <- jaspProcess:::.procGraphAddParNamesSingleModel(graph)
  modProbes <- list(contcor2 = c("2.5%" = 0.1, "50%" = 0.5, "97.5%" = 0.9))
  contrasts <- list()

  syntax <- jaspProcess:::.procMedEffectsSyntax(graph, modProbes, contrasts)
  expect_equal(syntax, "contGamma__contNormal.contcor2__2.5 := (c1 + c3*0.1)\ncontGamma__contNormal.contcor2__50 := (c1 + c3*0.5)\ncontGamma__contNormal.contcor2__97.5 := (c1 + c3*0.9)\ncontGamma__contcor1__contNormal := a1*b1\ntot.contGamma__contNormal.contcor2__2.5 := (c1 + c3*0.1) + a1*b1\ntot.contGamma__contNormal.contcor2__50 := (c1 + c3*0.5) + a1*b1\ntot.contGamma__contNormal.contcor2__97.5 := (c1 + c3*0.9) + a1*b1\ntotInd.contGamma__contNormal := a1*b1")
})

test_that("Test that .procResCovSyntax works", {
  graph <- createDummyGraphModelModeratedMediation()

  syntax <- jaspProcess:::.procResCovSyntax(graph)

  expect_equal(syntax, "contGamma ~~ contNormal\ncontGamma ~~ contcor1\ncontcor1 ~~ contNormal\ncontcor2 ~~ contNormal\ncontGamma:contcor2 ~~ contNormal")
})

test_that("Test that .procCheckFitModel works", {
  graphFit <- createDummyGraphModelModeratedMediation()
  graphNotFit <- createDummyGraphModelModeratedMediation()
  igraph::E(graphNotFit)$source[1] <- jaspProcess:::.procVarEncoding()[["X"]]

  expect_true(jaspProcess:::.procCheckFitModel(graphFit))
  expect_false(jaspProcess:::.procCheckFitModel(graphNotFit))
})

test_that("Test that .procIsModelNumberGraph works", {
  graph <- createDummyGraphModelModeratedMediation()
  modelNumberTrue <- 5
  modelNumberFalse <- 6
  modelOptions <-  list(
    modelNumberIndependent = "contGamma",
    modelNumberMediators = list("contcor1"),
    modelNumberModeratorW = "contcor2",
    modelNumberModeratorZ = ""
  )
  globalDependent <- "contNormal"

  expect_true(jaspProcess:::.procIsModelNumberGraph(modelNumberTrue, graph, modelOptions, globalDependent))
  expect_false(jaspProcess:::.procIsModelNumberGraph(modelNumberFalse, graph, modelOptions, globalDependent))
})

test_that("Test that .procRecognizeModelNumber works", {
  graph_5_14 <- createDummyGraphModelModeratedMediation()
  graph_3 <- createDummyGraphModelModeratedModeration()

  expect_equal(jaspProcess:::.procRecognizeModelNumber(graph_5_14), c(5, 14))
  expect_equal(jaspProcess:::.procRecognizeModelNumber(graph_3), 3)
})

test_that("Test that .procEffectsTablesGetConditionalLabels works", {
  paths <- list(
    list("contGamma", c("contcor1", "25"), c("contcor2", "25")),
    list("contGamma", c("contcor1", "50"), c("contcor2", "50"))
  )

  mods <- c("contcor1", "contcor2")

  labels <- list(contcor1 = c("25", "50"), contcor2 = c("25", "50"))

  expect_equal(jaspProcess:::.procEffectsTablesGetConditionalLabels(paths, mods), labels)
})

test_that("Test that .procMainGraphLayoutPosHelper works", {
  expect_equal(jaspProcess:::.procMainGraphLayoutPosHelper(1), 1)
  expect_equal(jaspProcess:::.procMainGraphLayoutPosHelper(1:2), c(1, -1))
  expect_equal(jaspProcess:::.procMainGraphLayoutPosHelper(1:3), c(2, 1, -1))
})

test_that("Test that .procMainGraphLayout works", {
  graph <- createDummyGraphModelModeratedMediation()

  graphWithLayout <- jaspProcess:::.procMainGraphLayout(graph)

  expect_equal(igraph::V(graphWithLayout)$posX, rep(c(0, 1, NA), c(1, 1, 3)))
  expect_equal(igraph::V(graphWithLayout)$posY, rep(c(0, NA), c(2, 3)))
})

test_that("Test that .procMedGraphLayout works - Simple mediation", {
  graph <- createDummyGraphModelModeratedMediation()

  graphWithLayout <- jaspProcess:::.procMedGraphLayout(graph)

  expect_equal(igraph::V(graphWithLayout)$posX, rep(c(0, 2, 1, NA), c(1, 1, 1, 2)))
  expect_equal(igraph::V(graphWithLayout)$posY, rep(c(0, 1, NA), c(2, 1, 2)))
})

test_that("Test that .procMedGraphLayout works - Two parallel mediators", {
  graph <- createDummyGraphModelTwoParallelMediators()

  graphWithLayout <- jaspProcess:::.procMedGraphLayout(graph)

  expect_equal(igraph::V(graphWithLayout)$posX, rep(c(0, 2, 1), c(1, 1, 2)))
  expect_equal(igraph::V(graphWithLayout)$posY, rep(c(0, 1, -1), c(2, 1, 1)))
})

test_that("Test that .procMedGraphLayout works - Three parallel mediators", {
  graph <- createDummyGraphModelThreeParallelMediators()

  graphWithLayout <- jaspProcess:::.procMedGraphLayout(graph)

  expect_equal(igraph::V(graphWithLayout)$posX, rep(c(0, 2, 1), c(1, 1, 3)))
  expect_equal(igraph::V(graphWithLayout)$posY, rep(c(0, 2, 1, -1), c(2, 1, 1, 1)))
})

test_that("Test that .procMedGraphLayout works - Two serial mediators", {
  graph <- createDummyGraphModelTwoSerialMediators()

  graphWithLayout <- jaspProcess:::.procMedGraphLayout(graph)

  expect_equal(igraph::V(graphWithLayout)$posX, c(0, 3, 1, 2))
  expect_equal(igraph::V(graphWithLayout)$posY, rep(c(0, -1, 1), c(2, 1, 1)))
})

test_that("Test that .procMedGraphLayout works - Two fully connected serial mediators", {
  graph <- createDummyGraphModelTwoSerialMediators(connected = TRUE)

  graphWithLayout <- jaspProcess:::.procMedGraphLayout(graph)

  expect_equal(igraph::V(graphWithLayout)$posX, c(0, 3, 1, 2))
  expect_equal(igraph::V(graphWithLayout)$posY, rep(c(0, -1), c(2, 2)))
})

test_that("Test that .procMedGraphLayout works - Two sparsely connected serial mediators", {
  graph <- createDummyGraphModelTwoSerialMediators(sparse = TRUE)

  graphWithLayout <- jaspProcess:::.procMedGraphLayout(graph)

  expect_equal(igraph::V(graphWithLayout)$posX, c(0, 3, 1, 2))
  expect_equal(igraph::V(graphWithLayout)$posY, rep(c(0, 1), c(2, 2)))
})

test_that("Test that .procMedGraphLayout works - Three serial mediators", {
  graph <- createDummyGraphModelThreeSerialMediators()

  graphWithLayout <- jaspProcess:::.procMedGraphLayout(graph)

  expect_equal(igraph::V(graphWithLayout)$posX, c(0, 4, 1, 2, 3))
  expect_equal(igraph::V(graphWithLayout)$posY, rep(c(0, -1, 1, 2), c(2, 1, 1, 1)))
})

test_that("Test that .procGraphLayoutConceptual works - Moderated mediation", {
  graph <-  createDummyGraphModelModeratedMediation()

  graphWithLayout <- jaspProcess:::.procGraphLayoutConceptual(graph)

  expect_equal(igraph::V(graphWithLayout)$posX, c(0, 2, 1, 1, NA, 1))
  expect_equal(igraph::V(graphWithLayout)$posY, c(0, 0, 1, -1, NA, 0))
})

test_that("Test that .procGraphLayoutConceptual works - Moderated moderation", {
  graph <-  createDummyGraphModelModeratedModeration()

  graphWithLayout <- jaspProcess:::.procGraphLayoutConceptual(graph)

  expect_equal(igraph::V(graphWithLayout)$posX, rep(c(0, 1, 0.5, 1.5, NA, 0.5), c(1, 1, 1, 1, 4, 2)))
  expect_equal(igraph::V(graphWithLayout)$posY, rep(c(0, 1, 0.5, NA, 0, 0.5), c(2, 1, 1, 4, 1, 1)))
})

test_that("Test that .procGraphLayoutConceptual works - Double moderated moderation", succeed(
  message = "Needs to be implemented"
))

test_that("Test that .procGraphLayoutConceptual works - Two moderated moderators", {
  graph <-  createDummyGraphModelTwoModeratedModerators()

  graphWithLayout <- jaspProcess:::.procGraphLayoutConceptual(graph)

  expect_equal(igraph::V(graphWithLayout)$posX, rep(c(0, 1, 0.5, 1.5, 0.5, 1.5, NA, 0.5), c(1, 1, 1, 1, 1, 1, 8, 4)))
  expect_equal(igraph::V(graphWithLayout)$posY, rep(c(0, 1, 0.5, -1, -0.5, NA, 0, 0.5, 0, -0.5), c(2, 1, 1, 1, 1, 8, 1, 1, 1, 1)))
})

test_that("Test that .procGraphLayoutStatistical works - Moderated mediation", {
  graph <-  createDummyGraphModelModeratedMediation()

  graphWithLayout <- jaspProcess:::.procGraphLayoutStatistical(graph)

  expect_equal(igraph::V(graphWithLayout)$posX, c(0, 2, 1, 1, 1))
  expect_equal(igraph::V(graphWithLayout)$posY, c(0, 0, 1, 2, 3))
})

test_that("Test that .procGraphLayoutStatistical works - Moderated moderation", {
  graph <-  createDummyGraphModelModeratedModeration()

  graphWithLayout <- jaspProcess:::.procGraphLayoutStatistical(graph)

  expect_equal(igraph::V(graphWithLayout)$posX, rep(c(0, 1, 0.5), c(1, 1, 6)))
  expect_equal(igraph::V(graphWithLayout)$posY, c(0, 0:6))
})

test_that("Test that .procGraphLayoutStatistical works - Double moderated moderation", succeed(
  message = "Needs to be implemented"
))

test_that("Test that .procGraphLayoutStatistical works - Two moderated moderators", {
  graph <-  createDummyGraphModelTwoModeratedModerators()

  graphWithLayout <- jaspProcess:::.procGraphLayoutStatistical(graph)

  expect_equal(igraph::V(graphWithLayout)$posX, rep(c(0, 1, 0.5), c(1, 1, 12)))
  expect_equal(igraph::V(graphWithLayout)$posY, c(0, 0 , 1, 2, -1, -2, 3, 4, -3, -4, 5, -5, 6, -6))
})
