.HardCodedModels <- function(number) {

  ## TODO: Models involving moderated moderation 3,11,12,13,18,19,20,68,69,70,71,72,73
  ## TODO: Models involving flexible amount of mediators 6,80,81

  if (number == 1) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      )
    )
  }

  if (number == 2) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "Z"
      )
    )
  }

  if (number == 4) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M"
      )
    )
  }

  if (number == 5) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      )
    )
  }

  if (number == 6) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M1"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M2"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M1",
        processType = "mediators",
        processVariable = "M2"
      )
    )
  }

  if (number == 7) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M"
      ),
      list(
        processDependent = "M",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      )
    )
  }

  if (number == 8) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M"
      ),
      list(
        processDependent = "M",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      )
    )
  }

  if (number == 9) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M"
      ),
      list(
        processDependent = "M",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "M",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "Z"
      )
    )
  }

  if (number == 10) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M"
      ),
      list(
        processDependent = "M",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "M",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "Z"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "Z"
      )
    )
  }


  if (number == 14) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M",
        processType = "moderators",
        processVariable = "W"
      )
    )
  }

  if (number == 15) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      )
    )
  }

  if (number == 16) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M",
        processType = "moderators",
        processVariable = "Z"
      )
    )
  }

  if (number == 17) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M",
        processType = "moderators",
        processVariable = "Z"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "Z"
      )
    )
  }

  if (number == 21) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M"
      ),
      list(
        processDependent = "M",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M",
        processType = "moderators",
        processVariable = "Z"
      )
    )
  }

  if (number == 22) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M"
      ),
      list(
        processDependent = "M",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M",
        processType = "moderators",
        processVariable = "Z"
      )
    )
  }

  if (number == 28) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M"
      ),
      list(
        processDependent = "M",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M",
        processType = "moderators",
        processVariable = "Z"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "Z"
      )
    )
  }

  if (number == 29) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M"
      ),
      list(
        processDependent = "M",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M",
        processType = "moderators",
        processVariable = "Z"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "Z"
      )
    )
  }

  if (number == 58) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M"
      ),
      list(
        processDependent = "M",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M",
        processType = "moderators",
        processVariable = "W"
      )
    )
  }

  if (number == 59) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M"
      ),
      list(
        processDependent = "M",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      )
    )
  }

  if (number == 60) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M"
      ),
      list(
        processDependent = "M",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "M",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "Z"
      )
    )
  }

  if (number == 61) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M"
      ),
      list(
        processDependent = "M",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "M",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "Z"
      )
    )
  }

  if (number == 62) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M"
      ),
      list(
        processDependent = "M",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "M",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "Z"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "Z"
      )
    )
  }

  if (number == 63) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M"
      ),
      list(
        processDependent = "M",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "M",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "Z"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "Z"
      )
    )
  }

  if (number == 64) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M"
      ),
      list(
        processDependent = "M",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M",
        processType = "moderators",
        processVariable = "Z"
      )
    )
  }

  if (number == 65) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M"
      ),
      list(
        processDependent = "M",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M",
        processType = "moderators",
        processVariable = "Z"
      )
    )
  }

  if (number == 66) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M"
      ),
      list(
        processDependent = "M",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M",
        processType = "moderators",
        processVariable = "Z"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "Z"
      )
    )
  }

  if (number == 67) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M"
      ),
      list(
        processDependent = "M",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M",
        processType = "moderators",
        processVariable = "Z"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "Z"
      )
    )
  }

  if (number == 75) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M"
      ),
      list(
        processDependent = "M",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "M",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "Z"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M",
        processType = "moderators",
        processVariable = "Z"
      )
    )
  }

  if (number == 76) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M"
      ),
      list(
        processDependent = "M",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "M",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "Z"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M",
        processType = "moderators",
        processVariable = "Z"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "Z"
      )
    )
  }

  if (number == 80) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M1"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "Mk"
      ),
      list(
        processDependent = "Mk",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M1"
      ),
      list(
        processDependent = "Mk",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "Mk-1"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M1",
        processType = "mediators",
        processVariable = "Mk"
      ),
      list(
        processDependent = "Y",
        processIndependent = "Mk-1",
        processType = "mediators",
        processVariable = "Mk"
      )
    )
  }

  if (number == 81) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M1"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "Mk"
      ),
      list(
        processDependent = "M2",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M1"
      ),
      list(
        processDependent = "Mk",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M1"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M1",
        processType = "mediators",
        processVariable = "M2"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M1",
        processType = "mediators",
        processVariable = "Mk"
      )
    )
  }

  if (number == 82) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M1"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M2"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M3"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M4"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M1",
        processType = "mediators",
        processVariable = "M2"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M3",
        processType = "mediators",
        processVariable = "M4"
      )
    )
  }

  if (number == 83) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M1"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M2"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M1",
        processType = "mediators",
        processVariable = "M2"
      ),
      list(
        processDependent = "M1",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      )
    )
  }

  if (number == 84) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M1"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M2"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M1",
        processType = "mediators",
        processVariable = "M2"
      ),
      list(
        processDependent = "M1",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "M2",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      )
    )
  }

  if (number == 85) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M1"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M2"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M1",
        processType = "mediators",
        processVariable = "M2"
      ),
      list(
        processDependent = "M1",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "M2",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      )
    )
  }

  if (number == 86) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M1"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M2"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M1",
        processType = "mediators",
        processVariable = "M2"
      ),
      list(
        processDependent = "M1",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      )
    )
  }

  if (number == 87) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M1"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M2"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M1",
        processType = "mediators",
        processVariable = "M2"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M2",
        processType = "moderators",
        processVariable = "W"
      )
    )
  }

  if (number == 88) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M1"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M2"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M1",
        processType = "mediators",
        processVariable = "M2"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M1",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M2",
        processType = "moderators",
        processVariable = "W"
      )
    )
  }

  if (number == 89) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M1"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M2"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M1",
        processType = "mediators",
        processVariable = "M2"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M1",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M2",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      )
    )
  }

  if (number == 90) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M1"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M2"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M1",
        processType = "mediators",
        processVariable = "M2"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M2",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      )
    )
  }

  if (number == 91) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M1"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M2"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M1",
        processType = "mediators",
        processVariable = "M2"
      ),
      list(
        processDependent = "M2",
        processIndependent = "M1",
        processType = "moderators",
        processVariable = "W"
      )
    )
  }

  if (number == 92) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M1"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = "M2"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M1",
        processType = "mediators",
        processVariable = "M2"
      ),
      list(
        processDependent = "M1",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "M2",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "M2",
        processIndependent = "M1",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M1",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "M2",
        processType = "moderators",
        processVariable = "W"
      )
    )
  }
  return(processRelationships)

} # end of function on hardcoded models
