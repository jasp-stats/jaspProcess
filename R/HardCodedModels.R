#
# Copyright (C) 2023 University of Amsterdam and Netherlands eScience Center
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

.HardCodedModels <- function(number, k) {

  # k = number of mediators

  ## TODO: Models involving moderated moderation 19,20,69,71,73

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

  if (number == 3) {
    processRelationships <- list(
      list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "moderators",
        processVariable = "W"
      ),
      list(
        processDependent = "Y",
        processIndependent = "W",
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
    processRelationships <- .generateProcessRelationships_6(k)
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

  if (number == 11) {
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
        processIndependent = "W",
        processType = "moderators",
        processVariable = "Z"
      )
    )
  }

  if (number == 12) {
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
        processIndependent = "W",
        processType = "moderators",
        processVariable = "Z"
      ),
      list(
        processDependent = "Y",
        processIndependent = "W",
        processType = "moderators",
        processVariable = "Z"
      )
    )
  }

  if (number == 13) {
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
        processIndependent = "W",
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

  if (number == 18) {
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
        processIndependent = "W",
        processType = "moderators",
        processVariable = "Z"
      )
    )
  }

  # Enable once multiple moderated moderation is working
  #
  # if (number == 19) {
  #   processRelationships <- list(
  #     list(
  #       processDependent = "Y",
  #       processIndependent = "X",
  #       processType = "mediators",
  #       processVariable = "M"
  #     ),
  #     list(
  #       processDependent = "Y",
  #       processIndependent = "M",
  #       processType = "moderators",
  #       processVariable = "W"
  #     ),
  #     list(
  #       processDependent = "Y",
  #       processIndependent = "X",
  #       processType = "moderators",
  #       processVariable = "W"
  #     ),
  #     list(
  #       processDependent = "Y",
  #       processIndependent = "W",
  #       processType = "moderators",
  #       processVariable = "Z"
  #     )#,
  #      # list(
  #      #   processDependent = "Y",
  #      #   processIndependent = "W",
  #      #   processType = "moderators",
  #      #   processVariable = "Z"
  #      # )
  #   )
  # }
  #
  # if (number == 20) {
  #   processRelationships <- list(
  #     list(
  #       processDependent = "Y",
  #       processIndependent = "X",
  #       processType = "mediators",
  #       processVariable = "M"
  #     ),
  #     list(
  #       processDependent = "Y",
  #       processIndependent = "M",
  #       processType = "moderators",
  #       processVariable = "W"
  #     ),
  #     list(
  #       processDependent = "Y",
  #       processIndependent = "X",
  #       processType = "moderators",
  #       processVariable = "W"
  #     ),
  #     list(
  #       processDependent = "Y",
  #       processIndependent = "W",
  #       processType = "moderators",
  #       processVariable = "Z"
  #     )
  #   )
  # }

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

  if (number == 68) {
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
        processIndependent = "W",
        processType = "moderators",
        processVariable = "Z"
      )
    )
  }

  # Enable once multiple moderated moderation is working
  #
  # if (number == 69) {
  #   processRelationships <- list(
  #     list(
  #       processDependent = "Y",
  #       processIndependent = "X",
  #       processType = "mediators",
  #       processVariable = "M"
  #     ),
  #     list(
  #       processDependent = "M",
  #       processIndependent = "X",
  #       processType = "moderators",
  #       processVariable = "W"
  #     ),
  #     list(
  #       processDependent = "Y",
  #       processIndependent = "M",
  #       processType = "moderators",
  #       processVariable = "W"
  #     ),
  #     list(
  #       processDependent = "Y",
  #       processIndependent = "X",
  #       processType = "moderators",
  #       processVariable = "W"
  #     ),
  #     list(
  #       processDependent = "M",
  #       processIndependent = "W",
  #       processType = "moderators",
  #       processVariable = "Z"
  #     ),
  #     list(
  #       processDependent = "Y",
  #       processIndependent = "W",
  #       processType = "moderators",
  #       processVariable = "Z"
  #     )
  #   )
  # }

  if (number == 70) {
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
        processIndependent = "W",
        processType = "moderators",
        processVariable = "Z"
      )
    )
  }

  if (number == 72) {
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
        processIndependent = "W",
        processType = "moderators",
        processVariable = "Z"
      ),
      list(
        processDependent = "M",
        processIndependent = "W",
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
    processRelationships <- .generateProcessRelationships_80(k)
  }

  if (number == 81) {
    processRelationships <- .generateProcessRelationships_81(k)
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

  return(.procEncodeProcessRelationships(processRelationships))
} # end of function on hardcoded models

.procVarEncoding <- function() {
  # Encoding for dummy variables
  return(list(
    Y = "JaspProcess_Dependent_Encoded",
    X = "JaspProcess_Independent_Encoded",
    W = "JaspProcess_ModeratorW_Encoded",
    Z = "JaspProcess_ModeratorZ_Encoded",
    M = "JaspProcess_Mediator_Encoded"
  ))
}

.procEncodePath <- function(path) {
  # Encode all variables in a path
  return(lapply(path, function(v) {
    if (v %in% c("mediators", "moderators", "confounders", "directs"))
      return(v)
    if (grepl("M", v))
      return(gsub("M", .procVarEncoding()[["M"]], v))
    return(.procVarEncoding()[[v]])
  }))
}

.procEncodeProcessRelationships <- function(processRelationships) {
  # Encode all paths
  return(lapply(processRelationships, .procEncodePath))
}

.procDecodeVarNames <- function(varNames) {
  # Decode a vector of var names
  encoding <- .procVarEncoding()
  for (nm in names(encoding)) {
    varNames <- gsub(encoding[[nm]], nm, varNames)
  }
  return(varNames)
}

# model 6
.generateProcessRelationships_6 <- function(k) {

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
      processDependent = "M2",
      processIndependent = "M1",
      processType = "directs",
      processVariable = ""
    )
  )

  if (k > 2) {
    for (i in 3:k) {
      path <- list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = paste0("M", i)
      )
      processRelationships <- append(processRelationships, list(path))
      
      for (j in 1:(i-1)) {
        path <- list(
          processDependent = paste0("M", i),
          processIndependent = paste0("M", j),
          processType = "directs",
          processVariable = ""
        )
        processRelationships <- append(processRelationships, list(path))
      }
    }
  }

  return(processRelationships)
}

# model 80
.generateProcessRelationships_80 <- function(k) {

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
      processVariable = paste0("M", k)
    ),
    list(
      processDependent = paste0("M", k),
      processIndependent = "M1",
      processType = "directs",
      processVariable = ""
    )
  )

  if (k > 2) {
    for (i in 2:(k-1)) {
      pathY <- list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = paste0("M", i)
      )
      pathMk <- list(
        processDependent = paste0("M", k),
        processIndependent = paste0("M", i),
        processType = "directs",
        processVariable = ""
      )
      processRelationships <- append(processRelationships, list(pathY, pathMk))
    }
  }

  return(processRelationships)
}


# model 81
.generateProcessRelationships_81 <- function(k) {

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
      processDependent = "M2",
      processIndependent = "M1",
      processType = "directs",
      processVariable = ""
    )
  )

  if (k > 2) {
    for (i in 3:k) {
      pathY <- list(
        processDependent = "Y",
        processIndependent = "X",
        processType = "mediators",
        processVariable = paste0("M", i)
      )
      pathMk <- list(
        processDependent = paste0("M", i),
        processIndependent = "M1",
        processType = "directs",
        processVariable = ""
      )
      processRelationships <- append(processRelationships, list(pathY, pathMk))
    }
  }

  return(processRelationships)
}
