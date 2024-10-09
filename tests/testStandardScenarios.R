library(madrat)
library(mrdrivers)
library(mrremind)
library(dplyr)
library(tidyr)

testStandardScenarios <- function(folderName, path = "."){

  outputFolder <- file.path(path, paste0(format(Sys.time(), "%Y-%m-%d"),
                                                                 "-", folderName))

  allScens <-  tribble(
    ~SSPscen,         ~transportPolScen,        ~isICEban,    ~demScen,
    'SSP2',        'Mix1',                 FALSE,       'default',
    'SSP2',        'Mix2',                 TRUE,        'default',
    'SSP2',        'Mix3',                 TRUE,        'default',
    'SSP2',        'Mix4',                 TRUE,        'default'
  )

  # generate list from data frame rows
  allScens <- split(allScens, seq(nrow(allScens)))


  EdgeTransportSAdata <- lapply(allScens,
                                function(x) {
                                  calcEdgeTransportSA(SSPscen = x[["SSPscen"]],
                                                      transportPolScen = x[["transportPolScen"]],
                                                      isICEban = x[["isICEban"]],
                                                      demScen = x[["demScen"]],
                                                      isTransportReported = TRUE,
                                                      isTransportExtendedReported = TRUE,
                                                      isREMINDinputReported = TRUE,
                                                      isStored = TRUE,
                                                      outputFolder = outputFolder)
                                })
}
