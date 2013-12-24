#!/bin/env Rscript
#   Copyright 2007-2010 The OpenMx Project
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
# 
#        http://www.apache.org/licenses/LICENSE-2.0
# 
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

# This file is meant to be opened using the source command from an R instance
#   with working directory of either
#  a) the root of the R library directory in which is 
#  b) the root of the SwiftR source trunk
#  ie.  
# > source("Swift/tests/OpenMx/BootstrapParallelBigger.R")
#
# You can then make a custom bootstrap performance testing suite
# using makeBootstrapTestGroup and makeTestSuite, and run it
# using swiftR's test suite mechanism.

require(OpenMx)
require(Swift)
source("Swift/tests/perf/perfutils.R")



# This is the main test function, which can be
# run with varying combinations of the three
# parameters.
bootstrapTest <- function (nVar, nObs, nReps) {
    set.seed(10)
    startTime = proc.time()[["elapsed"]]
    topModel <- buildModels(nVar, nObs, nReps)
    endTime = proc.time()[["elapsed"]]
    cat(paste("Took", endTime - startTime, "s to build models\n"))

    modelResults <- mxRun(topModel, silent=TRUE, suppressWarnings=TRUE)

    print(modelResults@output$wallTime)
    # Only return some statistics about the model: the returned model data
    # can be huge in size otherwise
    return (modelResults@output)
}


# This function sets up the OpenMx models to 
# use for the test
buildModels <- function (nVar, nObs, nReps) {
    # parameters for the simulation: lambda = factor loadings,
    # specifics = specific variances
    goodStartValues <- TRUE
    if (!is.logical(goodStartValues)) {
      stop("'goodStartValues' should be logical. Try again.")
    }

    lambda <- matrix(1:nVar*2/nVar, nVar, 1)
    specifics <- diag(nVar)
    chl <- chol(lambda %*% t(lambda) + specifics)

    # indices for parameters and hessian estimate in results
    pStrt <- 3
    pEnd <- pStrt + 2*nVar - 1
    hStrt <- pEnd + 1
    hEnd <- hStrt + 2*nVar - 1

    # dimension names for OpenMx
    dn <- list()
    dn[[1]] <- paste("Var", 1:nVar, sep="")
    dn[[2]] <- dn[[1]]

    # function to get a covariance matrix
    randomCov <- function(nObs, nVar, chl, dn) {
      x <- matrix(rnorm(nObs*nVar), nObs, nVar)
      x <- x %*% chl
      thisCov <- cov(x)
      dimnames(thisCov) <- dn
      return(thisCov)  
    }

    createNewModel <- function(index, prefix, model) {
            modelname <- paste(prefix, index, sep='')
            model@data@observed <- randomCov(nObs, nVar, chl, dn)
            model@name <- modelname
            return(model)
    }

    getStats <- function(model) {
            retval <- c(model@output$status[[1]],
                    max(abs(model@output$gradient)),
                    model@output$estimate,
                    sqrt(diag(solve(model@output$hessian))))
            return(retval)
    }


    # initialize obsCov for MxModel
    obsCov <- randomCov(nObs, nVar, chl, dn)

    # results matrix: get results for each simulation
#    results <- matrix(0, nReps, hEnd)
#    dnr <- c("inform", "maxAbsG", paste("lambda", 1:nVar, sep=""),
#             paste("specifics", 1:nVar, sep=""),
#             paste("hessLambda", 1:nVar, sep=""),
#             paste("hessSpecifics", 1:nVar, sep=""))
#    dimnames(results)[[2]] <- dnr

    # instantiate MxModel
    template <- mxModel(name="stErrSim",
                           mxMatrix(name="lambda", type="Full", nrow=nVar, ncol=1,
                                    free=TRUE, values=(1:nVar*2/nVar)*goodStartValues),
                           mxMatrix(name="specifics", type="Diag", nrow=nVar,
                                    free=TRUE, values=rep(1, nVar)),
                           mxAlgebra(lambda %*% t(lambda) + specifics,
                                     name="preCov", dimnames=dn),
                           mxData(observed=obsCov, type="cov", numObs=nObs),
                           mxMLObjective(covariance='preCov'),
                           independent = TRUE)

    topModel <- mxModel(name = 'container')

    # Don't need to export as local environment will automatically
    # be sent
    #swiftExportAll()
    if (FALSE) {
        oldSetting <- getOption("swift.callsperbatch")
        options(swift.callsperbatch=128)
        submodels <- omxLapply(1:nReps, createNewModel, 'stErrSim', template)
        options(swift.callsperbatch=oldSetting)
    }
    else {
        submodels <- lapply(1:nReps, createNewModel, 'stErrSim', template)
    }

    cat(length(submodels), "submodels created\n")
#    names(submodels) <- imxExtractNames(submodels)
    names(submodels) <- imxExtractNames(submodels)
    topModel@submodels <- submodels

    return (topModel)
}



# Construct a performance test group which contains bootstrap instances with 
# a range of different parameter values.  Additional arguments are
# passed to swiftInit or sfInit as appropriate
makeBootstrapTestGroup <- function (argsList, ...) {
    makePerfTestGroup(
            name="BootstrapParallelBigger",
            f=bootstrapTest,
            allargs=argsList,
            ...  )
}



testArgs <- list(
                list(75, 100, 4),
                list(75, 100, 8),
                list(75, 100, 16),
                list(75, 100, 32),
                list(75, 100, 64),
                list(75, 100, 128),
                list(75, 100, 256),
                list(75, 100, 512),
                list(75, 100, 786),
                list(75, 100, 1024),
                list(75, 100, 1024 + 512),
                list(75, 100, 2048 )
                )

testArgsShort <- list(
                list(25, 100, 4),
                list(25, 100, 8),
                list(25, 100, 16),
                list(25, 100, 32),
                list(25, 100, 64),
                list(25, 100, 128),
                list(25, 100, 256),
                list(25, 100, 512),
                list(25, 100, 786),
                list(25, 100, 1024),
                list(25, 100, 1024 + 512),
                list(25, 100, 2048 )
                )



# An example test suite which runs on a ssh cluster
# on multiple cores of a machine and in vanilla R
sampleTestSuite <- Swift:::makeTestSuite(
    groups=list(
       # 16 cores
       makeBootstrapTestGroup(mode="swift", server="ssh", cores=2, 
            hosts=c("nettle", "wapato", "dandelion", "cattail", 
                "chicory", "echinacea", "amaranth", "black-cohosh"),
            argsList=testArgs[3:5]),
       # 8 cores
       makeBootstrapTestGroup(mode="swift", server="ssh", cores=2, 
            hosts="nettle wapato dandelion cattail", 
            argsList=testArgs[2:5]),
       makeBootstrapTestGroup(mode="swift", server="local", cores=4,
            argsList=testArgs[1:4]), 
       makeBootstrapTestGroup(mode="swift", server="local", cores=2,
            argsList=testArgs[1:4]), 
       makeBootstrapTestGroup(mode="swift", server="local", cores=1,
            argsList=testArgs[1:2]), 
       makeBootstrapTestGroup(mode="swift", server="local", cores=1,
            argsList=testArgs[1:2]), 
       makeBootstrapTestGroup(mode="serial", argsList=testArgs[1:2])
       ))

