#
# OpenMx Script to demonstrate use of R's boot package for bootstrapping
#
# Author: M.C. Neale 1 September 2009
#

# Load required libraries
require(OpenMx)
require(boot)

bootdemo <- function(swift=FALSE,R=100) {

# Define a function called mles which will return maximum likelihood estimates
# It uses the demoOneFactor dataset and one factor model on the OpenMx homepage
# http://openmx.psyc.virginia.edu


mles<-function(dataset,wt){
require(OpenMx)
        manifests <- names(dataset)
        latents <- c("G")
        covwt <- cov.wt(dataset,wt)
        mlevals <- mxRun(mxModel("One Factor", type="RAM",
            manifestVars = manifests,
            latentVars = latents,
            mxPath(from=latents, to=manifests),
            mxPath(from=manifests, arrows=2),
            mxPath(from=latents, arrows=2,
            free=F, values=1.0),
            mxData(covwt$cov, type="cov",
            numObs=500)))
        return(as.vector(mlevals@output$estimate))}
    
# Run R bootstraps

boottime = system.time(boot(demoOneFactor,mles,R=R,swift=swift))
cat("done booting - system.time is:\n")
print(boottime)

# For comparison, take a look at the SE output from running the homepage job once

data(demoOneFactor)
manifests <- names(demoOneFactor)
latents <- c("G")
factorModel <- mxModel("One Factor", type="RAM",
      manifestVars = manifests,
      latentVars = latents,
      mxPath(from=latents, to=manifests),
      mxPath(from=manifests, arrows=2),
      mxPath(from=latents, arrows=2,
            free=F, values=1.0),
      mxData(cov(demoOneFactor), type="cov",
            numObs=500))
facrun<-mxRun(factorModel)
summary(facrun)

# the estimates and standard errors should match up pretty well, though the number of replicates R above might be increased
# therefore, only the factorModel estimates are compared:

loadings<-facrun@matrices$A@values[1:5,6]
errors<-diag(facrun@matrices$S@values[1:5,1:5])
estimates<-as.vector(c(loadings,errors))
omxCheckCloseEnough(as.vector(c(0.3971525,0.5036615,0.5772418,0.7027743,0.7962506,0.04081422,0.03802001,0.04082720,0.03938708,0.03628711)),estimates,.001)

# The above should indicate that the results are close enough.

} # function bootdemo


