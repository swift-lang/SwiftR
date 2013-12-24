require(OpenMx)

    data(demoOneFactor)
    manifests <- names(demoOneFactor)
    latents <- c("G")

    factorModel <- mxModel("One Factor",
        type="RAM",
            manifestVars = manifests,
            latentVars = latents,
            mxPath(from=latents, to=manifests),
            mxPath(from=manifests, arrows=2),
            mxPath(from=latents, arrows=2, free=F, values=1.0),
            mxData(observed=cov(demoOneFactor), type="cov", numObs=500))

    summary(mxRun(factorModel))
