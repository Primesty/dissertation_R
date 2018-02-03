partialOmegas <- function(mod){
        aovMod <- mod
        if(!any(class(aovMod) %in% 'aov')) aovMod <- aov(mod)
        sumAov     <- summary(aovMod)[[1]]
        residRow   <- nrow(sumAov)
        dfError    <- sumAov[residRow,1]
        msError    <- sumAov[residRow,3]
        nTotal     <- nrow(model.frame(aovMod))
        dfEffects  <- sumAov[1:{residRow-1},1]
        ssEffects  <- sumAov[1:{residRow-1},2]
        msEffects  <- sumAov[1:{residRow-1},3]
        partOmegas <- abs((dfEffects*(msEffects-msError)) /
                                  (ssEffects + (nTotal -dfEffects)*msError))
        names(partOmegas) <- rownames(sumAov)[1:{residRow-1}]
        partOmegas
}