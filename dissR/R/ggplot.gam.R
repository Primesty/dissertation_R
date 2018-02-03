
#' GAM plotting
#'
#' Emoji test function
#' @param model GAM object
#' @param type "conditional" or ?
#' @param res Residuals TRUE/FALSE
#' @keywords GAM plotting
#' @export
#' @examples
#' ggplot.gam(model, type="conditional", res=FALSE)

ggplot.gam <- function(model, type="conditional", res=FALSE) {
        require(visreg)
        require(plyr)
        require(ggplot2)
        source("theme_matt.R")
        plotdata <- visreg(model, type = type, plot = FALSE)
        smooths <- ldply(plotdata, function(part)   
                data.frame(Variable = part$meta$x, 
                           x=part$fit[[part$meta$x]], 
                           smooth=part$fit$visregFit, 
                           lower=part$fit$visregLwr, 
                           upper=part$fit$visregUpr,
                           yvar = part$meta$y))
        residuals <- ldply(plotdata, function(part)
                data.frame(Variable = part$meta$x, 
                           x=part$res[[part$meta$x]], 
                           y=part$res$visregRes))
        if (res)
                ggplot(smooths, aes(x, smooth)) + geom_line(col = "steelblue") +
                geom_line(aes(y=lower), linetype="dashed") +
                geom_line(aes(y=upper), linetype="dashed") +
                geom_point(data = residuals, aes(x, y), size = 1.2, alpha = .3) +
                xlab("Variable values") +
                ylab(paste("Fitted values (", smooths$yvar, ")")) +
                facet_grid(. ~ Variable, scales = "free_x") +
                theme_matt() + 
                theme(strip.background = element_rect(color = "black", fill = "white"),
                                     strip.text = element_text(size = 10, face = "bold"),
                                     strip.text.x = element_text(size = 10),
                                     strip.text.y = element_text(size = 10))
        else
                ggplot(smooths, aes(x, smooth)) + geom_line(col = "steelblue") +
                geom_line(aes(y=lower), linetype="dashed") +
                geom_line(aes(y=upper), linetype="dashed") +
                facet_grid(. ~ Variable, scales = "free_x") +
                xlab("Variable values") +
                ylab(paste("Fitted values (", smooths$yvar, ")")) +
                theme_matt() + 
                theme(strip.background = element_rect(color = "black", fill = "white"),
                                     strip.text = element_text(size = 10, face = "bold"),
                                     strip.text.x = element_text(size = 10),
                                     strip.text.y = element_text(size = 10))
}