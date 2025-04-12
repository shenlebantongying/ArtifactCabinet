library(tidyverse)

construct_z_table <- function() {
    rowSeq <- seq(0.0, 1.0, 0.1)
    t <- data.frame(z_1st_decimal <- rowSeq)

    for (col in seq(0.00, 0.09, 0.01)) {
        t[as.character(col)] <- modify(rowSeq, \(row) 1 - pnorm(row + col))
    }

    return(t)
}


View(construct_z_table())
