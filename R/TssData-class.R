## class 'TssData' ##
setClass("TssData",
         representation(reads="list",
                        regions="data.frame",
                        annotation="ANY",
                        timestamp="POSIXct")
         )

setValidity("TssData",
            function(object) {
              res <- TRUE
              if(!all.equal(length(object@reads), nrow(object@regions))) {
                res <- "Number of regions inconsistent."
              }
              return(res)
            })
