## class 'TssData' ##
setClass("TssData",
         representation(reads="list",
                        segments="data.frame",
                        annotation="ANY",
                        timestamp="POSIXct")
         )

setValidity("TssData",
            function(object) {
              res <- TRUE
              if(!all.equal(length(object@reads), nrow(object@segments))) {
                res <- "Number of segments inconsistent."
              }
              return(res)
            })
