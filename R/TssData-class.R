## class 'TssData' ##
setClass("TssData",
         representation(reads="list",
                        segments="data.frame",
                        parameters="list",
                        annotation="ANY")
         )

setValidity("TssData",
            function(object) {
              res <- TRUE
              if(!all.equal(length(object@reads), nrow(object@segments))) {
                res <- "Number of segments inconsistent."
              }
              return(res)
            })
