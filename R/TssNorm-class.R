## class 'TssNorm' ##
setClass("TssNorm",
         representation(data="list",
                        region="character",
                        chr="factor",
                        parameter="list",
                        date="character")
         )

setValidity("TssNorm",
            function(object) {
              res <- TRUE
              if(!all.equal(length(object@data), length(object@region),
                            length(object@region))) {
                res <- "Number of regions inconsistent."
                return(res)
              }
              return(res)
            })
