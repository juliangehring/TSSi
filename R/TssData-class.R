## class 'TssData' ##
setClass("TssData",
         representation(data="list",
                        region="character",
                        chr="factor",
                        date="character")
         )

setValidity("TssData",
            function(object) {
              res <- TRUE
              if(!all.equal(length(object@data), length(object@region),
                            length(object@region))) {
                res <- "Number of regions inconsistent."
                return(res)
              }
              return(res)
            })
