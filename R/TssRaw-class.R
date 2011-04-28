## class 'TssData' ##
setClass("TssRaw",
         representation(data="list",
                        region="character",
                        chr="factor",
                        date="character")
         )

setValidity("TssRaw",
            function(object) {
              res <- TRUE
              if(!all.equal(length(object@data), length(object@region),
                            length(object@region))) {
                res <- "Number of regions inconsistent."
                return(res)
              }
              return(res)
            })
