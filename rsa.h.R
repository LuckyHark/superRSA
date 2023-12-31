
# This file is automatically generated, you probably don't want to edit this

rsaOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "rsaOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            dep = NULL,
            groupA = NULL,
            groupB = NULL, ...) {

            super$initialize(
                package="SuperRSA",
                name="rsa",
                requiresData=TRUE,
                ...)

            private$..dep <- jmvcore::OptionVariable$new(
                "dep",
                dep)
            private$..groupA <- jmvcore::OptionVariable$new(
                "groupA",
                groupA)
            private$..groupB <- jmvcore::OptionVariable$new(
                "groupB",
                groupB)

            self$.addOption(private$..dep)
            self$.addOption(private$..groupA)
            self$.addOption(private$..groupB)
        }),
    active = list(
        dep = function() private$..dep$value,
        groupA = function() private$..groupA$value,
        groupB = function() private$..groupB$value),
    private = list(
        ..dep = NA,
        ..groupA = NA,
        ..groupB = NA)
)

rsaResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "rsaResults",
    inherit = jmvcore::Group,
    active = list(
        text = function() private$.items[["text"]],
        tableS = function() private$.items[["tableS"]],
        tableA = function() private$.items[["tableA"]],
        tableB = function() private$.items[["tableB"]],
        plot = function() private$.items[["plot"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Response Surface Analysis")
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="text",
                title="\u54CD\u5E94\u9762\u5206\u6790\u7ED3\u679C"))
            self$add(jmvcore::Table$new(
                options=options,
                name="tableS",
                title="Model Summary",
                rows=5,
                columns=list(
                    list(
                        `name`="var", 
                        `title`="Info", 
                        `type`="text"),
                    list(
                        `name`="fit", 
                        `title`="Comments", 
                        `type`="text"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="tableA",
                title="Polynomial Regression Model",
                rows=6,
                columns=list(
                    list(
                        `name`="var", 
                        `title`="Variables", 
                        `type`="text"),
                    list(
                        `name`="label", 
                        `title`="label", 
                        `type`="text"),
                    list(
                        `name`="est", 
                        `title`="b", 
                        `type`="number"),
                    list(
                        `name`="se", 
                        `title`="SE", 
                        `type`="text"),
                    list(
                        `name`="tvalue", 
                        `title`="t", 
                        `type`="text"),
                    list(
                        `name`="pvalue", 
                        `title`="p", 
                        `type`="text", 
                        `format`="zto,pvalue"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="tableB",
                title="Curvature and Slope",
                rows=5,
                notes=list(
                    `a1a2`="a1,a2 is the slope and curvature of congruence line; a3, a4 is the slope and curvature of incongruence line; a5 suggests the degree of lateral shift."),
                columns=list(
                    list(
                        `name`="var", 
                        `title`="Indicator", 
                        `type`="text"),
                    list(
                        `name`="label", 
                        `title`="label", 
                        `type`="text"),
                    list(
                        `name`="est", 
                        `title`="b", 
                        `type`="number"),
                    list(
                        `name`="se", 
                        `title`="SE", 
                        `type`="text"),
                    list(
                        `name`="tvalue", 
                        `title`="z", 
                        `type`="text"),
                    list(
                        `name`="pvalue", 
                        `title`="p", 
                        `type`="text", 
                        `format`="zto,pvalue"))))
            self$add(jmvcore::Image$new(
                options=options,
                name="plot",
                title="Response Surface Plot",
                width=800,
                height=600,
                renderFun=".plot"))}))

rsaBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "rsaBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "SuperRSA",
                name = "rsa",
                version = c(1,0,0),
                options = options,
                results = rsaResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'auto')
        }))

#' Response Surface Analysis
#'
#' 
#' @param data .
#' @param dep .
#' @param groupA .
#' @param groupB .
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$text} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$tableS} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$tableA} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$tableB} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$plot} \tab \tab \tab \tab \tab an image \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$tableS$asDF}
#'
#' \code{as.data.frame(results$tableS)}
#'
#' @export
rsa <- function(
    data,
    dep,
    groupA,
    groupB) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("rsa requires jmvcore to be installed (restart may be required)")

    if ( ! missing(dep)) dep <- jmvcore::resolveQuo(jmvcore::enquo(dep))
    if ( ! missing(groupA)) groupA <- jmvcore::resolveQuo(jmvcore::enquo(groupA))
    if ( ! missing(groupB)) groupB <- jmvcore::resolveQuo(jmvcore::enquo(groupB))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(dep), dep, NULL),
            `if`( ! missing(groupA), groupA, NULL),
            `if`( ! missing(groupB), groupB, NULL))


    options <- rsaOptions$new(
        dep = dep,
        groupA = groupA,
        groupB = groupB)

    analysis <- rsaClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

