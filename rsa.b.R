
# This file is a generated template, your changes will not be overwritten

rsaClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "rsaClass",
    inherit = rsaBase,
    private = list(
        .run = function() {

            formula <- paste(self$options$dep, '~', self$options$groupA, '*',self$options$groupB)
            formula <- as.formula(formula)

            results <- RSA::RSA(formula, self$data)
            #self$results$text$setContent(paste("\n","Formula Model: ",results$formula))
            ind<-lavaan::parameterEstimates(results$models[["full"]])
            mod<-lavaan::summary(results$models$full)

            #set the tableA coefficients
            self$results$tableA$setRow(rowNo=1,value=list(
                var="intercept",
                label="b0",
                est=results$LM$coefficients[1,1],
                se =results$LM$coefficients[1,2],
                tvalue=results$LM$coefficients[1,3],
                pvalue=results$LM$coefficients[1,4]))

            self$results$tableA$setRow(rowNo=2,value=list(
                var=self$options$groupA,
                label=ind[1,"label"],
                est=ind[1,"est"],
                se =ind[1,"se"],
                tvalue=ind[1,"z"],
                pvalue=ind[1,"pvalue"]))

            self$results$tableA$setRow(rowNo=3,value=list(
                var=self$options$groupB,
                label=ind[2,"label"],
                est=ind[2,"est"],
                se =ind[2,"se"],
                tvalue=ind[2,"z"],
                pvalue=ind[2,"pvalue"]))

            self$results$tableA$setRow(rowNo=4,value=list(
                var=paste(self$options$groupA,":",self$options$groupB),
                label=ind[4,"label"],
                est=ind[4,"est"],
                se =ind[4,"se"],
                tvalue=ind[4,"z"],
                pvalue=ind[4,"pvalue"]))

            self$results$tableA$setRow(rowNo=5,value=list(
                var=paste(self$options$groupA,"^2"),
                label=ind[3,"label"],
                est=ind[3,"est"],
                se =ind[3,"se"],
                tvalue=ind[3,"z"],
                pvalue=ind[3,"pvalue"]))

            self$results$tableA$setRow(rowNo=6,value=list(
                var=paste(self$options$groupB,"^2"),
                label=ind[5,"label"],
                est=ind[5,"est"],
                se =ind[5,"se"],
                tvalue=ind[5,"z"],
                pvalue=ind[5,"pvalue"]))

            #set the tableB coefficients
            self$results$tableB$setRow(rowNo=1,value=list(
                var="a1",
                label=ind[28,"rhs"],
                est=ind[28,"est"],
                se =ind[28,"se"],
                tvalue=ind[28,"z"],
                pvalue=ind[28,"pvalue"]))

            self$results$tableB$setRow(rowNo=2,value=list(
                var="a2",
                label=ind[29,"rhs"],
                est=ind[29,"est"],
                se =ind[29,"se"],
                tvalue=ind[29,"z"],
                pvalue=ind[29,"pvalue"]))

            self$results$tableB$setRow(rowNo=3,value=list(
                var="a3",
                label=ind[30,"rhs"],
                est=ind[30,"est"],
                se =ind[30,"se"],
                tvalue=ind[30,"z"],
                pvalue=ind[30,"pvalue"]))

            self$results$tableB$setRow(rowNo=4,value=list(
                var="a4",
                label=ind[31,"rhs"],
                est=ind[31,"est"],
                se =ind[31,"se"],
                tvalue=ind[31,"z"],
                pvalue=ind[31,"pvalue"]))

            self$results$tableB$setRow(rowNo=5,value=list(
                var="a5",
                label=ind[32,"rhs"],
                est=ind[32,"est"],
                se =ind[32,"se"],
                tvalue=ind[32,"z"],
                pvalue=ind[32,"pvalue"]))

            #set the tableS coefficients
            self$results$tableS$setRow(rowNo=1,value=list(
                var="Estimate method",
                fit="ML"))

            self$results$tableS$setRow(rowNo=2,value=list(
                var="Formula",
                fit= paste(self$options$dep,"~",self$options$groupA,"*",self$options$groupB),sep=""))

            self$results$tableS$setRow(rowNo=3,value=list(
                var="R squared",
                fit= results$r.squared))

            self$results$tableS$setRow(rowNo=4,value=list(
                var="Sample size",
                fit= nrow(self$data)))

            self$results$tableS$setRow(rowNo=5,value=list(
                var="Degrees of freedom",
                fit= paste("df1=",results$LM$df[1],", df2=",results$LM$df[2],", df3=",results$LM$df[3])))

            # draw picture
            formulab<-paste(self$options$groupA, "~", self$options$groupB)
            formulab <- as.formula(formulab)
            formula <- paste(self$options$dep, '~SO(', self$options$groupA, ',',self$options$groupB,')')
            formula <- as.formula(formula)

            plotData<-persp(rsm::rsm(formula, self$data),formulab,zlab="X",
                            contours = list(z="top"),col=hcl.colors(50),border=NULL)

            image<-self$results$plot
            image$setState(self$data)

        },
        .plot=function(image, ...) {
            plotData<-image$state
            plot<-persp(rsm::rsm(
                as.formula(paste(self$options$dep, '~SO(', self$options$groupA, ',',self$options$groupB,')'))
                , plotData),
                as.formula(paste(self$options$groupA, "~", self$options$groupB)),
                zlab="X",contours = list(z="top"),col=hcl.colors(50),border=NULL)
            dev.off()

            print(plot)
            TRUE
        })
)
