library(R6)


CalculateTaxes <- R6Class("CalculateTaxes", 
    public = list(

            setAnnualIncome = function(val) {
                private$annualIncome <- val
            },

            setAnnualMarriedIncome = function(val) {
                private$marriedIncome <- val
            },

            getTaxedAnnualIncome = function() {
                return(private$totalTaxedIncome)
            },

            initialize = function(val = NULL, val2 = NULL) {
                if (!is.null(val) && is.null(val2)) {
                    self$setAnnualIncome(as.numeric(val))
                    self$setAnnualMarriedIncome(as.numeric(val))
                } else if (!is.null(val) && !is.null(val2)) {
                    combinedIncome = as.numeric(val + val2)
                    self$setAnnualMarriedIncome(combinedIncome)
                }
            },

            standardDeductions = function() {

            },

            # #Create "federal taxes" and "state taxes" functions. then master "calculate taxes function"

            singleFederalTaxes = function() {
                #Ask questions, such as "filing status, HOH? etc"
                # Tax tables: https://smartasset.com/taxes/current-federal-income-tax-brackets
                # Understand marginal tax rate and how only portions of income are taxed at different rates.

                # Constants - applied to original annual income
                # socialSecurityTax <- 0.062
                # socialSecurityTaxAmount <- private$annualIncome * socialSecurityTax

                # private$federalTaxedIncome <- private$federalTaxedIncome + socialSecurityTaxAmount 
                # https://www.khanacademy.org/college-careers-more/personal-finance/pf-taxes/pf-personal-taxes-tutorial/v/calculating-federal-taxes-and-take-home-pay
                tax <- 0
                if (private$annualIncome <= 11000) {
                    tax <- private$annualIncome * 0.1
                } else if (private$annualIncome <= 44725) {
                    tax <- (11000 * 0.1) + (0.12 * (private$annualIncome - 11000))
                } else if (private$annualIncome <= 95375) {
                    tax <- (11000 * 0.1) + (0.12 * (44725 - 11000)) + (0.22 * (private$annualIncome - 44725))
                } else if (private$annualIncome <= 182100) {
                    tax <- (11000 * 0.1) + (0.12 * (44725 - 11000)) + (0.22 * (95375 - 44725)) + (0.24 * (private$annualIncome - 95375))
                } else if (private$annualIncome <= 231250) {
                    tax <- (11000 * 0.1) + (0.12 * (44725 - 11000)) + (0.22 * (95375 - 44725)) + (0.24 * (182100 - 95375)) + (0.32 * (private$annualIncome - 182100))
                } else if (private$annualIncome <= 578125) {
                    tax <- (11000 * 0.1) + (0.12 * (44725 - 11000)) + (0.22 * (95375 - 44725)) + (0.24 * (182100 - 95375)) + (0.32 * (231250 - 182100)) + (0.35 * (private$annualIncome - 231250))
                } else if (private$annualIncome > 578125) {
                    tax <- (11000 * 0.1) + (0.12 * (44725 - 11000)) + (0.22 * (95375 - 44725)) + (0.24 * (182100 - 95375)) + (0.32 * (231250 - 182100)) + (0.35 * (578125 - 231250)) + (0.37 * private$annualIncome)
                }
                private$taxesThing(list(11000, 44725, 95375, 182100, 231250, 578125), list(0.1, 0.12, 0.22, 0.24, 0.32, 0.35, 0.37))
                print(paste("CORRECT TAX: ", tax))
                return(tax)
            }
            # Formula, first operation is always the same. Last operation is always the same.
            # Everything in the middle repeats but is varied


    ), 

    private = list (
        annualIncome = NA,
        marriedIncome = NA,
        federalTaxedIncome = NA,
        stateTaxedIncome = NA,
        localTaxedIncome = NA,
        totalTaxedIncome = NA,
        # might be completely working now
        taxesThing = function(brackets, percents) {
            tax <- 0
            for (i in seq_along(brackets)) {
            if (private$annualIncome <= brackets[[1]]) {
                tax <- private$annualIncome * percents[[1]]
                break;
            } else if (private$annualIncome <= brackets[[i]] && private$annualIncome > brackets[[i-1]] && i > 1) {
                previous <- (11000 * 0.1)
                for (j in seq_along(brackets)) {
                    if (j+1 != i) {
                        previous = previous + (percents[[j+1]] * (brackets[[j+1]] - brackets[[j]]))
                        print(paste(percents[[j+1]], " * (", brackets[[j+1]], " - ", brackets[[j]], ")"))
                        # print(paste("pre: ", previous))
                    } else {
                        tax <- previous + (percents[[i]] * (private$annualIncome - brackets[[i-1]]))
                        # print(paste("percent: ", percents[[i]]))
                        # matching current/end bracket
                        break;
                    }
                }
            }
            }
            print(paste("TT", tax))
        }
    )
)



taxes <- CalculateTaxes$new(196600)
print(paste("TEST: ", taxes$singleFederalTaxes()))
# taxes$marriedFilingTogetherFederalTaxes()