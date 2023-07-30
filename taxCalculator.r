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

            singleFederalTaxes = function() {
                #Ask questions, such as "filing status, HOH? etc"
                # Tax tables: https://smartasset.com/taxes/current-federal-income-tax-brackets
                # Understand marginal tax rate and how only portions of income are taxed at different rates.

                # private$federalTaxedIncome <- private$federalTaxedIncome + socialSecurityTaxAmount 
                # https://www.khanacademy.org/college-careers-more/personal-finance/pf-taxes/pf-personal-taxes-tutorial/v/calculating-federal-taxes-and-take-home-pay
                # if (private$annualIncome <= 11000) {
                #     tax <- private$annualIncome * 0.1
                # } else if (private$annualIncome <= 44725) {
                #     tax <- (11000 * 0.1) + (0.12 * (private$annualIncome - 11000))
                # } else if (private$annualIncome <= 95375) {
                #     tax <- (11000 * 0.1) + (0.12 * (44725 - 11000)) + (0.22 * (private$annualIncome - 44725))
                # } else if (private$annualIncome <= 182100) {
                #     tax <- (11000 * 0.1) + (0.12 * (44725 - 11000)) + (0.22 * (95375 - 44725)) + (0.24 * (private$annualIncome - 95375))
                # } else if (private$annualIncome <= 231250) {
                #     tax <- (11000 * 0.1) + (0.12 * (44725 - 11000)) + (0.22 * (95375 - 44725)) + (0.24 * (182100 - 95375)) + (0.32 * (private$annualIncome - 182100))
                # } else if (private$annualIncome <= 578125) {
                #     tax <- (11000 * 0.1) + (0.12 * (44725 - 11000)) + (0.22 * (95375 - 44725)) + (0.24 * (182100 - 95375)) + (0.32 * (231250 - 182100)) + (0.35 * (private$annualIncome - 231250))
                # } else if (private$annualIncome > 578125) {
                #     tax <- (11000 * 0.1) + (0.12 * (44725 - 11000)) + (0.22 * (95375 - 44725)) + (0.24 * (182100 - 95375)) + (0.32 * (231250 - 182100)) + (0.35 * (578125 - 231250)) + (0.37 * private$annualIncome)
                # }
                private$taxBracketsFormula(list(11000, 44725, 95375, 182100, 231250, 578125), list(0.1, 0.12, 0.22, 0.24, 0.32, 0.35, 0.37))
                # print(paste("CORRECT TAX: ", tax))
            },

            headOfHouseholdFederalTaxes = function() {
                private$taxBracketsFormula(list(15700, 59850, 95350, 182100, 231250, 578100), list(0.1, 0.12, 0.22, 0.24, 0.32, 0.35, 0.37))
            },

            marriedSeparateFederalTaxes = function() {
                private$taxBracketsFormula(list(11000, 44725, 95375, 182100, 231250, 346875), list(0.1, 0.12, 0.22, 0.24, 0.32, 0.35, 0.37))
            },

            marriedTogetherFederalTaxes = function() {
                private$taxBracketsFormula(list(22000, 89450, 190750, 364200, 462500, 693750), list(0.1, 0.12, 0.22, 0.24, 0.32, 0.35, 0.37))
            },

            # Current tax rates are not out yet, unfortunately uses 2022 brackets
            singleCaliforniaTaxes = function() {
                private$taxBracketsFormula(list(10099, 23942, 37788, 52455, 66295, 338639, 406364, 677275), list(0.01, 0.02, 0.04, 0.06, 0.08, 0.093, 0.103, 0.113, 0.123), 1)
            },

            headOfHouseholdCaliforniaTaxes = function() {
                private$taxBracketsFormula(list(20212, 47887, 61730, 76397, 90240, 460547, 552658, 921095), list(0.01, 0.02, 0.04, 0.06, 0.08, 0.093, 0.103, 0.113, 0.123), 1)
            },

            marriedSeparateCaliforniaTaxes = function() {
                private$taxBracketsFormula(list(10099, 23942, 37788, 52455, 66295, 338639, 406364, 677275), list(0.01, 0.02, 0.04, 0.06, 0.08, 0.093, 0.103, 0.113, 0.123), 1)
            },

            marriedTogetherCaliforniaTaxes = function() {
                private$taxBracketsFormula(list(20198, 47884, 75576, 104910, 132590, 677278, 812728, 1354550), list(0.01, 0.02, 0.04, 0.06, 0.08, 0.093, 0.103, 0.113, 0.123), 1)
            },

            
            ficaTaxes = function() {
                socialSecurityTaxAmount <- private$annualIncome * 0.062
                medicareTaxAmount <- private$annualIncome * 0.0145
                ficaTaxes <- socialSecurityTaxAmount + medicareTaxAmount
                private$federalTaxedIncome <- private$federalTaxedIncome + ficaTaxes
            },

            californiaSdiTax = function() {
                if (private$annualIncome <= 145000) {
                    sdiTax <- (private$annualIncome * 0.009)
                    if (sdiTax >= 1378.48) {
                        sdiTax <- 1378.48
                        private$stateTaxedIncome <- private$stateTaxedIncome + sdiTax
                    } else {
                        private$stateTaxedIncome <- private$stateTaxedIncome + (private$annualIncome * 0.009)
                    }
                    # print(sdiTax)
                }
            },

            totalSingleCaliforniaFedTaxes = function() {
                self$singleFederalTaxes()
                self$ficaTaxes()
                self$singleCaliforniaTaxes()
                self$californiaSdiTax()
                private$totalTaxedIncome <- private$federalTaxedIncome + private$stateTaxedIncome
                private$biweeklyTakeHomePay <- (private$annualIncome - private$totalTaxedIncome) / 26
                return(private$totalTaxedIncome)
            },

            totalHeadOfHouseholdCaliforniaFedTaxes = function() {
                self$headOfHouseholdFederalTaxes()
                self$ficaTaxes()
                self$headOfHouseholdCaliforniaTaxes()
                self$californiaSdiTax()
                private$totalTaxedIncome <- private$federalTaxedIncome + private$stateTaxedIncome
                private$biweeklyTakeHomePay <- (private$annualIncome - private$totalTaxedIncome) / 26
                return(private$totalTaxedIncome)
            },

            getBiweeklyTakeHomePay = function() {
                return(private$biweeklyTakeHomePay)
            }


    ), 

    private = list (
        annualIncome = NA,
        marriedIncome = NA,
        federalTaxedIncome = NA,
        stateTaxedIncome = NA,
        localTaxedIncome = NA,
        totalTaxedIncome = NA,
        biweeklyTakeHomePay = NA,
        # @params {list(), list(), number}
        taxBracketsFormula = function(brackets, percents, flag = NULL) {
            income <- NA
            tax <- 0
            if (!is.na(private$annualIncome)) {
                income <- private$annualIncome
            } else if (!is.na(private$marriedIncome)) {
                income <- private$marriedIncome
            }
            for (i in seq_along(brackets)) {
            if (income <= brackets[[1]]) {
                tax <- income * percents[[1]]
                break;
            } else if (i == length(brackets) && income > brackets[[i]]) {
                previous <- (brackets[[1]] * percents[[1]])
                for (j in seq_along(brackets)) {
                    if (j+1 <= i) {
                        previous = previous + (percents[[j+1]] * (brackets[[j+1]] - brackets[[j]]))
                        # print(paste(percents[[j+1]], " * (", brackets[[j+1]], " - ", brackets[[j]], ")"))
                    } else {
                        tax <- previous + (percents[[i+1]] * (income))
                        break;
                    }
                }
            } else if (income <= brackets[[i]] && income > brackets[[i-1]] && i > 1) {
                previous <- (brackets[[1]] * percents[[1]])
                for (j in seq_along(brackets)) {
                    if (j+1 != i) {
                        previous = previous + (percents[[j+1]] * (brackets[[j+1]] - brackets[[j]]))
                        # print(paste(percents[[j+1]], " * (", brackets[[j+1]], " - ", brackets[[j]], ")"))
                    } else {
                        tax <- previous + (percents[[i]] * (income - brackets[[i-1]]))
                        break;
                    }
                }
            }
            }
            if (flag == 0 || is.null(flag)) {
                private$federalTaxedIncome <- tax
            } else if (flag == 1) {
                private$stateTaxedIncome <- tax
            }
            # print(paste("TT", tax))
        }
    )
)


#always include FICA taxes after running one of the federal income filing options
# taxes <- CalculateTaxes$new(41600)
# print(paste("TEST: ", taxes$totalSingleCaliforniaFedTaxes()))
# print(taxes$getBiweeklyTakeHomePay())
# print(paste("TEST: ", taxes$totalHeadOfHouseholdCaliforniaFedTaxes()))
# print(taxes$getBiweeklyTakeHomePay())

# taxes$marriedFilingTogetherFederalTaxes()