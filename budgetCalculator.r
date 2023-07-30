library(R6)

#Eventually load environment variables into calculator to avoid repetion.
#Load CSV, excel, or ods file for 
# AND/OR put data in sqlite file for later access.
source("taxCalculator.r")

BudgetCalculator <- R6Class("BudgetCalculator",
    inherit = CalculateTaxes,
    public = list(

    setHouseholdCount = function(householdNumber = NULL) {
        if (is.null(householdNumber)) {
        print("How many people in the household pay bills?")
        householdNumber <- as.numeric(readline())
        while (householdNumber < 1) {
            print("Please enter a household number greater than zero.")
            householdNumber <- as.numeric(readline())
        }
        }
        private$householdCount <- householdNumber
    },

    setNames = function(names = NULL) {
    if (is.null(names)) {
    iterator <- private$householdCount
    while (iterator != 0) {
        iterator <- iterator - 1
        print("Enter each household member's names one by one")
        name <- readline()
        while (!nzchar(name)) {
            print("The name you entered was blank, please try again")
            name <- readline()
        }
        private$peopleInfo[[name]] <- name
    }
    } else {
        for (name in names) {
            private$peopleInfo[[name]] <- name
        }
    }
    },
    setIncome = function(incomeInfo = NULL) {
        if (is.null(incomeInfo)) {
        for (name in private$peopleInfo) {
            print(paste("How much does", name, "make per hour?"))
            hourlyWage = as.numeric(readline())
            print(paste("How many hours did", name, "work per day?"))
            hoursWorked = as.numeric(readline())
            print(paste("How many days did", name, "work?"))
            daysWorked <- as.numeric(readline())
            calculatedIncome <- hourlyWage * hoursWorked * daysWorked
            private$peopleInfo[[name]] <- list("weeklyIncome" = calculatedIncome, "annualIncome" = calculatedIncome * 52) 
        }
      } else {
            for (name in names(incomeInfo)) {
                private$peopleInfo[[name]] <- list("weeklyIncome" = incomeInfo[[name]], "annualIncome" = calculatedIncome * 52)
            }
        }
    },

    #Currently only based on california.
    calculateTaxes = function() {

        for (name in names(private$peopleInfo)) {
            annualIncome <- private$peopleInfo[[name]][["annualIncome"]]
            super$setAnnualIncome(annualIncome)
            print(paste("What is", name, "'s filing status?"))
            print("Single, Head of Household, or Married?")
            filingStatus <- tolower(readline())
            while (filingStatus != "single" && filingStatus != "head of household" && filingStatus != "hoh" && filingStatus != "married") {
                print("Please enter an appropriate value")
            }
            if (filingStatus == "single") {

                super$totalSingleCaliforniaFedTaxes()

            } else if (filingStatus == "head of household" || filingStatus == "hoh") {

                super$totalHeadOfHouseholdCaliforniaFedTaxes()

            } else if (filingStatus == "married") {
                print("Are you filing separately or together?")
                choice <- tolower(readline())
            }
            print(paste("take home pay: ", super$getBiweeklyTakeHomePay()))
        }
    },

    setBills = function() {
        for (name in names(private$peopleInfo)) {
        print(paste("How many bills does", name, "pay for?"))
        billCount <- as.numeric(readline())
        print(paste("What is the name for this bill?"))
        while (billCount > 0) {
            billName <- tolower(readline())
            private$peopleInfo[[name]] <- list()
            billCount <- billCount - 1
            print(paste("What is the name for this bill?"))
        }
        # loop until billcount is zero
        }
    },

    getPeopleInfo = function() {
        print(private$peopleInfo)
    }
    ),
    private = list(
        householdCount = NA,
        peopleInfo = list()
    )
)



calc <- BudgetCalculator$new()
calc$setHouseholdCount()
calc$setNames()
calc$setIncome()
calc$setBills()
calc$calculateTaxes()
calc$getPeopleInfo()
