### Custom MasterData Checks ###
Check_NewCascade <- function(theData) {
    test <- theData$calib
    d2010 <- test[test$year == 2010,]
    subTest <- subset(test, test$value != 0 & !is.na(test$value))

    if (any(d2010$indicator == "PLHIV")) {
        PLHIV_test <- !is.na(d2010[d2010$indicator == "PLHIV","value"]) & d2010[d2010$indicator == "PLHIV","value"] != 0 & !is.na(d2010[d2010$indicator == "PLHIV","weight"])
    } else PLHIV_test <- FALSE

    if (any(d2010$indicator == "PLHIV on ART")) {
        PLHIV_ART_test <- !is.na(d2010[d2010$indicator == "PLHIV on ART","value"]) & d2010[d2010$indicator == "PLHIV on ART","value"] != 0 & !is.na(d2010[d2010$indicator == "PLHIV on ART","weight"])
    } else PLHIV_ART_test <- FALSE


    if (PLHIV_test) {
        if (PLHIV_ART_test) {
            if (dim(subTest)[1] != 0) {
                if (!any(is.na(subTest$weight)) & !any(subTest$weight == "NA")) {
                    return(TRUE)
                } else return(FALSE)
            } else return(FALSE)
        } else return(FALSE)
    } else return(FALSE)
}

Check_NewCD4 <- function(theData) {
    if (dim(theData$cd4)[1] != 0) {
        if (!any(is.na(theData$cd4))) {
            if (round(sum(theData$cd4[2:8]), digits = 2) == 1) {
                if (round(sum(theData$cd4[9:15]), digits = 2) == 1) {
                    return(TRUE)
                } else {
                    return(FALSE)
                }
            } else {
                return(FALSE)
            }
        } else return(FALSE)
    } else return(FALSE)
}

Check_NewCD42015 <- function(theData) {
    if (dim(theData$cd4_2015)[1] != 0) {
        if (!any(is.na(theData$cd4_2015))) {
            if (round(sum(theData$cd4_2015[2:8]), digits = 2) == 1) {
                if (round(sum(theData$cd4_2015[9:15]), digits = 2) == 1) {
                    return(TRUE)
                } else {
                    return(FALSE)
                }
            } else {
                return(FALSE)
            }
        } else return(FALSE)
    } else return(FALSE)
}

CheckOrder <- function(x) if (x[3] <= x[2] & x[2] <= x[1]) return(TRUE) else return(FALSE)

Check_NewIncidence <- function(theData) {
    test <- theData$incidence
    if (dim(test)[1] != 0) {
        if (sum(test[,as.character(seq(2010,2016,1))] <= 0) | sum(is.na(test[,as.character(seq(2010,2016,1))]))) {
            return(FALSE)
        } else {
            ordered <- test[order(test$type, decreasing = TRUE),]
            if (CheckOrder(t(ordered[,"2010"])) & CheckOrder(t(ordered[,"2011"])) & CheckOrder(t(ordered[,"2012"])) & CheckOrder(t(ordered[,"2013"])) & CheckOrder(t(ordered[,"2014"])) & CheckOrder(t(ordered[,"2015"])) & CheckOrder(t(ordered[,"2016"]))) {
                return(TRUE)
            } else return(FALSE)
        }
    } else return(FALSE)
}

Check_NewGuidelines <- function(theData) {
    test <- theData$treatment_guidelines
    if (!any(is.na(test[,2:6]))) {
        if (GuidelineCheck(tx_l200 = test$less200, tx_l250 = test$less250, tx_l350 = test$less350, tx_l500 = test$less500, tx_m500 = test$more500)) {
            return(TRUE)
        } else return(FALSE)
    } else return(FALSE)
}
