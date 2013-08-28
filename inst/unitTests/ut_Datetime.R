test.Datetime <- function() {
    x <- as.Date(c("1996-05-30","1996-05-20"))

    checkEquals(lastWeekday(6, x, 0),
                structure(c(9641, 9641), class = "Date"))

    checkEquals(lastWeekday(6, x, -1),
                structure(c(9641, 9641), class = "Date") - 7)

    ## from <- ISOdatetime(2012,1,1,12,00,00)
    ## to <- from + 36000
    ## timegrid(from, to,
    ##          interval = "15 sec",
    ##          excludeWeekends = TRUE, holidays = NULL)
}
