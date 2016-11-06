## -*- truncate-lines: t; -*-

test.quote32 <- function() {

    require("PMwR")
    require("RUnit")
    


    checkEquals(q32("100-170"),
                structure(100.53125,
                          handle = 100,
                          ticks = 17,
                          fraction = 0,
                          class = "quote32"))

    checkEquals(q32("100-172"),
                structure(100.5390625,
                          handle = 100,
                          ticks = 17,
                          fraction = 1,
                          class = "quote32"))

    checkEquals(q32("100-175"),
                structure(100.546875,
                          handle = 100,
                          ticks = 17,
                          fraction = 2,
                          class = "quote32"))

    checkEquals(q32("100-177"),
                structure(100.5546875,
                          handle = 100,
                          ticks = 17,
                          fraction = 3,
                          class = "quote32"))

    
    
     ## q32("100-272") - q32("100-270")
     ## as.numeric(q32("100-272") - q32("100-270"))
     
    checkEqualsNumeric(as.numeric(q32("109-047")), 109+4.75/32)
    
}
