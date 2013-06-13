value <- function(x, ...) {
    UseMethod("value")
}
print.value <- function(x, ...) {
    print(unclass(x))    
}
