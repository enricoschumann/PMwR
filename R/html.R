.xhtml.skeleton <- function(encoding = "UTF-8",
                            language = "en",
                            title = "Title",
                            head = "",
                            body = "...") {

    x <- 
'<?xml version="1.0" encoding="#encoding#"?>
<!DOCTYPE html 
     PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
    "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="#language#" lang="#language#">
<head>
  <title>#title#</title>
  #head#
</head>
<body>
  #body#
</body>
</html>'


    if (length(body) > 1L)
        body <- paste(body, collapse = "\n")

    if (length(head) > 1L)
        head <- paste(head, collapse = "\n")


    x <- gsub("#encoding#", encoding, x)
    x <- gsub("#language#", language, x)
    x <- gsub("#title#", title, x)
    x <- gsub("#head#", head, x)
    x <- gsub("#body#", body, x)
x
}


.html5.skeleton <- function(encoding = "UTF-8",
                            language = "en",
                            title = "Title",
                            head = "",
                            body = "...") {

    x <- 
'<!DOCTYPE html>
<head>
  <meta charset="#encoding#">
  <title>#title#</title>
  #head#
</head>
<body>
  #body#
</body>
</html>'

    
    if (length(body) > 1L)
        body <- paste(body, collapse = "\n")

    if (length(head) > 1L)
        head <- paste(head, collapse = "\n")


    x <- gsub("#encoding#", encoding, x)
    x <- gsub("#language#", language, x)
    x <- gsub("#title#", title, x)
    x <- gsub("#head#", head, x)
    x <- gsub("#body#", body, x)
x
}

.rss.skeleton <- function(encoding = "UTF-8",
                            language = "en",
                            title = "Title",
                            head = "",
                            body = "...") {

## TODO





}
