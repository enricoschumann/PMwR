saveAttachment <- function(pattern, fname, writeIfExists = TRUE, adjustfn = TRUE) {

    mbox <- readLines(fname, n = -1)
    ilines <- grep(paste("filename=\"?.*", pattern, sep = ""),
                   mbox, ignore.case = TRUE)
    atts <- gsub(".*filename=\"?([^\"]*)\"?.*", "\\1", mbox[ilines])

    if (any(duplicated(atts)))
        warnings("The pattern matches several files with *identical* names.")

    if (!length(ilines))
        message("no matches found")

    il <- 0L
    for (iline in ilines) {
        il <- il + 1

        message("Saving file ", atts[il])

        lb <- 7L ## look back
        lf <- 5L ## look forward
        checkLines <- (iline - lb):(iline + lf)
        checkLines <- checkLines[checkLines < length(mbox) &
                                 checkLines > 0L]
        ## TODO: update lf and lb when at start or end of mbox file
        f7delim <- max(grep("^--", mbox[checkLines]))
        delim <- mbox[iline-lb-1L+f7delim]
        f7enc <- grep("Content-Transfer-Encoding", mbox[checkLines],
                      ignore.case = TRUE)

        do.decode <- TRUE
        if (!grepl("base64", mbox[iline-lb-1L+f7enc], ignore.case = TRUE)) {
            warning("apparently not base64 encoded; file will be written 'as is'.")
            message("--> ", sQuote(mbox[iline-lb-1L+f7enc]))
            do.decode <- FALSE
        }

        start <- iline + min(which(mbox[iline:(iline+lf)] == ""))
        end <- grep(delim, mbox, fixed = TRUE)
        end <- min(end[end > start]) - 1L

        outfile <- atts[il]
        if (adjustfn)
            outfile <- gsub(":", "-", outfile) ## on Windows
        f7file <- 0
        if (file.exists(outfile) && writeIfExists == FALSE) {
            warning("File ", outfile, " exists: nothing saved")
            next
        }
        while (file.exists(outfile)) {
            f7file <- f7file + 1
            tmp <- strsplit(atts[il], "\\.")[[1L]]
            fn <- tmp[1L]
            fext <- tmp[2L]
            outfile <- paste(fn, "-", f7file, ".", fext, sep = "")
            outfile <- gsub(":", "-", outfile)
        }
        if (do.decode) {
            tf <- tempfile()
            writeLines(mbox[start:end], con = tf,
                       sep = "\n", useBytes = FALSE)
            base64decode(file = tf, output = outfile)
            unlink(tf)
        } else
            writeLines(mbox[start:end], con = outfile,
                       sep = "\n", useBytes = FALSE)

    }
    atts
}
