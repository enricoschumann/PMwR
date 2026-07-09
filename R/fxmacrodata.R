fxmacrodata_url <- function(path, query = list(), api_key = Sys.getenv("FXMD_API_KEY", "")) {
    if (!nzchar(path))
        stop("'path' must not be empty")
    if (substring(path, 1L, 1L) != "/")
        path <- paste0("/", path)
    if (nzchar(api_key))
        query$api_key <- api_key
    qs <- vapply(names(query), function(name) {
        value <- query[[name]]
        if (is.null(value) || is.na(value))
            return("")
        paste0(utils::URLencode(name, reserved = TRUE), "=",
               utils::URLencode(as.character(value), reserved = TRUE))
    }, character(1L), USE.NAMES = FALSE)
    qs <- qs[nzchar(qs)]
    paste0("https://api.fxmacrodata.com/v1", path,
           if (length(qs)) paste0("?", paste(qs, collapse = "&")) else "")
}

fxmacrodata_read <- function(path, query = list(), api_key = Sys.getenv("FXMD_API_KEY", "")) {
    paste(readLines(fxmacrodata_url(path, query, api_key), warn = FALSE), collapse = "\n")
}

fxmacrodata_endpoints <- function(currency = "usd", indicator = "policy_rate",
                                  base = "eur", quote = "usd") {
    currency <- tolower(currency)
    base <- tolower(base)
    quote <- tolower(quote)
    list(
        data_catalogue = fxmacrodata_url(paste0("/data_catalogue/", currency)),
        announcements = fxmacrodata_url(paste0("/announcements/", currency, "/", indicator)),
        calendar = fxmacrodata_url(paste0("/calendar/", currency)),
        predictions = fxmacrodata_url(paste0("/predictions/", currency, "/", indicator)),
        forex = fxmacrodata_url(paste0("/forex/", base, "/", quote)),
        cot = fxmacrodata_url(paste0("/cot/", currency)),
        commodities_latest = fxmacrodata_url("/commodities/latest"),
        commodity = fxmacrodata_url("/commodities/commodity_price_energy"),
        curves = fxmacrodata_url(paste0("/curves/", currency)),
        curve_proxies = fxmacrodata_url(paste0("/curve_proxies/", currency)),
        forward_curves = fxmacrodata_url(paste0("/forward_curves/", currency)),
        market_sessions = fxmacrodata_url("/market_sessions"),
        risk_sentiment = fxmacrodata_url("/risk_sentiment"),
        news = fxmacrodata_url(paste0("/news/", currency)),
        press_releases = fxmacrodata_url(paste0("/press-releases/", currency)),
        central_bankers = fxmacrodata_url(paste0("/central_bankers/", currency))
    )
}
