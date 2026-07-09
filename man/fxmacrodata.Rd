\name{fxmacrodata}
\alias{fxmacrodata_url}
\alias{fxmacrodata_read}
\alias{fxmacrodata_endpoints}
\title{FXMacroData macro and FX endpoint helpers}
\description{Build and read FXMacroData URLs for macro announcements, release calendars, predictions, FX rates, COT, commodities, curves, sessions, risk sentiment, news, press releases and central-bank context.}
\usage{
fxmacrodata_url(path, query = list(), api_key = Sys.getenv("FXMD_API_KEY", ""))
fxmacrodata_read(path, query = list(), api_key = Sys.getenv("FXMD_API_KEY", ""))
fxmacrodata_endpoints(currency = "usd", indicator = "policy_rate",
                      base = "eur", quote = "usd")
}
\arguments{
\item{path}{FXMacroData API path under \code{/v1}.}
\item{query}{Named list of query parameters.}
\item{api_key}{Optional FXMacroData API key.}
\item{currency}{Three-letter currency code.}
\item{indicator}{Indicator slug such as \code{policy_rate}.}
\item{base, quote}{FX pair currencies.}
}
\value{\code{fxmacrodata_url} returns a URL, \code{fxmacrodata_read} returns raw response text, and \code{fxmacrodata_endpoints} returns a named list of example URLs.}
