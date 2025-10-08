# Portfolio Management with R

## About PMwR

Functions for the practical management of financial
portfolios: backtesting investment and trading strategies,
computing profit-and-loss and returns, analysing trades,
reporting, and more. PMwR provides a small set of reliable,
efficient and convenient tools that help with processing and
analysing trade/portfolio data. The package does not
provide a complete application that could be used 'as is',
but building blocks for creating such an application.

## What PMwR provides

The package provides functions that can serve as building
blocks for many activities in portfolio management.

- Keeping track of transactions: The package provides
     functions for handling journals (sometimes called
     blotters). See ?journal and ?position.
- Testing strategies: See ?btest (and this
  [tutorial on backtesting](https://dx.doi.org/10.2139/ssrn.3374195)).
- Computing profit/loss and returns: See ?returns,
     ?rc, ?pl or ?unit_prices.

All details are in the
[manual](https://enricoschumann.net/R/packages/PMwR/manual/PMwR.html).
New features are often described in these
[notes](https://enricoschumann.net/notes/PMwR/).

I am grateful for comments, suggestions and corrections;
bug reports, in particular, can be started directly from
within R:

    library("utils")
    bug.report("[PMwR] Unexpected behaviour in function XXX",
               address = maintainer("PMwR"),
               package = "PMwR")

Applications, as long as they are finance-related,
should be discussed on the R-SIG-Finance mailing list:

[https://stat.ethz.ch/mailman/listinfo/r-sig-finance](https://stat.ethz.ch/mailman/listinfo/r-sig-finance)




## Installation

The latest stable version of the package is available
from [CRAN](https://cran.r-project.org/package=PMwR).
The latest *development* version is available from
[https://enricoschumann.net/R/packages/PMwR/](https://enricoschumann.net/R/packages/PMwR/). You
can install either version directly from within R:

    install.packages('PMwR') ## CRAN stable version

    install.packages('PMwR', ## development version
	                 type = 'source',
                     repos = c('https://enricoschumann.net/R',
                               getOption('repos')))

The package depends on several other packages, which
can be obtained from the same repository and from CRAN.

There are also publicly-available repositories at
[https://git.sr.ht/~enricoschumann/PMwR](https://git.sr.ht/~enricoschumann/PMwR) ,
[https://gitlab.com/enricoschumann/PMwR](https://gitlab.com/enricoschumann/PMwR) and
[https://github.com/enricoschumann/PMwR](https://github.com/enricoschumann/PMwR).
