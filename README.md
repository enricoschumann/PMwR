# Portfolio Management with R


## About PMwR

Functions for the practical management of financial
portfolios: backtesting investment and trading strategies,
computing profit-and-loss and returns, analysing trades,
reporting, and more. PMwR provides a small set of reliable,
efficient and convenient tools that help in processing and
analysing trade/portfolio data. The package does not
provide a complete application that could be used 'as is',
but building blocks for creating such an application.

PMwR grew out of various pieces of software that I have
written since 2008. While the package has become fairly
stable in recent years, the interfaces of functions may still
not be fully stable (e.g., argument names might still be
made consistent across functions); in some cases, generic
functions might be introduced. The *recommended* *practice*
is therefore to *explicitly* *name* *arguments* *in*
*function* *calls* (and not pass arguments by
position). Any changes in argument names will be documented
in the
[NEWS file](http://enricoschumann.net/R/packages/PMwR/NEWS) and
so can be followed easily. More details are in the
[ChangeLog](http://enricoschumann.net/R/packages/PMwR/ChangeLog).


## What PMwR provides

The package provides functions that can serve as building
blocks for many activities in portfolio management.

- Keeping track of transactions: The package provides
     functions for handling journals (sometimes called
     blotters). See ?journal and ?position.
- Testing strategies: See ?btest (and this
  [tutorial on backtesting](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3374195)).
- Computing profit/loss and returns: See ?returns,
     ?rc, ?pl or ?unit_prices.

All details are in the
[manual](http://enricoschumann.net/R/packages/PMwR/manual/PMwR.html).
New features are often described in these
[notes](http://enricoschumann.net/notes/PMwR/).

I am grateful for comments, suggestions and corrections.



## Installation

The latest stable version of the package is available from
[CRAN](https://cran.r-project.org/package=PMwR).  The
latest *development* version is available from
http://enricoschumann.net/R/packages/PMwR/ . You can
install either version directly from within R:

    install.packages('PMwR') ## CRAN stable version

    install.packages('PMwR', ## development version
                     repos = c('http://enricoschumann.net/R',
                               getOption('repos')))

The package depends on several other packages, which
can be obtained from the same repository and from CRAN.

There are also publicly-available repositories at
https://git.sr.ht/~enricoschumann/PMwR ,
https://gitlab.com/enricoschumann/PMwR and
https://github.com/enricoschumann/PMwR.
