
# empatica

This is a package for working with data from [Empatica
monitors](https://www.empatica.com). Currently supports the following:

* Reading individual `avro` files ([raw data](https://www.empatica.com/rawdata/))


## Installation

You can install the development version of empatica from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("paulhibbing/empatica")
```


## Important Note

To read `avro` files, you'll need a particular configuration on your computer,
which has only been tested on Windows thus far. The configuration requires that
you install Python, then install Python's `fastavro` package. The latter package
comes with a 'dumping' script that can be executed from the command line (see
the [PyPI Documentation](https://pypi.org/project/fastavro/0.7.1/)). This is
what `empatica` uses to parse `avro` files, i.e., it invokes some system code to
gain quick access to the `avro` data via Python. It's convoluted, but it's fast.
