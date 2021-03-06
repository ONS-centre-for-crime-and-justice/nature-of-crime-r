% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/items_stolen.R
\name{items_stolen}
\alias{items_stolen}
\title{items_stolen}
\usage{
items_stolen(config, master_config, return_as_dataframes = FALSE)
}
\arguments{
\item{config}{A list of lists (and other items) which is the result of applying \code{yaml::read.yaml()} to an appropriate "Nature
of Crime" .yaml file, and then \code{autotable::process_config()}.}

\item{master_config}{A list of lists (and other objects) which is the result of applying \code{yaml::read.yaml()} to the master
configuration file.}
}
\value{
The "Items stolen" table for the headline crime type specified by \code{config}, as a \code{GPTable} object.
}
\description{
Builds the "Cost of items stolen" table for a headline crime type, as seen in the "Nature of Crime" dataset for that
headline crime type.
}
\details{
This function builds a "Items stolen" table based on two configuration files. The first config file, \code{config}
corresponds to one of the headline crime types, and chiefly determines the output of the table. The second, \code{master_config},
allows you to tweak things that apply to multiple tables (probably including this one), with the idea being that it's easier to
change the master config file once than it is to update each config file for those multiple tables - which could potentially
constitute changing 50 different files... bothersome and time-consuming).

In this function, each subtable of the table is processed in turn. The processing of the table is handled by lower-order functions,
one corresponding to each part of the table. For the "Items stolen" table, there are currently 2 parts in general (albeit we
don't build both parts for every crime type at present): "Were any items stolen?" and "Which items?"; these are built by the
functions \code{items_stolen_any_items} and \code{items_stolen_which items} respectively.

If applicable, the function then binds these 2 parts together into a single data frame to get the full "Items stolen" table
before processing it into a \code{GPTable} object (see the User Guide for more details) which is what is returned.
}
\examples{
config = autotable::process_config(yaml::read.yaml("configs/items_stolen/BURGLARY.yaml"))
master_config = yaml::read_yaml(file = "configs/MASTER.yaml")
items_stolen(config = config, master_config = master_config)

}
\seealso{
See the following URL for an example of a Nature of Crime dataset (where the headline crime type in the example is
"Vehicle-related theft"): \url{https://www.ons.gov.uk/peoplepopulationandcommunity/crimeandjustice/datasets/natureofcrimevehiclerelatedtheft}
}
\author{
Eleanor Scott-Allen, Katie Edser, Matthew Morrison, Nicholas O'Neil; ONS Centre for Crime and Justice, Titchfield, PO15 5RR.
}
