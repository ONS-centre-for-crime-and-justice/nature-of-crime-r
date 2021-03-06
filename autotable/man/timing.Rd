% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timing.R
\name{timing}
\alias{timing}
\title{timing}
\usage{
timing(config, master_config)
}
\arguments{
\item{config}{A list of lists (and other items) which is the result of applying \code{yaml::read.yaml()} to an appropriate "Nature
of Crime" .yaml file, and then \code{autotable::process_config()}.}

\item{master_config}{A list of lists (and other items) which is the result of applying \code{yaml::read.yaml()} to the master
configuration file.}
}
\value{
The "Timing of when incidents occured" table for the headline crime type specified by \code{config}, as a \code{GPTable}
object.
}
\description{
Builds the "Timing of when incidents occurred" table for a headline crime type, as seen in the "Nature of Crime"
dataset for that headline crime type.
}
\details{
This function builds a "Timing of when incidents occurred" table based on two configuration files. The first config
file, \code{config}, corresponds to one of the headline crime types, and chiefly determines the output of the table. The second,
\code{master_config}, allows you to tweak things that apply to multiple tables (probably including this one), with the idea being
that it's easier to change the master config file once than it is to update each config file for those multiple tables - which
could potentially constitute changing 50 different files... bothersome and time-consuming).

In this function, each subtable of the table is processed in turn. The processing of the table is handled by lower-order functions,
one corresponding to each part of the table. For the "Timing of when incidents occurred" table, there are currently 3 parts:
"Time of day", "Time of week", and "Daylight or dark"; these are built by the functions \code{timing_time_of_day},
\code{timing_time_of_week}, and \code{timing_daylight_or_dark} respectively, which each return a data frame.

The function then binds these 3 parts together into a single data frame to get the full "Timing of when incidents occurred" table
before processing it into a \code{GPTable} object (see the User Guide for more details) which is what is returned.
}
\examples{
config = autotable::process_config(yaml::read.yaml("configs/timing/BURGLARY.yaml"))
master_config = yaml::read_yaml(file = "configs/MASTER.yaml")
timing(config = config, master_config = master_config)

}
\seealso{
See the following URL for an example of a Nature of Crime dataset (where the headline crime type in the example is
"Vehicle-related theft"): \url{https://www.ons.gov.uk/peoplepopulationandcommunity/crimeandjustice/datasets/natureofcrimevehiclerelatedtheft}
}
\author{
Eleanor Scott-Allen, Katie Edser, Matthew Morrison, Nicholas O'Neil; ONS Centre for Crime and Justice, Titchfield, PO15 5RR.
}
