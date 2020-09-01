#'@title set_up_logger
#'
#'@description Sets up a logger, which can then be used to write messages (particularly log messages) to the console and/or
#'a given file.
#'
#'@details This function uses the \code{futile.logger} R package to handle message logging. Users can choose whether they
#'want log messages to be written to the console, to a file (which must be specified by \code{path}), to both, or to
#'neither (though there is a caveat on the neither case - see: Note). Log messages will take the form
#'\emph{<message level> <YY-DD-MM HH::MM:SS> <message>}, each appearing on a new line, and the file name for any logfile
#'will be of the form \emph{YYYY-MM-DD__HH-MM-SS.txt}. All messages above \code{TRACE} level will be logged.
#'
#'@param write_to_console A Boolean: \code{TRUE} (default) means that messages are written to the R console; \code{FALSE}
#'means that they are not.
#'
#'@param write_to_file A Boolean: \code{TRUE} (default) means that messages are written to a file in the directory specified
#'by \code{path}; \code{FALSE} means that they are not.
#'
#'@param path A string specifying the folder path where the logfile should be saved, should \code{write_to_file} be set as
#'\code{TRUE}. If the given path does not already exist, it will be created.
#'
#'@param threshold One of the six log level constants offered by the \code{futile.logger} package. That is, there are 6 severity
#'levels of log message supported by the logger; in order, these are \code{TRACE}, \code{DEBUG}, \code{INFO}, \code{WARN},
#'\code{ERROR} and \code{FATAL}. When \code{threshold} is set to one of these levels, the logger will only log messages of
#'this severity and above. The default value for \code{threshold} is \code{INFO}; if you are wanting to debug your code,
#'however, you may wish to print more log messages, and therefore lower the threshold to \code{TRACE}. Note that this argument
#'should not be passed as a string; the direct name of the object is required.
#'
#'@return \code{invisible(NULL)}; the return value is void, yet the logger will now be set up and ready-to-go.
#'
#'@note The \code{futile.logger} package requires that every log message is written to either the console, to the file, or
#'to both; it is impossible to use e.g. \code{flog.info()} and have it not point to either. Therefore, if both
#'\code{write_to_console} and \code{write_to_file} are set to \code{FALSE}, then a workaround is employed: code proceeds
#'as if \code{write_to_console} were \code{TRUE}, but with the \code{flog.threshold} set to \code{FATAL}, which
#'is the most severe log level. Effectively, this means that all message levels below \code{FATAL} (i.e. \code{ERROR},
#'\code{WARN}, \code{INFO}, \code{DEBUG} and \code{TRACE}) are not written to the console nor the file, yet any
#'\code{FATAL} level messages will still be written to the console.
#'
#'@note For more information on logging with \code{futile.logger}, see:
#'\url{https://cran.r-project.org/web/packages/futile.logger/futile.logger.pdf}.
#'
#'@examples \code{set_up_logger(path = "logfiles/")}
#'flog.info("Our logger is now set up. Look!")
#'flog.warn("R is too much fun! Take breaks and drink water!")
#'
#'@export
#'

set_up_logger = function(write_to_console = TRUE,
                         write_to_file    = TRUE,
                         path             = NULL,
                         threshold        = INFO) {

  if (write_to_console & write_to_file) {

    logfile = paste0(path, '/', format(Sys.time(), format = "%Y-%m-%d__%H-%M-%S.txt"))

    if(!base::file.exists(logfile)){ # "If that logfile does not already exist...
      base::file.create(logfile)     # ... then make it!"
    }

    invisible(flog.logger("ROOT", threshold = threshold, appender = appender.tee(logfile)))

  } else if (write_to_file) {

    logfile = paste0(path, '/', format(Sys.time(), format = "%Y-%m-%d__%H-%M-%S"), ".txt")

    if(!base::file.exists(logfile)){ # "If that logfile does not already exist...
      base::file.create(logfile)     # ... then make it!"
    }

    invisible(flog.logger("ROOT", threshold = threshold, appender = appender.file(logfile)))

  } else if (write_to_console) {

    invisible(flog.logger("ROOT", threshold = threshold, appender = appender.console()))

  } else {

    invisible(flog.logger("ROOT", threshold = FATAL, appender = appender.console()))

  }

}
