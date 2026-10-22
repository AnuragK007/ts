\name{sets}
\alias{sets}
\alias{set1}
\alias{set2}
\alias{set3}
\alias{set4}
\alias{set5}
\alias{set6}
\title{Practical code sets for Time Series Analysis}
\usage{
set1(topic = NULL, run = FALSE, copy = FALSE)

set2(topic = NULL, run = FALSE, copy = FALSE)

set3(topic = NULL, run = FALSE, copy = FALSE)

set4(topic = NULL, run = FALSE, copy = FALSE)

set5(topic = NULL, run = FALSE, copy = FALSE)

set6(topic = NULL, run = FALSE, copy = FALSE)
}
\arguments{
\item{topic}{Topic number (1, 2, ...) or a word from the topic title.
\code{NULL} (default) means all topics of the set.}

\item{run}{If \code{TRUE}, run the code in the console instead of printing it.}

\item{copy}{If \code{TRUE}, also copy the code to the clipboard (needs clipr).}
}
\value{
Invisibly, the code lines that were shown.
}
\description{
\code{set1()} to \code{set6()} print (or run) the code of each practical
question set, split topic-wise.
}
\examples{
\dontrun{
set1()
set1(2)
set3("regression")
set5(1, run = TRUE)
}
}
