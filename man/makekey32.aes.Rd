\name{makekey32.aes}
\alias{makekey32.aes}
\title{Make 32 bit key}
\description{
Creates a 32 bit raw key for encryption.
}
\usage{
makekey32.aes(seedVal)
}
\arguments{
  \item{seedVal}{A integer value used to seed the random creation of an encryption key.}
}
\details{This function creates a 32 bit key that can be used to encrypt a data frame.

}
\examples{
key <- make32.aes(12)}

