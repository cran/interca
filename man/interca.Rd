\name{interca}
\alias{interca}
\title{
interca
}
\description{
The function performs multiple correspondence analysis (MCA) on a given data set and a given number of categorical variables. The function returns for each point for each axis the coordinates, the interpretive coordinates, the contributions, and the quality of display.
}
\usage{
interca(data = data, numaxes = 10)
}
\arguments{
  \item{data}{
A data frame containing the data to be analyzed.
}
  \item{numaxes}{
The number of categorical variables that will be retained for the calculation of the elements referred to in the function description.
}
}
\value{
The function returns a list with the principal MCA coordinates coordinates (\code{coords}), the interpretive MCA coordinates (\code{ecoords}), the values of the CTR (\code{ctr}) and COR (\code{cor}), the labels of the variable categories (\code{lbl}) and the scree plot (\code{plot}).
}
\references{
Moschidis, S., Markos, A., & Thanopoulos, A. C. (2022). "Automatic" interpretation of multiple correspondence analysis (MCA) results for nonexpert users, using R programming. Applied Computing and Informatics, (ahead-of-print).
}

\examples{
# Set the seed to ensure reproducibility
set.seed(123)
# Create three categorical variables
X1 <- sample(c("X1_1", "X1_2"), size = 200, replace = TRUE)
X2 <- sample(c("X2_1", "X2_2", "X2_3"), size = 200, replace = TRUE)
X3 <- sample(c("X3_1", "X3_2", "X3_3", "X3_4"), size = 200, replace = TRUE)
# the resulting data frame
df <- data.frame(cbind(X1,X2,X3))
# convert to factors
df$X1 <- factor(df$X1)
df$X2 <- factor(df$X2)
df$X3 <- factor(df$X3)

res <- interca(df, 5)
}

