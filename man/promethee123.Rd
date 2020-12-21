\name{promethee123}

\alias{promethee123}

\title{
PROMETHEE I, II, and III Methods
}

\description{
The PROMETHEE method is a multti-criteria decision-making method addressing with outranking problems. The method establishes a preference structure between the alternatives, having a preference function for each criterion. IN this context, three variants of the method is carried out: PROMETHEE I (Partial pre-ordering), PROMETHEE II (Total pre-ordering), and PROMETHEE III (pre-ordering by inervals).
}

\usage{ promethee123(alternatives, criteria, decision_matrix, min_max,
normalization_function, q_indifference, p_preference, s_curve_change, criteria_weights)
}


\arguments{


  \item{alternatives}{
The names respective to set of alternatives in evaluation
}
  \item{criteria}{
The names respective to set of criteria in evaluation
}
  \item{decision_matrix}{
A matrix where rows correspond to the criteria and columns correspond to alternatives, there is inputed the performance of alternatives in each criterion
}
  \item{min_max}{
A vector with objectives, minimize or maximize, to each criteria.
}
  \item{normalization_function}{
Numerical description relative to each type of normalization function to each criterion
}
  \item{q_indifference}{
Indifference threshold
}
  \item{p_preference}{
Preference threshold
}
  \item{s_curve_change}{
Threshold of changing in the curve
}
  \item{criteria_weights}{
Numerical representation of the respective importance for each criterion
}

}

\details{

- For normalization function we have six types:
         [ 1 ] for USUAL      (0 or 1)                  ---
         [ 2 ] for U-SHAPE    (0 or 1)                 q
         [ 3 ] for V-SHAPE    (x/p or 1)                   p
         [ 4 ] for LEVEL      (0, 0.5 or 1)            q , p
         [ 5 ] for V-SHAPE I  (0, (x-q)/(p-q) or 1)    q , p
         [ 6 ] for GAUSSIAN   (0 or 1-e^(-x^2/2*s^2))    s
        ----------------------------------
            q = indifference parameter
            p = preference parameter
            s = parameter to indicate change in the preference curve

- The input of thresholds depends of the type of preference function used;

- The sum of weights must be 1;


}

\value{

- Performance in each criterion;

- Global Index of Importance;

- Importance Flows (Positive, Negative, and Net);

- Preference relations in PROMETHEE I;

- Total Outranking in PROMETHEE II;

- Preference relations in PROMETHEE III;

- Graphical representations of PROMETHEE I, II, and III.

}

\references{
BRANS, Jean-Pierre; DE SMET, Yves. PROMETHEE methods. In: Multiple criteria decision analysis. Springer, New York, NY, 2016. p. 187-219. DOI: 10.1007/978-1-4939-3094-4_6. <https://link.springer.com/chapter/10.1007/978-1-4939-3094-4_6>
}

\author{Miguel Angelo Lellis Moreira \email{miguellellis@hotmail.com},
        Marcos dos Santos \email{marcosdossantos_doutorado_uff@yahoo.com.br},
        Carlos Francisco Simoes Gomes \email{cfsg1@bol.com.br}}


\examples{

alternatives <- c("SARP", "ORAC", "TOTS", "MICRO", "IBRP")

criteria <- c("Price", "Complexity", "Security", "Performance")

decision_matrix <- matrix(c(15, 29, 38, 24, 25.5,
                            7.5, 9, 8.5, 8,    7,
                            1, 2,   4, 3,    3,
                            50, 110, 90, 75, 85),

                            ncol = length(alternatives), nrow = length(criteria), byrow = TRUE)

min_max <- c("min", "min", "max", "max")

normalization_function <- c( 5 , 5 , 5 , 5 )
q_indifference <- c(2, 0.5 , 1 , 10)
p_preference <- c(5 , 1 , 2 , 20)
s_curve_change <- c("","","","")

criteria_weights <- c(0.2 , 0.2 , 0.3 , 0.3)

promethee123(alternatives, criteria, decision_matrix, min_max, normalization_function,
q_indifference, p_preference, s_curve_change, criteria_weights)

}


