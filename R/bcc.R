#' Beta Control Charts
#'
#' p-Charts and np-Charts are commonly used in monitoring variables of the fraction type and these charts assume that the monitored variables are binomially distributed. The Beta Chart presents the control limits based on the Beta probability distribution. It was applied for monitoring the variables in three real studies, and it was compared to the control limits with three schemes. The comparative analysis showed that: (i) Beta approximation to the Binomial distribution was more appropriate with values confined in the [0, 1]- interval; and (ii) the charts proposed were more sensitive to the average run length (ARL), in both in-control and out-of-control processes monitoring. The Beta Charts outperform the control charts analyzed for monitoring fraction data.
#'
#'
#'
#' @export bcc
#' @import qcc
#' @import methods
#' @import stats
#' @param data a data frame, a matrix or a vector containing observed data for the variable to chart. Each row of a data frame or a matrix, and each value of a vector, refers to a sample or ''rationale group''.
#' @param type a character string specifying the group statistics to compute. There are two possible types, 1 for discrete data and 2 for continuous data.
#' @param sizes a value or a vector of values specifying the sample sizes associated with each group. If data is continuous this parameter should be ignored
#' @param std.dev a value or an available method specifying the within-group standard deviation(s) of the process.
#' @param limits a two-values vector specifying control limits.
#' @param data.name a string specifying the name of the variable which appears on the plots. If not provided is taken from the object given as data.
#' @param labels a character vector of labels for each group.
#' @param newdata a data frame, matrix or vector, as for the \code{data} argument, providing further data to plot but not included in the computations.
#' @param newsizes a vector as for the \code{sizes} argument providing further data sizes to plot but not included in the computations.
#' @param newdata.name a string specifying the name of the variable which appears on the plots. If not provided is taken from the object given as newdata.
#' @param newlabels a character vector of labels for each new group defined in the argument \code{newdata}.
#' @param nsigmas a numeric value specifying the number of sigmas to use for computing control limits. It is ignored when the \code{confidence.level} argument is  provided.
#' @param confidence.level a numeric value between 0 and 1 specifying the confidence level of the computed probability limits.
#' @param rules a value or a vector of values specifying the rules to apply to the chart. See \code{\link{qccRules}} for possible values and their meaning.
#' @param plot logical. If \code{TRUE} a Shewhart chart is plotted.
#' @param ...
#'
#' @references SANT'ANNA, Ângelo M. O; CATEN, Carla Schwengberten. Beta control charts for monitoring fraction data. Expert Systems With Applications, p. 10236-10243. 1 set. 2012.
#' @references Scrucca, L. (2004). qcc: an R package for quality control charting and statistical process control. \emph{R News} 4/1, 11-17.
#'


bcc<-function(data, type = c("1", "2"),
              sizes, center, std.dev, limits, data.name, labels, newdata, newsizes,
              newdata.name, newlabels, nsigmas = 3, confidence.level=0.9, rules = shewhart.rules,
              plot = TRUE){

  res <- type

  if (type == 1){

    qcc(data = data, type = "beta",
        sizes = sizes, center = center, std.dev = std.dev, limits = limits, data.name = data.name, labels = labels, newdata = newdata, newsizes = newsizes,
        newdata.name = newdata.name, newlabels = newlabels, nsigmas = 3, confidence.level = confidence.level, rules = shewhart.rules,
        plot = TRUE, ...)
  }


  if (type == 2){

    qcc(data = data, type = "beta.prop",
        center = center, std.dev = std.dev, limits = limits, data.name = data.name, labels = labels, newdata = newdata, newsizes = newsizes,
        newdata.name = newdata.name, newlabels = newlabels, nsigmas = 3, confidence.level = confidence.level, rules = shewhart.rules,
        plot = TRUE, ...)

  }
}
