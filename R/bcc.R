#' Beta Control Charts
#'
#' p-Charts and np-Charts are commonly used in monitoring variables of the fraction type and these charts assume that the monitored variables are binomially distributed. In this paper we propose a new control chart called Beta Charts, for monitoring fraction data (p). The Beta Chart presents the control limits based on the Beta probability distribution. It was applied for monitoring the variables in three real studies, and it was compared to the control limits with three schemes. The comparative analysis showed that: (i) Beta approximation to the Binomial distribution was more appropriate with values confined in the [0, 1]- interval; and (ii) the charts proposed were more sensitive to the average run length (ARL), in both in-control and out-of-control processes monitoring. The Beta Charts outperform the control charts analyzed for monitoring fraction data.
#'
#'
#'
#' @export bcc
#' @import qcc
#' @import methods
#' @import stats
#' @reference SANT'ANNA, Ângelo M. O; CATEN, Carla Schwengberten. Beta control charts for monitoring fraction data. Expert Systems With Applications, p. 10236-10243. 1 set. 2012.



bcc<-function(data, type = c("1", "2"),
              sizes, center, std.dev, limits, data.name, labels, newdata, newsizes,
              newdata.name, newlabels, nsigmas = 3, confidence.level=0.9, rules = shewhart.rules,
              plot = TRUE, ...){

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
