#' Beta Control Charts
#'
#' p-Charts and np-Charts are commonly used in monitoring variables of the fraction type and these charts assume that the monitored variables are binomially distributed. In this paper we propose a new control chart called Beta Charts, for monitoring fraction data (p). The Beta Chart presents the control limits based on the Beta probability distribution. It was applied for monitoring the variables in three real studies, and it was compared to the control limits with three schemes. The comparative analysis showed that: (i) Beta approximation to the Binomial distribution was more appropriate with values confined in the [0, 1]- interval; and (ii) the charts proposed were more sensitive to the average run length (ARL), in both in-control and out-of-control processes monitoring. The Beta Charts outperform the control charts analyzed for monitoring fraction data.
#'
#'
#'
#' @export bcc
#' @import qcc
#' @reference SANT'ANNA, Ângelo M. O; CATEN, Carla Schwengberten. Beta control charts for monitoring fraction data. Expert Systems With Applications, p. 10236-10243. 1 set. 2012.



bcc<-function(data, sizes, type){

  res <- type

  #caso com n=x, com x=n?mero inteiro
  if (res == "1"){

    qcc(data = data, sizes = sizes, type = "beta", confidence.level = 0.9)
  }

  ####caso com propor??o
  if (res == "2"){

    qcc(data = data, type= "beta.prop", confidence.level = 0.9)

  }
}
