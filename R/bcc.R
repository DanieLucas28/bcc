#' BCC
#'
#' Apply the beta control chart to the indicated values.
#'
#'
#'
#' @export bcc
#' @import qcc
#' @import svDialogs

bcc<-function(data, sizes){

  res <- dlg_message("Is it a case of proportion?", "yesno")$res

  #caso com n=x, com x=n?mero inteiro
  if (res == "no"){

    qcc(data = data, sizes = sizes, type = "beta", confidence.level = 0.9)
  }

  ####caso com propor??o
  if (res == "yes"){

    qcc(data = data, type= "beta.prop", confidence.level = 0.9)

  }
}
