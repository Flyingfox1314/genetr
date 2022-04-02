#' Gene ID transform
#'
#' @param type ENTREZID or SYMBOLID
#' @param ID Set of genes named ENTREZID or SYMBOLID
#' @return ENTREZID or SYMBOLID
#' @keywords Gene ID transform
#' @export


gene_tr = function(type,ID){

  if (type == 'ENTREZID') {
    ## 如果输入是ENTREZID
    checkmate::assert_integer(ID)
    len = length(ID)
    SYMBOL = NA
    for (x in 1:len) {

      hanghao = which(tr_data$ENTREZID == ID[x], arr.ind = T)

      if (length(hanghao) != 0) {
        SYMBOL[x] = tr_data[hanghao,5]
      }

    }

    df = cbind(ID, SYMBOL)
    colnames(df) = c('ENTREZID', 'SYMBOL')
    options(warn = 1)
    warning(paste(length(df$ENTREZID)/len ,'% of input gene IDS are fail tp map...'))
    return(df)

  }else if(type == 'SYMBOL') {
    ## 如果输入是SYMBOL
    checkmate::assert_character(ID)
    len = length(ID)
    ENTREZID = NA
    for (x in 1:len) {
      hanghao = which(tr_data$SYMBOL == ID[x], arr.ind = T)

      if (length(hanghao) != 0) {
        ENTREZID[x] = tr_data[hanghao,2]
      }
    }

    df = cbind(ID, ENTREZID)
    colnames(df) = c('SYMBOL', 'ENTREZID')
    options(warn = 1)
    warning(paste(length(df$ENTREZID)/len ,'% of input gene IDS are fail tp map...'))
    return(df)
  }else {
    paste('Please enter ENTREZID or SYMBOL')
  }


}



