#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#' @param erp_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' arReceivableUploadServer()
arReceivableUploadServer <- function(input, output, session, dms_token, erp_token) {

  files_arReceivable = tsui::var_file('files_arReceivable')

  shiny::observeEvent(input$btn_arReceivable_upload, {

    filename=files_arReceivable()

    if(filename==''  || is.null(filename)){

      tsui::pop_notice("请先上传文件")


    }else{

      mdlEMsalOutStockUploadPkg::dms_ar_receivable_input_delete(dms_token =dms_token )

      mdlEMsalOutStockUploadPkg::dms_ar_receivableEntry_input_delete(dms_token =dms_token )



      data <- readxl::read_excel(filename,col_types = c("text", "text", "text","text", "date", "date",
                                                        "text", "text","text", "text", "text", "text",
                                                        "text","text", "numeric", "numeric", "text",
                                                        "text", "text", "text", "text", "numeric",
                                                        "numeric", "numeric", "numeric",
                                                        "numeric", "numeric", "text", "numeric",
                                                        "text", "text", "numeric")
      )

      data = as.data.frame(data)
      data = tsdo::na_standard(data)

      tsda::db_writeTable2(token = erp_token,table_name = 'rds_src_ods_t_ar_receivable_input',r_object = data,append = TRUE)

      mdlEMsalOutStockUploadPkg::erp_arReceivable_input_update(erp_token =erp_token )


      data_erp = mdlEMsalOutStockUploadPkg::erp_arReceivable_select(erp_token =erp_token )

      tsda::db_writeTable2(token = dms_token,table_name = 'rds_dms_ods_t_ar_receivable_input',r_object = data_erp,append = TRUE)


      dataEntry_erp = mdlEMsalOutStockUploadPkg::erp_arReceivableEntry_select(erp_token =erp_token )


      tsda::db_writeTable2(token = dms_token,table_name = 'rds_dms_ods_t_ar_receivableEntry_input',r_object = dataEntry_erp,append = TRUE)



      mdlEMsalOutStockUploadPkg::dms_ar_receivable_upload(dms_token =dms_token )


      mdlEMsalOutStockUploadPkg::dms_ar_receivableEntry_upload(dms_token =dms_token )



      tsui::pop_notice("上传成功")


    }

  })
}







#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param erp_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' arReceivableViewServer()
arReceivableViewServer <- function(input, output, session, dms_token, erp_token) {



  shiny::observeEvent(input$btn_arReceivable_view, {



    text_arReceivable_FBillNo = tsui::var_text("text_arReceivable_FBillNo")

    text_date_arReceivable_FDate = tsui::var_dateRange('text_date_arReceivable_FDate')


    FDate = text_date_arReceivable_FDate()

    FStartDate = FDate[1]

    FEndDate = FDate[2]

    FBillNo=text_arReceivable_FBillNo()



    data = mdlEMsalOutStockUploadPkg::dms_ar_receivable_view(dms_token = dms_token,FBillNo =FBillNo,FStartDate,FEndDate)

    tsui::run_dataTable2(id = 'arReceivable_resultView',data = data)

    tsui::run_download_xlsx(id = 'dl_arReceivable',data = data,filename='应收单上传日志查询.xlsx')



  })






}


#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param erp_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' arReceivableSyncServer()
arReceivableSyncServer <- function(input, output, session, dms_token, erp_token) {



  shiny::observeEvent(input$btn_arReceivable_sync, {


    mdlEMsalOutStockUploadr::arReceivable_ERPSync()

    tsui::pop_notice("回传成功")



  })






}




#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param erp_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' arReceivableServer()
arReceivableServer <- function(input, output, session, dms_token, erp_token) {
  arReceivableUploadServer(input = input, output = output, session = session, dms_token = dms_token, erp_token = erp_token)


  arReceivableViewServer(input = input, output = output, session = session, dms_token = dms_token, erp_token = erp_token)

  arReceivableSyncServer(input = input, output = output, session = session, dms_token = dms_token, erp_token = erp_token)
}
