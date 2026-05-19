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
#' outStockMergeUploadServer()
outStockMergeUploadServer <- function(input, output, session, dms_token, erp_token) {

  files_outStockMerge = tsui::var_file('files_outStockMerge')

  shiny::observeEvent(input$btn_outStockMerge_upload, {

    filename=files_outStockMerge()

    if(filename==''  || is.null(filename)){

      tsui::pop_notice("请先上传文件")


    }else{


      mdlEMsalOutStockUploadPkg::dms_sal_outStock_input_delete(dms_token =dms_token )

      mdlEMsalOutStockUploadPkg::dms_sal_outStockEntry_input_delete(dms_token =dms_token )




      data <- readxl::read_excel(filename,col_types = c("text", "text", "text",
                                                        "text", "text", "text", "text", "text",
                                                        "text", "text", "text", "text", "text",
                                                        "text", "numeric", "text", "text",
                                                        "text", "numeric", "text", "text",
                                                        "numeric", "text", "text", "text",
                                                        "text", "text", "text", "numeric",
                                                        "numeric", "numeric", "numeric",
                                                        "numeric")
      )

      data = as.data.frame(data)
      data = tsdo::na_standard(data)

      print(1)

      tsda::db_writeTable2(token = erp_token,table_name = 'rds_src_ods_t_sal_outStock_input',r_object = data,append = TRUE)

      print(2)

      mdlEMsalOutStockUploadPkg::erp_outStockMerge_input_update(erp_token =erp_token )

      print(3)
      data_erp = mdlEMsalOutStockUploadPkg::erp_outStock_select(erp_token =erp_token )

      tsda::db_writeTable2(token = dms_token,table_name = 'rds_dms_ods_t_sal_outStock_input',r_object = data_erp,append = TRUE)

      dataEntry_erp = mdlEMsalOutStockUploadPkg::erp_outStockEntry_select(erp_token =erp_token )

      tsda::db_writeTable2(token = dms_token,table_name = 'rds_dms_ods_t_sal_outStockEntry_input',r_object = dataEntry_erp,append = TRUE)


      mdlEMsalOutStockUploadPkg::dms_sal_outStock_upload(dms_token =dms_token )

      mdlEMsalOutStockUploadPkg::dms_sal_outStockEntry_upload(dms_token =dms_token )



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
#' outStockMergeViewServer()
outStockMergeViewServer <- function(input, output, session, dms_token, erp_token) {



  shiny::observeEvent(input$btn_outStockMerge_view, {



    text_outStockMerge_FBillNo = tsui::var_text("text_outStockMerge_FBillNo")

    FBillNo=text_outStockMerge_FBillNo()



    data = mdlEMsalOutStockUploadPkg::dms_sal_outStock_view(dms_token = dms_token,FBillNo =FBillNo )

    tsui::run_dataTable2(id = 'outStockMerge_resultView',data = data)

    tsui::run_download_xlsx(id = 'dl_outStockMerge',data = data,filename='销售出库上传日志查询.xlsx')



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
#' outStockMergeServer()
outStockMergeServer <- function(input, output, session, dms_token, erp_token) {
  outStockMergeUploadServer(input = input, output = output, session = session, dms_token = dms_token, erp_token = erp_token)


  outStockMergeViewServer(input = input, output = output, session = session, dms_token = dms_token, erp_token = erp_token)
}
