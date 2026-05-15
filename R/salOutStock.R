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
#' salOutStockUploadServer()
salOutStockUploadServer <- function(input, output, session, dms_token, erp_token) {

  files_salOutStock = tsui::var_file('files_salOutStock')

  files_saleOutStockEntry = tsui::var_file('files_saleOutStockEntry')


  shiny::observeEvent(input$btn_salOutStock_upload, {

    filename=files_salOutStock()

    if(filename==''  || is.null(filename)){

      tsui::pop_notice("请先上传文件")


    }else{

      mdlEMsalOutStockUploadPkg::salOutStock_input_delete(dms_token = dms_token)

      data <- readxl::read_excel(filename,col_types = c("numeric", "text", "text",
                                                        "text", "text", "text", "text", "text",
                                                        "text", "text", "text", "text", "text",
                                                        "numeric", "numeric", "numeric",
                                                        "text", "text", "text", "text", "text",
                                                        "text", "text", "numeric", "text",
                                                        "numeric", "numeric", "numeric",
                                                        "text")
                                 )

      data = as.data.frame(data)
      data = tsdo::na_standard(data)

      tsda::db_writeTable2(token = dms_token,table_name = 'rds_dms_ods_t_sal_outStock_input',r_object = data,append = TRUE)

      mdlEMsalOutStockUploadPkg::salOutStock_insert(dms_token =dms_token )

      tsui::pop_notice("上传成功")


    }

  })
}

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
#' saleOutStockEntryUploadServer()
saleOutStockEntryUploadServer <- function(input, output, session, dms_token, erp_token) {

  files_saleOutStockEntry = tsui::var_file('files_saleOutStockEntry')

  shiny::observeEvent(input$btn_saleOutStockEntry_upload, {

    filename=files_saleOutStockEntry()

    if(filename==''  || is.null(filename)){

      tsui::pop_notice("请先上传文件")


    }else{


      mdlEMsalOutStockUploadPkg::dms_sal_outStockEntry_input_delete(dms_token = dms_token )

      mdlEMsalOutStockUploadPkg::erp_sal_outStockEntry_input_delete(erp_token = erp_token)

      data <- readxl::read_excel(filename,col_types = c("numeric", "numeric", "text",
                                                        "text", "text", "numeric", "numeric",
                                                        "numeric", "numeric", "text", "text",
                                                        "numeric", "numeric", "numeric",
                                                        "numeric", "numeric", "text", "numeric",
                                                        "numeric", "numeric", "text", "text",
                                                        "text", "numeric", "text", "text",
                                                        "text", "text", "numeric", "numeric",
                                                        "numeric", "numeric", "numeric",
                                                        "numeric", "numeric", "text", "text", "text")
      )

      data = as.data.frame(data)
      data = tsdo::na_standard(data)

      tsda::db_writeTable2(token = erp_token,table_name = 'rds_dms_ods_t_sal_outStockEntry_input',r_object = data,append = TRUE)


      data_erp = mdlEMsalOutStockUploadPkg::erp_outStockEntry_select(erp_token =erp_token )

      tsda::db_writeTable2(token = dms_token,table_name = 'rds_dms_ods_t_sal_outStockEntry_input',r_object = data_erp,append = TRUE)

      mdlEMsalOutStockUploadPkg::salOutStockEntry_insert(dms_token = dms_token)

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
#' salOutStockViewServer()
salOutStockViewServer <- function(input, output, session, dms_token, erp_token) {

  shiny::observeEvent(input$btn_salOutStock_view, {



    text_salOutStock_FBillNo = tsui::var_text("text_salOutStock_FBillNo")

    FBillNo=text_salOutStock_FBillNo()

    data = mdlEMsalOutStockUploadPkg::salOutStock_select(dms_token = dms_token,FBillNo = FBillNo)

    tsui::run_dataTable2(id = 'salOutStock_resultView',data = data)

    tsui::run_download_xlsx(id = 'dl_salOutStock',data = data,filename='salOutStock.xlsx')



  })

  shiny::observeEvent(input$btn_saleOutStockEntry_view, {



    text_saleOutStockEntry_FBillNo = tsui::var_text("text_saleOutStockEntry_FBillNo")

    FBillNo=text_saleOutStockEntry_FBillNo()



    data = mdlEMsalOutStockUploadPkg::dms_salOutStockEntry_select(dms_token = dms_token,FBillNo = FBillNo)

    tsui::run_dataTable2(id = 'salOutStock_resultView',data = data)

    tsui::run_download_xlsx(id = 'dl_saleOutStockEntry',data = data,filename='saleOutStockEntry.xlsx')



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
#' salOutStockServer()
salOutStockServer <- function(input, output, session, dms_token, erp_token) {
  salOutStockUploadServer(input = input, output = output, session = session, dms_token = dms_token, erp_token = erp_token)
  saleOutStockEntryUploadServer(input = input, output = output, session = session, dms_token = dms_token, erp_token = erp_token)



  salOutStockViewServer(input = input, output = output, session = session, dms_token = dms_token, erp_token = erp_token)
}
