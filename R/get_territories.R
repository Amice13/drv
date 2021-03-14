get_territories <- function(id) {
  #' Get the list of territories of the specified region
  #'
  #' Returns a data frame with information about territories in the
  #' specified region.
  #'
  #' The data frame contains the following variables
  #'
  #' \itemize{
  #'   \item \strong{Region_Id} - a region identifier
  #'   \item \strong{ATO_Raj} - a territory neighborhood
  #'   \item \strong{ATO_Rad} - a territory local council name
  #'   \item \strong{ATO_Name} - a territory name
  #'   \item \strong{ATO_id} - a territory id
  #' }
  #'
  #' @param id a region identifier from \strong{Region_Id} in \strong{get_regions}
  #' @export

  # Assign constants
  user_agent <- httr::user_agent("http://github.com/amice13/drv")
  headers <- httr::add_headers('Content-Type'='text/xml;charset=UTF-8')
  base_url <- "https://www.drv.gov.ua/ords/svc/personal/API/Opendata"
  encoding <- "UTF-8"

  soap <- "
  <soap:Envelope xmlns:soap=\"http://www.w3.org/2003/05/soap-envelope\" xmlns:drv=\"http://www.drv.gov.ua/\">
    <soap:Header/>
    <soap:Body>
      <drv:GetATO>
        <drv:ATOParams>
          <drv:Reg_ID>REPLACE</drv:Reg_ID>
        </drv:ATOParams>
      </drv:GetATO>
    </soap:Body>
  </soap:Envelope>"
  soap <- gsub("REPLACE", id, soap)
  result <- httr::POST(base_url, user_agent, headers, body = soap)
  xml_content <- httr::content(result, "text", encoding = encoding)
  if (grepl("XXXXX|QUERRY_RESULT>-1", xml_content)) {
    warning("The provided ID is wrong!")
    return(FALSE)
  }
  xml_data <-xml2::read_xml(xml_content)
  xml_list <- xml2::as_list(xml2::xml_find_all(xml_data, ".//d:ATO"))
  vectors_list <- lapply(xml_list, function (x) {
    for (name in names(x)) {
      if (length(x[[name]]) == 0) x[[name]] <- NA
    }
    unlist(x)
  })
  data <- do.call(rbind.data.frame, vectors_list)
  names(data) <- names(xml_list[[1]])
  data
}
