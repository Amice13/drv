get_districts <- function() {
  #' Get the list of election districts
  #'
  #' Returns a data frame with IDs, names, shorts name and central localities
  #' of Ukrainian regions.
  #'
  #' The data frame contains the following variables
  #'
  #' \itemize{
  #'   \item \strong{Region_Id} - a region identifier
  #'   \item \strong{Area_Num} - a district number
  #'   \item \strong{Area_Center} - a central locality of the district
  #'   \item \strong{Area_Desc} - borders of the district
  #'   \item \strong{Area_OvkAdr} - an address of the district election
  #'   election commission
  #'   \item \strong{Area_OvkLocation} - a place where the district election
  #'   election commission is located
  #' }
  #'
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
      <drv:GetAreas />
    </soap:Body>
  </soap:Envelope>"

  result <- httr::POST(base_url, user_agent, headers, body = soap)
  xml_content <- httr::content(result, "text", encoding = encoding)
  xml_data <-xml2::read_xml(xml_content)
  xml_list <- xml2::as_list(xml2::xml_find_all(xml_data, ".//d:Area"))
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
