get_drv_entites <- function() {
  #' Get the list of organizations of Ukrainian State Register of Voters
  #'
  #' Returns a data frame with IDs of regions, IDs, names, numbers, addresses
  #' and contacts of entities of Ukrainian State Register of Voters
  #'
  #' The data frame contains the following variables
  #'
  #' \itemize{
  #'   \item \strong{Region_Id} - a region identifier
  #'   \item \strong{Organ_Id} - an entity identifier
  #'   \item \strong{Organ_Num} - an entity number
  #'   \item \strong{Organ_Name} - an entity name
  #'   \item \strong{Organ_Adr} - an entity address
  #'   \item \strong{Organ_Tel} - entity contacts
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
      <drv:GetOrgansService />
    </soap:Body>
  </soap:Envelope>"

  result <- httr::POST(base_url, user_agent, headers, body = soap)
  xml_content <- httr::content(result, "text", encoding = encoding)
  xml_data <-xml2::read_xml(xml_content)
  xml_list <- xml2::as_list(xml2::xml_find_all(xml_data, ".//d:Organ"))
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
