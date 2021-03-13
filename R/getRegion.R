# Set constants --------------------------------------

user_agent <- httr::user_agent("http://github.com/amice13/drv")
headers <- httr::add_headers('Content-Type'='text/xml;charset=UTF-8')
base_url <- "https://www.drv.gov.ua/ords/svc/personal/API/Opendata"
encoding <- "UTF-8"

region_soap <- "
<soap:Envelope xmlns:soap=\"http://www.w3.org/2003/05/soap-envelope\" xmlns:drv=\"http://www.drv.gov.ua/\">
  <soap:Header/>
  <soap:Body>
    <drv:GetRegionsService />
  </soap:Body>
</soap:Envelope>"

get_regions <- function() {
  #' Get the list of regions
  #'
  #' Returns a data frame with IDs, names, shorts name and central localities
  #' of Ukrainian regions
  #'
  #' @export

  result <- httr::POST(base_url, user_agent, headers, body = region_soap)
  xml_content <- httr::content(result, "text", encoding = encoding)
  xml_data <-xml2::read_xml(xml_content)
  xml_list <- xml2::as_list(xml2::xml_find_all(xml_data, ".//d:Region"))
  vectors_list <- lapply(xml_list, function (x) { unlist(x) })
  data <- do.call(rbind.data.frame, vectors_list)
  names(data) <- names(xml_list[[1]])
  data
}
