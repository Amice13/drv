get_polling_stations <- function(id) {
  #' Get the list of polling stations of the specified district
  #'
  #' Returns a data frame with information about polling stations in the
  #' specified district.
  #'
  #' The data frame contains the following variables
  #'
  #' \itemize{
  #'   \item \strong{Region_Id} - a region identifier
  #'   \item \strong{PS_Area} - a district number
  #'   \item \strong{PS_Num} - a polling station number
  #'   \item \strong{PS_Type} - a polling station type
  #'   \item \strong{PS_Desc} - a polling station borders
  #'   \item \strong{PS_CommissionAdr} - a polling station address
  #'   \item \strong{PS_PlaceVotingAdr} - a polling station location
  #'   \item \strong{PS_PlaceVotingLocation} - a polling station voting address
  #'   \item \strong{PS_GeoData} - a polling station polygon (GeoJSON)
  #'   \item \strong{PS_GeoDVK} - coordinates of the polling station address
  #'   \item \strong{PS_GeoPG} - coordinates of the polling station voting address
  #'   \item \strong{PS_Size} - a polling station size
  #' }
  #'
  #' @param id a district number from \strong{Area_Num} in \strong{get_districts}
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
      <drv:GetPollingStations>
        <drv:PSParams>
          <drv:Area>REPLACE</drv:Area>
        </drv:PSParams>
      </drv:GetPollingStations>
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
  xml_list <- xml2::as_list(xml2::xml_find_all(xml_data, ".//d:PollingStation"))
  vectors_list <- lapply(xml_list, function (x) {
    x$PS_AddInformation <- NULL
    x
  })
  vectors_list <- lapply(vectors_list, function (x) {
    for (name in names(x)) {
      if (length(x[[name]]) == 0) x[[name]] <- NA
    }
    unlist(x)
  })
  data <- do.call(rbind.data.frame, vectors_list)
  names(data) <- names(xml_list[[1]])[!grepl("PS_AddInformation", names(xml_list[[1]]))]
  data
}
