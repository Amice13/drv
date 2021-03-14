get_addresses <- function(id) {
  #' Get the list of addresses of the specified territory
  #'
  #' Returns a data frame with information about territories in the
  #' specified region.
  #'
  #' The data frame contains the following variables
  #'
  #' \itemize{
  #'   \item \strong{ATO_Id} - a territory identifier
  #'   \item \strong{Geon_Id} - a street id
  #'   \item \strong{Geon_Name} - a name of the street
  #'   \item \strong{Geon_OldNames} - old names of the street
  #'   \item \strong{Bld_ID} - a building identifier
  #'   \item \strong{Bld_Area} - a building area
  #'   \item \strong{Bld_PS} - a polling station which the building belongs to
  #'   \item \strong{Bld_Flats} - a number of flats in the building
  #'   \item \strong{Bld_Ind} - an address index
  #'   \item \strong{Bld_Korp} - an address block
  #'   \item \strong{Bld_Num} - an address number
  #' }
  #'
  #' @param id a territory identifier from \strong{ATO_Id} in \strong{get_territories}
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
      <drv:GetAdrReg>
        <drv:AdrRegParams>
          <drv:ATO_ID>REPLACE</drv:ATO_ID>
        </drv:AdrRegParams>
      </drv:GetAdrReg>
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
  xml_find <- xml2::xml_find_all(xml_data, ".//d:GEONIM")
  xml_res <- lapply(xml_find, function (datum) {
    general <- xml2::as_list(datum, recursive = F)
    Geon_Id <- general$Geon_Id[[1]]
    Geon_Name <- general$Geon_Name[[1]]
    if (length(general$Geon_OldNames) > 0) {
      Geon_OldNames <- general$Geon_OldNames
    } else {
      Geon_OldNames <- NA
    }
    datum <- xml2::xml_children(datum)
    builds_find <- xml2::xml_find_all(datum, ".//d:BUILD")
    builds_list <- lapply(xml2::as_list(builds_find), function(x) {
      x[["Geon_Id"]] <- Geon_Id
      x[["Geon_Name"]] <- Geon_Name
      x[["Geon_OldNames"]] <- Geon_OldNames
      x[["ATO_Id"]] <- id
      for (name in names(x)) {
        if (length(x[[name]]) == 0) x[[name]] <- NA
      }
      unlist(x)
    })
    if (length(builds_list) == 0) return(F)
    n <- names(builds_list[[1]])
    d <- as.data.frame(builds_list, stringsAsFactors = FALSE)
    d <- as.data.frame(t(d), stringsAsFactors = FALSE, row.names = F)
    names(d) <- n
    d
  })
  data <- do.call(rbind, xml_res)
  data
}
