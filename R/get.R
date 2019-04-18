#' Adresse to the FT oda 'referater' FTP database
#'
#' @references \url{https://www.ft.dk/~/media/sites/ft/pdf/dokumenter/aabne-data/oda-browser_brugervejledning.ashx}
#'
#' @keywords internal
base_url <- "ftp://oda.ft.dk/ODAXML/Referat/samling/"


#' Get a list of all available parliamentary sessions
#'
#' @return A character vector of all available parliamentary sessions
#'
#' @importFrom RCurl getURL
#'
#' @export
getList_Session <- function(){

  filenames <- RCurl::getURL(base_url, ftp.use.epsv = FALSE, dirlistonly = TRUE)

  filenames <- strsplit(filenames, "\n")[[1]]

  return(filenames)

}


#' Get a list of all available parliamentary meetings
#'
#' @param Samling character; The name of the session(s)
#' you wish to get a list of meetings for. Use
#' \code{\link{getList_Session}} to get a full list of
#' all available sessions.
#'
#' @return A list of character vectors with all meetings
#' within the given session(s).
#'
#' @importFrom RCurl getURL
#'
#' @export
getList_Meeting <- function(Samling){

  SamlingList <- sapply(Samling, function(saml){

    url <- paste0(base_url, saml, "/")

    filenames <- RCurl::getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)

    filenames <- strsplit(filenames, "\n")[[1]]

    return(filenames)

  }, simplify = FALSE, USE.NAMES = TRUE)

  return(SamlingList)

}


#' Get minutes from the Danish Parliament
#'
#' @param MeetingList list; A list of meetings you wish
#' to get data on buildt using \code{\link{getList_Meeting}}.
#' @param return character; The form to return data in. See return for more.
#' @param verbose Tells you what is going on.
#'
#' @return Data can be returned in one of the following formats:
#'
#' \itemize{
#'   \item \strong{DTlist:} (default) A list containing three data.tables
#'   named Meeting, DagsordenPunkt and Aktivitet.
#'   \item \strong{DTnested:} A nested data.table, with colum
#'   'DagsordenPunkt' containing a data.table for each DagsordenPunkt,
#'   which again contains the column 'Aktivitet' containing a
#'   data.table for each Aktivitet
#'   \item \strong{XMLlist:} The raw XML data parsed as an R list
#'   \item \strong{XMLdata:} The raw XML data as an XMLInternalDocument
#' }
#'
#' @examples
#'
#' # Get a list of all available parliamentary sessions
#' FT_sessions <- getList_Session()
#'
#' # Get a list of all meetings in the third session
#' FT_meetings <- getList_Meeting(FT_sessions[3])
#'
#' # Return the parsed meeting minutes of that session as a list of data.tables
#' FT_data <- get_MeetingData(FT_meetings)
#'
#'
#' @importFrom XML xmlParse xmlToList
#'
#' @export
get_MeetingData <- function(MeetingList, return = "DTlist", verbose = FALSE){

  return <- match.arg(return, c("DTlist", "DTnested", "XMLlist", "XMLdata"))

  Samlinger <- names(MeetingList)

  MeetingListName <- gsub(".xml", "", unlist(MeetingList, use.names = FALSE))

  MeetingList <- lapply(seq(MeetingList), function(x) paste0(Samlinger[x], "/", MeetingList[[x]]))
  MeetingList <- unlist(MeetingList, use.names = FALSE)

  names(MeetingList) <- MeetingListName

  MeetingData <- lapply(MeetingList, function(x){

    url <- paste0(base_url, x)

    no <- which(MeetingList == x)

    if(verbose) cat(paste0("(", no, "/", length(MeetingList), ") ", MeetingListName[no], " - download and parse XML data"), fill = return == "XMLdata")
    data <- XML::xmlParse(url)

    if(return != "XMLdata"){

      if(verbose) cat(" - convert xml data to R-like list", fill = return == "XMLlist")
      xml_data <- XML::xmlToList(data)

    }

    if(return %in% c("DTnested", "DTlist")){

      if(verbose) cat(" - parsing data to nested data.table", fill = TRUE)

      outputdata <- parse_Meeting(xml_data = xml_data, returnNested = TRUE)

      return(outputdata)

    } else {

      return(xml_data)

    }

  })

  if(return %in% c("XMLdata", "XMLlist")){

    return(MeetingData)

  }

  if(return %in% c("DTnested", "DTlist")) {

    MeetingDT <- data.table::rbindlist(MeetingData)

    if(return == "DTnested"){

      return(MeetingDT)

    }

    if(return == "DTlist"){

      DagsordenPunktDT <- data.table::rbindlist(MeetingDT$DagsordenPunkt, use.names = TRUE, fill = TRUE)
      MeetingDT[, DagsordenPunkt := NULL]

      AktivitetDT <- data.table::rbindlist(DagsordenPunktDT$Aktivitet, use.names = TRUE, fill = TRUE)
      DagsordenPunktDT[, Aktivitet := NULL]

      return(list(Meeting = MeetingDT, DagsordenPunkt = DagsordenPunktDT, Aktivitet = AktivitetDT))

    }

  }

}


globalVariables(c("id", "DagsordenPunkt", "Aktivitet", "ParliamentarySession", "MeetingNumber"))


