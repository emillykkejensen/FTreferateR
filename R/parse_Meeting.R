#' Parse Aktivitet from xml data
#'
#' @param xml_dataX XML data containing 'Aktivitet' from 'DagsordenPunkt'
#'
#' @keywords internal
parse_Aktivitet <- function(Aktivitet){

  AktivitetList <- Aktivitet$TaleSegment$MetaSpeechSegment
  AktivitetList <- append(AktivitetList, Aktivitet$Taler$MetaSpeakerMP)
  AktivitetList <- append(AktivitetList, list(TalerTitel = paste(unlist(Aktivitet$Taler$TalerTitel), collapse = " ")))
  AktivitetList <- append(AktivitetList, list(TaleType = unlist(Aktivitet$TaleType, use.names = FALSE)))

  tale <- lapply(Aktivitet$TaleSegment$TekstGruppe, function(tekst) unlist(tekst, use.names = FALSE))
  names(tale) <- NULL

  AktivitetList <- append(AktivitetList, list(Tale = list(tale = tale)))

  AktivitetList[sapply(AktivitetList, is.null)] <- NA_character_

  return(AktivitetList)

}

#' Parse DagsordenPunkt from xml data
#'
#' @param xml_dataX XML data containing 'DagsordenPunkt'
#'
#' @import data.table
#'
#' @keywords internal
parse_DagsordenPunkt <- function(xml_dataX){

  DagsordenPunkt <- xml_dataX$MetaFTAgendaItem
  DagsordenPunkt <- append(DagsordenPunkt, list(Dagsorden = list(unlist(xml_dataX$PunktTekst, use.names = FALSE))))
  DagsordenPunkt <- append(DagsordenPunkt, list(Rubrica = unlist(xml_dataX$Aktivitet$Rubrica, use.names = FALSE)))

  DagsordenPunkt[sapply(DagsordenPunkt, is.null)] <- NA_character_

  TaleN <- which(names(xml_dataX$Aktivitet) == "Tale")

  Aktivitet <- lapply(TaleN, function(x) parse_Aktivitet(xml_dataX$Aktivitet[[x]]))

  DagsordenPunkt$Aktivitet <- list(Aktivitet)

  return(DagsordenPunkt)

}

#' Parse FT xml data
#'
#' @param xml_data XML data
#' @param returnNested If TRUE (default) returns one data.table with all
#' data nested. If FALSE returns tree data.tables - Meeting, DagsordenPunkt
#' and Aktivitet.
#'
#' @import data.table
#'
#' @keywords internal
parse_Meeting <- function(xml_data, returnNested = TRUE){

  Meeting <- xml_data$MetaMeeting
  Meeting <- append(Meeting, list(Titel = unlist(xml_data$TitelGruppe$Titel, use.names = FALSE)))
  Meeting <- append(Meeting, list(UnderTitel = unlist(xml_data$TitelGruppe$UnderTitel, use.names = FALSE)))
  Meeting <- append(Meeting, list(Dagsorden = list(unlist(xml_data$DagsordenPlan, use.names = FALSE))))

  Meeting[sapply(Meeting, is.null)] <- NA_character_

  Meeting <- data.table::as.data.table(Meeting)

  Meeting[, id := paste0(ParliamentarySession, "_", MeetingNumber)]
  data.table::setcolorder(Meeting, "id")

  meetingid <- Meeting$id

  DagsordenPlan <- xml_data$DagsordenPlan

  dagsordenN <- which(names(xml_data) == "DagsordenPunkt")

  DagsordenPunkt <- lapply(dagsordenN, function(x) parse_DagsordenPunkt(xml_data[[x]]))
  DagsordenPunkt <- data.table::rbindlist(DagsordenPunkt, use.names = TRUE, fill = TRUE)

  DagsordenPunkt[, id := paste0(meetingid, "_", .I)]
  data.table::setcolorder(DagsordenPunkt, "id")

  DagsordenPunkt[, Aktivitet := list(lapply(1:nrow(DagsordenPunkt), function(x){

    tempDT <- data.table::rbindlist(DagsordenPunkt$Aktivitet[[x]], use.names = TRUE, fill = TRUE)
    tempDT[, `:=` (id = paste0(DagsordenPunkt$id[x], "_", .I))]
    data.table::setcolorder(tempDT, "id")

  }))]

  if(returnNested){

    Meeting[, DagsordenPunkt := list(list(DagsordenPunkt))]

    return(Meeting)

  } else {

    Aktivitet <- data.table::rbindlist(DagsordenPunkt$Aktivitet, use.names = TRUE, fill = TRUE)
    DagsordenPunkt[, Aktivitet := NULL]

    return(list(Meeting = Meeting, DagsordenPunkt = DagsordenPunkt, Aktivitet = Aktivitet))

  }

}
