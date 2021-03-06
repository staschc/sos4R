################################################################################
# Copyright (C) 2015 by 52 North                                               #
# Initiative for Geospatial Open Source Software GmbH                          #
#                                                                              #
# Contact: Andreas Wytzisk                                                     #
# 52 North Initiative for Geospatial Open Source Software GmbH                 #
# Martin-Luther-King-Weg 24                                                    #
# 48155 Muenster, Germany                                                      #
# info@52north.org                                                             #
#                                                                              #
# This program is free software; you can redistribute and/or modify it under   #
# the terms of the GNU General Public License version 2 as published by the    #
# Free Software Foundation.                                                    #
#                                                                              #
# This program is distributed WITHOUT ANY WARRANTY; even without the implied   #
# WARRANTY OF MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU #
# General Public License for more details.                                     #
#                                                                              #
# You should have received a copy of the GNU General Public License along with #
# this program (see gpl-2.0.txt). If not, write to the Free Software           #
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA or #
# visit the Free Software Foundation web page, http://www.fsf.org.             #
#                                                                              #
# Author: Daniel Nuest (daniel.nuest@uni-muenster.de)                          #
# Created: 2010-06-18                                                          #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
#                                                                              #
################################################################################

#
# Function extracts om:OM_Observation elements from sos:observationData elements.
#
parseObservation_2.0 <- function(obj, sos, verbose = FALSE) {
	
  obj <- obj[[om20OM_Observation]]
  
  .id <- xmlGetAttr(node = obj, name = "id",
			default = NA_character_)
	if(verbose) cat("[parseObservation]", .id, "\n")
	
  #TODO adjust the following OM 1.0 parsing functionality
  
	# 52N SOS only returns om:Observation with procedure ids xlink:href
	.procedure <- xmlGetAttr(node = obj[[omProcedureName]], name = "href",
			default = NA_character_)
	
	.observedProperty <- parsePhenomenonProperty(obj[[omObservedPropertyName]],
			sos = sos, verbose = verbose)
	
	if(!is.null(obj[[om20PhenomenonTimeName]])) {
		.phenomenonTime <- parseTime(obj = obj[[om20PhenomenonTimeName]],
				format = sosTimeFormat(sos = sos), verbose = verbose)
	} else {
		warning("om:phenomenonTime is mandatory in om:Observation, but is missing!")
		.phenomenonTime <- NULL
	}
	
	if(!is.null(obj[[omFeatureOfInterestName]])) {
		.featureOfInterest <- parseFOI(obj[[omFeatureOfInterestName]],
				sos = sos, verbose = verbose)
		if(!is.null(.featureOfInterest@href)){
		  if(verbose) cat("[trying to get referenced featureOfInterest]\n")
		  
		  foiList <- getFeatureOfInterest(sos = sos, featureOfInterest = .featureOfInterest@href)
		  # getFeatureOfInterest returns a list, but a request including a featureOfInterest id
		  #  should return only one featureOfInterest
		  if(length(foiList) > 0){
		    .featureOfInterest <- foiList[[1]]
		  } 
		}
		  
	} else {
		warning("om:featureOfInterest is mandatory in om:Observation, but is missing!")
		.featureOfInterest <- NULL
	}
	
	# result parser is exchangeable
	.resultParsingFunction <- sosParsers(sos)[[omResultName]]
	.result <- .resultParsingFunction(obj[[omResultName]], sos, verbose)
	
	# optional elements
	if(!is.null(obj[[omResultTimeName]])) {
		.resultTime <- parseSamplingTime(obj = obj[[omResultTimeName]],
				format = sosTimeFormat(sos = sos), verbose = verbose)
	}
	else {
		.resultTime <- NULL
	}
	
	# TODO optionals elements for OmObservation
	#.metadata
	#.resultQuality
	#.parameter
	#.metadata
	
	.obs <- OmOM_Observation(phenomenonTime = .phenomenonTime,
			procedure = .procedure, observedProperty = .observedProperty,
			featureOfInterest = .featureOfInterest, result = .result)
	
	return(.obs)
}

#
# create according GmlTimeObject from om:samplingTime
#
parseTime <- function(obj, format, verbose = FALSE) {
  if(verbose) cat("[parseTime]\n")
  
  .tiXML <- xmlChildren(obj)[[gmlTimeInstantName]]
  .tpXML <- xmlChildren(obj)[[gmlTimePeriodName]]
  .timeObject <- NULL
  if(!is.null(.tiXML)) {
    if(verbose) cat("[parseTime] time instant.\n")
    .timeObject <- parseTimeInstant(obj = .tiXML, format = format)
  }
  else if(!is.null(.tpXML)) {
    if(verbose) cat("[parseTime] time period.\n")
    .timeObject <- parseTimePeriod(obj = .tpXML, format = format)
  }
  else {
    warning(paste("Could not create GmlTimeObject from given samplingTime,", 
                  " require gml:TimeInstant or gml:TimePeriod as children."))
    .timeObject <- GmlTimeInstant(timePosition = GmlTimePosition(
      time = as.POSIXct(x = NA)))
  }
  
  return(.timeObject)
}
