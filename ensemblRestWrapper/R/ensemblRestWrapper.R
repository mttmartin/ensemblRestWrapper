

.get_request <- function(ext, query_list=NULL, server="http://rest.ensemblgenomes.org") {

	if (is.null(query_list)) {
		r <- httr::GET(paste(server, ext, sep = ""),httr::content_type("application/json"))
	} else {
		r <- httr::GET(paste(server, ext, sep = ""), httr::content_type("application/json"), query=query_list)
	}
	httr::stop_for_status(r)
	return(r)
}

.response_df <- function(r) {
	return(jsonlite::fromJSON(jsonlite::toJSON(httr::content(r))))
}

#' Get gene name from protein ID
#'
#' Retrieves a gene name from a provided protein ID using Uniprot.
#' @param id The protein id
#' @export
get_gene_id <- function(id) {
	uniprot_server <- "https://www.ebi.ac.uk"
	ext <-"/proteins/api/proteins?"
	query_list = list("size"="-1", "accession"=id)
	r <- .get_request(ext, query_list=query_list, server=uniprot_server)
	df <- .response_df(r)
	gene_ID <- as.character(unlist(df$gene)[names(unlist(df$gene)) == "olnNames.value"])
	return (gene_ID)
}

#' Get GO information
#'
#' Retrieves GO term info relating to a gene ID.
#' @param id The gene identifier you would like GO information for.
#' @export
get_GO_info <- function(id) {
	server <- "http://rest.ensemblgenomes.org"
	ext <- paste("/xrefs/id/", id, "?", sep="")
	query_list = list('external_db'='GO', 'all_levels'='1')
	r <- .get_request(ext, query_list=query_list, server=server)
	df <- .response_df(r)
	GOs <- unlist(df$primary_id)
	return (GOs)
}

#res <- get_GO_info(id="MAV_4441")
