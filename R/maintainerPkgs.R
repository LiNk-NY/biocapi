#' Get Bioconductor packages maintained by a specific maintainer
#'
#' This function queries the Bioconductor API to retrieve a list of packages
#' maintained by a specified maintainer.
#'
#' @param main `character(1)` A regex string to search for in the `Author` and
#'   `Maintainer` fields in the `VIEWS` data
#'
#' @importFrom httr2 request req_perform resp_body_json
#' @importFrom BiocBaseUtils isScalarCharacter
#'
#' @returns A `data.frame` with the list of packages maintained by the specified
#'   maintainer. The data frame contains columns such as `Package`, `Version`,
#'   `Author`, and `Maintainer`
#'
#' @examplesIf interactive()
#' maintainerPkgs(main = "maintainer@bioconductor.org")
#' @export
maintainerPkgs <- function(
    main = "maintainer@bioconductor.org"
) {
    stopifnot(
        isScalarCharacter(main)
    )
    req_url <- paste0(.TEST_API_URL, "views/", main)
    res <- request(req_url) |>
        req_perform() |>
        resp_body_json()
    return(
        do.call(rbind.data.frame, res)
    )
}
