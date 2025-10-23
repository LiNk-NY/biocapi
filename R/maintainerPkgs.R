#' Get Bioconductor packages maintained by a specific maintainer
#'
#' This function queries the Bioconductor API to retrieve a list of packages
#' maintained by a specified maintainer.
#'
#' @param api `character(1)` The base URL for the Bioconductor API (default is a
#'   local test server)
#'
#' @param main `character(1)` A regex string to search for in the `Author` and
#'   `Maintainer` fields in the `VIEWS` data
#'
#' @param version `package_version` The Bioconductor version to use for
#'   `biocMaintained` function corresponding to the list of packages associated
#'   with the given maintainer.
#'
#' @importFrom httr2 request req_perform resp_body_json
#' @importFrom BiocManager version
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
    api = .TEST_API_URL,
    main = "maintainer@bioconductor.org",
    version = BiocManager::version()
) {
    stopifnot(
        isScalarCharacter(api),
        isScalarCharacter(main),
        is.package_version(version) || is.character(version)
    )
    paste0(api, "views/", main) |>
        request() |>
        req_perform() |>
        resp_body_json(simplifyVector = TRUE)
}
