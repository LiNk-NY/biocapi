.TEST_API_URL <- "http://127.0.0.1:8000/"

.SENTINEL_CHECK_RES <- data.frame(
    pkg = character(0L),
    author = character(0L),
    version = character(0L),
    git_last_commit = character(0L),
    git_last_commit_date = character(0L),
    pkgType = character(0L)
)

#' Bioconductor Package Build Report
#'
#' Queries the Bioconductor API to retrieve build reports for all packages
#'
#' @inheritParams maintainerPkgs
#'
#' @importFrom httr2 request req_perform resp_body_json
#' @importFrom BiocPkgTools biocMaintained
#' @importFrom BiocBaseUtils isScalarCharacter
#'
#' @return A `data.frame` with the build report information for all packages
#'   associated with the specified maintainer.
#'
#' @examplesIf interactive()
#' buildreport(main = "maintainer@bioconductor.org")
#' @export
buildreport <- function(
    api = .TEST_API_URL,
    main = "maintainer@bioconductor.org",
    version = BiocManager::version()
) {
    stopifnot(
        isScalarCharacter(api),
        isScalarCharacter(main)
    )
    pkgs <- maintainerPkgs(main = main, version = version)[["Package"]]
    pkgList <- lapply(pkgs, .checkres_pkg)
    do.call(rbind.data.frame, pkgList)
}

.checkres_pkg <- function(pkg) {
    message("Trying package: ", pkg)
    tryCatch({
        paste0(.TEST_API_URL, "checkResults/package/", pkg) |>
            request() |>
            req_perform() |>
            resp_body_json(simplifyVector = TRUE)
    }, error = function(e) {
        warning(conditionMessage(e))
        .SENTINEL_CHECK_RES
    })
}
