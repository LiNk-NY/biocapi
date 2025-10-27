#' @name pkgMetadata
#'
#' @title Get Bioconductor Package Version, Type, and other metadata
#'
#' @description This set of functions queries the Bioconductor API to retrieve
#'   metadata information for a specified package, including its version and
#'   type.
#'
#' @inheritParams maintainerPkgs
#'
#' @param pkg `character(1)` The name of the Bioconductor package for which to
#'   retrieve the version.
#'
#' @returns `biocpkgversion`: A `character(1)` string representing the version
#'   of the specified Bioconductor package.
#'
#' @examplesIf interactive()
#' biocpkgversion(pkg = "BiocPkgTools")
#' @export
biocpkgversion <- function(
    api = .TEST_API_URL,
    pkg,
    version = BiocManager::version()
) {
    if (missing(pkg))
        stop("Argument 'pkg' is required.")

    stopifnot(
        isScalarCharacter(api),
        isScalarCharacter(pkg)
    )
    paste0(api, "package/version/", pkg) |>
        request() |>
        req_perform() |>
        resp_body_json(simplifyVector = TRUE) |>
        unlist() |>
        unname() |>
        package_version()
}

#' @rdname pkgMetadata
#'
#' @returns `biocpkgtype`: A `character(1)` string representing the type of the
#'   specified Bioconductor package.
#'
#' @examplesIf interactive()
#' biocpkgtype(pkg = "BiocPkgTools")
#' @export
biocpkgtype <- function(
    api = .TEST_API_URL,
    pkg,
    version = BiocManager::version()
) {
    if (missing(pkg))
        stop("Argument 'pkg' is required.")

    stopifnot(
        isScalarCharacter(api),
        isScalarCharacter(pkg)
    )
    paste0(api, "package/type/", pkg) |>
        request() |>
        req_perform() |>
        resp_body_json(simplifyVector = TRUE) |>
        unlist() |>
        unname()
}

#' @rdname pkgMetadata
#'
#' @importFrom BiocBaseUtils isCharacter
#'
#' @returns `biocpkgsversions`: A `data.frame` of the provided packages and
#' their versions within the specified Bioconductor version.
#'
#' @examplesIf interactive()
#' biocpkgsversions(pkgs = c("BiocPkgTools", "GenomicRanges"))
#' @export
biocpkgsversions <- function(
    api = .TEST_API_URL,
    pkgs,
    version = BiocManager::version()
) {
    stopifnot(
        isScalarCharacter(api),
        isCharacter(pkgs)
    )

    paste0(api, "packages/versions") |>
        request() |>
        req_headers(`Content-Type`="text/plain") |>
        req_body_raw(paste(pkgs, collapse=",")) |>
        req_perform() |>
        resp_body_json(simplifyVector=TRUE)
}

#' @rdname pkgMetadata
#'
#' @returns `biocpkgstypes`: A `data.frame` of the provided packages and their
#'   types within the specified Bioconductor version.
#'
#' @examplesIf interactive()
#' biocpkgstypes(pkgs = c("BiocPkgTools", "curatedTCGAData"))
#' @export
biocpkgstypes <- function(
    api = .TEST_API_URL,
    pkgs,
    version = BiocManager::version()
) {
    stopifnot(
        isScalarCharacter(api),
        isCharacter(pkgs)
    )
    paste0(api, "packages/types") |>
        request() |>
        req_headers(`Content-Type` = "text/plain") |>
        req_body_raw(paste(pkgs, collapse = ",")) |>
        req_perform() |>
        resp_body_json(simplifyVector = TRUE)
}
