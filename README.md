
# streetlightR

<!-- badges: start -->

[![R build
status](https://github.com/Metropolitan-Council/streetlightR/workflows/R-CMD-check/badge.svg)](https://github.com/Metropolitan-Council/streetlightR/actions)
<!-- badges: end -->

R wrapper for StreetLight Insights API.

IMPORTANT: This package is not affiliated with nor maintained by
[StreetLight Data](https://www.streetlightdata.com/). This package is
tool for StreetLight API users authored by StreetLight API users.

## Installation

``` r
remotes::install_github("Metropolitan-Council/streetlightR")
```

## StreetLight API

StreetLight offers a limited-feature API, which allows users to upload
zone sets, create analyses, check project status, and download results.
Contact StreetLight support to request an API key and quick start guide.

### API key management

Contact StreetLight Data support to request an API key. Use
`streetlight_api_key()` to save the key for future use.

``` r
streetlight_api_key(key = "9999999", install = TRUE)
```

## Contributing

We welcome contributions!

Please review the [CONTRIBUTING](.github/CONTRIBUTING.md) guide before
making a contribution.

Thanks to our contributors: [@ehesch](https://github.com/ehesch), and
[@eroten](https://github.com/eroten).

<a href="https://metrocouncil.org" target="_blank"><img src="man/figures/main-logo.png" style="margin-left: 50%;margin-right: 50%;">

<div>

</div>

</a>
