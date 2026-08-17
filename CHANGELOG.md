
# Changelog

All notable changes to the [edgeTransport](https://github.com/pik-piam/edgeTransport) model, incl. [mrtransport](https://github.com/pik-piam/mrtransport) and [reporttransport](https://github.com/pik-piam/reporttransport) packages, will be documented in this file.
The sections in this file correspond to release versions of the [remindmodel](https://github.com/remindmodel/remind/releases).
All respective package versions for a REMIND release can be found [here](https://github.com/remindmodel/remind/tree/master/renv/archive).
The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).
edgeTransport and accompanying packages use versioning reflecting MAJOR.MINOR.PATCH notation.

## [Unreleased]
#### `edgeTransport`
#### `mrtransport`
#### `reporttransport`

### changed

### added

- **CHANGELOG.md** [edgeTransport #414](https://github.com/pik-piam/edgeTransport/pull/414)

### removed

- **deprecated `gdxrrw` dependency** switched to read/write gdx functions based on `gamstransfer`[edgeTransport #413](https://github.com/pik-piam/edgeTransport/pull/413) [reporttransport #55](https://github.com/pik-piam/reporttransport/pull/55) [rmndt #9](https://github.com/pik-piam/rmndt/pull/9)

### fixed

### validated



## [[REMIND 3.7.0](https://github.com/remindmodel/remind/releases/tag/v3.7.0)] - 2026-07-15
#### `edgeTransport 3.18.0`
#### `mrtransport 1.4.1`
#### `reporttransport 0.16.4`

### changed

- **BET sales shares** 
  improved near-term representation [edgeTransport #405](https://github.com/pik-piam/edgeTransport/pull/405)

- **US and CAZ passenger demands** 
  added continuous growth to reflect later saturation [edgeTransport #407](https://github.com/pik-piam/edgeTransport/pull/407)

- **IND historical ES** 
  improved representation [mrtransport #55](https://github.com/pik-piam/mrtransport/pull/55) [edgeTransport #411](https://github.com/pik-piam/edgeTransport/pull/411)

- **IND truck fleet composition** improved representation of truck fleet and sizes [edgeTransport #401](https://github.com/pik-piam/edgeTransport/pull/401) [mrtransport #51](https://github.com/pik-piam/mrtransport/pull/51) [mrtransport #54](https://github.com/pik-piam/mrtransport/pull/54)


### added

- **fleet tracking 2-& 3-Wheeler**
  added technology-resolved tracking of vintages for 2W, 3W, analogous to LDVs, Busses and Trucks [edgeTransport #409](https://github.com/pik-piam/edgeTransport/pull/409) [reporttransport #50](https://github.com/pik-piam/reporttransport/pull/50)

- **iterativeEdgeTransport reloading of input data** 
  added functionality: starting from the second call in a REMIND run, unchanged edgeT input data is reloaded from the output folder [edgeTransport #408](https://github.com/pik-piam/edgeTransport/pull/408)


### removed

- **`mrcommons` dependency** was replaced by more clearly defined `mrcommonsenergy` in a larger restructuring effort in pik-piam packages [mrtransport #52](https://github.com/pik-piam/mrtransport/pull/52)


### validated

- **SSP2 NPi2025 / Mix2ICEban** 
  in-depth validation with regards to near-term realism and IEA projections
  

## [[REMIND 3.6.0](https://github.com/remindmodel/remind/releases/tag/v3.6.0)] - 2026-03-27
#### `edgeTransport 3.13.4`
#### `mrtransport 0.14.0`
#### `reporttransport 1.2.1`


## [[REMIND x.y.z](link to release tag)] - yyyy-mm-dd
#### `edgeTransport x.y.z`
#### `mrtransport x.y.z`
#### `reporttransport x.y.z`

### changed

### added

### removed

### fixed

### validated
