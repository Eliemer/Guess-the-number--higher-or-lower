# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.1.0] - 2022-11-26
### Changed
- Rewrote all logic as an Elmish state machine

## [1.0.1] - 2022-11-11
### Added
- "Thank you for playing" message
### Changed
- Remove all mutability of state by using recursion.

## [1.0.0] - 2022-11-1
### Added
- Basic functionality of guessing game. Works, but very *mutable* and crude.
- Added `CHANGELOG.md` and `README.md`

### Fixed
- Instructions and feedback message were contradictory