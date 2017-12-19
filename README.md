# LNDR

Server, CLI, and UCAC Solidity contracts.

[![Build
Status](https://travis-ci.org/blockmason/lndr.svg?branch=master)](https://travis-ci.org/blockmason/lndr)

## API

Dev server answering requests at `http://34.202.214.156:80`
Production server answering requests at `http://34.238.20.130`

## Pre-install on Mac

Once XCode and Homebrew are installed, run:
```
curl -sSL https://get.haskellstack.org/ | sh
brew install automake
brew install libtool
brew install postgres
```

## Install

Once you have [stack](https://github.com/commercialhaskell/stack) installed, run the following commands:

```
stack setup
stack build
stack install
```

For `stack install` to register binaries properly, you must have `.local/bin/`
on your `PATH`.

## lndr-backend

[lndr-backend README](lndr-backend/README.md)

## lndr-cli

[lndr-cli README](lndr-cli/README.md)

## UCAC contract

LNDR Contract Address: `0x869a8f2c3D22Be392618Ed06C8F548D1D5b5aeD6`

[Lndr.sol README](ucac/README.md)
