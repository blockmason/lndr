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
