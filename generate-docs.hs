#! /usr/bin/env runghc

import System.Process

main = createDocDir <> runHaddock <> changeIndex

createDocDir = callProcess "mkdir" ["docs", "--parents"]

runHaddock = callProcess "haddock" ["either-list-functions/src/Data/List/EitherFunctions.hs", "-o", "docs", "--html", "--hyperlinked-source"]

changeIndex = callProcess "mv" ["docs/Data-List-EitherFunctions.html", "docs/index.html", "--force"]
