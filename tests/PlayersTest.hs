{-# LANGUAGE OverloadedStrings #-}
module PlayersTest
    ( playersSpecs
    ) where

import TestImport
import Prelude

playersSpecs :: Spec
playersSpecs =
    ydescribe "players GET" $

        yit "loads the index and checks it looks right" $ do
            get PlayersR
            statusIs 200
            htmlAllContain "h1" "Hello"

            request $ do
                setMethod "POST"
                setUrl PlayersR
                addNonce
                fileByLabel "Choose a file" "tests/main.hs" "text/plain" -- talk about self-reference
                byLabel "What's on the file?" "Some Content"

            statusIs 200
            printBody
            htmlCount ".message" 1
            htmlAllContain ".message" "Some Content"
            htmlAllContain ".message" "text/plain"
