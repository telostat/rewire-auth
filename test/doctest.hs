import Test.DocTest (doctest)


main :: IO ()
main =
  doctest
    [ "-XDeriveGeneric"
    , "-XGADTs"
    , "-XGeneralizedNewtypeDeriving"
    , "-XOverloadedStrings"
    , "-XQuasiQuotes"
    , "-XScopedTypeVariables"
    , "-XTemplateHaskell"
    , "-XTypeApplications"
    , "-isrc"
    , "src"
    ]
