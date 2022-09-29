-- | This module provides Rewire Auth application entrypoint.
module Main where

import Control.Monad (join)
import Data.Version (showVersion)
import qualified Options.Applicative as OA
import Paths_rewire_auth (version)
import Rewire.Auth.Web (runWebServer)


-- | Main entry point.
main :: IO ()
main = join $ OA.execParser cliProgramParserInfo


-- | CLI program information.
cliProgramParserInfo :: OA.ParserInfo (IO ())
cliProgramParserInfo =
  OA.info
    (OA.helper <*> parserProgramOptions)
    (OA.fullDesc <> OA.progDesc "rewire-auth" <> OA.header "Rewire Auth Server and Toolkit")


-- | Program options parser.
parserProgramOptions :: OA.Parser (IO ())
parserProgramOptions =
  OA.subparser $
    OA.command "version" (OA.info (pure $ putStrLn (showVersion version)) (OA.progDesc "Show version and exit"))
      <> OA.command "serve" (OA.info (runWebServer <$> optParsePort) (OA.progDesc "Run Web server"))
  where
    optParsePort = OA.option OA.auto (OA.long "port" <> OA.short 'p' <> OA.metavar "PORT" <> OA.value 8080 <> OA.help "Port to run Webserver on")
