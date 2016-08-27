{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Text.Printf
import Data.List

letters :: [String]
letters =
  [ "A"
  , "B"
  , "C"
  , "D"
  , "E"
  , "F"
  , "G"
  ]

mk1 :: String -> String
mk1 = (++ "1")

mk2 :: String -> String
mk2 = (++ "2")

_sources :: [String] -> [String]
_sources = map mk1

_sinks :: [String] -> [String]
_sinks = map mk2

_drivers :: [String] -> [String]
_drivers = map (\x -> printf "(sink: %s) => %s" (mk2 x) (mk1 x))

_params :: [String] -> [String]
_params = map (\x -> printf "%s, %s" (mk1 x) (mk2 x))

type Printer = [String] -> String

printType :: Printer
printType xs =
  printf
    "function Cycle<%s>(main: ((sources: %s) => %s), drivers: %s, options: CycleOptions): CycleExecution<%s, %s>;"
    params
    sources
    sinks
    drivers
    sources
    sinks
  where
    params :: String = intercalate ", " $ _params xs
    sources :: String = printf "[%s]" $ intercalate ", " $ _sources xs
    sinks :: String = printf "[%s]" $ intercalate ", " $ _sinks xs
    drivers :: String = printf "[%s]" $ intercalate ", " $ _drivers xs

loop :: Printer -> [String] -> [String]
loop printer xs =
  if null xs
    then mempty
    else loop printer (init xs) ++ [printer xs]

main :: IO ()
main = mapM_ putStrLn $ loop printType letters
