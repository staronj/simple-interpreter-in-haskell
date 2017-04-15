-- Jakub Staroń, 2017

module FormatString (format) where

import Data.Char

format :: String -> [String] -> String
format format_string strings = case format_string of
    '%' : '%' : format_string' -> '%' : format format_string' strings
    '%' : format_string' -> insert format_string' strings
    [] -> []
    c : format_string' -> c : (format format_string' strings)


insert :: String -> [String] -> String
insert format_string strings
    | null format_string = error "Expected digit or '%' after '%', readched end of string"
    | isDigit $ head format_string = (getElement strings $ head format_string) ++ (format (tail format_string) strings)
    | otherwise = error "Expected digit or '%' after '%', read " ++ [head format_string]

getElement :: [String] -> Char -> String
getElement xs digit = let i = digitToInt digit in
 case drop i xs of
   x:_ -> x
   [] -> error "Index of argument out of range!"
