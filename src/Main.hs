module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Word
import System.Environment
import Data.Char
import Numeric
import Codec.Binary.UTF8.String
import Data.List
import Anca.Text

(|>) :: a -> (a -> b) -> b
x |> f = f x

main :: IO ()
main = do
    args <- getArgs
    stream <- BL.getContents
    parse args (BL.unpack stream) |> BL.pack |> BL.putStr


parse ["-x",text, colour] = find_it (readHexStr text) (readColour colour)
parse [text,colour] = find_it (map ord text |> map fromIntegral) (readColour colour)

readColour "black" = 0x30
readColour "red" = 0x31
readColour "green" = 0x32
readColour "yellow" = 0x33
readColour "blue" = 0x34
readColour "magenta" = 0x35
readColour "cyan" = 0x36
readColour "white" = 0x37
readColour num = read num


tag_spec :: [Word8] 
tag_spec = [255,1,254,127] 
tag_text :: [Word8] -> [Word8]
tag_text []  = [27,0x5b,0x31,0x3b,0x33,0x34,0x6d]
tag_text [col,_] = [27,0x5b,0x31,0x3b,0x33,col,0x6d]
reset_text = [27,0x5b,0x30,0x6d]

find_it [] _ l = l
find_it  _ _ [] = []
find_it f colour l@(x:xs)
    | isPrefixOf tag_spec l = tagf ++ (find_it f colour tagrest)
    | isPrefixOf f l = (tag_spec ++  [fromIntegral count,colour]) ++ found ++ (find_it f colour rest)
    | otherwise = x : (find_it f colour xs)
    where 
        count = length f
        (found, rest) = splitAt count l
        (tagf, tagrest) = splitAt 6 l
        




