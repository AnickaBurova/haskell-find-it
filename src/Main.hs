module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Word
import System.Environment
import Data.Char
import Numeric
import Codec.Binary.UTF8.String
import Data.List
import Anca.Text
import Anca.Pipe


main :: IO ()
main = do
    args <- getArgs
    stream <- BL.getContents
    parse args (BL.unpack stream) |> BL.pack |> BL.putStr


parse ["-x",text, colour] = find_it tag needle
                    where 
                        needle = readHexStr text
                        colourValue = readColour colour
                        tag = toTag needle colourValue
parse [text,colour] = find_it tag needle
                    where 
                        needle = map fromIntegral $ map ord text
                        colourValue = readColour colour
                        tag = toTag needle colourValue

toTag text colour = tag_spec ++ [fromIntegral count, colour]
                where count = length text

find_it :: [Word8] -> [Word8] -> [Word8] -> [Word8]
find_it tag needle haystack = concat $ map addtag $ map (\x -> (head x,needle `isPrefixOf` x)) $ filter (not.null) (tails haystack) 
                where 
                    addtag (x, True) = tag ++ [x]
                    addtag (x, False) = [x] 

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





