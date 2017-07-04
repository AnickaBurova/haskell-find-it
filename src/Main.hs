module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Word
import System.Environment
import Data.Char
import Numeric
import Codec.Binary.UTF8.String
import Data.List
import qualified Numeric as N

readHexStr :: (Integral a) => String -> [a]
readHexStr [] = []
readHexStr (t:xs)
        | isSpace t = readHexStr xs
readHexStr (x:y:xs) = fst (head (N.readHex [x,y])) : readHexStr xs


main :: IO ()
main = do
    args <- getArgs
    stream <- BL.getContents
    BL.putStr.BL.pack $ parse args $ BL.unpack stream


parse ["-x",text, colour] = findIt tag needle
                    where
                        needle = readHexStr text
                        colourValue = readColour colour
                        tag = toTag needle colourValue
parse [text,colour] = findIt tag needle
                    where
                        needle = map (fromIntegral.ord) text
                        colourValue = readColour colour
                        tag = toTag needle colourValue

toTag text colour = tagSpec ++ [fromIntegral count, colour]
                where count = length text

findIt :: [Word8] -> [Word8] -> [Word8] -> [Word8]
findIt tag needle haystack = concatMap (addtag . (\x -> (head x,needle `isPrefixOf` x))) $ filter (not.null) (tails haystack)
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


tagSpec :: [Word8]
tagSpec = [255,1,254,127]
tagText :: [Word8] -> [Word8]
tagText []  = [27,0x5b,0x31,0x3b,0x33,0x34,0x6d]
tagText [col,_] = [27,0x5b,0x31,0x3b,0x33,col,0x6d]
{-resetText = [27,0x5b,0x30,0x6d]-}





