module Sequences where

import Data.Char (ord, chr)

-- | Returns the first argument if it is larger than the second,
-- the second argument otherwise
maxOf2 :: Int -> Int -> Int
maxOf2 x y
        | x > y     = x
        | otherwise = y

-- | Returns the largest of three Ints
maxOf3 :: Int -> Int -> Int -> Int
maxOf3 x y z = if (maxOf2 x y > maxOf2 y z) && (maxOf2 x y > maxOf2 x z)
        then maxOf2 x y
        else if (maxOf2 y z > maxOf2 x y) && (maxOf2 y z > maxOf2 x z)
                then maxOf2 y z
                else maxOf2 x z

-- | Returns True if the character represents a digit '0'..'9';
-- False otherwise
isADigit :: Char -> Bool
isADigit character
        | ord character >= 48 && ord character <= 57 = True
        | otherwise = False

-- | Returns True if the character represents an alphabetic
-- character either in the range 'a'..'z' or in the range 'A'..'Z';
-- False otherwise
isAlpha :: Char -> Bool
isAlpha character
        | (ord character >= 65 && ord character <= 90) || (ord character >= 97 && ord character <= 122) = True
        | otherwise = False

-- | Returns the integer [0..9] corresponding to the given character.
-- Note: this is a simpler version of digitToInt in module Data.Char,
-- which does not assume the precondition.
digitToInt :: Char -> Int
-- Pre: the character is one of '0'..'9'
digitToInt character = ord character - ord '0'

-- | Returns the upper case character corresponding to the input.
-- Uses guards by way of variety.
toUpper :: Char -> Char
toUpper character 
        | ord character >= 97 && ord character <= 122 = chr (ord character - 32)
        | otherwise = character

--
-- Sequences and series
--

-- | Arithmetic sequence
arithmeticSeq :: Double -> Double -> Int -> Double
arithmeticSeq = undefined

-- | Geometric sequence
geometricSeq :: Double -> Double -> Int -> Double
geometricSeq = undefined

-- | Arithmetic series
arithmeticSeries :: Double -> Double -> Int -> Double
arithmeticSeries = undefined

-- | Geometric series
geometricSeries :: Double -> Double -> Int -> Double
geometricSeries = undefined
