module InternalUtils where
import Data.Word

foldrHead :: (a -> b -> b) -> b -> [a] -> Int -> b
foldrHead _ acc _ 0 = acc
foldrHead _ acc [] _ = acc
foldrHead f acc (x:xs) num = foldrHead f (f x acc) xs (num - 1)

word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral . toInteger
