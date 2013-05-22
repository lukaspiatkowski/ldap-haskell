module BER where
import Data.Bits
import Data.Word
import InternalUtils


newtype BERMessage = BERData { berData :: [Word8] } deriving (Eq, Ord, Show)
data BERClass = Universal | Application | ContextSpecific | Private deriving (Eq, Ord, Show, Enum)
data BERPC = Primitive | Constructed deriving (Eq, Ord, Show, Enum)
data BERTree = Sequence { berSequence :: [BERTree] } |
                  OctetString { berString :: [Word8] } |
                  IntNode { intValue :: Int } |
                  BoolNode { boolValue :: Bool } |
                  Enumerated { enumeratedValue :: Int } |
                  ChoiceNode { choiceValue :: Int, subtree :: [BERTree] } |
                  ContextNode { contextTag :: Int, contextValue :: [Word8] } |
                  RawData { rawValue :: BERMessage , rawPCValue :: BERPC, rawTagValue :: Int } |
                  EOFNode |
                  SetOf {berSet :: [BERTree] } |
                  ApplicationSpecific { tag :: Int, appSequence :: [BERTree] }
                  deriving (Eq, Ord, Show)


index :: Int -> BERMessage -> Word8
index i = (flip (!!) $ i) . berData

newMessage :: BERMessage
newMessage = BERData []

-- Returns size of message body needed to read (can return less)
sizeLeft :: BERMessage -> Maybe Int
sizeLeft dat = if currentLength < 2 then Just $ 2 - currentLength else sizeLeft' dat
    where
        currentLength = length $ berData dat

nonZeroOrNothing 0 = Nothing
nonZeroOrNothing x = Just x

-- Returns size by reading size from the content
sizeLeft' dat = if not $ hasExtendedSize dat
                    then
                        nonZeroOrNothing $ (getNormalSize dat + getHeaderSize dat) - currentLength
                    else
                        if (2 + getLengthOfSize dat) <= currentLength
                            then
                                nonZeroOrNothing $ (getExtendedSize dat + getHeaderSize dat) - currentLength
                            else
                                Just $ 2 + getLengthOfSize dat - currentLength
                    
    where
        currentLength = length $ berData dat

-- Check whether message has long size
hasExtendedSize :: BERMessage -> Bool
hasExtendedSize dat = testBit (index 1 dat) 7

-- Get size of message if message has short size
getNormalSize :: BERMessage -> Int
getNormalSize dat = word8ToInt $ index 1 dat

-- Get length of message size
getLengthOfSize :: BERMessage -> Int
getLengthOfSize dat = clearBit (word8ToInt $ index 1 dat) 7

-- Get size if message has long size
getExtendedSize :: BERMessage -> Int
getExtendedSize dat = foldrHead (\v -> \acc -> acc * 2 ^ 8 + (word8ToInt v)) 0 (drop 2 $ berData dat) (getLengthOfSize dat)

getHeaderSize :: BERMessage -> Int
getHeaderSize dat = if hasExtendedSize dat then 3 else 2

-- Returns length of content without header
getLength dat = if (getClass dat) == Universal && getTag dat == 0
                then (-1)
                else if hasExtendedSize dat then getExtendedSize dat else getNormalSize dat

-- Returns message class
getClass :: BERMessage -> BERClass
getClass dat = case shiftR (index 0 dat) 6 of
                    0 -> Universal
                    1 -> Application
                    2 -> ContextSpecific
                    3 -> Private

-- Returns message P/C
getPC :: BERMessage -> BERPC
getPC dat = case testBit (index 0 dat) 5 of
                False -> Primitive
                True -> Constructed

-- Returns message tag
getTag :: BERMessage -> Int
getTag dat =  word8ToInt $ ((31)::Word8) .&. (index 0 dat)

-- Transformation of message to ASN.1 tree
-- Test:: map (read . (++) "0x") $ words "30 0c 02 01 01 60 07 02 01 03 04 00 80 00" :: [Word8]
buildTree :: BERMessage -> BERTree
buildTree mes = buildSubTree mes' pc cl tag
    where
        len = getLength mes
        mes' = BERData $ take len $ drop (getHeaderSize mes) $ berData mes
        pc = getPC mes
        cl = getClass mes
        tag = getTag mes

buildTreeSequence :: [Word8] -> [BERTree]
buildTreeSequence [] = []
buildTreeSequence dat = if len <= (length dat) then (buildTree mes):(buildTreeSequence $ drop len dat) else []
    where
        mes = BERData dat
        len = getLength mes + getHeaderSize mes

messageToInt :: BERMessage -> Int
messageToInt mess = messageToInt' (berData mess)
    where
        messageToInt' dat = foldr (\v -> \acc -> acc * 2 ^ 8 + (word8ToInt v)) 0 dat

buildSubTree :: BERMessage -> BERPC -> BERClass -> Int -> BERTree
buildSubTree mes pc Universal 0 = EOFNode
buildSubTree mes pc Universal 1 = BoolNode $ (messageToInt mes) > 0
buildSubTree mes pc Universal 2 = IntNode $ (messageToInt mes)
buildSubTree mes pc Universal 4 = OctetString $ berData mes
buildSubTree mes pc Universal 10 = Enumerated $ (messageToInt mes)
buildSubTree (BERData []) pc Universal 16 = Sequence []
buildSubTree mes pc Universal 16 = if len > length (berData mes)
                                       then error "Transmission error, length is inappopriate"
                                       else Sequence $ (buildTree mes):(berSequence sequence)
    where
        sequence = buildSubTree mes' pc Universal 16
        len = (getLength mes) + (getHeaderSize mes)
        mes' = BERData $ drop (len) $ berData mes

buildSubTree (BERData []) pc Universal 17 = SetOf []
buildSubTree mes pc Universal 17 = if len > length (berData mes)
                                       then error "Transmission error, length is inappopriate"
                                       else SetOf $ (buildTree mes):(berSet sequence)
    where
        sequence = buildSubTree mes' pc Universal 17
        len = (getLength mes) + (getHeaderSize mes)
        mes' = BERData $ drop (len) $ berData mes

buildSubTree mes Constructed ContextSpecific tag = ChoiceNode tag $ buildTreeSequence $ berData mes
buildSubTree mes Primitive ContextSpecific tag = ContextNode tag $ berData mes

buildSubTree mes Constructed Application tag = ChoiceNode tag $ buildTreeSequence $ berData mes
buildSubTree mes pc Application tag = RawData mes pc tag
-- buildSubTree mes pc Application _ = buildSubTree mes pc Universal 16
-- should be

buildSubTree _ _ t tag = error $ "Error: This should not appear. Try to add new message types" ++ show t ++ " " ++ show tag ++ "\n"


-- WRITE BER

writeData :: BERClass -> BERPC -> Int -> [Word8] -> [Word8]
writeData clas pc tag dat = writeHeader clas pc tag : writeLength (length dat) ++ dat

writeHeader :: BERClass -> BERPC -> Int -> Word8
writeHeader cl pc tag =
    ((intToWord8 $ fromEnum cl .&. 3) `shift` 6) .|.
    ((intToWord8 $ fromEnum pc .&. 1) `shift` 5) .|.
    ((intToWord8 tag .&. 31))

writeLength :: Int -> [Word8]
writeLength len =
    if len < 2^7
        then [intToWord8 len]
        else ((intToWord8 $ length extLength ) .|. (2^7) ):extLength
    where
        extLength = writeInt len

writeInt :: Int -> [Word8]
writeInt 0 = []
writeInt len = writeInt (div len (2^8)) ++ [intToWord8 len]

writeTree (Sequence berSequence) = writeData Universal Constructed 16 (writeTreeSequence berSequence)
writeTree (SetOf berSequence) = writeData Universal Constructed 17 (writeTreeSequence berSequence)
writeTree (ApplicationSpecific tag seq) = writeData Application Constructed tag (writeTreeSequence seq)
writeTree (OctetString str) = writeData Universal Primitive 4 str
writeTree (IntNode int) = writeData Universal Primitive 2 (writeInt int)
writeTree (BoolNode int) = writeData Universal Primitive 2 (writeInt $ fromEnum int)
writeTree (Enumerated int) = writeData Universal Primitive 10 (writeInt int)
writeTree (ChoiceNode tag val) = writeData Universal Constructed tag (writeTreeSequence val)
writeTree (ContextNode tag val) = writeData ContextSpecific pc tag val
    where
        pc = if length val > 0 then Constructed else Primitive
writeTree (RawData val pc tag) = writeData Application pc tag (berData val)

writeTreeSequence :: [BERTree] -> [Word8]
writeTreeSequence = concat . map writeTree


