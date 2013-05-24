module Encoding.LDAPRead where
import Encoding.BER
import Encoding.LDAPDefs
import Control.Monad.Error
import qualified Codec.Binary.UTF8.String as Codec
import Data.Word

type ParseT a = Either String a

buildLDAP :: BERTree -> ParseT LDAPMessage
buildLDAP (Sequence ((IntNode num):dat:controls)) = do
    op <- buildProtocolOp dat
    controls <- buildControls controls
    return (num, op, controls)
    
buildLDAP _ = Left "Error: Wrong type of message" 

buildControls [] = return Nothing
buildControls controls = Left "Controls not supported"

buildProtocolOp :: BERTree -> ParseT ProtocolOp 
buildProtocolOp (ChoiceNode tag val) = buildProtocolOp' tag $ Sequence $ val
buildProtocolOp (RawData val pc tag) = buildProtocolOp' tag $ RawData val pc tag

buildProtocolOp _ = Left "Error: Wrong type of message"


buildProtocolOp' :: Int -> BERTree -> ParseT ProtocolOp 

--bind request
buildProtocolOp' 0 (Sequence (v:n:a:[])) = do
    version <- buildInt v
    name <- buildString n
    auth <- buildAuthentication a
    return $ BindRequest version name auth

-- unbind request
buildProtocolOp' 2 _ = return UnbindRequest

-- search request
buildProtocolOp' 3 (Sequence (obj:scope:alias:sizeL:timeL:typesO:filter:attr:[])) = do
    baseObject <- buildString obj
    searchScope <- liftM toEnum $ buildEnumerate scope 
    darefAliases <- liftM toEnum $ buildEnumerate alias 
    sizeLimit <- buildInt sizeL
    timeLimit <- buildInt timeL
    typesOnly <- buildBool typesO
    filter <- buildFilter filter
    attributes <- buildAttributeDescrList attr
    return $ SearchRequest baseObject searchScope darefAliases sizeLimit timeLimit typesOnly filter attributes

-- modify request
buildProtocolOp' 6 (Sequence (obj:changes:[])) = do
    baseObject <- buildString obj
    changeList <- buildModifyChanges changes
    return $ ModifyRequest baseObject changeList

-- add request
buildProtocolOp' 8 (Sequence (obj:attrs:[])) = do
    baseObject <- buildString obj
    attributes <- buildAttributes attrs
    return $ AddRequest baseObject attributes 

-- del request
buildProtocolOp' 10 (Sequence (obj:[])) = do
    baseObject <- buildString obj
    return $ DelRequest baseObject 

--buildProtocolOp' 0 (Sequence )= 

buildProtocolOp' op typ = Left $ "Operation " ++ show op ++ " not supported or wrong type" ++ show typ

buildInt :: BERTree -> ParseT Int
buildInt (IntNode i)= return i
buildInt _ = Left "Expected int type"

buildBool :: BERTree -> ParseT Bool
buildBool (BoolNode b) = return b
buildBool _ = Left "Expected bool type"

buildEnumerate :: BERTree -> ParseT Int
buildEnumerate (Enumerated e) = return e
buildEnumerate _ = Left "Expected enumerate type"

buildString :: BERTree -> ParseT String
buildString (OctetString str) = return $ Codec.decode str
buildString arg = Left $ "Expected string type got " ++ show arg

decodeString :: [Word8] -> ParseT String
decodeString str = return $ Codec.decode str

buildFilter :: BERTree -> ParseT Filter
buildFilter (ContextNode 0 rawData) = do
    case buildTree $ BERData rawData of
        (SetOf dat) -> do
            filters <- forM (dat) buildFilter
            return $ And filters
        _ -> Left "Got unexpected type instead AND set"

buildFilter (ContextNode 1 rawData) = do
    case buildTree $ BERData rawData of
        (SetOf dat) -> do
            filters <- forM dat buildFilter
            return $ Or filters
        _ -> Left "Got unexpected type instead OR set"

buildFilter (ContextNode 2 subtree) = do
    filter <- buildFilter $ buildTree $ BERData subtree
    return $ Not filter

buildFilter (ContextNode 3 subtree) = do
    attr <- buildAttrValAssert $ buildTree $ BERData  subtree
    return $ EqualityMatch attr

buildFilter (ContextNode 4 subtree) = do
    substrF <- buildSubstrFilter $ buildTree $ BERData subtree
    return $ SubstringsF substrF

buildFilter (ContextNode 5 subtree) = do
    attr <- buildAttrValAssert $ buildTree $ BERData subtree
    return $ GreaterOrEqual attr

buildFilter (ContextNode 6 subtree) = do
    attr <- buildAttrValAssert $ buildTree $ BERData subtree
    return $ LessOrEqual attr

buildFilter (ContextNode 7 subtree) = do
    attr <- decodeString subtree
    return $ Present attr

buildFilter (ContextNode 8 subtree) = do
    attr <- buildAttrValAssert $ buildTree $ BERData subtree
    return $ ApproxMatch attr

-- TODO
--buildFilter (ChoiceNode 9 subtree) = do
--    attr <- buildMatchRuleAssert subtree
--    return $ ExtensibleMatch attr

buildFilter _ = Left "Expected filter choice"


buildAttrValAssert :: BERTree -> ParseT AttributeValueAssertion
buildAttrValAssert (Sequence (descr:assertVal:[])) = do
    description <- buildString descr
    assertionValue <- buildString assertVal
    return (description, assertionValue)


buildAttrValAssert _ = Left "Expected Attribute Value Assertion" 

buildSubstrFilter :: BERTree -> ParseT SubstringFilter
buildSubstrFilter (Sequence (desc:substr)) = do
    descr <- buildString desc
    substrings <- forM substr buildSubstring
    return (descr, substrings)

buildSubstrFilter _ = Left "Expected Substring Filter Value" 


buildSubstring :: BERTree -> ParseT Substrings
buildSubstring (ContextNode 0 str) = do
    substring <- decodeString str
    return $ Initial substring

buildSubstring (ContextNode 1 str) = do
    substring <- decodeString str
    return $ Any substring

buildSubstring (ContextNode 2 str) = do
    substring <- decodeString str
    return $ Final substring

buildSubstring _ = Left "Expected Substring Value " 

--buildMatchRuleAssert :: BERTree -> ParseT MatchingRuleAssertion

buildAttrSelect :: BERTree -> ParseT AttributeDescriptionList
buildAttrSelect (Sequence tree) = forM tree buildString
buildAttrSelect _ = Left "Expected Substring Value " 

buildAuthentication :: BERTree -> ParseT AuthenticationChoice
buildAuthentication (ContextNode 0 value) = do
    str <- decodeString value
    return $ Simple str

buildAuthentication (ChoiceNode 1 (subtree:[])) = do
    val <- buildSaslCredentials subtree
    return $ Sasl val

buildAuthentication _ = Left "Expected Authentication Value " 

buildSaslCredentials :: BERTree -> ParseT SaslCredentials
buildSaslCredentials (Sequence (str:(OctetString octet):[])) = do 
    mechanism <- buildString str
    return (mechanism, Just octet)

buildSaslCredentials (Sequence (str:[])) = do 
    mechanism <- buildString str
    return (mechanism, Nothing)

buildSaslCredentials _ = Left "Expected Sasl Credentials" 

buildAttributeDescrList :: BERTree -> ParseT AttributeDescriptionList
buildAttributeDescrList (Sequence attr) = forM attr buildString

buildModifyChanges :: BERTree -> ParseT [(Operation, PartialAttribute)]
buildModifyChanges (Sequence xs) = forM xs buildModifyChanges'

buildModifyChanges _ = Left "Expected modify changes"

buildModifyChanges' :: BERTree -> ParseT (Operation, PartialAttribute)
buildModifyChanges' (Sequence (op:modif:[])) = do
    operation <- liftM toEnum $ buildEnumerate op
    modification <- buildPartialAttribute modif
    return (operation, modification)

buildModifyChanges' _ = Left "Expected modify change"
    
buildPartialAttribute :: BERTree -> ParseT PartialAttribute
buildPartialAttribute (Sequence (typ:(SetOf vals):[])) = do
    attrType <- buildString typ
    attrValues <- forM vals buildString
    return (attrType, attrValues)

buildPartialAttribute _ = Left "Expected partial attribute"

buildAttributes :: BERTree -> ParseT AttributeList

buildAttributes (Sequence attributes) = forM attributes buildPartialAttribute

buildAttributes _ = Left "Expected partial attribute"

