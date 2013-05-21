module LDAPDefs where

import Data.Word as Word

type LDAPMessage = (MessageID, ProtocolOp, (Maybe Controls)) 

data ProtocolOp = BindRequest Int LDAPDN AuthenticationChoice |
                  BindResponse LDAPResult (Maybe [Int]) |
                  UnbindRequest |
                  SearchRequest LDAPDN SearchScope DerefAliases Int Int Bool Filter AttributeDescriptionList |
                  SearchResultEntry LDAPDN PartialAttributeList |
                  SearchResultDone LDAPResult |
                  SearchResultReference [LDAPURL] |
                  ModifyRequest LDAPDN [(Operation, PartialAttribute)] |
                  ModifyResponse LDAPResult |
                  AddRequest LDAPDN AttributeList |
                  AddResponse LDAPResult |
                  DelRequest LDAPDN |
                  DelResponse LDAPResult |
                  ModifyDNRequest LDAPDN RelativeLDAPDN Bool (Maybe LDAPDN) |
                  ModifyDNResponse LDAPResult |
                  CompareRequest LDAPDN AttributeValueAssertion |
                  CompareResponse LDAPResult |
                  AbandonRequest MessageID |
                  ExtendedRequest LDAPOID (Maybe [Int]) |
                  ExtendedResponse  LDAPResult (Maybe LDAPOID) (Maybe [Int]) deriving (Show)

type MessageID = Int

--maxInt INTEGER ::= 2147483647 -- (2^31 - 1) --


type LDAPOID = String

type LDAPDN = String

type RelativeLDAPDN = String

type AttributeType = String

type AttributeDescription = String

type AttributeDescriptionList = [AttributeDescription]

type AttributeValue = String

type AttributeValueAssertion = (AttributeDescription, AssertionValue)

type AssertionValue = String

type Attribute = PartialAttribute

type MatchingRuleId = String

data ResultCode = Success |
                  OperationsError |
                  ProtocolError |
                  TimeLimitExceeded |
                  SizeLimitExceeded |
                  CompareFalse |
                  CompareTrue |
                  AuthMethodNotSupported |
                  StrongAuthRequired |
                  ResultCode9Unused |
                  Referral |
                  AdminLimitExceeded |
                  UnavailableCriticalExtension |
                  ConfidentialityRequired |
                  SaslBindInProgress |
                  NoSuchAttribute |
                  UndefinedAttributeType |
                  InappropriateMatching |
                  ConstraintViolation |
                  AttributeOrValueExists |
                  InvalidAttributeSyntax |
                  --ResultCodes unused needed for enum correct handling
                  RCUn22 | RCUn23 | RCUn24 | RCUn25 | RCUn26 | RCUn27 | RCUn28 | RCUn29 | RCUn30 | RCUn31 | RCUn32 |
                  NoSuchObject |
                  AliasProblem |
                  InvalidDNSyntax |
                  RSUn35 |
                  AliasDereferencingProblem |
                  RSUn37 | RSUn38 | RSUn39 | RSUn40 | RSUn41 | RSUn42 | RSUn43 | RSUn44 | RSUn45 | RSUn46 | RSUn47 |
                  InappropriateAuthentication |
                  InvalidCredentials |
                  InsufficientAccessRights |
                  Busy |
                  Unavailable |
                  UnwillingToPerform |
                  RSUn55 | RSUn56 | RSUn57 | RSUn58 | RSUn59 | RSUn60 | RSUn61 | RSUn62 | RSUn63 |
                  LoopDetect |
                  NamingViolation |
                  ObjectClassViolation |
                  RSUn70 |
                  NotAllowedOnNonLeaf |
                  NotAllowedOnRDN |
                  EntryAlreadyExists |
                  ObjectClassModsProhibited |
                  AffectsMultipleDSAs |
                  Other deriving (Show, Enum)

type LDAPResult = (ResultCode, LDAPDN, String, (Maybe Referral))

type Referral = [LDAPURL]

type LDAPURL = String -- limited to characters permitted in URLs

type Controls = [Control]

type Control = (LDAPOID, Bool, (Maybe [Int]))

data AuthenticationChoice = Simple [Word.Word8] | Sasl SaslCredentials deriving (Show)

type SaslCredentials = (String, (Maybe [Word8]))

data SearchScope = BaseObject |
                   SingleLevel |
                   WholeSubtree deriving (Show, Enum)
                   
data DerefAliases = NeverDerefAliases |
                    DerefInSearching |
                    DerefFindingBaseObj |
                    DerefAlways deriving (Show, Enum)

data Filter = And [Filter] |
              Or [Filter] |
              Not Filter |
              EqualityMatch AttributeValueAssertion |
              SubstringsF SubstringFilter |
              GreaterOrEqual AttributeValueAssertion |
              LessOrEqual AttributeValueAssertion |
              Present AttributeDescription |
              ApproxMatch AttributeValueAssertion |
              ExtensibleMatch MatchingRuleAssertion deriving (Show)

data Substrings = Initial String |
                  Any String |
                  Final String deriving (Show)

type SubstringFilter = (AttributeDescription, [Substrings])

type MatchingRuleAssertion = ((Maybe MatchingRuleId), (Maybe AttributeDescription), AssertionValue, Bool) 

type PartialAttribute = (AttributeDescription, [AttributeValue])

type PartialAttributeList = [PartialAttribute]

        
data Operation = Add | Delete | Replace deriving (Show, Enum)

type AttributeList = [Attribute]

