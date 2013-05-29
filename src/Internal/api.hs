
add :: LDAPDN -> AttributeList -> LDAPInternalState
add = undefined

remove :: LDAPDN -> LDAPInternalState
remove = undefined

modify :: LDAPDN -> [(Operation, PartialAttribute)] -> LDAPInternalState
modify = undefined

search :: LDAPDN -> SearchScope -> ([Attribute] -> Bool) -> LDAPInternalState
search = undefined
