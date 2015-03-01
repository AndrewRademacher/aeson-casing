# aeson-casing
Tools to change the formatting of field names in Aeson instances.

The casing utilities allow you to specify how Aeson renders and parses
the field names in JSON messages. Snake, Camel, and Pascal case are all
supported. To include casing modifiers in Aeson, attach options to instances
of ToJSON and FromJSON.

```haskell
data Person = Person
    { personFirstName :: Text
    , personLastName  :: Text
    } deriving (Generic)

instance ToJSON Person where
    toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON Person where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase
```

The above code will produce JSON messages like the following...

```json
{
    "first_name": "John",
    "last_name": "Doe"
}
```
- - -
![Shared Acumen](http://images.sharedacumen.com/logo-github.png)
