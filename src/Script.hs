{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Script where

import qualified Data.Map.Strict as M
import qualified Data.Vector     as V
import qualified Data.Text       as T
import qualified Data.ByteString.Lazy as LB

import           Control.Exception
import           Control.Monad
import           Data.List (sort, group)
import           Data.Maybe

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Aeson as Aeson
import           Data.Aeson ((.:), (.:?), (.!=), (.=))
import qualified Data.Aeson.Encode.Pretty as Aeson

import Utils


-- | All the information about how a ELIZA bot should respond.
data Script = Script { reflections :: M.Map T.Text T.Text
                     , keywords    :: M.Map T.Text Keyword
                     , groups      :: M.Map T.Text (V.Vector T.Text)
                     , defaultSays :: V.Vector T.Text
                     , greetings   :: V.Vector T.Text
                     , goodbyes    :: V.Vector T.Text
                     }
  deriving (Show, Eq)

-- | Store all the necessary info for a given keyword
data Keyword = Keyword { kwWords      :: [T.Text]
                       , kwPrecedence :: Int
                       , kwRules      :: V.Vector Rule
                       , kwMemory     :: V.Vector Rule
                       }
  deriving (Show, Eq, Ord)

data Rule = Rule DRule (V.Vector RRule)
  deriving (Show, Eq, Ord)

getDecompRule :: Rule -> DRule
getDecompRule (Rule x _) = x

getMatchingRules :: Rule -> [MatchingRule]
getMatchingRules (Rule (DRule x) _) = x

getRecompRules :: Rule -> V.Vector RRule
getRecompRules (Rule _ x) = x

-- | The possible ways to match a text during 'Eliza.disassemble'.
data MatchingRule = MatchWord T.Text     -- ^ Exactly match a given word
                  | MatchMany            -- ^ Match zero or more words (consumes until the following rule matches)
                  | MatchN Int           -- ^ Match exactly n words
                  | MatchChoice [T.Text] -- ^ Match ony word in a given list
                  | MatchGroup T.Text    -- ^ Match any word pertaining to a certain group
  deriving (Show, Eq, Ord)

newtype DRule = DRule [MatchingRule]
  deriving (Show, Eq, Ord)

data ReassemblyRule = ReturnText T.Text -- ^ Return a text verbatim (including spaces)
                    | ReturnIndex Int   -- ^ Lookup the ith element on decomposed list and return it
  deriving (Show, Eq, Ord)

-- | Possible recomposition rules
data RRule = RRule [ReassemblyRule] -- ^ How to reassemble a text after decomposition
           | RKeyword T.Text        -- ^ Try decomposition rules for another keyword
           | RNewkey                -- ^ Continue looking into the keyword stack
  deriving (Show, Eq, Ord)

-- | Find a 'Keyword' by name.
findKeyword :: MonadPlus m => T.Text -> Script -> m Keyword
findKeyword w script = genericLookup (T.toLower w) (keywords script)

-- | Lookup the reflection text (if it exists).
findReflection :: MonadPlus m => T.Text -> Script -> m T.Text
findReflection w script = genericLookup (T.toLower w) (reflections script)

-- | Lookup all elements of a given group.
findGroup :: T.Text -> Script -> V.Vector T.Text
findGroup w script = maybe V.empty id
 (M.lookup (T.toLower w) (groups script))

-- | Test wheter a keyword has memory rules
isMemorizable :: Keyword -> Bool
isMemorizable = not . V.null . kwMemory

----------------------------
-- * Convert Text to Rules
----------------------------

readDRule :: T.Text -> DRule
readDRule input = case parse parserDRule "" input of
  Left e  -> error (errorBundlePretty e)
  Right r -> r

readRRule :: T.Text -> RRule
readRRule input = case parse parserRRule "" input of
  Left e  -> error (errorBundlePretty e)
  Right r -> r

parserDRule :: Parser DRule
parserDRule = fmap DRule parserMatchingRules

parserMatchingRules :: Parser [MatchingRule]
parserMatchingRules = space *> some rule
  where
   rule = choice [ MatchWord   <$> word
                 , MatchMany   <$  symbol "*"
                 , MatchN      <$> (char '#' *> positiveInteger)
                 , MatchChoice <$> brackets (some word)
                 , MatchGroup  <$> (char '@' *> word)
                 ]

parserRRule :: Parser RRule
parserRRule = parserRNewkey <|> parserRKeyword <|> fmap RRule parserReassemblyRules

parserRKeyword :: Parser RRule
parserRKeyword = RKeyword <$> (char '=' *> word <* eof)

parserRNewkey :: Parser RRule
parserRNewkey = RNewkey <$ (char ':' *> exactWord "newkey" <* eof)

parserReassemblyRules :: Parser [ReassemblyRule]
parserReassemblyRules = space *> some (returnIndex <|> returnText)
  where
   returnIndex = ReturnIndex <$> (char '$' *> L.decimal)
   returnText  = ReturnText . T.pack <$> some validChar
   validChar   = satisfy (/='$')

----------------------------
-- * Convert Rules to Text
----------------------------

textifyMatchingRule :: MatchingRule -> T.Text
textifyMatchingRule = \case
  MatchWord w    -> w
  MatchMany      -> "*"
  MatchN n       -> "#" <> T.pack (show n)
  MatchChoice ws -> "[" <> T.unwords ws <> "]"
  MatchGroup  g  -> "@" <> g

textifyReassemblyRule :: ReassemblyRule -> T.Text
textifyReassemblyRule = \case
  ReturnText  t -> t
  ReturnIndex n -> "$" <> T.pack (show n)

textifyDRule :: DRule -> T.Text
textifyDRule (DRule ms) = T.unwords (fmap textifyMatchingRule ms)

textifyRRule :: RRule -> T.Text
textifyRRule = \case
  RNewkey     -> ":newkey"
  RKeyword kw -> "=" <> kw
  RRule    rs -> T.concat (fmap textifyReassemblyRule rs)

-- | Turn a 'Script' into a human-readable JSON..
textifyScript :: Script -> LB.ByteString
textifyScript = Aeson.encodePretty' cfg
 where
  cfg = Aeson.defConfig
    { Aeson.confIndent = Aeson.Spaces 2
    , Aeson.confCompare = Aeson.keyOrder $
        ["greetings", "goodbyes", "default" ,"reflections", "groups", "keywords"]
        <> ["keyword", "precedence", "rules", "memory"]
        <> ["decomposition", "reassembly"]
    }

----------------------------
-- * Deal with JSON
----------------------------

-- | Exception to throw when loading a Script.
-- This signals that the supplied file exists but the json is incorrect.
data ScriptReadException = ScriptReadException FilePath String
    deriving Show

instance Exception ScriptReadException where
  displayException (ScriptReadException filename errmsg) =
       "Failed to load file " <> filename
    <> "\nError message: "    <> errmsg

-- | Read a JSON file containing an ELIZA script.
loadScript :: FilePath -> IO Script
loadScript filename = do
  json <- Aeson.eitherDecodeFileStrict' filename
  case json of
    Left  errmsg -> throwIO . ScriptReadException filename $ errmsg
    Right script -> pure script

instance Aeson.FromJSON Script where
 parseJSON = Aeson.withObject "Script" $ \ v -> do
   greetings   <- v .:  "greetings"
   goodbyes    <- v .:  "goodbyes"
   defaultSays <- v .:  "default"
   groups      <- v .:? "groups"      .!= M.empty
   reflections <- v .:? "reflections" .!= M.empty
   keywords    <- v .:  "keywords"
   pure $ Script
     { greetings   = greetings
     , goodbyes    = goodbyes
     , defaultSays = defaultSays
     , groups      = groups
     , reflections = reflections
     , keywords    = M.fromList . concatMap matchAliases $ (keywords :: [Keyword])
     }
  where
   -- | Create a List of pairs: (alias of a keyword, keyword)
   matchAliases k = [(T.toLower w, k) | w <- kwWords k]

instance Aeson.FromJSON Keyword where
 parseJSON = Aeson.withObject "Keyword" $ \ v -> do
   kword  <- fmap T.toLower <$> v .: "keyword"
   prec  <- v .:? "precedence" .!= 0
   rules <- v .:  "rules"
   memo  <- v .:? "memory"     .!= V.empty
   pure $ Keyword { kwWords      = kword
                  , kwPrecedence = prec
                  , kwRules      = rules
                  , kwMemory     = memo
                  }

instance Aeson.FromJSON Rule where
 parseJSON = Aeson.withObject "Rule" $ \ o -> do
   decomp  <- readDRule <$> o .: "decomposition"
   recomps <- fmap readRRule <$> o .: "reassembly"
   pure $ Rule decomp recomps

instance Aeson.ToJSON Script where
 toJSON Script {reflections, keywords, groups, defaultSays, greetings, goodbyes} =
   Aeson.object [ "reflections" .= reflections
                , "keywords"    .= toListOfKeywords keywords
                , "groups"      .= groups
                , "default"     .= defaultSays
                , "greetings"   .= greetings
                , "goodbyes"    .= goodbyes
                ]
  where toListOfKeywords kws = uniq . fmap snd . M.toList $ kws
        uniq = fmap head . group . sort

instance Aeson.ToJSON Keyword where
 toJSON Keyword {kwWords, kwPrecedence, kwRules, kwMemory} =
   Aeson.object [ "keyword"    .= kwWords
                , "precedence" .= kwPrecedence
                , "rules"      .= kwRules
                , "memory"     .= kwMemory
                ]

instance Aeson.ToJSON Rule where
 toJSON (Rule decomp recomps) =
   Aeson.object [ "decomposition" .= textifyDRule decomp
                , "reassembly"    .= fmap textifyRRule recomps
                ]


{- * Default Script -}
defaultScript :: Script
defaultScript = fromJust $ Aeson.decode defaultScriptJSON

defaultScriptJSON :: LB.ByteString
defaultScriptJSON = "{\"greetings\":[\"Hello, how are you feeling today?\",\"How do you do? Please tell me your problem\"],\"goodbyes\":[\"Goodbye\",\"Till next time\",\"Hope to see you again soon\"],\"default\":[\"I am not sure I understand you fully\",\"Please go on\",\"What does that suggest to you?\",\"Do you feel strongly about discussing such things?\"],\"reflections\":{\"dont\":\"don't\",\"cant\":\"can't\",\"wont\":\"won't\",\"i\":\"you\",\"am\":\"are\",\"were\":\"was\",\"dreamed\":\"dreamt\",\"dreams\":\"dream\",\"your\":\"my\",\"me\":\"you\",\"you're\":\"I am\",\"myself\":\"yourself\",\"yourself\":\"myself\",\"mom\":\"mother\",\"dad\":\"father\",\"my\":\"your\",\"you\":\"I\"},\"groups\":{\"belief\":[\"feel\",\"think\",\"believe\",\"wish\"],\"family\":[\"mother\",\"father\",\"sister\",\"brother\",\"wife\",\"children\"],\"noun\":[\"mother\",\"father\"]},\"keywords\":[{\"keyword\":[\"sorry\"],\"precedence\":0,\"rules\":[{\"decomposition\":\"*\",\"reassembly\":[\"Please don't apologize\",\"Apologies are not necessary\",\"What feelings do you have when you apologize?\",\"I've told you that apologies are not required\"]}]},{\"keyword\":[\"remember\"],\"precedence\":5,\"rules\":[{\"decomposition\":\"* you remember *\",\"reassembly\":[\"Do you often think of $4?\",\"Does thinking of $4 bring anything else to mind?\",\"What else do you remember?\",\"Why do you remember $4 just now?\",\"What in the present situation reminds you of $4?\",\"What is the connection between me and $4?\"]},{\"decomposition\":\"* do i remember *\",\"reassembly\":[\"Did you think I would forget $5?\",\"Why do you think I should recall $5 now?\",\"What about $5?\",\"=what\",\"You mentioned $5\"]},{\"decomposition\":\"*\",\"reassembly\":[\":newkey\"]}]},{\"keyword\":[\"if\"],\"precedence\":3,\"rules\":[{\"decomposition\":\"* if *\",\"reassembly\":[\"Do you think it's likely that $3?\",\"Do you wish that $3?\",\"What do you think about $3?\",\"Really, $2 $3?\"]}]},{\"keyword\":[\"dreamt\",\"dreamed\"],\"precedence\":4,\"rules\":[{\"decomposition\":\"* you dreamt *\",\"reassembly\":[\"Really, $4?\",\"Have you ever fantasied $4 while you were awake?\",\"Have you dreamt $4 before?\",\"=dream\",\":newkey\"]}]},{\"keyword\":[\"dream\",\"dreams\"],\"precedence\":3,\"rules\":[{\"decomposition\":\"*\",\"reassembly\":[\"What does that dream suggest to you?\",\"Do you dream often?\",\"What people appear in your dreams?\",\"Don't you believe that dream has something to do with your problem?\",\":newkey\"]}]},{\"keyword\":[\"perhaps\",\"maybe\"],\"precedence\":0,\"rules\":[{\"decomposition\":\"*\",\"reassembly\":[\"You don't seem quite certain\",\"Why the uncertain tone?\",\"Can't you be more positive?\",\"You aren't sure...\",\"Don't you know?\"]}]},{\"keyword\":[\"name\"],\"precedence\":15,\"rules\":[{\"decomposition\":\"*\",\"reassembly\":[\"I am not interested in names\",\"I've told you before, I don't care about names --- Please continue\"]}]},{\"keyword\":[\"xfremd\",\"deutsch\",\"francais\",\"italiano\",\"espanol\"],\"precedence\":0,\"rules\":[{\"decomposition\":\"*\",\"reassembly\":[\"I am sorry, I speak only English\"]}]},{\"keyword\":[\"hello\"],\"precedence\":0,\"rules\":[{\"decomposition\":\"*\",\"reassembly\":[\"How do you do? Please state your problem\"]}]},{\"keyword\":[\"computer\",\"machine\",\"machines\",\"computers\"],\"precedence\":50,\"rules\":[{\"decomposition\":\"*\",\"reassembly\":[\"Do computers worry you?\",\"Why do you mention computers?\",\"What do you think machines have to do with your problem?\",\"Don't you think computers can help people?\",\"What about machines worries you?\",\"What do you think about machines?\"]}]},{\"keyword\":[\"am\"],\"precedence\":0,\"rules\":[{\"decomposition\":\"* are you *\",\"reassembly\":[\"Do you believe you are $4?\",\"Would you want to be $4?\",\"You wish I would tell you you are $4\",\"What would it mean if you were $4?\",\"=what\"]},{\"decomposition\":\"*\",\"reassembly\":[\"Why do you say 'am'?\",\"I don't understand that\"]}]},{\"keyword\":[\"are\"],\"precedence\":0,\"rules\":[{\"decomposition\":\"* are i *\",\"reassembly\":[\"Why are you interested in whether I am $4 or not?\",\"Would you prefer if I weren't $4?\",\"Perhaps I am $4 in your fantasies\",\"Do you sometimes think I am $4?\",\"=what\"]},{\"decomposition\":\"* are *\",\"reassembly\":[\"Did you think they might not be $3?\",\"Would you like it if they were not $3?\",\"What if they were not $3?\",\"Possibly they are $3\"]}]},{\"keyword\":[\"your\"],\"precedence\":0,\"rules\":[{\"decomposition\":\"* my *\",\"reassembly\":[\"Why are you concerned over my $3?\",\"What about your own $3?\",\"Are you worried about someone else's $3?\",\"Really, my $3?\"]}]},{\"keyword\":[\"was\",\"were\"],\"precedence\":2,\"rules\":[{\"decomposition\":\"* was you *\",\"reassembly\":[\"What if you were $4?\",\"Do you think you were $4?\",\"Were you $4?\",\"What would it mean if you were $4?\",\"What does '$4' suggest to you?\",\"=what\"]},{\"decomposition\":\"* you was *\",\"reassembly\":[\"Were you really?\",\"Why do you tell me you were $4 now?\",\"Perhaps I already knew you were $4\"]},{\"decomposition\":\"* was i *\",\"reassembly\":[\"Would you like to believe I was $4?\",\"What suggests that I was $4?\",\"What do you think?\",\"Perhaps I was $4\",\"What if I had been $4?\"]},{\"decomposition\":\"*\",\"reassembly\":[\":newkey\"]}]},{\"keyword\":[\"i\"],\"precedence\":0,\"rules\":[{\"decomposition\":\"* you [want need] *\",\"reassembly\":[\"What would it mean to you if you got $4?\",\"Why do you want $4?\",\"Suppose you got $4 soon\",\"What if you never got $4\",\"What would getting $4 mean to you?\",\"What does wanting $4 have to do with this discussion?\"]},{\"decomposition\":\"* you are * [happy elated glad better] *\",\"reassembly\":[\"How have I helped you to be $5?\",\"Has your treatment made you $5?\",\"What makes you $5 just now?\",\"Can you explain why you are suddenly $5?\"]},{\"decomposition\":\"* you was *\",\"reassembly\":[\"=was\"]},{\"decomposition\":\"* you @belief you *\",\"reassembly\":[\"Do you really think so?\",\"But you are not sure you $5\",\"Do you really doubt you $5?\"]},{\"decomposition\":\"* you * @belief * I *\",\"reassembly\":[\"=you\"]},{\"decomposition\":\"* you are *\",\"reassembly\":[\"Is it because you are $4 that you came to me?\",\"How long have you been $4?\",\"Do you believe it normal to be $4?\",\"Do you enjoy being $4?\"]},{\"decomposition\":\"* you [can't cannot] *\",\"reassembly\":[\"How do you know you can't $4?\",\"Have you tried?\",\"Perhaps you could $4 now\",\"Do you really want to be able to $4?\"]},{\"decomposition\":\"* you don't *\",\"reassembly\":[\"Don't you really $4?\",\"Why don't you $4?\",\"Do you wish to be able to $4?\",\"Does that trouble you?\"]},{\"decomposition\":\"* you feel *\",\"reassembly\":[\"Tell me more about such feelings\",\"Do you often feel $4?\",\"Do you enjoy feeling $4?\",\"Of what does feeling $4 remind you?\"]},{\"decomposition\":\"* you * i *\",\"reassembly\":[\"Perhaps in your fantasy we $3 each other\",\"Do you wish to $3 me?\",\"Do you $3 anyone else?\"]},{\"decomposition\":\"*\",\"reassembly\":[\"You say $1\",\"Can you elaborate on that?\",\"Do you say $1 for some special reason?\",\"That's quite interesting\"]}]},{\"keyword\":[\"you\"],\"precedence\":0,\"rules\":[{\"decomposition\":\"* i remind you of *\",\"reassembly\":[\"=dit\"]},{\"decomposition\":\"* i are *\",\"reassembly\":[\"What makes you think I am $4?\",\"Does it please you to believe I am $4?\",\"Do you sometimes wish you were $4?\",\"Perhaps you would like to be $4\"]},{\"decomposition\":\"* i * you\",\"reassembly\":[\"Why do you think I $3 you?\",\"You like to think I $3 you --- don't you\",\"What makes you think I $3 you?\",\"Really, I $3 you?\",\"Do you wish to believe I $3 you?\",\"Suppose I did $3 to you --- What would that mean?\",\"Does someone else believe I $3 you?\"]},{\"decomposition\":\"* i *\",\"reassembly\":[\"We were discussing you --- not me\",\"Oh, I $3\",\"You're not really talking about me, are you?\",\"What are your feelings now?\"]}]},{\"keyword\":[\"yes\",\"certainly\"],\"precedence\":0,\"rules\":[{\"decomposition\":\"*\",\"reassembly\":[\"You seem quite positive\",\"You are sure\",\"I see\",\"I understand\"]}]},{\"keyword\":[\"no\"],\"precedence\":0,\"rules\":[{\"decomposition\":\"*\",\"reassembly\":[\"Are you saying 'no' just to be negative?\",\"You are being a bit negative\",\"Why not?\",\"why 'no'?\"]}]},{\"keyword\":[\"my\"],\"precedence\":2,\"rules\":[{\"decomposition\":\"* your * @family *\",\"reassembly\":[\"Tell me more about your family\",\"Who else in your family $5?\",\"Your $4\",\"What else comes to mind when you think of your $4?\"]},{\"decomposition\":\"* your *\",\"reassembly\":[\"Your $3\",\"Why do you say your $3?\",\"Does that suggest anything else which belongs to you?\",\"Is it important to you that $1 your $3?\"]}],\"memory\":[{\"decomposition\":\"* your *\",\"reassembly\":[\"Let's discuss further why your $3\",\"Earlier you said your $3\",\"But your $3\",\"Does that have anything to do with the fact that your $3?\"]}]},{\"keyword\":[\"can\"],\"precedence\":0,\"rules\":[{\"decomposition\":\"* can i *\",\"reassembly\":[\"You believe I can $4, don't you?\",\"You want me to be able to $4\",\"Perhaps you would like to be able to $4 yourself\"]},{\"decomposition\":\"* can you *\",\"reassembly\":[\"Whether or not you can $4 depends on you more than me\",\"Do you want to be able to $4?\",\"Perhaps you don't want to $4\",\"=what\"]}]},{\"keyword\":[\"what\",\"how\",\"when\"],\"precedence\":0,\"rules\":[{\"decomposition\":\"*\",\"reassembly\":[\"Why do you ask?\",\"Does that question interest you?\",\"What is it you really want to know?\",\"Are such questions much on your mind?\",\"What answer would please you most?\",\"What do you think?\",\"What comes to your mind when you ask that?\",\"Have you asked such question before?\",\"Have you asked anyone else?\"]}]},{\"keyword\":[\"because\"],\"precedence\":0,\"rules\":[{\"decomposition\":\"*\",\"reassembly\":[\"Is that the real reason?\",\"Don't any other reasons come to mind?\",\"Does that reason seem to explain anything else?\",\"What other reasons might there be?\"]}]},{\"keyword\":[\"why\"],\"precedence\":0,\"rules\":[{\"decomposition\":\"* why don't i *\",\"reassembly\":[\"Do you believe I don't $5?\",\"Perhaps I will $5 in good time\",\"Should you $5 yourself?\",\"You want me to $5\",\"=what\"]},{\"decomposition\":\"* why can't you *\",\"reassembly\":[\"Do you think you should be able to $5?\",\"Do you want to be able to $5?\",\"Do you believe this will help you to $5?\",\"Have you any idea why you can't $5?\",\"=what\"]},{\"decomposition\":\"*\",\"reassembly\":[\"=what\"]}]},{\"keyword\":[\"everyone\",\"everybody\",\"nobody\",\"noone\"],\"precedence\":2,\"rules\":[{\"decomposition\":\"* [everyone everybody nobody noone] *\",\"reassembly\":[\"Really, $2?\",\"Surely not $2\",\"Can you think of anyone in particular?\",\"Who, for example?\",\"You are thinking of a very special person\",\"Who, may I ask?\",\"Some special perhaps\",\"You have a particular person in mind, don't you?\",\"Who do you think you're talking about?\"]}]},{\"keyword\":[\"always\"],\"precedence\":1,\"rules\":[{\"decomposition\":\"*\",\"reassembly\":[\"Can you think of a specific example?\",\"When?\",\"What incident are you thinking of?\",\"Really, always?\"]}]},{\"keyword\":[\"like\"],\"precedence\":10,\"rules\":[{\"decomposition\":\"* [am is are was] * like *\",\"reassembly\":[\"=dit\"]},{\"decomposition\":\"*\",\"reassembly\":[\":newkey\"]}]},{\"keyword\":[\"dit\",\"alike\",\"same\"],\"precedence\":0,\"rules\":[{\"decomposition\":\"*\",\"reassembly\":[\"In what way?\",\"What resemblance do you see?\",\"What does that similarity suggest to you?\",\"What other connections do you see?\",\"What do you suppose that resemblance means?\",\"What is the connection, do you suppose?\",\"Could there really be some connection?\",\"How?\"]}]}]}"
