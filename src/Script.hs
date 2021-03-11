{-# LANGUAGE OverloadedStrings #-}
module Script where

import qualified Data.Map.Strict as M
import qualified Data.Vector     as V
import qualified Data.Text       as T
import qualified Data.ByteString.Lazy as LB

import           Data.Maybe
import           Control.Arrow ((&&&))

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Aeson as Aeson
import           Data.Aeson ((.:))

import Utils


-- All the information about how a ELIZA bot should respond
data Script = Script { reflections :: M.Map T.Text T.Text
                     , keywords    :: M.Map T.Text Keyword
                     , defaultSays :: V.Vector T.Text
                     , greetings   :: V.Vector T.Text
                     , groups      :: M.Map T.Text [T.Text]
                     }
  deriving Show

-- Store all the necessary info for a given keyword
data Keyword = Keyword { kwWord       :: T.Text
                       , kwPrecedence :: Int
                       , kwRules      :: V.Vector Rule
                       }
  deriving Show

-- maybe I should delete it and use only keywords?
data Rule = Rule [MatchingRule] (V.Vector [ReassemblyRule])
  deriving Show

getDecompRule (Rule x _) = x
getRecompRules (Rule _ x) = x

-- The possible ways to match a text
data MatchingRule = MatchWord T.Text
                  | MatchAll
                  | MatchN Int
                  | MatchChoice [T.Text]
                  | MatchGroup T.Text
  deriving Show

data ReassemblyRule = ReturnText T.Text
                    | ReturnIndex Int
  deriving Show


loadScript :: FilePath -> IO Script
loadScript filename = do
  json <- LB.readFile filename
  pure $ case Aeson.eitherDecode json of
    Left  emsg   -> error $ "Failed to load file " <> filename
                          <> "\nError message: " <> emsg
    Right script -> script

findKeyword :: T.Text -> Script -> Maybe Keyword
findKeyword w script = M.lookup (T.toLower w) (keywords script)

findReflection :: T.Text -> Script -> Maybe T.Text
findReflection w script = M.lookup (T.toLower w) (reflections script)

{-
  Megaparsec Parsers
-}

readDecompRule :: T.Text -> [MatchingRule]
readDecompRule input = concat (parseMaybe parserMatchingRules input)

readReassemblyRule :: T.Text -> [ReassemblyRule]
readReassemblyRule input = concat (parseMaybe parserReassemblyRules input)

parserMatchingRules :: Parser [MatchingRule]
parserMatchingRules = space *> some rule
  where
   matchWord, matchAll, matchN, matchChoice, matchGroup :: Parser MatchingRule
   matchWord   = MatchWord   <$> word
   matchAll    = MatchAll    <$  symbol "*"
   matchN      = MatchN      <$> (char '#' *> positiveInteger)
   matchChoice = MatchChoice <$> brackets (some word)
   matchGroup  = MatchGroup  <$> (char '@' *> word)
   rule = choice [matchAll, matchN, matchChoice, matchGroup, matchWord]

parserReassemblyRules :: Parser [ReassemblyRule]
parserReassemblyRules = space *> some (returnIndex <|> returnText)
  where
   returnIndex, returnText :: Parser ReassemblyRule
   returnIndex = ReturnIndex <$> (char '$' *> L.decimal)
   returnText  = ReturnText . T.pack <$> some validChar
   validChar   = satisfy (not .  (=='$'))

{-
  Read from JSON
-}
instance Aeson.FromJSON Script where
 parseJSON = Aeson.withObject "Script" $ \ v -> do
   greetings   <- v .: "greetings"
   defaultSays <- v .: "default"
   groups      <- v .: "groups"
   reflections <- v .: "reflections"
   keywords    <- v .: "keywords"
   pure $ Script { greetings   = greetings
                 , defaultSays = defaultSays
                 , groups      = groups
                 , reflections = reflections
                 , keywords    = M.fromList (fmap (kwWord &&& id) keywords)
                 }

instance Aeson.FromJSON Keyword where
 parseJSON = Aeson.withObject "Keyword" $ \ v -> do
   word  <- T.toLower <$> v .: "keyword"
   prec  <- v .: "precedence"
   rules <- v .: "rules"
   pure $ Keyword word prec rules

instance Aeson.FromJSON Rule where
 parseJSON = Aeson.withObject "Rule" $ \ o -> do
   decomp <- readDecompRule <$> o .: "decomposition"
   recomps <- fmap readReassemblyRule <$> o .: "reassembly"
   pure (Rule decomp recomps)


{- Default Script -}
defaultScript :: Script
defaultScript = fromJust $ Aeson.decode defaultScriptJSON

defaultScriptJSON :: LB.ByteString
defaultScriptJSON = "{\"greetings\":[\"Hello, how are you feeling today?\",\"How do you do? Please tell me your problem\"],\"default\":[\"I am not sure I understand you fully\",\"Please go on\",\"What does that suggest to you?\",\"Do you feel strongly about discussing such things?\"],\"reflections\":{\"dont\":\"don't\",\"cant\":\"can't\",\"wont\":\"won't\",\"i\":\"you\",\"am\":\"are\",\"were\":\"was\",\"dreamed\":\"dreamt\",\"dreams\":\"dream\",\"your\":\"my\",\"me\":\"you\",\"you're\":\"I am\",\"myself\":\"yourself\",\"yourself\":\"myself\",\"mom\":\"mother\",\"dad\":\"father\",\"my\":\"your\",\"you\":\"I\"},\"groups\":{\"belief\":[\"feel\",\"think\",\"believe\",\"wish\"],\"family\":[\"mother\",\"father\",\"sister\",\"brother\",\"wife\",\"children\"],\"noun\":[\"mother\",\"father\"]},\"keywords\":[{\"keyword\":\"sorry\",\"precedence\":0,\"rules\":[{\"decomposition\":\"*\",\"reassembly\":[\"Please don't apologize\",\"Apologies are not necessary\",\"What feelings do you have when you apologize?\",\"I've told you that apologies are not required\"]}]},{\"keyword\":\"remember\",\"precedence\":5,\"rules\":[{\"decomposition\":\"* you remember *\",\"reassembly\":[\"Do you often think of $4?\",\"Does thinking of $4 bring anything else to mind?\",\"What else do you remember?\",\"Why do you remember $4 just now?\",\"What in the present situation reminds you of $4?\",\"What is the connection between me and $4?\"]},{\"decomposition\":\"* do i remember *\",\"reassembly\":[\"Did you think I would forget $5?\",\"Why do you think I should recall $5 now?\",\"What about $5?\",\"You mentioned $5\"]}]},{\"keyword\":\"if\",\"precedence\":3,\"rules\":[{\"decomposition\":\"* if *\",\"reassembly\":[\"Do you think it's likely that $3?\",\"Do you wish that $3?\",\"What do you think about $3?\",\"Really, $2 $3?\"]}]},{\"keyword\":\"dreamt\",\"precedence\":4,\"aliases\":[\"dreamed\"],\"rules\":[{\"decomposition\":\"* you dreamt *\",\"reassembly\":[\"Really, $4?\",\"Have you ever fantasied $4 while you were awake?\",\"Have you dreamt $4 before?\"]}]},{\"keyword\":\"dream\",\"precedence\":3,\"aliases\":[\"dreams\"],\"rules\":[{\"decomposition\":\"*\",\"reassembly\":[\"What does that dream suggest to you?\",\"Do you dream often?\",\"What people appear in your dreams?\",\"Don't you believe that dream has something to do with your problem?\"]}]},{\"keyword\":\"perhaps\",\"precedence\":0,\"aliases\":[\"maybe\"],\"rules\":[{\"decomposition\":\"*\",\"reassembly\":[\"You don't seem quite certain\",\"Why the uncertain tone?\",\"Can't you be more positive?\",\"You aren't sure...\",\"Don't you know?\"]}]},{\"keyword\":\"name\",\"precedence\":15,\"rules\":[{\"decomposition\":\"*\",\"reassembly\":[\"I am not interested in names\",\"I've told you before, I don't care about names --- Please continue\"]}]},{\"keyword\":\"xfremd\",\"precedence\":0,\"aliases\":[\"deutsch\",\"francais\",\"italiano\",\"espanol\"],\"rules\":[{\"decomposition\":\"*\",\"reassembly\":[\"I am sorry, I speak only English\"]}]},{\"keyword\":\"hello\",\"precedence\":0,\"aliases\":[],\"rules\":[{\"decomposition\":\"*\",\"reassembly\":[\"How do you do? Please state your problem\"]}]},{\"keyword\":\"computer\",\"precedence\":50,\"aliases\":[\"machine\",\"machines\",\"computers\"],\"rules\":[{\"decomposition\":\"*\",\"reassembly\":[\"Do computers worry you?\",\"Why do you mention computers?\",\"What do you think machines have to do with your problem?\",\"Don't you think computers can help people?\",\"What about machines worries you?\",\"What do you think about machines?\"]}]},{\"keyword\":\"am\",\"precedence\":0,\"aliases\":[],\"rules\":[{\"decomposition\":\"* are you *\",\"reassembly\":[\"Do you believe you are $4?\",\"Would you want to be $4?\",\"You wish I would tell you you are $4\",\"What would it mean if you were $4?\"]},{\"decomposition\":\"*\",\"reassembly\":[\"Why do you say 'am'?\",\"I don't understand that\"]}]},{\"keyword\":\"are\",\"precedence\":0,\"aliases\":[],\"rules\":[{\"decomposition\":\"* are i *\",\"reassembly\":[\"Why are you interested in whether I am $4 or not?\",\"Would you prefer if I weren't $4?\",\"Perhaps I am $4 in your fantasies\",\"Do you sometimes think I am $4?\"]},{\"decomposition\":\"* are *\",\"reassembly\":[\"Did you think they might not be $3?\",\"Would you like it if they were not $3?\",\"What if they were not $3?\",\"Possibly they are $3\"]}]},{\"keyword\":\"your\",\"precedence\":0,\"aliases\":[],\"rules\":[{\"decomposition\":\"* my *\",\"reassembly\":[\"Why are you concerned over my $3?\",\"What about your own $3?\",\"Are you worried about someone else's $3?\",\"Really, my $3?\"]}]},{\"keyword\":\"was\",\"precedence\":2,\"aliases\":[\"were\"],\"rules\":[{\"decomposition\":\"* was you *\",\"reassembly\":[\"What if you were $4?\",\"Do you think you were $4?\",\"Were you $4?\",\"What would it mean if you were $4?\",\"What does '$4' suggest to you?\"]},{\"decomposition\":\"* you was *\",\"reassembly\":[\"Were you really?\",\"Why do you tell me you were $4 now?\",\"Perhaps I already knew you were $4\"]},{\"decomposition\":\"* was i *\",\"reassembly\":[\"Would you like to believe I was $4?\",\"What suggests that I was $4?\",\"What do you think?\",\"Perhaps I was $4\",\"What if I had been $4?\"]}]},{\"keyword\":\"i\",\"precedence\":0,\"aliases\":[],\"rules\":[{\"decomposition\":\"* you [want need] *\",\"reassembly\":[\"What would it mean to you if you got $4?\",\"Why do you want $4?\",\"Suppose you got $4 soon\",\"What if you never got $4\",\"What would getting $4 mean to you?\",\"What does wanting $4 have to do with this discussion?\"]},{\"decomposition\":\"* you are * [happy elated glad better] *\",\"reassembly\":[\"How have I helped you to be $5?\",\"Has your treatment made you $5?\",\"What makes you $5 just now?\",\"Can you explain why you are suddenly $5?\"]},{\"decomposition\":\"* you was *\",\"reassembly\":[\"Tell me more\"]},{\"decomposition\":\"* you @belief you *\",\"reassembly\":[\"Do you really think so?\",\"But you are not sure you $5\",\"Do you really doubt you $5?\"]},{\"decomposition\":\"* you * @belief * I *\",\"reassembly\":[\"Tell me more\"]},{\"decomposition\":\"* you are *\",\"reassembly\":[\"Is it because you are $4 that you came to me?\",\"How long have you been $4?\",\"Do you believe it normal to be $4?\",\"Do you enjoy being $4?\"]},{\"decomposition\":\"* you [can't cannot] *\",\"reassembly\":[\"How do you know you can't $4?\",\"Have you tried?\",\"Perhaps you could $4 now\",\"Do you really want to be able to $4?\"]},{\"decomposition\":\"* you don't *\",\"reassembly\":[\"Don't you really $4?\",\"Why don't you $4?\",\"Do you wish to be able to $4?\",\"Does that trouble you?\"]},{\"decomposition\":\"* you feel *\",\"reassembly\":[\"Tell me more about such feelings\",\"Do you often feel $4?\",\"Do you enjoy feeling $4?\",\"Of what does feeling $4 remind you?\"]},{\"decomposition\":\"* you * i *\",\"reassembly\":[\"Perhaps in your fantasy we $3 each other\",\"Do you wish to $3 me?\",\"Do you $3 anyone else?\"]},{\"decomposition\":\"*\",\"reassembly\":[\"You say $1\",\"Can you elaborate on that?\",\"Do you say $1 for some special reason?\",\"That's quite interesting\"]}]},{\"keyword\":\"you\",\"precedence\":0,\"aliases\":[],\"rules\":[{\"decomposition\":\"* i remind you of *\",\"reassembly\":[\"Why do you think so?\"]},{\"decomposition\":\"* i are *\",\"reassembly\":[\"What makes you think I am $4?\",\"Does it please you to believe I am $4?\",\"Do you sometimes wish you were $4?\",\"Perhaps you would like to be $4\"]},{\"decomposition\":\"* i * you\",\"reassembly\":[\"Why do you think I $3 you?\",\"You like to think I $3 you --- don't you\",\"What makes you think I $3 you?\",\"Really, I $3 you?\",\"Do you wish to believe I $3 you?\",\"Suppose I did $3 to you --- What would that mean?\",\"Does someone else believe I $3 you?\"]},{\"decomposition\":\"* i *\",\"reassembly\":[\"We were discussing you --- not me\",\"Oh, I $3\",\"You're not really talking about me, are you?\",\"What are your feelings now?\"]}]},{\"keyword\":\"yes\",\"precedence\":0,\"aliases\":[\"certainly\"],\"rules\":[{\"decomposition\":\"*\",\"reassembly\":[\"You seem quite positive\",\"You are sure\",\"I see\",\"I understand\"]}]},{\"keyword\":\"no\",\"precedence\":0,\"aliases\":[],\"rules\":[{\"decomposition\":\"*\",\"reassembly\":[\"Are you saying 'no' just to be negative?\",\"You are being a bit negative\",\"Why not?\",\"why 'no'?\"]}]},{\"keyword\":\"my\",\"precedence\":2,\"aliases\":[],\"rules\":[{\"decomposition\":\"* your * @family *\",\"reassembly\":[\"Tell me more about your family\",\"Who else in your family $5?\",\"Your $4\",\"What else comes to mind when you think of your $4?\"]},{\"decomposition\":\"* your *\",\"reassembly\":[\"Your $3\",\"Why do you say your $3?\",\"Does that suggest anything else which belongs to you?\",\"Is it important to you that $2 $3?\"]}]},{\"keyword\":\"can\",\"precedence\":0,\"aliases\":[],\"rules\":[{\"decomposition\":\"* can i *\",\"reassembly\":[\"You believe I can $4, don't you?\",\"You want me to be able to $4\",\"Perhaps you would like to be able to $4 yourself\"]},{\"decomposition\":\"* can you *\",\"reassembly\":[\"Whether or not you can $4 depends on you more than me\",\"Do you want to be able to $4?\",\"Perhaps you don't want to $4\"]}]},{\"keyword\":\"what\",\"precedence\":0,\"aliases\":[\"how\",\"when\"],\"rules\":[{\"decomposition\":\"*\",\"reassembly\":[\"Why do you ask?\",\"Does that question interest you?\",\"What is it you really want to know?\",\"Are such questions much on your mind?\",\"What answer would please you most?\",\"What do you think?\",\"What comes to your mind when you ask that?\",\"Have you asked such question before?\",\"Have you asked anyone else?\"]}]},{\"keyword\":\"because\",\"precedence\":0,\"aliases\":[],\"rules\":[{\"decomposition\":\"*\",\"reassembly\":[\"Is that the real reason?\",\"Don't any other reasons come to mind?\",\"Does that reason seem to explain anything else?\",\"What other reasons might there be?\"]}]},{\"keyword\":\"why\",\"precedence\":0,\"aliases\":[],\"rules\":[{\"decomposition\":\"* why don't i *\",\"reassembly\":[\"Do you believe I don't $5?\",\"Perhaps I will $5 in good time\",\"Should you $5 yourself?\",\"You want me to $5\"]},{\"decomposition\":\"* why can't you *\",\"reassembly\":[\"Do you think you should be able to $5?\",\"Do you want to be able to $5?\",\"Do you believe this will help you to $5?\",\"Have you any idea why you can't $5?\"]}]},{\"keyword\":\"everyone\",\"precedence\":2,\"aliases\":[\"everybody\",\"nobody\",\"noone\"],\"rules\":[{\"decomposition\":\"* [everyone everybody nobody noone] *\",\"reassembly\":[\"Really, $2?\",\"Surely not $2\",\"Can you think of anyone in particular?\",\"Who, for example?\",\"You are thinking of a very special person\",\"Who, may I ask?\",\"Some special perhaps\",\"You have a particular person in mind, don't you?\",\"Who do you think you're talking about?\"]}]},{\"keyword\":\"always\",\"precedence\":1,\"aliases\":[],\"rules\":[{\"decomposition\":\"*\",\"reassembly\":[\"Can you think of a specific example?\",\"When?\",\"What incident are you thinking of?\",\"Really, always?\"]}]},{\"keyword\":\"like\",\"precedence\":10,\"aliases\":[],\"rules\":[{\"decomposition\":\"* [am is are was] * like *\",\"reassembly\":[\"In what way?\"]},{\"decomposition\":\"*\",\"reassembly\":[\"Tell me more\"]}]},{\"keyword\":\"dit\",\"precedence\":10,\"aliases\":[\"alike\",\"same\"],\"rules\":[{\"decomposition\":\"*\",\"reassembly\":[\"In what way?\",\"What resemblance do you see?\",\"What does that similarity suggest to you?\",\"What other connections do you see?\",\"What do you suppose that resemblance means?\",\"What is the connection, do you suppose?\",\"Could there really be some connection?\",\"How?\"]}]}]}"
