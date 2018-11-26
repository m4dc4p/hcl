{- |

This module provides a set of functions for building simple command-line interfaces. It allows interfaces which collect values (such as Integers, Dates, or other structured values), build lists of values, and use simple menus. It is not intended to build complex interfaces with full cursor control. It is oriented towards line-based interfaces.

/Requests/

The central concept of the library is the 'Request' type, which embodies an interactive request for data. When requesting data, there is always the possibility of failure. That is, the user may enter a value that doesn't parse, or may want to quit the process. For this reason, the value stored by a request is @"IO" ("Maybe" a)@, which shows there may not always be a value available. 'Request' is a monad, and when a request fails, no subsequent requests are asked. Instead, the whole request chain is abandoned.

The function 'reqResp' gives the most basic request possible, which is for a string. From this, other requests can be built. The library provides several:

 * 'reqInt' - Requests "Int" values.

 * 'reqInteger' - Requests "Integer" values.

 * 'reqChar' - Requests a single character (without waiting for the user to press enter)

 * 'reqPassword' - Like 'reqResp', but doesn't echo the user's input to the console.

 * 'reqRead' - Requests "Read"-able values.

 * 'reqList' - Asks a request repeatedly and builds a list of the responses, which are returned when the user
    enters a failure value.

 * 'reqMenu' - Given a list of items, asks the user to pick one of the items and returns it.

 * 'reqFail' - Always results in failure. Useful in menus for creating a \"quit\" or \"none\" selection.

A number of request patterns are also exported by the module. These embody different control schemes that are useful when building command-line interfaces. These include:

  * 'reqIf' - Takes a request which evaluates to a boolean and two requests representing \"then\" and \"else\" branches. The appropriate subsequent request is used, based on the value of the conditional request.
  
  * 'reqAgree' - Takes a request and determines if the user answers yes or no. A default can also be provided.
  
  * 'reqForever' - Takes a request and asks it over and over, until a failure value appears.
  
  * 'reqIterate' - Takes a function which, given a value, produces a request. An initial 'Request' value is also provided. The initial value is given to the function, and the value produced by the function is fed back into it. This continues until a failure occurs. This model is useful for shell-type applications which take a state, operate on it, and produce a new state, which is then fed back in.
  
  * 'reqCont' - Takes a request and a \"continuation\" request. If the first request fails, the \"continuation\" request is run. This is useful for confirming if the user really wants to quit an application, or other escape mechanisms. 

/Running Requests/

Requests can be run with two different functions:

  * 'execReq' - Takes a request, runs it, and returns a meaningless value. This is most often used to run a request from \'main\'.
  
  * 'runRequest' - Runs a request and returns the raw @"IO" ("Maybe a")@ value returned. This is useful for   running a request and extracting the value returned out of it.
  
/Prompting/

In most req functions, except 'reqMenu' and 'reqChoices', nothing is printed to the screen. Instead, a set of functions is provided which take a request and a string to use as a prompt. These functions include:

  * 'prompt' - Displays a message and gets a response. If the message ends in a space, it is assumed that input should be typed on the same line. Otherwise, a newline is printed and input is then gathered.

  * 'prompt1' -- Simple way to ask for a response and provide a default.

  * 'promptAgree' -- Simple way to ask for a yes\/no response.

/Simple Programs/

Getting values combines prompting and requests. Here's a \'guess a number\' game which probably isn't real fun (from examples\\guess_num.hs):

> guess_num_boring =
>     do
>       num <- prompt "Enter your guess between 1 - 100: " reqInt
>       if num == 50
>         then reqIO $ putStrLn "You win!"
>         else reqIO $ putStrLn "Too bad!"

To run the program, type @play_game guess_num_boring@ at the prompt. A better program might actually randomize the number, and tell you if you are low or high (again from examples\\guess_num.hs):


> guess_num_fun =
>       do
>         target <- reqIO $ getStdRandom (randomR (1::Integer,100))
>         let guessed val =
>               case compare target val of
>                 GT -> do { reqIO $ putStrLn "Too low!"; return False }
>                 LT -> do { reqIO $ putStrLn "Too high!"; return False }
>                 EQ -> do { reqIO $ putStrLn "You win!"; return True }
>         reqUntil guessed (prompt "Enter a number between 1 and 100: " reqInteger)
> 
> play_game game = execReq game

To run the program, type @play_game guess_num_fun@ at the prompt. Several features of this program are worth pointing out:

  * 'reqIO' - This function is used to lift IO operations into the 'Request' type.
  
  * 'reqUntil' - This function takes a condition and a request, and runs the request until the condition is satisfied. The conditional has the type @(a -> Request b)@, which allows the conditional to produce output, or base its decision on other requests. Naturally, the second argument has the type @(Request a)@, which means the result of the request can be passed to the condition. Other functions which wrap up input patterns are 'reqFoldl', 'reqList', 'reqCont', and others.
  
/Combining Requests/

The functions in this library are designed to allow more complex 'Request' values to be built from them. For example, imagine you are coding for a tax form submission and have a data type like this (from examples\\taxpayer.hs):

>  data Taxpayer = Taxpayer { name :: String, age :: Int, ssn :: String }
>   deriving (Read, Show)

Because @Taxpayer@ derives "Read", a simple way of collecting a @Taxpayer@ value from the user would be:

> reqTaxpayer :: Request Taxpayer
> reqTaxpayer = prompt "Please enter tax payer information: " (reqRead reqResp)

Of course, this isn't very friendly:

> *Main> getTaxpayer reqTaxpayer
> Please enter tax payer information: Taxpayer {name="John", age = 30, ssn = "" }
> You entered: Taxpayer {name = "John", age = 30, ssn = ""}

Typing @Taxpayer { name = \"John\" ... }@ each time
is pretty tedious. A better solution builds the value from simpler pieces:

> reqTaxpayerEasy :: Request Taxpayer
> reqTaxpayerEasy =
>   do
>     name <- prompt "Please enter the tax payer's name: " reqResp
>     age <- prompt "Please enter their age: " reqInt
>     ssn <- prompt "What is their SSN/ASN: " reqResp
>     return (Taxpayer name age ssn)

Now, when tax payer info must be entered a nice set of prompts is displayed:

> *Main> getTaxpayer reqTaxpayerEasy
> Please enter the tax payer's name: Bob
> Please enter their age: 50
> Please enter their SSN/ASN: 111-11-1111
> You entered: Taxpayer {name = "Bob", age = 50, ssn = "111-11-1111"}

/Validation/

HCL provides the 'reqWhile' and 'reqUntil' functions which help ensure values entered are correct. For example, in the above, we could validate SSN's fairly easily like so (again, from example\\tax_payer.hs):

> reqSSN :: Request String -> Request String
> reqSSN req =
>   do
>     -- very simple validation
>     let
>       matchSSN = matchRegex (mkRegex "^...-..-....$")
>       invalidSSN ssn = return $ isNothing (matchSSN ssn)
>     ssn <- reqWhile invalidSSN req
>     return ssn

In the above, 'reqWhile' repeatedly uses @invalidSSN@ to determine if the value entered matches the (very simple) regular expression provided. When it does, the SSN entered is returned. Until then, the request is asked over and over. One subtlety to note is that a request to get the actual value is passed in to the function as @req@. This allows the function @reqTaxpayerValidate@ to pass it's own prompt and request into reqSSN:

> reqTaxpayerValidate :: Request Taxpayer
> reqTaxpayerValidate =
>   do
>     name <- prompt "Please enter the tax payer's name: " reqResp
>     age <- prompt "Please enter their age: " reqInt
>     ssn <- reqSSN (prompt "What is their SSN/ASN: " reqResp)
>     return (Taxpayer name age ssn)

Running @reqTaxpayerValidate@ from the prompt then gives:

> *Main> getTaxpayer reqTaxpayerValidate
> Please enter the tax payer's name: Bob
> Please enter their age: 20
> What is their SSN/ASN: 324=12=1231
> What is their SSN/ASN: 324-12-1211
> You entered: Taxpayer {name = "Bob", age = 20, ssn = "324-12-1211"}

/Dealing with Failure/

A fundamental assumption of the 'Request' type is that requests can fail. The user can enter no input or provide bad input. The discussion of validation above is a bit disingenuous because it does not mention what happens when the user just types a newline at the prompt. In all cases, the request chain ends and the program exits.

This is due to the behavior of the 'Request' monad - as soon as one request fails, the rest fail. The library provides several functions for dealing with this:

  * 'reqDefault' - Allows a default value to be supplied, which will be returned if the user provides no input or bad input.
  
  * 'required' - Repeatedly asks a request until the user provides input. \"Failure\" values will not occur.
  
  * 'reqCont' - Takes two request arguments. If the first fails, the second is used. Useful for providing a \"continuation\" to a request chain.
  
  * 'reqWhich' - Indicates if a request failed or not, through the use of the "Either" type. There is no direct way to determine if a request failed (that is, if it evaluates to Nothing, the entire chain fails and you won't see it). This function allows some visibility into if a specific request succeeded or not.

One use for 'reqCont' is to confirm if the user really wants to quit a program.  In the guess-a-number game, hitting Enter at a prompt stops the game. This can be avoided by changing how the guess a number game is launched:

> guess_num_cont =
>     reqCont guess_num_fun confirm
>   where
>     confirm =
>       reqIf (promptAgree "Are you sure you want to quit? " (Just False) reqResp)
>         reqFail
>         guess_num_cont

Above, 'reqCont' will run @guess_num_fun@ until it returns a @Just@ value. If @Nothing@ is returned, then @reqConfirm@ is run. If the user does not wish to quit, @reqConfirm@ will run @guess_num_confirm@ again. Otherwise, 'reqFail' is run, which causes the request to fail and thus the program to exit. Notice that the confirmation behavior was added by just adding another layer to the request chain. The @guess_num_fun@ function was used to provide gameplay - @guess_num_confirm@ just added a layer to control when the game ends.

However, because this pattern is fairly common, HCL provides the 'reqConfirm' function, which acts just like the 'reqCont' pattern above. That is, it takes a request to run and a request which returns a "Bool". If the initial request fails, the confirmation request is run. If that request results in @True@, the failure is allowed to propagate. Otherwise, the initial request is run again. The function @guess_num_confirm@ gives an example of its usage:

> guess_num_confirm =
>     reqConfirm confirm guess_num_fun 
>   where
>     confirm = promptAgree "Are you sure you want to quit? " (Just False) reqResp

  
/Making Menus/  

Several functions are used to build simple, hierarchical menus. A menu is defined as a list of pairs, where the first element is the label and the second a value to return. Usually, that value is a 'Request'. In some cases it is not. There are two functions used for building menus:

  * 'reqChoices' - A low-level means to build menus. It does not expect the second item in the pair to be a request, and is thus very general. 
  
  * 'reqMenu' - Expects the list given to be a pair of a string and another request. When an item is   selected, that request is run and the value is returned.
  
  * 'reqSubMenu' - Inserts a menu into a menu. When the item for the submenu is selected, the submenu will display its choices. When the user wishes to exit (by providing a failure value), the previously displayed menu will display again.
  
  * 'reqMenuItem' - Constructs an indvidual menu item.
  
  * 'reqMenuEnd' - Indicates the end of a list of menu items.
  
  * 'reqMenuExit' - A specialized menu item which will cause the menu request to fail. That means we return to the previous menu or exit the request chain altogether, depending on how the menus are structured.
  
'reqMenu' and 'reqSubMenu' work together to build hierarchical menus in which the user can automatically navigate \"up\" by just hitting return. For example, imagine a simple menu-driven PIM:

> *Main> pim
> 1. Manage contacts
> 2. Manage calendar
> ? 1
> 1. Add a contact
> 2. Remove a contact
> ? <-- User hits return here, returns to main menu
> 1. Manage contacts
> 2. Manage calendar
> ?

Setting this up is fairly straightforward (from examples\\pim.hs):

> pim = execReq $ reqConfirm confirm topMenu 
>   where
>     confirm = promptAgree "Are you sure you want to quit?" (Just False) reqResp
>     
> topMenu =
>   reqMenu $
>   -- Insert a submenu defined elsewhere
>   reqSubMenu topMenu "Manage contacts" manageContactsMenu $
>   -- Insert a sub menu directly
>   reqSubMenu topMenu "Manage calendar"
>     (reqMenuItem "Add an event" notImpl $
>       ...
>       reqMenuExit "Return to previous menu"
>       reqMenuEnd) $
>   ...
>   -- End the menu definition
>   reqMenuEnd
>   
> -- Defines a partial menu
> manageContactsMenu =
>   reqMenuItem "Add a contact" notImpl $
>   ...
>   reqMenuExit "Return to previous menu"
>   reqMenuEnd
> 
> notImpl = reqIO $ putStrLn "This function is not implemented."

'reqMenu' begins the process of definining a menu. 'reqMenuItem' is used to build a menu item, and when combined with @($)@ as above can be used to define a list of menu items \"in-line\". 'reqSubMenu' takes the menu to return to as its first argument (in the case above, @topMenu@), a label to name the menu item, and a request which will become the submenu. As seen above, submenus can be inserted directly (e.g. \"Manage calendar\"), or they can be defined independently (e.g. \"Manage contacts\"). 'reqMenuExit' allows the submenu to return to control to its calling menu. Finally, 'reqMenuEnd' can be used to end an \"in-line\" menu definition.

/Just Plain Cool/

Some of the other functions included are just cool to use:

  * 'reqIterate' - This take a function which maps a value to a request and a request. The request is evaluated and the results passed to the function. The result of that function is passed back into the function again. 'reqIterate' is useful for applications that manipulate some sort of environment by  repeatedly passing the modified environment back into themselves. An example of this is shown in  examples\\shell.hs where the @shell@ function is repeatedly called from @main@ using 'reqIterate'. The  hangman game in hangman\\hangman.hs also uses this when the @playRound@ function is repeatedly called from @main@.
  
  * 'reqFoldl' - Like @foldl@, but for requests. The accumulating function takes values of type a (which  come from the request given) and type b (the accumulating value) and produces a 'Request' of type b. If and when the initial request fails, whatever accumulated value that was built is returned.
  
  * 'reqList' - Takes a request and repeatedly runs it, building a list of the results. When the request  fails, the list is returned.
  
  * 'makeReq' - Not really so cool, but allows you to construct your own 'Request' values. Values created with 'makeReq' can be extracted with 'runRequest'. However, they will come back with the type @("IO" ("Maybe a")@, where the value is always a @Just@ value.

/Examples/

Several examples are included with the library, including a hangman game you can play:

  * examples\\guess_num.hs - Demonstrates various ways of implementing a "guess a number" game.
  
  * examples\\pim.hs - Shows how to build simple menus.
  
  * examples\\shell.hs - Shows how to use reqIterate to build a simple shell.
  
  * examples\\tax_payer.hs - Demonstrates how to construct requests for specific structured data types from simpler requests.
  
  * hangman\\hangman.hs - Implements the hangman game. An executable is installed when you install the library - just run @hangman@ at the command line.
  
-}

module System.Console.HCL 
(
-- * Request type and related functions
  Request,
  runRequest, execReq, reqIO, makeReq,
-- * Request building blocks
  reqResp, reqInteger, reqInt, reqRead, reqChar, reqPassword,
-- * Functions lifted into Requests
  andReq, orReq, notReq, reqIf, reqConst, reqLift, reqLift2,
  reqMaybe,
-- * Request patterns
  reqAgree, reqFail, required, reqUntil, reqWhile, reqDefault, reqForever,
  reqChoices, reqIterate, reqCont, reqConfirm, reqWhich, reqFoldl, 
  reqList, 
-- * Menus
  reqMenu, reqMenuItem, reqMenuEnd, reqSubMenu, reqMenuExit,
-- * Prompting
  prompt, promptWithDefault, prompt1, promptAgree
) where
 
import Data.Char (isSpace, toLower, isPrint)
import System.IO
import Test.QuickCheck 
import System.IO.Unsafe (unsafePerformIO)
import System.Random
import Data.Maybe (isNothing, isJust)
import Control.Monad (when)
import Control.Monad.Trans 

{- |
The @Request@ data type represents a value requested interactively. The
request may have failed or been no response, in which case the request
fails. Otherwise, the request holds the response given. -}
newtype Request a = Request (IO (Maybe a))

{- |
Runs a request, throws away the result, and
returns an @IO@ type (rather than a @Request@). Useful when a request
should just be run and we don't care about the result. Generally used at the top
level to evaluate a request in main. -}
execReq :: Request a -- ^ Request to run.
           -> IO () -- ^ No meaningful value is returned.
execReq (Request req) =
  do
    result <- req
    maybe (return ()) (\_ -> return ()) result

-- | Extracts the value from a given request.
runRequest :: Request a  -- ^ The request to evaluate.
              -> IO (Maybe a) -- ^ Result of the request.
runRequest (Request r) = r

{- |
Because we have defined @Request@ as @Applicative@,
we must also define it as @Functor@. -}
instance Functor Request where
  fmap = reqLift

{- |
Because we have defined @Request@ as @Monad@,
we must also define it as @Applicative@. -}
instance Applicative Request where
  pure = makeReq
  f <*> x = f `andMaybe` \f' ->
    fmap f' x

{- |
Request behavior as a @Monad@ covers failure - when
a request results in @Nothing@, all bind
operations fail afterwards. Thus, when one request fails,
all subsequent requests automatically fail. -}
instance Monad Request where
  f >>= g = f `andMaybe` g
  fail _ = reqFail

{- |
Takes a value and makes it into a request. Should
not be an @IO (Maybe a)@ type value, unless
multiply nested values is desired. -}
makeReq :: a -- ^ The value to turn into a Request.
           -> Request a -- ^ The value as a Request.
makeReq val = Request (return $ Just val)

{- |
If the request given results in @Nothing@, @Nothing@
is returned. Otherwise, the value held in the Just
constructor is passed to the "next" function given. This is essentially
the bind operation. -}
andMaybe :: Request a -- ^ Request to try.
            -> (a -> Request b) -- ^ Function which processes the result of the previous request and returns a new request.
            -> Request b -- ^ The new request returned.
andMaybe (Request req) next =
  Request $
  do
    v <- req
    case v of
        Nothing -> return Nothing
        Just x  -> nextReqVal
          where
            Request nextReqVal = next x

-- | Allow the Request type to use IO operations.
instance MonadIO Request where
  liftIO = reqIO

{- |
Allows @IO@ operations in the @Request@
type. Same as @liftIO@ in "MonadIO" class (in @Control.Monad.Trans@ module) -}
reqIO :: IO a -- ^ IO action to perform
         -> Request a -- ^ Result of the IO action, as a Request.
reqIO io = Request ioVal
  where
    ioVal =
      do
        val <- io
        return $ Just val

{- |
The basic request - get a string from the user. If a newline or all whitespace
is entered, the request is assumed to be a failure. -}
-- Read a string from the user.
reqResp :: Request String
reqResp =
  Request $
  do
    val <- getLine
    if all isSpace val
     then return Nothing
     else return $ Just val

{- |
Gets an "Integer" from the user. If the value entered cannot be converted,
the request fails. -}
reqInteger :: Request Integer
reqInteger = reqRead reqResp

{- |
Gets an "Int" from the user. If the value entered cannot be converted, the
request fails. -}
reqInt :: Request Int
reqInt = reqRead reqResp

{- |
Uses @reads@ to process a request. If the value cannot be parsed,
fails. Otherwise, returns the value parsed. -}
reqRead :: (Read a) => Request String -- ^ A request that returns a string (generally 'reqResp'), which will then be parsed.
           -> Request a -- ^ The value parsed.
reqRead req =
  req `andMaybe` \val ->
    Request $
    do
      case reads val of
        []          -> return Nothing
        ((v, _):[]) -> return $ Just v
        _           -> return Nothing

{- |
@reqChar@ requests a single character. Unlike other @Request@s, it
does not wait for the user to hit enter; it simply returns the first
keystroke. -}
reqChar :: Request Char
reqChar = Request $ do
  mode <- hGetBuffering stdin
  hSetBuffering stdin NoBuffering
  val <- getChar
  when (val /= '\n') $ putStrLn ""
  hSetBuffering stdin mode
  return $ Just val

{- |
@reqPassword@ works like 'reqResp' except that it does not echo the user's input to standard output. -}
reqPassword :: Request String
reqPassword = Request $ do
  echo <- hGetEcho stdin
  hSetEcho stdin False
  val <- runRequest reqResp
  putStrLn ""
  hSetEcho stdin echo
  return val

{- |
@&&@ operator for requests (with failure). Behaves similarly, including
"short-circuit" behavior. If either condition fails, the entire @Request@
fails. -}
andReq :: Request Bool -- ^ Left boolean value.
          -> Request Bool -- ^ Right boolean value.
          -> Request Bool -- ^ Result value.
andReq left right =
      left `andMaybe` \lb ->
      Request $
        case lb of
          False -> return $ Just False
          True  -> runRequest right   
          
{- |
@||@ operator for requests (with failure). Behaves similarly, including
"short-circuit" behavior. If either condition fails, the entire @Request@
fails. -}
orReq :: Request Bool -- ^ Left boolean value.
         -> Request Bool -- ^ Right boolean value.
         -> Request Bool -- ^ Result value.
orReq left right =
    left `andMaybe` \lb ->
    Request $ 
      case lb of
          True -> return (Just True)
          False -> runRequest right 


-- | not operator for requests.
notReq :: Request Bool -- ^ Request to evaluate.
          -> Request Bool -- ^ Result value.
notReq expr =
  expr `andMaybe` \nb ->
    Request $ return (Just $ not nb)

-- | If statement for requests. 
reqIf :: Request Bool -- ^ The test to apply
         -> Request a -- ^ Request to evaluate if test is true.
         -> Request a -- ^ Request to evaluate if test if false.
         -> Request a -- ^ Result.
reqIf test thenCase elseCase =
  test `andMaybe` \tb ->
    if tb
    then thenCase
    else elseCase

-- | Takes a value and makes it into a request. 
reqConst :: a -- ^ Value to make into a request.
            -> Request a -- ^ Result.
reqConst val = return val 

-- | Lifts a one-argument function into @Request@ types. 
reqLift :: (a -> b) -- ^ Function to lift.
           -> Request a -- ^ Argument to function.
           -> Request b -- ^ Result.
reqLift f req =
  do
    reqVal <- req
    return (f reqVal)

{- |
Lifts a two argument function into @Request@ types. The arguments to the function
are evaluated in order, from left to right, since the @Request@ monad imposes
sequencing. -}
reqLift2 :: (a -> b -> c) -- ^ Function to lift.
            -> Request a -- ^ First argument to function.
            -> Request b -- ^ Second argument to function.
            -> Request c -- ^ Result.
reqLift2 f left right =
  do
    leftVal <- left
    rightVal <- right
    return (f leftVal rightVal)

{- |
Returns true if the user answer @y@ or @Y@. Allows
a default to be specified, and allows failure if
no default is given. -}
reqAgree :: Maybe Bool -- ^ Default value (if any).
            -> Request String -- ^ Request which gets a string (usually reqResp).
            -> Request Bool -- ^ Result.
reqAgree def req = Request result
  where
    Request result = reqMaybe req (Request returnDefault) (Request . returnAgreement)
    returnDefault = return $ maybe Nothing (\d -> Just d) def
    returnAgreement resp =
      case clean resp of
          ('y':_) -> return $ Just True
          ('n':_) -> return $ Just False
          _ -> returnDefault
    clean = (map toLower) . filter (not . isSpace)

-- | Automatic failure. Useful in menus to quit or return to the previous menu.
reqFail :: Request a
reqFail = Request $ return Nothing

{- |
Takes a request and guarantees a value will be
returned. That is, the request is repeated until a
valid (i.e. not @Nothing@) response is returned. -}
required :: Request a -- ^ Request to evaluate.
            -> Request a -- ^ Result.
required (Request req) =
    Request required'
  where
    required' =
      do
        val <- req
        case val of
          Nothing -> required'
          Just v -> return (Just v)

{- |
Like the @maybe@ function, but for requests. Given a request value, 
a default value,and a function that maps @b@ to @Request a@,
this function either returns the default if the request value is nothing,
or it applies the function given to the value of the request and returns it.
-}
reqMaybe :: Request a -- ^ Request to evaluate.
            -> Request b -- ^ Default value.
            -> (a -> Request b) -- ^ Function to map b to Request a.
            -> Request b -- ^ Result.
reqMaybe (Request req) (Request def) fun =
  Request $
  do
    val <- req
    case val of
      Nothing -> def
      Just v -> nextReqVal
        where
          Request nextReqVal = fun v

{- |
Runs the request while the condition given holds,
then returns the result. Good for verification. -}
reqWhile :: (a -> Request Bool)
            -> Request a
            -> Request a
reqWhile cond req =
  do
    reqVal <- req
    testVal <- cond reqVal
    if testVal
      then reqWhile cond req
      else return reqVal
      
{- |
Runs the request until the condition given is satisfied,
then returns the result. -}
reqUntil :: (a -> Request Bool) -- ^ Condition to test.
            -> Request a -- ^ Request value to evaluate according to test.
            -> Request a -- ^ Result.
reqUntil cond req = reqWhile ((reqLift not) . cond) req
      
{- |
Requests a response from user. If @Nothing@ is returned,
assumes default and returns that. -}
reqDefault :: Request a -- ^ Request to evaluate.
              -> a -- ^ Default value.
              -> Request a -- ^ Result.
reqDefault req def =
  Request $ 
  do
    val <- runRequest req
    case val of
      Nothing -> return $ Just def
      v -> return v

{- |
Ask a request forever -- until failure. -}
reqForever :: Request a -- ^ Request to ask forever.
              -> Request a -- ^ Result.
reqForever req =
  req `andMaybe` \_ -> reqForever req

{- |
Given a list of items and programs to run, displays a menu
of the items and runs the selected program. Very low level - usually @reqMenu@
is used instead. If the user selects an invalid choice, failure occurs. -}
reqChoices :: [(String, a)] -- ^ List of choices and labels which will be selected from.
              -> Request Int -- ^ Request which gets the selection from the user.
              -> Request a -- ^ Result of selection.
reqChoices choices req =
  do
    let choiceCnt = length choices
        choiceList = zip [(1::Int)..] (map (\(label, _) -> label) choices)
    sequence (map (\(idx, label) -> reqIO $ putStrLn ((show idx) ++ ". " ++ label)) choiceList)
    idx <- prompt "? " req
    if idx < 1 || idx > length choices
      then reqFail
      else return (snd (choices !! (idx - 1)))

{- |
Takes a list of strings and requests and forms a menu out of them. Menus can
built using 'reqMenuItem', 'reqSubMenu', 'reqMenuExit', and 'reqMenuEnd'.
-}
reqMenu :: [(String, Request a)] -- ^ List of request choices and labels.
           -> Request a -- ^ Result.
reqMenu choices =
  do
    choice <- reqChoices choices reqInt
    choice

-- | Used to add an individual entry to a menu that is being built. 
reqMenuItem :: String
  -> Request a
  -> [(String, Request a)]
  -> [(String, Request a)]
reqMenuItem label item = (:) (label, item) 

-- | Creates a submenu within a menu. When the submenu exits, control returns to the item specified.
reqSubMenu :: Request a -- ^ The menu to return to.
  -> String -- ^ The label of the submenu (in the current menu)
  -> [(String, Request a)] -- ^ The submenu itself
  -> [(String, Request a)] -- ^ The existing menu into which this submenu will be inserted.
  -> [(String, Request a)] -- ^ The menu item built and returned.
reqSubMenu prevMenu label subMenu = (:) (label, reqForever $ reqCont (reqMenu subMenu) prevMenu)  

-- | Causes the program to exit from the current menu.  
reqMenuExit :: String
  -> [(String, Request a)]
  -> [(String, Request a)]
reqMenuExit label = (:) (label, reqFail)

-- | Ends a list of menu item definitions.
reqMenuEnd :: [(String, Request a)]
reqMenuEnd = []

{- |
Executes the request given and, if a failure value occurs,
executes the "Bool" request given (usually some sort of prompt asking
if they want to quit). If the answer is @True@, the failure value propagates. Otherwise,
the initial request is run again.
-}
reqConfirm :: Request Bool -- ^ When evaluated, determines if the failure is allowed to proceed or not.
  -> Request a -- ^ The request to run and to watch for failure
  -> Request a -- ^ Result of the request (if it did not fail).
reqConfirm conf req = reqCont req (reqIf conf reqFail (reqConfirm conf req))
  
{- |
Takes an initial value and function which produces a request
from that value. Applies the function to the initial value
and then recurses. Useful for functions which operate off their
own output (e.g. a shell maintaining an environment). -}
reqIterate :: (a -> Request a) -- ^ Iterative function which transforms a to Request a.
              -> a -- ^ Initial value used.
              -> Request a -- ^ Result of evaulation.
reqIterate fn initial =
  do
    result <- reqWhich (fn initial)
    case result of
      Left _ -> return initial
      Right val -> reqIterate fn val

{- |
Takes a request and a "continuation" request. If the
first request results in @Nothing@, run the second request.
In either case, return the result of the successful request. -}
reqCont :: Request a -- ^ First request to evaluate.
           -> Request a -- ^ Continuation request which is evaluated if first fails.
           -> Request a -- ^ Result.
reqCont req cont =
  do
    result <- reqWhich req
    case result of
      Left _ -> cont
      Right val -> return val

{-
Indicates if the request failed or succceeded. If @Left ()@ is
returned, the request failed. If @Right v@ is returned, the request
produce a value. Though the value returned is itself a request, it
will always be valid. -}
reqWhich :: Request a -- ^ Request to evaluate.
            -> Request (Either () a) -- ^ Result.
reqWhich req =
  do
    let -- default value, indicating a bad selection was made.
        failed = Request (return (Just (Left ())))
        -- Indicates a valid item was selected.
        success val =  Request (return (Just (Right val)))
    reqMaybe req failed success

{- |
Give a function from @a -> b@, an initial value,
and a @Request@ for @a@, builds a @Request@ for @b@. When @(Request a)@ fails,
then the function returns whatever @(Request b)@ has been built.
-}
reqFoldl :: (a -> b -> Request b) -- ^ Accumulating function.
            -> b -- ^ Initial value.
            -> Request a -- ^ Request to evaluate.
            -> Request b -- ^ Result.
reqFoldl fn initial req =
    reqFoldl' initial 
  where
    reqFoldl' acc =
      do
        result <- reqWhich req
        case result of
          Left _ -> return acc
          Right val ->
            do
              result <- fn val acc
              reqFoldl' result


{- |
Given a request, builds a list of response. When
the user enters @Nothing@, the list building ends -}
reqList :: Request a -- ^ Request to evaluate.
           -> Request [a] -- ^ Result.
reqList req = reqFoldl (\l ls -> return (l:ls)) [] req

{- |
Prints a message and makes a request. If the message ends in a space, it is assumed
that the user should enter values on the same line. Otherwise, a new line is printed
and the reqeust is evaulated. -}
prompt :: String -- ^ Message to display. 
          -> Request a -- ^ Request which gathers input
          -> Request a -- ^ Result.
prompt msg (Request req) =
  Request $
  do
    if isSpace (last msg)
      then putStr msg
      else putStrLn msg
    hFlush stdout
    val <- req
    return val

{- |
Displays a message prompt and a default choice in a common way. If
the user doesn't provide a choice or enters bad data, the default value provided
is returned. Otherwise, the value entered is returned. -}
prompt1 :: (Show a) => String -- ^ Message to display. Follows conventions of 'prompt'.
                     -> Request a -- ^ Request to evaluate.
                     -> a -- ^ Default value to use if necessary. 
                     -> Request a -- ^ Result.
prompt1 msg req def =
  let msgWithDefault = msg ++ " [" ++ show def ++ "] "
  in
    prompt msgWithDefault (reqDefault req def)

-- ^ Deprecated name for prompt1.
promptWithDefault :: (Show a) => String -> Request a -> a -> Request a 
promptWithDefault = prompt1

{- |
Prints a message, displays defaults (if any), and
turns a @Request String@ into a @Request Bool@. If
a default value is provided, it will be returned if the
user enters nothing or an invalid response. -}
promptAgree :: String -- ^ Message to display. Follows conventions of 'prompt'.
               -> Maybe Bool -- ^ Default value, if any.
               -> Request String -- ^ Request which gets a string (usually reqResp).
               -> Request Bool -- ^ Result.
promptAgree msg def req =
    prompt msgWithDefault (reqAgree def req)
  where
    msgWithDefault =
      maybe msg
      (\v -> if v then (msg ++ "(Y/n) ") else (msg ++ "(y/N) "))
      def

{- |
Used to define an arbitrary that generates a request which randomizes
its response, each time it is evaluated. -}
newtype RandomRequest a = RandomRequest { request :: Request a }

{- |
  Everytime the request returned is evaluated, it returns a random
  Just v or Nothing value. -}
instance (Arbitrary a) => Arbitrary (RandomRequest a) where
  arbitrary =
    let random val = Request $
          do
            rnd <- newStdGen
            let (lo, rnd') = randomR (1 :: Int, 10 :: Int) rnd
            if lo > 5
              then return Nothing
              else return $ Just val
    in
      do
        val <- arbitrary
        return (RandomRequest $ random val)
  
{- |
Creates a request which will return a random value or Nothing. The
request returns the same value every time it is evaluated. -}
instance (Arbitrary a) => Arbitrary (Request a) where
  arbitrary =
    do
      val <- arbitrary
      rnd <- arbitrary
      if rnd
        then return $ Request (return (Just val))
        else return $ Request (return Nothing)

-- | Show for random requests.  
instance (Show a) => Show (RandomRequest a) where
  show (RandomRequest req) = show req
  
-- | Show for requests.  
instance (Show a) => Show (Request a) where
  show = showRequest

-- | Show for requests.  
showRequest (Request r) = "requesting " ++ (show $ unsafePerformIO (r >>= \result -> return result))

{- |
  Ensures @required@ always returns a @(Just _)@ value. Kind of
  a bogus test because this will hang if required does NOT return
  a @Just@. -}
prop_requiredReturns :: RandomRequest Integer -> Bool
prop_requiredReturns reqRandom =
    unsafePerformIO $
    do
      let req = request reqRandom
          Request result = required req
      ioVal <- result
      case ioVal of
        Nothing -> return False
        _ -> return True

{- |
 Test that @reqDefault@ always returns a default value if the request
 resulted in @Nothing@. Otherwise, ensure it returned the request value. -}
prop_reqDefaultReturnsDefault :: Request Integer -> Integer -> Bool
prop_reqDefaultReturnsDefault req def =
  unsafePerformIO $
    do
      let Request result = reqDefault req def
          Request input = req
      inputVal <- input
      Just resultVal <- result
      case inputVal of
        Nothing -> return (resultVal == def)
        Just v -> return (resultVal == v)

-- | Test that bad choices don't cause exceptions. This test is noisy because it prints 
-- a lot of garbage to the screen, some of which are bell characters!
prop_reqChoicesDoesntFail :: [(String, Int)] -> Request Int -> Bool
prop_reqChoicesDoesntFail choices req@(Request input) =
  unsafePerformIO $
    do
      let Request result = reqChoices choices req
      inputVal <- input
      resultVal <- result
      case inputVal of
        -- If we got a valid number, ensure reqChoice behavior is correct.
        Just n | n < 1 || n > (length choices) -> return $ isNothing resultVal
               -- Result value of nothing here means a valid index was selected but nothing was returned.
               | isNothing resultVal -> return False
               -- Determine value returned was in fact in the choices
               | otherwise ->
                    let choiceVal = snd (choices !! (n - 1))
                        Just v = resultVal
                    in
                      return $ choiceVal == v
        Nothing -> return $ isNothing resultVal 
                    
{- |
  Test that the @andMaybe@ function works as specified. Good test because
  \>\>= is implemented using it! -}
prop_andMaybeWorks :: Request Int -> Request Int -> Bool
prop_andMaybeWorks first@(Request firstReq) second@(Request secondReq) =
  unsafePerformIO $
    do
      let Request resultReq = first `andMaybe` \v -> second
      resultVal <- resultReq
      firstVal <- firstReq
      secondVal <- secondReq
      case resultVal of
        -- If the result is nothing, the first thing (or the second) must
        -- be nothing.
        Nothing -> return $ isNothing firstVal || isNothing secondVal 
        -- Otherwise, it is the value of the second request.
        Just n -> return $ maybe False (\v -> n == v) secondVal

-- | Ensure that @reqWhich@ works as expected
prop_reqWhichWorks :: Request Int -> Bool
prop_reqWhichWorks req@(Request inputReq) =
  unsafePerformIO $
    do
      let Request resultReq = reqWhich req
      resultVal <- resultReq
      inputVal <- inputReq
      case resultVal of
        Nothing -> return False
        Just (Left _) -> return $ isNothing inputVal
        Just (Right _) -> return $ isJust inputVal

-- | Ensure that @reqMaybe@ works as expected.
prop_reqMaybeWorks :: Request Int -> Request Int -> Bool
prop_reqMaybeWorks first@(Request firstReq) def@(Request defaultReq) =
  unsafePerformIO $
    do
      let Request resultReq = reqMaybe first def (return . id)
          compareMaybes n = maybe False (\v -> n == v)
      firstVal <- firstReq
      defaultVal <- defaultReq
      resultVal <- resultReq
      case resultVal of
        Nothing -> return $ isNothing defaultVal
        Just n | isNothing firstVal -> return $ compareMaybes n defaultVal
               | otherwise -> return $ compareMaybes n firstVal
