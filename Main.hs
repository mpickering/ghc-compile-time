{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
module Main(main) where

import Data.Aeson (Value, encode, ToJSON, toJSON, object, (.=))
import Data.String
import Data.Text (Text, append)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy as TL
--import Text.Blaze.Html
import qualified Text.Blaze.Html5            as H hiding (main, map)
import Text.Blaze.Html5            (Html, AttributeValue, (!), meta, script, body, h1, preEscapedToHtml, link, docTypeHtml)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String
import Data.List
import Data.Text.Encoding
import Data.FileEmbed
import GHC.Generics
import GHC.RTS.Events hiding (process, name, time)
import System.Environment
import Debug.Trace
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import Data.Ord
import qualified Data.Set as Set
import Text.Printf
import qualified Data.OrdPSQ as PS
import Data.Maybe (fromMaybe, catMaybes)
import Data.Tuple (swap)
import Control.Monad (forM_, forM, guard)
import Text.Read
import Data.Coerce
import System.Directory
import System.FilePath


data Mode = Simple | Hypothetical

main :: IO ()
main = do
  [dir] <- getArgs
--  els <- listDirectory dir
  --res <- parseMany dir els
  --runAnalyis res
--  generateModule dir
  {-
  generateDetailed el
  generateModule el
  -}

  core_graph <- readCoreGraph dir
  writeFile "analyse.py" (genPyDag 1 0 [] [] core_graph)
  runCoreAnalysis core_graph

parseMany :: FilePath -> [FilePath] -> IO Result
parseMany  dir fps = fmap (mconcat . catMaybes) . forM fps $ \fp -> do
  print fp
  Right !events <- readEventLogFromFile (dir </> fp)
  let !EL{..} = process fp events
  -- This is a simple check to see if the eventlog was from compiling a package or not
  print (unitTimes result)
  return $ if Map.null (units result)
    then  Nothing
    else Just result


detailed :: Mode -> Result -> (Map.Map Node [Node], [Duration Node])
detailed mode = (\Result{..} -> ( mkDependencyTree mode mods
                            , mkDurationNode (match spans) ))

generateDetailed :: FilePath -> IO ()
generateDetailed =
  generate (detailed Simple)

simple :: Result
       -> (Map.Map ModuleNode [ModuleNode],
           [Duration ModuleNode])
simple = (\Result{..} -> ( Map.mapKeys ModuleNode (coerce mods)
                       , mkDurationModule (match spans) ) )

generateModule :: FilePath -> IO ()
generateModule =
  generate simple

generate :: (Show a, Ord a)
         => (Result -> (Map.Map a [a], [Duration a]))
         -> FilePath
         -> IO ()
generate f el_f = do
  Right events <- readEventLogFromFile el_f
  let el@EL{result=Result{..}} = process el_f events
  runAnalysis (result el)


optics = UnitName "optics-core-0.4-2115a1ecc5fa542a6970529b64c66e221bf61df8313a8f602665b32491d74a4d"



runAnalysis :: Result -> IO ()
runAnalysis r@Result{..} = do
  let times = [t | (_, _, t) <- spans]
      total_time = maximum times - minimum times
--      deps = detailed --units
--      dur = Map.elems unitTimes
      (deps, dur) = simple r
--      deps' = simplifyEdges deps
  let insts = toInstants dur
--      dur' = (fmap PackageNode <$> dur)
  let  h = template (toJSON dur)
  writeFile "test.html" (renderHtml h)
  let s = simulate insts
  printSimulation "REAL" s
  let hcaps = 6
      g = genGraph dur deps
      (simple_deps, simple_dur) = detailed Simple r
      g' = genGraph (fmap PackageNode <$> simple_dur) (simplifyEdges simple_deps)
      (simple_deps', simple_dur') = detailed Hypothetical r
      g'' = genGraph simple_dur' simple_deps'
--  print (sum (map duration simple_dur))
  print (sum (map duration dur))
--  print (Map.size simple_deps)
  print (Map.size deps)
  --print (sum (map fst (Map.elems g)))
--  print (sum (map fst (Map.elems g')))
--  print (sum (map fst (Map.elems g'')))
--  let hs = simulate $ toInstants $ hypothetical g' hcaps
  let hyp = hypothetical g hcaps
--  print hyp
  let simple_hs = simulate $ toInstants $ hyp
--  print simple_hs
      hs' = simulate $ toInstants $ hypothetical g' hcaps
--  let simple_hs' = simulate $ toInstants $ hypothetical g'' hcaps
--  printSimulation ("Hypothetical caps(S)=" ++ show hcaps) hs
--  printSimulation ("Hypothetical caps(D)=" ++ show hcaps) simple_hs'

  printSimulation ("Package = 1, -j=" ++ show hcaps) simple_hs
  return ()
  printSimulation ("Package = jsem, -j=" ++ show hcaps) hs'

  let new = simulateJsem14905 r
      hyp_new = hypothetical new 6
      hs_new = simulate $ toInstants $ hyp_new
  writeFile "analyse.py" (genPyDag 1 0 insts [] g)
  printSimulation ("PAR") hs_new


type DepMap a = Map.Map a (Double, [a])

simulateCurrent :: Result -> DepMap UnitName
simulateCurrent Result{..} = genGraph (Map.elems unitTimes) units
simulateJsem :: Result -> DepMap (PackageWrapper Node)
simulateJsem r =
  let (simple_deps, simple_dur) = detailed Simple r
  in genGraph (fmap PackageNode <$> simple_dur) (simplifyEdges simple_deps)
simulateJsem14905 :: Result -> DepMap (PackageWrapper Node)
simulateJsem14905 r =
  let (simple_deps, simple_dur) = detailed Hypothetical r
  in genGraph (fmap PackageNode <$> simple_dur) (simplifyEdges simple_deps)


simulateOne :: (Show a, Ord a) => DepMap a -> Int -> Map.Map Int Double
simulateOne g caps = durs $ simulate $ toInstants $ hypothetical g caps

runCoreAnalysis :: DepMap Int -> IO ()
runCoreAnalysis dm = do
  forM_ [1..32] $ \caps -> do
    let res = simulateOne dm caps
    print $ show caps ++ ": " ++ show (sum (map snd $ Map.toList res))

runAnalysis2 :: Result -> IO ()
runAnalysis2 r = do
  let instants caps = [("current", simulateOne (simulateCurrent r) caps)
                      ,("jsem", simulateOne (simulateJsem r) caps)
                      ,("jsem-14905", simulateOne (simulateJsem14905 r) caps)]
  putStrLn (intercalate "," ("caps" : map fst (instants 0)))
  forM_ [1..32] $ \caps -> compareTime caps (map snd (instants caps))

-- Different scenarios to simulate
-- * Current behaviour (-j1 Cabal -j)
-- * Jsem (-jsem Cabal -j)
-- * Jsem + Hypothetical edges


mkDurationNode :: [ModuleSpan] -> [Duration Node]
mkDurationNode mss =
  [(Duration (Node name p) (getStartTime ms p) (getPhaseTime ms p)) | p <- phases, ms@ModuleSpan{..} <- mss]


compareTime :: Int -> [Map.Map Int Double] -> IO ()
compareTime n ms = putStrLn $ (show n) ++ concatMap (\m -> "," ++ (time m)) ms
  where
    time s = show (sum (map snd (Map.toList s)) / 1000)


printSimulation :: Show a => String -> SimState a -> IO ()
printSimulation herald s = do
  putStrLn "-------------------------------"
  putStrLn herald
--  putStrLn "Time par"
  let durs_list = (Map.toList (durs s))
  mapM_ ((\(c, d) -> printf "%d: %.2fs\n" c (d / 1000))) durs_list
  printf "TOTAL: %.2fs\n" (sum (map snd durs_list) / 1000)
  putStrLn "Waited for (top 10):"
  let lags' = sortBy (comparing snd) (lags s)
  mapM_ ((\(c, d) -> printf "%s: %.2fs\n" (show c) (d / 1000))) (take 10 $ reverse (lags'))


getPhaseTime :: ModuleSpan -> Phase -> Double
getPhaseTime ms k = getTime ms (phaseEnd k) - getTime ms (phaseStart k)


-- Generate dependency edges
-- Normal mode = id
-- Hypothetical mode, more complicated
{-

┌───────┬────┬────┬────┐    ┌────────────┬────┬─────┬─────┬────┬──────────────┐
│ Mod A │ Ps │ Rn │ Tc │───>│ ModIface A │ Ds │ C2C │ C2S │ Cg │ ModDetails A │
└───────┴────┴────┴────┘    └────────────┴────┴─────┴─────┴────┴──────────────┘
  │                          ┆                                  ┆
  │                          ┆                                ╭┄╯
  │                          ┆                                ┆
  │   ┌───────┬────┐         ┆  ┌────┬────┬────┬─────┬─────┐  ┆  ┌────┬──────────────┐
  ├─> │ Mod B │ Ps │─────────┼─>│ Rn │ Tc │ Ds │ C2C │ C2S │──┼─>│ Cg │ ModDetails B │
  │   └───────┴────┘         ┆  └────┴────┴────┴─────┴─────┘  ┆  └────┴──────────────┘
  │                          ┆                                ┆
  │                          ┆                                ┆
  │                          ┆                                ┆
  │   ┌───────┬────┐         ┆  ┌────┬────┬────┬─────┬─────┐  ┆  ┌────┬──────────────┐
  └─> │ Mod C │ Ps │─────────┴─>│ Rn │ Tc │ Ds │ C2C │ C2S │──┴─>│ Cg │ ModDetails C │
      └───────┴────┘            └────┴────┴────┴─────┴─────┘     └────┴──────────────┘

Ps = Parse          Rn = Rename        Tc = TypeCheck
Ds = Desugar
Cg = StgToCmm, C-- pipeline, and NCG
C2C = Core-to-Core
C2S = Core-to-Stg

-}

genDepsCurrent :: Module -> [Module] -> [(Node, [Node])]
genDepsCurrent cur_mod deps =
  (Node cur_mod Parse, map (\d -> Node d CodeGen) deps)
  : intraModDeps cur_mod

intraModDeps :: Module -> [(Node, [Node])]
intraModDeps cur_mod =
  (Node cur_mod CodeGen, [Node cur_mod TypeCheck])
  : (Node cur_mod TypeCheck, [Node cur_mod Parse])
  : []

genDepsOneHypothetical :: Module -> [Module] -> [(Node, [Node])]
genDepsOneHypothetical cur_mod deps =
  let mk = Node
  in
  -- Step 1: P has no deps
  (mk cur_mod Parse, [])
  :
  -- Step 2: Rn depends on Tc
  (mk cur_mod TypeCheck, map (\d -> mk d TypeCheck) deps)
  :
  -- Step 3: CGStart depends on CGEnd
  (mk cur_mod CodeGen , map (\d -> mk d CodeGen) deps)
  : intraModDeps cur_mod


mkDependencyTree :: Mode -> Map.Map Module [Module] -> Map.Map Node [Node]
mkDependencyTree mode mod_deps =
  Map.fromListWith (++) (concatMap (uncurry gen) ((Map.toList (Map.map (intersect home_mods) mod_deps))))
  where
    home_mods = Map.keys mod_deps
    gen = case mode of
            Simple -> genDepsCurrent
            Hypothetical -> genDepsOneHypothetical

mkDurationModule :: [ModuleSpan] -> [Duration ModuleNode]
mkDurationModule mss =
  [Duration (ModuleNode (name ms)) (start ms) (end ms - start ms) | ms <- mss]

-- Rewrite internal pointing edges to PackageFinished nodes
simplifyEdges :: Map.Map Node [Node] -> Map.Map (PackageWrapper Node) [(PackageWrapper Node)]
simplifyEdges dep_map = Map.fromListWith (++) $ concatMap go $ Map.toList dep_map
  where
    go :: (Node, [Node]) -> [(PackageWrapper Node, ([PackageWrapper Node]))]
    go (old_n@(getNodeUnit -> uid), ns) =
      (PackageFinished uid, [PackageNode old_n]) :
      (PackageNode old_n, map (\n -> if getNodeUnit n == uid then (PackageNode n) else (PackageFinished (getNodeUnit n)) ) ns) : []

-- Simplifier the graph starting from a specific place
cullFrom :: Ord a => a -> Map.Map a [a] -> Map.Map a [a]
cullFrom start m = case vs of
                      Just ns -> foldr cullFrom m' ns
                      Nothing -> m'
  where
    (vs, m') = Map.updateLookupWithKey (\_ _ -> Nothing) start m

getNodeUnit :: Node -> UnitName
getNodeUnit (Node (Module uid' _) _) = uid'

-- | This prunes the dependencies so that only nodes with times are kept in the graph.
genGraph :: (Show a, Ord a)
          => [Duration a]
          -> Map.Map a [a] -- Dependencies
          -> Map.Map a (Double, [a])
genGraph dur mod_deps = final_map
  where
    times = Map.fromListWith (error "clash") [(n, d) | Duration n _s d <- dur]
    care = Map.keys mod_deps
    k :: Show a => Map.SimpleWhenMissing a Double (Double, [a])
    k = Map.mapMissing (\k x -> error (show k ++ show x))

    g :: Map.SimpleWhenMissing a [a] (Double, [a])
    g = Map.mapMissing (\k x -> (0, x))

    final_map = Map.merge k g (Map.zipWithMatched (\_ time mods -> (time, care `intersect` mods))) times mod_deps



-- Start/End is in milliseconds
data ModuleSpan = ModuleSpan { name :: Module
                             , start :: Double
                             , rn :: Double
                             , tc :: Double
                             , cgstart :: Double
                             , cgend :: Double
                             , end :: Double } deriving (Generic, Show)

deriving instance ToJSON ModuleSpan
deriving instance ToJSON Module

data Marker = StartComp | RN | TC | CGStart | CGEnd | EndComp deriving (Show, Ord, Eq, Enum)

data Phase = Parse | TypeCheck | CodeGen deriving (Show, Enum, Ord, Eq)

data PackageWrapper a = PackageFinished UnitName | PackageNode a deriving (Eq, Ord)

instance Show a => Show (PackageWrapper a) where
  show (PackageFinished un) = "FIN:" ++ T.unpack (getUnitName un)
  show (PackageNode a) = show a

newtype ModuleName = ModuleName { getModuleName :: Text }
  deriving (Show, Eq, Ord)
  deriving newtype ToJSON

data Module = Module { moduleUnitId :: UnitName
                     , moduleName :: ModuleName } deriving (Eq, Ord, Generic)

newtype UnitName = UnitName { getUnitName :: Text }
  deriving (Eq, Ord)
  deriving newtype ToJSON

instance Show UnitName where
  show (UnitName u) = show u

data Node = Node {
              nodeModule :: Module,
              nodePhase :: Phase
              }  deriving (Ord, Eq)

newtype ModuleNode = ModuleNode Module deriving (Ord, Eq)


instance Show ModuleNode where
  show (ModuleNode m) = show m

instance Show Module where
  show (Module u p) = T.unpack (getModuleName p) <> ":" <> T.unpack (getUnitName u)

instance Show Node where
  show (Node m p) = show p <> ":" <> show m

data Duration a =
      Duration { durationValue :: a
               , durationStart :: Double
               , duration :: Double } deriving (Show, Functor)


instance Show a => ToJSON (Duration a) where
  toJSON (Duration node cur_time dur) = object ["name" .= show node, "start" .= cur_time, "end" .= (cur_time + dur)]

durationToInstants :: Duration a -> [Instant a]
durationToInstants (Duration node start dur) = [Instant start node Start, Instant (start + dur) node End]

-- | Returns an ordered list
toInstants :: [Duration a] -> [Instant a]
toInstants = sortBy (comparing time) . concatMap durationToInstants

data Instant a = Instant {
                 time :: Double
                 , instantNode :: a
                 , instanceDir :: StartOrEnd
                 }

data StartOrEnd = Start | End


getStartTime :: ModuleSpan -> Phase -> Double
getStartTime ms = getTime ms . phaseStart

getTime :: ModuleSpan -> Marker -> Double
getTime ModuleSpan{..} m =
  case m of
    StartComp -> start
    RN -> rn
    TC -> tc
    CGStart -> cgstart
    CGEnd -> cgend
    EndComp -> end

phaseStart :: Phase -> Marker
phaseStart Parse = StartComp
phaseStart TypeCheck = RN
phaseStart CodeGen   = CGStart

phaseEnd :: Phase -> Marker
phaseEnd Parse = RN
phaseEnd TypeCheck = CGStart
phaseEnd CodeGen = EndComp

markers :: [Marker]
markers = enumFrom StartComp

phases :: [Phase]
phases = enumFrom Parse

match :: [(Module, Marker, Double)] -> [ModuleSpan]
match xss = go (normalise (sort xss))
  where
    -- Account for if codegen runs multiple times
    normalise ((l, CGStart, d):(_, CGStart, _):xs) = normalise ((l, CGStart, d) : xs)
    normalise ((_, CGEnd, _):(l, CGEnd, d):xs) = normalise ((l, CGEnd, d) : xs)
    normalise (x:xs) = x : normalise xs
    normalise [] = []

    go :: [(Module, Marker, Double)] -> [ModuleSpan]
    go [] = []
    go xs =
        let (prefix, ys) = splitAt (length markers) xs
            ([start, rn, tc, cgstart, cgend, end], leftover) = check markers prefix
            (l, _, _) = head prefix
        in ModuleSpan {name = l,.. } : go (leftover ++ ys)

    -- Only *needs* the Start/End events, the other events are filled in with End
    -- if they are missing. This is convenient for hs-boot files which don't have
    -- a codegen phase
    check :: [Marker] -> [(Module, Marker, Double)] -> ([Double], [(Module, Marker, Double)])
    check (expected_dir:dirs) ((_l, dir, ts):xs)
      | expected_dir == dir = let (rest, leftover) = check dirs xs
                              in (ts: rest, leftover)
      | otherwise = if dir == EndComp then (replicate (length dirs + 1) ts, xs) else error (show expected_dir ++ show dir ++ show xs)
    check _ _ = ([], [])

data EL = EL { offset :: Double
             , result :: !Result }


data Result = Result { spans :: ![(Module, Marker, Double)]
             , mods :: !(Map.Map Module [Module])
             , units :: !(Map.Map UnitName [UnitName])
             , unitTimes :: !(Map.Map UnitName (Duration UnitName))}

instance Monoid Result where
  mempty = Result mempty mempty mempty mempty

instance Semigroup Result where
  (Result s1 m1 u1 ut1) <> (Result s2 m2 u2 ut2) = Result (s1 ++ s2) (Map.union m1 m2) (Map.union u1 u2) (Map.union ut1 ut2)

updateResult :: (Result -> Result) -> EL -> EL
updateResult f el = el { result = f (result el) }

nanoToMilli :: Integral n => n -> Double
nanoToMilli n = (fromIntegral n) / 1_000_000

process :: FilePath -> EventLog -> EL
process fp (EventLog _ (Data es)) =
  let res@EL{..} = foldl' processEvent (EL (error fp) mempty) es
      times = mkDurationModule (match (spans result))
      min_time = minimum (map durationStart times)
      total_time = sum (map duration times)
      [unit] = Map.keys (units result)
  in res {result = result { unitTimes = Map.map (\_ -> Duration unit min_time total_time) (units result)  }}

processEvent :: EL -> Event -> EL
processEvent el (Event t ei _) = processEventInfo t ei el


processEventInfo :: Timestamp -> EventInfo -> EL -> EL
processEventInfo t (WallClockTime _ s ns) el = el { offset = ((fromIntegral s * 1_000) + (nanoToMilli ns)) - nanoToMilli t }
processEventInfo t (UserMarker m) el = do
  let o = offset el
  case parseMessage m of
    Just (m', d) -> updateResult (\r -> r { spans = (m', d, nanoToMilli t + o) : spans r }) el
    Nothing -> case parseMod m of
                 Just (m', deps) -> updateResult (\r -> r { mods = Map.insert m' deps (mods r) }) el
                 Nothing -> case parsePackage m of
                              Just (un, udeps) -> updateResult (\r -> r { units = Map.insert un udeps (units r)}) el
                              Nothing -> el



processEventInfo _ _ el = el


parseMod :: Text -> Maybe (Module, [Module])
parseMod t = do
  ["MOD", info, deps_info] <- return $ T.splitOn ":" t
  modu <- parseModule info
  deps <- map (\(a,b) -> Module (UnitName a) (ModuleName b)) <$> readMaybe (T.unpack deps_info)
  return (modu, deps)

parsePackage :: Text -> Maybe (UnitName, [UnitName])
parsePackage t = do
  ("PACKAGES": modu: deps) <- return $ T.splitOn ":" t
  guard (modu /= "main")
  return (UnitName modu, (map (UnitName . T.pack) (map T.unpack deps)))

parseModule :: Text -> Maybe Module
parseModule info = do
  (unit, modu) <- readMaybe (T.unpack info)
  return (Module (UnitName unit) (ModuleName modu))

parseMessage :: Text -> Maybe (Module, Marker)
parseMessage t = do
  (raw_dir: info:_) <- return $ T.splitOn ":" t
  modu <- parseModule info
  dir <- parseDir raw_dir
  return (modu, dir)
parseDir :: Text -> Maybe Marker
parseDir "START" = Just StartComp
parseDir "END" = Just EndComp
parseDir "CGSTART" = Just CGStart
parseDir "CGEND" = Just CGEnd
parseDir "RN" = Just RN
parseDir "TC" = Just TC
parseDir _ = Nothing




insertJsonData :: Value -> Html
insertJsonData dat = preEscapedToHtml $ T.unlines [
    "module_data= " `append` dat' `append` ";"
  , "console.log(module_data);" ]
  where
    dat' = TL.toStrict (T.decodeUtf8 (encode dat))



jsScript :: String -> Html
jsScript url = script ! A.src (fromString $ url) $ ""
css :: AttributeValue -> Html
css url = link ! A.rel "stylesheet" ! A.href url

htmlHeader :: Value -> Html
htmlHeader dat  =
    H.head $ do
    H.title "Module Times"
    meta ! A.charset "UTF-8"
    script $ insertJsonData dat
    jsScript "https://unpkg.com/vis-timeline@latest/standalone/umd/vis-timeline-graph2d.min.js"
    css "https://unpkg.com/vis-timeline@latest/styles/vis-timeline-graph2d.min.css"

timeline :: Text
timeline = decodeUtf8 $(embedFile "timeline.js")


template :: Value -> Html
template dat = docTypeHtml $ do
--  H.stringComment $ "Generated with -" <> showVersion version
  htmlHeader dat
  body $ do
    h1 "Timeline"
    H.div ! A.id "visualization" $ ""
    script $ preEscapedToHtml timeline

-- Analysis script in Python <3
genPyDag :: (Ord a, Show a) => Int -> Double -> [Instant a] -> [(String, [(Double, Int)])] -> Map.Map a (Double, [a]) -> String
genPyDag caps total_time instants chart_info dag =
  let edges = Map.toList dag
      (spans) =  instants
--      simstate = simulate spans
      renderNode = T.pack . show
      mkEdge :: Text -> Text -> Double -> String
      mkEdge from_edge to_edge weight = "G.add_edge (" ++ show from_edge ++ "," ++ show to_edge ++ ", weight=" ++ show weight ++ (if weight>0 then ", capacity=1" else "") ++ ")"
      mkOne (t, (w, ds)) =
        let internal_node = "START:" <> renderNode t
        in (mkEdge internal_node (renderNode t) w) : map (\end -> mkEdge (renderNode end) internal_node 0) ds
         -- Intermediate edge
      added_edges = unlines (concatMap mkOne edges)

      mkEvent (Instant time node dir) = (time, show node,  showSorE dir)
      showSorE (Start {}) = "S" :: String
      showSorE (End{}) = "E" :: String
      events = "events =" ++ show (map mkEvent spans)
      gen (l, spans) = let (xs, ys) = unzip (sort $ spans)
                       in (xs, ys, l)
      c = unlines [
              "charts = " ++ show (map gen chart_info)
        ]
  in pyScript caps total_time added_edges events c


rawPScript :: Text
rawPScript = decodeUtf8 $(embedFile "analyse_template.py")




pyScript :: Int -> Double -> String -> String -> String -> String
pyScript _caps _total_time added events chart = unlines $
  [ "import networkx as nx"
  , "import matplotlib.pyplot as plt"
  , "import matplotlib.animation as animation"
  , "import itertools"
  , "import math"
  , "G = nx.DiGraph()"
  , added
  , events
  , chart
  , T.unpack $ rawPScript ]

data SimState a = SimState { n_jobs :: !Int
                         , start_period :: !Double
                         , durs :: !(Map.Map Int Double)
                         , chart :: ![(Double, Int)]
                         , active :: !(Set.Set a)
                         , lags :: ![(a, Double)]
                         } deriving Show

-- An accurate simulation of what actually happened
simulate :: Ord a => [Instant a] -> SimState a
simulate spans = foldl' go (SimState 0 t0 Map.empty [] Set.empty mempty) spans
  where
    t0 = time (head spans)


    go :: Ord a => SimState a -> Instant a -> SimState a
    go (SimState{..}) (Instant time node sore)  =
      let period_time = time - start_period
          durs' = Map.insertWith (+) n_jobs period_time durs
          chart' = (start_period, n_jobs) : (time, n_jobs) : chart
          n_jobs' = case sore of
                      Start -> n_jobs + 1
                      End -> n_jobs - 1
          active' = case sore of
                      Start -> Set.insert node active
                      End -> Set.delete node active
          start_period' = time
          lags' = case Set.toList active of
                   [single_node] -> (single_node, period_time) : lags
                   _ -> lags
      in SimState n_jobs' start_period' durs' (if period_time == 0 then chart else chart') active' lags'


data HypState a = HypState { cur_time :: Double
                         , capsFree  :: Int
                         , ready :: [(Double, a)]
                         , queue :: PS.OrdPSQ a Int (Double, a) -- Invariant, nothing of priority 0 in here.
                         , action :: PS.OrdPSQ a Double ()
                         , traces :: [Duration a]
                         } deriving Show

-- Assuming k capabilities, how fast could we have gone?
hypothetical :: forall a . (Show a, Ord a) => Map.Map a (Double, [a]) -> Int -> [Duration a]
hypothetical deps caps = reverse $ traces $ go (HypState 0 caps initReady initialQueue PS.empty [])
  where

    revDeps :: Map.Map a [a]
    revDeps = Map.fromListWith (++) $ concatMap (\(m, (_, ms)) -> map (,[m]) ms) $ Map.toList deps

    initReady = map (\(_, _, c) -> c) initReady'
    (initReady', initialQueue) =
      PS.atMostView 0 (PS.fromList allEdges)


    allEdges :: [(a, Int, (Double, a))]
    allEdges = [(n, length ds, (t, n)) | (n, (t, ds)) <- Map.toList deps]


    mkTrace :: Double -> (Double, a) -> Duration a
    mkTrace cur_time (dur, n) = Duration n cur_time dur


    alterDeps :: PS.OrdPSQ a Int b -> a -> (PS.OrdPSQ a Int b, Maybe b)
    alterDeps ps l = swap $ PS.alter f l ps
      where
        f Nothing = error ("Bad key:" ++ show l)
        f (Just (p, v)) = if p == 1 then (Just v, Nothing)
                                     else (Nothing, Just (p - 1, v))

    go :: HypState a -> HypState a
    go h@(HypState{..}) =
      case capsFree of
        -- caps free, and jobs ready, start them
        c | not (null ready) && c > 0 ->
          let (new_jobs, todo_jobs) = splitAt c ready -- Could use a smarter scheduler
              new_jobs_n = length new_jobs
              new_free_caps = c - new_jobs_n
              action' = foldr (\(t, l) am -> (PS.insert l (cur_time + t) () am)) action new_jobs
              durs  = map (mkTrace cur_time) new_jobs
              new_traces = durs ++ traces
          in go (HypState{capsFree = new_free_caps, ready = todo_jobs, traces = new_traces, action = action', .. })
        -- Otherwise, advance time until a job finishes and release the cap
        _ -> case PS.minView action of
              Nothing -> h -- We are done, no more actions to perform but free capabilities.
              Just (l, t, (), action') ->
                let rev_deps_l = fromMaybe [] (Map.lookup l revDeps)
                    (new_queue, new_ready) = mapAccumL alterDeps queue rev_deps_l
                    new_free_caps = capsFree + 1
                in go (HypState{ capsFree = new_free_caps, queue = new_queue, ready = catMaybes new_ready ++ ready, cur_time = t, action = action', .. })



readCoreGraph :: FilePath -> IO (Map.Map Int (Double, [Int]))
readCoreGraph fp = Map.fromList . map (assoc . rl) . lines <$> readFile fp

  where
    assoc (a,b,c) = (a, (fromIntegral b, filter (/= a) c))

    rl :: String -> (Int, Int, [Int])
    rl = read






