{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}
module Main(main) where

import Data.Aeson (Value, encode, ToJSON, toJSON)
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
import Control.Monad (forM_)


data Mode = Simple | Hypothetical


main :: IO ()
main = do
  [el] <- getArgs
  Right events <- readEventLogFromFile el
  let EL{..} = process events
      times = [t | (_, _, t) <- spans]
      total_time = maximum times - minimum times
      mod_spans = (match spans)
  let h = template (toJSON mod_spans)
  writeFile "test.html" (renderHtml h)
  writeFile "analyse.py" (genPyDag caps total_time spans (genGraph Simple mod_spans mods))
  let s = simulate (mkStartOrEnd mod_spans)
  printSimulation "REAL" s
  let hcaps = [1..20]
  forM_ hcaps $ \n -> do
    let hs = simulate . hypothetical Hypothetical mods mod_spans $ n
    let simple_hs = simulate . hypothetical Simple mods mod_spans $ n
    compareTime n hs simple_hs
    printSimulation ("Hypothetical caps=" ++ show n) hs

mkStartOrEnd :: [ModuleSpan] -> [(Text, StartOrEnd, Double)]
mkStartOrEnd mss =
  sortBy (comparing (\(_a, _b, c) -> c)) [(name, f p, getTimeSorE ms (f p)) | f <- [Start, End], p <- phases, ms@ModuleSpan{..} <- mss]


compareTime :: Int -> SimState -> SimState -> IO ()
compareTime n h c = putStrLn $ (show n) ++ "," ++ (time h) ++ "," ++ (time c)
  where
    time s = show (sum (map snd (Map.toList (durs s))) / 1000)


printSimulation :: String -> SimState -> IO ()
printSimulation herald s = do
  putStrLn "-------------------------------"
  putStrLn herald
--  putStrLn "Time par"
  let durs_list = (Map.toList (durs s))
  mapM_ ((\(c, d) -> printf "%d: %.2fs\n" c (d / 1000))) durs_list
  printf "TOTAL: %.2fs\n" (sum (map snd durs_list) / 1000)
  putStrLn "Waited for (top 10):"
  let lags' = sortBy (comparing snd) (lags s)
  mapM_ ((\(c, d) -> printf "%s: %.2fs\n" c (d / 1000))) (take 10 $ reverse (lags'))

-- Given a ModuleSpan, generate the times for the different segments
-- In normal mode, there is just one segment for the whole module but
-- in hypothetical mode, there are multiple segments per module.
genEvents :: ModuleSpan -> [(Node, Double)]
-- This is "normal mode"
--genEvents (ModuleSpan{..}) = [(end - start, name)]
-- This is "hypothetical mode"
genEvents ms =
  let mk h = (Node h (name ms),  getPhaseTime ms h)
  in map mk phases


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

genDepsCurrent :: Text -> [Text] -> [(Node, [Node])]
genDepsCurrent cur_mod deps =
  (Node Parse cur_mod, map (Node CodeGen) deps)
  : intraModDeps cur_mod

intraModDeps :: Text -> [(Node, [Node])]
intraModDeps cur_mod =
  (Node CodeGen cur_mod, [Node TypeCheck cur_mod])
  : (Node TypeCheck cur_mod, [Node Parse cur_mod])
  : []

genDepsOneHypothetical :: Text -> [Text] -> [(Node, [Node])]
genDepsOneHypothetical cur_mod deps =
  let mk = Node
  in
  -- Step 1: P has no deps
  (mk Parse cur_mod, [])
  :
  -- Step 2: Rn depends on Tc
  (mk TypeCheck cur_mod, map (mk TypeCheck) deps)
  :
  -- Step 3: CGStart depends on CGEnd
  (mk CodeGen cur_mod, map (mk CodeGen) deps)
  : intraModDeps cur_mod


mkDependencyTree :: Mode -> Map.Map Text [Text] -> Map.Map Node [Node]
mkDependencyTree mode mod_deps =
  Map.fromListWith (++) (concatMap (uncurry gen) ((Map.toList (Map.map (intersect home_mods) mod_deps))))
  where
    home_mods = Map.keys mod_deps
    gen = case mode of
            Simple -> genDepsCurrent
            Hypothetical -> genDepsOneHypothetical

genGraph :: Mode -> [ModuleSpan] -> Map.Map Text [Text] -> Map.Map Node (Double, [Node])
genGraph mode mss mod_deps = final_map
  where
    home_mods_map = Map.fromList (concatMap genEvents mss)
    mod_deps' = mkDependencyTree mode mod_deps
    final_map = Map.merge Map.dropMissing Map.dropMissing (Map.zipWithMatched (\_ time mods -> (time, mods))) home_mods_map mod_deps'


-- Start/End is in milliseconds
data ModuleSpan = ModuleSpan { name :: Text
                             , start :: Double
                             , rn :: Double
                             , tc :: Double
                             , cgstart :: Double
                             , cgend :: Double
                             , end :: Double } deriving (Generic, Show)

deriving instance ToJSON ModuleSpan

data Marker = StartComp | RN | TC | CGStart | CGEnd | EndComp deriving (Show, Ord, Eq, Enum)

data Phase = Parse | TypeCheck | CodeGen deriving (Show, Enum, Ord, Eq)

data Node = Node Phase Text deriving (Show, Ord, Eq)

data StartOrEnd = Start Phase | End Phase deriving Show

getStartOrEndMarker :: StartOrEnd -> Marker
getStartOrEndMarker (Start m) = phaseStart m
getStartOrEndMarker (End m) = phaseEnd m

getTimeSorE :: ModuleSpan -> StartOrEnd -> Double
getTimeSorE ms sore = getTime ms (getStartOrEndMarker sore)

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

match :: [(Text, Marker, Double)] -> [ModuleSpan]
match xss = go (normalise (sort xss))
  where
    -- Account for if codegen runs multiple times
    normalise ((l, CGStart, d):(_, CGStart, _):xs) = normalise ((l, CGStart, d) : xs)
    normalise ((_, CGEnd, _):(l, CGEnd, d):xs) = normalise ((l, CGEnd, d) : xs)
    normalise (x:xs) = x : normalise xs
    normalise [] = []

    go :: [(Text, Marker, Double)] -> [ModuleSpan]
    go [] = []
    go xs =
        let (prefix, ys) = splitAt (length markers) xs
            ([start, rn, tc, cgstart, cgend, end], leftover) = check markers prefix
            (l, _, _) = head prefix
        in ModuleSpan {name = l,.. } : go (leftover ++ ys)

    -- Only *needs* the Start/End events, the other events are filled in with End
    -- if they are missing. This is convenient for hs-boot files which don't have
    -- a codegen phase
    check :: [Marker] -> [(Text, Marker, Double)] -> ([Double], [(Text, Marker, Double)])
    check (expected_dir:dirs) ((_l, dir, ts):xs)
      | expected_dir == dir = let (rest, leftover) = check dirs xs
                              in (ts: rest, leftover)
      | otherwise = if dir == EndComp then (replicate (length dirs + 1) ts, xs) else error "No"
    check _ _ = ([], [])

data EL = EL { offset :: Double, caps :: !Int,  spans :: ![(Text, Marker, Double)], mods :: !(Map.Map Text [Text]) }

nanoToMilli :: Integral n => n -> Double
nanoToMilli n = (fromIntegral n) / 1_000_000

process :: EventLog -> EL
process (EventLog _ (Data es)) = foldl' processEvent (EL undefined 1 [] Map.empty) es

processEvent :: EL -> Event -> EL
processEvent el (Event t ei _) = processEventInfo t ei el


processEventInfo :: Timestamp -> EventInfo -> EL -> EL
processEventInfo t (WallClockTime _ s ns) el = el { offset = ((fromIntegral s * 1_000) + (nanoToMilli ns)) - nanoToMilli t }
processEventInfo _t (CapCreate _n) el = el { caps = 1 + (caps el) }
processEventInfo t (UserMarker m) el = do
  let o = offset el
  case parseMessage m of
    Just (m', d) -> el { spans = (m', d, nanoToMilli t + o) : spans el }
    Nothing -> case parseMod m of
                 Just (m', deps) -> el { mods = Map.insert m' deps (mods el) }
                 Nothing -> el


processEventInfo _ _ el = el


parseMod :: Text -> Maybe (Text, [Text])
parseMod t = do
  ["MOD", modu, deps] <- return $ T.splitOn ":" t
  return (modu, (map T.pack (read (T.unpack deps))))

parseMessage :: Text -> Maybe (Text, Marker)
parseMessage t = do
  (raw_dir: modu:_) <- return $ T.splitOn ":" t
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
genPyDag :: Int -> Double -> [(Text, Marker, Double)] -> Map.Map Node (Double, [Node]) -> String
genPyDag caps total_time spans dag =
  let edges = Map.toList dag
      renderNode (Node p l ) = T.pack (show p) <> ":" <> l
      mkEdge :: Text -> Text -> Double -> String
      mkEdge from_edge to_edge weight = "G.add_edge (" ++ show from_edge ++ "," ++ show to_edge ++ ", weight=" ++ show weight ++ (if weight>0 then ", capacity=1" else "") ++ ")"
      mkOne (t, (w, ds)) =
        let internal_node = "END:" <> renderNode t
        in (mkEdge (renderNode t) internal_node w) : map (\end -> mkEdge internal_node (renderNode end) 0) ds
         -- Intermediate edge
      added_edges = unlines (concatMap mkOne edges)


      t0 = minimum [t | (_, _, t) <- spans ]
      spans' = sortBy (comparing (\(_, _, c) -> c)) spans
      mkEvent (l, d, t) = (t - t0, l,  show d)
      events = "events = " ++ show (map mkEvent spans')
  in pyScript caps total_time added_edges events

{-
for node, outdegree in G.out_degree(G.nodes()):
    if outdegree == 0:
        G.add_edge(node, "SINK", weight=0.01)

for node, indegree in G.in_degree(G.nodes()):
    if indegree == 0:
        G.add_edge("SOURCE", node, weight=0.01)
        -}


rawPScript :: Text
rawPScript = decodeUtf8 $(embedFile "analyse_template.py")



pyScript :: Int -> Double -> String -> String -> String
pyScript _caps _total_time added events = unlines $
  [ "import networkx as nx"
  , "import matplotlib.pyplot as plt"
  , "import matplotlib.animation as animation"
  , "import itertools"
  , "import math"
  , "G = nx.DiGraph()"
  , added
  , events
  , T.unpack $ rawPScript ]

{-
  , "longest = nx.dag_longest_path(G)"
  , "longest_length = nx.dag_longest_path_length(G)"
  , "print(longest)"
  , "total_time = sum([d['weight'] for (u, v, d) in G.edges(data=True)])"
  , "print(\"Critical path length: {0:.2f}s\".format(longest_length / 1000))"
  , "print(\"Actual time: {0:.2f}s\".format(" ++ show total_time ++ " / 1000))"
  , "caps = " ++ show caps
  , "print(\"Theoretical minimum build time ({0:d} cores): {1:.2f}s\".format(caps, (total_time / (1000 * caps))))"
  ]
  -}

data SimState = SimState { n_jobs :: !Int
                         , start_period :: !Double
                         , durs :: !(Map.Map Int Double)
                         , active :: !(Set.Set Text)
                         , lags :: ![(Text, Double)]
                         } deriving Show

-- An accurate simulation of what actually happened
simulate :: [(Text, StartOrEnd, Double)] -> SimState
simulate spans = foldl' go (SimState 0 t0 Map.empty Set.empty mempty) spans'
  where
    spans' = sortBy (comparing (\(_, _, c) -> c)) spans
    (_, _, t0) = head spans'


    go :: SimState -> (Text, StartOrEnd, Double) -> SimState
    go (SimState{..}) (node, dir, time)  =
      let period_time = time - start_period
          durs' = Map.insertWith (+) n_jobs period_time durs
          n_jobs' = if isStart dir then n_jobs + 1 else n_jobs - 1
          active' = if isStart dir then Set.insert node active else Set.delete node active
          start_period' = time
          lags' = case Set.toList active of
                   [single_node] -> (single_node, period_time) : lags
                   _ -> lags
      in SimState n_jobs' start_period' durs' active' lags'

isStart :: StartOrEnd -> Bool
isStart (Start _) = True
isStart (End _) = False

data HypState = HypState { cur_time :: Double
                         , capsFree  :: Int
                         , ready :: [(Node, Double)]
                         , queue :: PS.OrdPSQ Node Int (Node, Double) -- Invariant, nothing of priority 0 in here.
                         , action :: PS.OrdPSQ Node Double ()
                         , traces :: [(Text, StartOrEnd, Double)]
                         } deriving Show

-- Assuming k capabilities, how fast could we have gone?
hypothetical :: Mode -> Map.Map Text [Text] -> [ModuleSpan] -> Int -> [(Text, StartOrEnd, Double)]
hypothetical mode deps mss caps = reverse $ traces $ go (HypState 0 caps initReady initialQueue PS.empty [])
  where

    mod_deps' :: Map.Map Node [Node]
    mod_deps' = mkDependencyTree mode deps

    revDeps :: Map.Map Node [Node]
    revDeps = Map.fromListWith (++) $ concatMap (\(m, ms) -> map (,[m]) ms) $ Map.toList mod_deps'
    numDeps k m = maybe 0 length (Map.lookup (Node k (name m)) mod_deps')
    initReady = map (\(_, _, c) -> c) initReady'

    (initReady', initialQueue) =
      PS.atMostView 0 (PS.fromList (concatMap (\ms -> [(Node p (name ms), numDeps p ms, (Node p (name ms), getPhaseTime ms p)) | p <- phases]) mss))

    mkAction :: Double -> (Node, Double) -> (Text, StartOrEnd, Double)
    mkAction cur_time (Node d l, t) =
        let k = (l, End d, end_time)
            end_time = cur_time + t
        in k

    mkStart :: Double -> (Node, a) -> (Text, StartOrEnd, Double)
    mkStart cur_time (Node d l, _) = (l, Start d, cur_time)


    alterDeps :: PS.OrdPSQ Node Int a -> Node -> (PS.OrdPSQ Node Int a, Maybe a)
    alterDeps ps l = swap $ PS.alter f l ps
      where
        f Nothing = error ("Bad key:" ++ show l)
        f (Just (p, v)) = if p == 1 then (Just v, Nothing)
                                     else (Nothing, Just (p - 1, v))

    go :: HypState -> HypState
    go h@(HypState{..}) =
      case capsFree of
        -- caps free, and jobs ready, start them
        c | not (null ready) && c > 0 ->
          let (new_jobs, todo_jobs) = splitAt c ready -- Could use a smarter scheduler
              new_jobs_n = length new_jobs
              new_free_caps = c - new_jobs_n
              action' = foldr (\(l,t) am -> (PS.insert l (cur_time + t) () am)) action new_jobs
              starts  = map (mkStart cur_time) new_jobs
              ends  = map (mkAction cur_time) new_jobs
              new_traces = ends ++ starts ++ traces
          in go (HypState{capsFree = new_free_caps, ready = todo_jobs, traces = new_traces, action = action', .. })
        -- Otherwise, advance time until a job finishes and release the cap
        _ -> case PS.minView action of
              Nothing -> h -- We are done, no more actions to perform but free capabilities.
              Just (l, t, (), action') ->
                let rev_deps_l = fromMaybe [] (Map.lookup l revDeps)
                    (new_queue, new_ready) = mapAccumL alterDeps queue rev_deps_l
                    new_free_caps = capsFree + 1
                in go (HypState{ capsFree = new_free_caps, queue = new_queue, ready = catMaybes new_ready ++ ready, cur_time = t, action = action', .. })










