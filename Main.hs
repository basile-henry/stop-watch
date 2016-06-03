{-# LANGUAGE DataKinds, GADTs #-}

module Main where

import           CLaSH.Prelude
import           SevenSeg

topEntity :: Signal (Bit, Bit, Bit) -> Signal SevenSegsOut
topEntity = sevenSegs . stopWatch . getInputs

data Clock = Clock
    { hours :: Unsigned 6
    , mins  :: Unsigned 7
    , ticks :: Unsigned 14 -- 250 ticks in a second, 15000 in a min
    }

data Input  = None
            | Next
            | Set
            | Reset

data ClockStatus = View
                 | SetMins
                 | SetHours

data TimerStatus = Pause
                 | Play

data StopWatch = StopWatch
    { clock     :: (Clock, ClockStatus)
    , timer     :: (Clock, TimerStatus)
    , current   :: Bit
    }

getInputs :: Signal (Bit, Bit, Bit) -> Signal Input
getInputs = getInputs' . unbundle
    where
        getInputs' :: (Signal Bit, Signal Bit, Signal Bit) -> Signal Input
        getInputs' (n, s, r) = select <$> bundle (isRising 1 n, isRising 1 s, isRising 1 r)

        select :: (Bool, Bool, Bool) -> Input
        select (True, _   , _   ) = Next
        select (_   , True, _   ) = Set
        select (_   , _   , True) = Reset
        select _                  = None

stopWatch :: Signal Input -> Signal SevenSegs
stopWatch = mealy stopWatchT $ StopWatch
    (Clock 0 0 0, View)
    (Clock 0 0 0, Pause)
    0

stopWatchT :: StopWatch -> Input -> (StopWatch, SevenSegs)
stopWatchT sw i = let sw' = updateStopWatch sw i in (sw', viewStopWatch sw')

updateStopWatch :: StopWatch -> Input -> StopWatch
updateStopWatch (StopWatch c t cu) Next = StopWatch (updateClock c Next) (updateTimer t Next) (complement cu)
updateStopWatch (StopWatch c t cu) None = StopWatch (updateClock c None) (updateTimer t None) cu
updateStopWatch (StopWatch c t 0)  i    = StopWatch (updateClock c i)    (updateTimer t None) 0
updateStopWatch (StopWatch c t 1)  i    = StopWatch (updateClock c None) (updateTimer t i)    1

updateClock :: (Clock, ClockStatus) -> Input -> (Clock, ClockStatus)
updateClock (c          , View)     Reset = (c                          , SetMins)
updateClock (c          , SetMins)  Reset = (c                          , SetHours)
updateClock (c          , SetHours) Reset = (c                          , View)
updateClock (Clock h m _, SetMins)  Set   = (Clock h (mod (m + 1) 60) 0 , SetMins)
updateClock (Clock h m _, SetHours) Set   = (Clock (mod (h + 1) 60) m 0 , SetHours)
updateClock (c          , View)     _     = (tickClock c                , View)
updateClock (Clock h m t, cs)       _     = (Clock h m $ mod (t + 1) 250, cs) -- t used for blinking

updateTimer :: (Clock, TimerStatus) -> Input -> (Clock, TimerStatus)
updateTimer (c, Play)  Set   = (c          , Pause)
updateTimer (c, Pause) Set   = (c          , Play)
updateTimer _          Reset = (Clock 0 0 0, Pause)
updateTimer (c, Play)  _     = (tickTimer c, Play)
updateTimer ct         _     = ct

viewStopWatch :: StopWatch -> SevenSegs
viewStopWatch (StopWatch c _ 0) = viewClock c
viewStopWatch (StopWatch _ t 1) = viewTimer t

viewClock :: (Clock, ClockStatus) -> SevenSegs
viewClock (Clock h m _, View)     = view (Just h, Just m)
viewClock (Clock h m t, SetMins)  = view (Just h, if t < 125 then Just m else Nothing)
viewClock (Clock h m t, SetHours) = view (if t < 125 then Just h else Nothing, Just m)

viewTimer :: (Clock, TimerStatus) -> SevenSegs
viewTimer (Clock h m _, Play)  = view (Just h, Just m)
viewTimer (Clock h m t, Pause) = view $ if t < 125 then (Just h, Just m) else (Nothing, Nothing)

view :: (Maybe (Unsigned 6), Maybe (Unsigned 7)) -> SevenSegs
view (u, l) = SevenSegs s3v s2v s1v s0v
    where
        -- view' :: Maybe (Unsigned n) -> (SevenSeg, SevenSeg)
        view' (Just n) = let (h, m) = divMod n 10 in
            (SevenSeg (Just $ slice d3 d0 h) 0, SevenSeg (Just $ slice d3 d0 m) 0)
        view' Nothing  = (SevenSeg Nothing 0, SevenSeg Nothing 0)

        (s3v, s2v) = view' u
        (s1v, s0v) = view' l

tickClock :: Clock -> Clock
tickClock (Clock h m t)
    | t < 14999 = Clock h m (t + 1)
    | m < 59    = Clock h (m + 1) 0
    | h < 23    = Clock (h + 1) 0 0
    | otherwise = Clock 0 0 0

tickTimer :: Clock -> Clock
tickTimer (Clock h m t)
    | t < 5     = Clock h m (t + 1)
    | m < 99    = Clock h (m + 2) 0
    | h < 59    = Clock (h + 1) 0 0
    | otherwise = Clock 0 0 0
