module ConcLog (openConcLog, ConcLog (), closeConcLog, logString, logValue, runLog, handleToConc) where

import Control.Concurrent
import Control.Concurrent.Chan
import qualified Data.ByteString.Char8 as BS
import System.IO
import System.Time

data ConcLog = ConcLog {inp :: Chan (BS.ByteString, ClockTime), hdl :: Handle, thd :: ThreadId}

handleToConc :: Handle -> IO ConcLog
handleToConc hd = do
    ch <- newChan
    th <- forkIO (runLogger ch hd)
    return (ConcLog ch hd th)

logValue :: Show a => ConcLog -> a -> IO ()
logValue ccl v = logString ccl (BS.pack $ show v)

logString :: ConcLog -> BS.ByteString -> IO ()
logString (ConcLog inp _ _) str = do
    clk <- getClockTime
    writeChan inp (str, clk)

openConcLog :: BS.ByteString -> IO ConcLog
openConcLog pth = do
    hd <- openFile (BS.unpack pth) AppendMode
    handleToConc hd

closeConcLog :: ConcLog -> IO ()
closeConcLog (ConcLog _ hdl thd) = do
    killThread thd
    hClose hdl

runLog :: BS.ByteString -> (ConcLog -> IO a) -> IO a
runLog str act = do
    ccl <- openConcLog str
    ans <- act ccl
    closeConcLog ccl
    return ans

runLogger :: Chan (BS.ByteString, ClockTime) -> Handle -> IO ()
runLogger chn hdl = do
    (str, tm) <- readChan chn
    cal <- toCalendarTime tm
    BS.hPutStrLn hdl (BS.concat [BS.pack ("[" ++ (calendarTimeToString cal) ++ "]\n"), str])
    hFlush hdl
    runLogger chn hdl
