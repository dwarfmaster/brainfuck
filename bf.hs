
import Data.Char

type BFMachine = ([Int], [Int])
data BFOp = Op Char | Code BFCode
type BFCode = [BFOp]

-- Showing oprations
op_show :: BFOp -> String
op_show (Op c)   = "(Op " ++ show c ++ ")"
op_show (Code c) = "(Code " ++ show c ++ ")"
instance Show BFOp where
    show = op_show

-- Operations on the BF machine
op_add :: Int -> BFMachine -> BFMachine
op_add _ p@([], _)    = p
op_add i (hd:hds, tl) = ((hd+i):hds, tl)

op_plus  = op_add 1
op_minus = op_add (-1)

op_next :: BFMachine -> BFMachine
op_next p@(_, [])    = p
op_next (hd, tl:tls) = (tl:hd, tls)

op_prev :: BFMachine -> BFMachine
op_prev p@([], _)    = p
op_prev (hd:hds, tl) = (hds, hd:tl)

op_check :: BFMachine -> Bool
op_check p@([], _) = False
op_check (hd:_, _) = if hd /= 0 then True else False

op_set :: Int -> BFMachine -> BFMachine
op_set _ p@([], _)    = p
op_set i (hd:hds, tl) = (i:hds, tl)

op_get :: BFMachine -> Int
op_get p@([], _) = 0
op_get (hd:_, _) = hd

-- Print a bit
op_print :: BFMachine -> IO BFMachine
op_print m = do putStrLn $ show $ op_get m
                return m

op_pchar :: BFMachine -> IO BFMachine
op_pchar m = do putChar $ chr $ op_get m
                return m

-- TODO : fix input code
op_read :: BFMachine -> IO BFMachine
op_read m = do putStr "Input: "
               c <- getChar
               return $ op_set (read [c]) m

-- Executing the code
exec_code :: BFMachine -> BFCode -> IO BFMachine
exec_code m []      = return m
exec_code m (op:tl) = do m2 <- exec_op m op
                         exec_code m2 tl

show_m :: BFMachine -> String
show_m (hd, tl) = show hd ++ " -- " ++ show (take 10 tl)

exec_op :: BFMachine -> BFOp -> IO BFMachine
exec_op m (Code c) = if op_get m == 0 then return m
                                       else do m2 <- exec_code m c
                                               exec_op m2 (Code c)
exec_op m (Op c)   = case c of
 '>'       -> return $ op_next  m
 '<'       -> return $ op_prev  m
 '+'       -> return $ op_plus  m
 '-'       -> return $ op_minus m
 '.'       -> op_print m
 ':'       -> op_pchar m
 ','       -> op_read  m
 otherwise -> return m

-- Reading the code
_read_op :: String -> (String,BFCode)
_read_op ""    = ("", [])
_read_op (c:s) = case c of
 '['       -> (rrs, Code rc : rrc)
  where (rs, rc)   = _read_op s
        (rrs, rrc) = _read_op rs
 ']'       -> (s, [])
 otherwise -> (rs, Op c : rc)
  where (rs, rc) = _read_op s

read_op :: String -> BFCode
read_op s = c
 where (_,c) = _read_op s

-- Main program
main :: IO ()
main = do s <- readFile "code.bf"
          let code = read_op s
          exec_code ([0], [0,0..]) code
          return ()

